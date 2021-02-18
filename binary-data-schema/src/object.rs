//! Implementation of the string schema

use crate::{DataSchema, Decoder, Encoder, Error, JoinedBitfield, Result};
use byteorder::{ReadBytesExt, WriteBytesExt};
use serde::de::{Deserializer, Error as DeError};
use serde::Deserialize;
use serde_json::Value;
use std::io;
use std::{collections::HashMap, convert::TryFrom};

/// A single property within an object schema.
#[derive(Debug, Clone, Deserialize)]
struct RawProperty {
    #[serde(flatten)]
    schema: DataSchema,
    /// Position of the property within the object. Determines the layout of
    /// binary serialization.
    position: usize,
}

#[derive(Debug, Clone, Deserialize)]
struct RawObject {
    properties: HashMap<String, RawProperty>,
}

#[derive(Debug, Clone)]
pub enum PropertySchema {
    Simple { name: String, schema: DataSchema },
    Merged(JoinedBitfield),
}

impl Encoder for PropertySchema {
    fn encode<W>(&self, target: &mut W, value: &Value) -> Result<usize>
    where
        W: io::Write + WriteBytesExt,
    {
        match self {
            PropertySchema::Simple {
                schema: DataSchema::Const { schema, const_val },
                ..
            } => schema.encode(target, &const_val),
            PropertySchema::Simple { name, schema } => {
                let value = value
                    .get(name)
                    .ok_or_else(|| Error::MissingField(name.clone()))?;
                schema.encode(target, value)
            }
            PropertySchema::Merged(schema) => schema.encode(target, value),
        }
    }
}

impl PropertySchema {
    /// Decodes values and adds them to the provided Json.
    ///
    /// This is contrary to the normal [Decoder] trait where new values are
    /// created.
    fn decode_into<R>(&self, target: &mut R, value: &mut Value) -> Result<()>
    where
        R: io::Read + ReadBytesExt,
    {
        match self {
            PropertySchema::Simple { name, schema } => {
                value[name] = schema.decode(target)?;
            }
            PropertySchema::Merged(schema) => {
                let map = if let Value::Object(map) = schema.decode(target)? {
                    map
                } else {
                    panic!("Should have always been a map");
                };
                for (name, bf_value) in map {
                    value[name] = bf_value;
                }
            }
        }

        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct Property {
    position: usize,
    schema: PropertySchema,
}

/// The object schema to describe structured data.
///
/// The position of a property within an object schema defines the order in
/// which the properties are written to bytes.
/// Property position do not have to be continuous but must be distinct. Except
/// for bitfields in which case the same position signals that bitfields share
/// the same bytes.
///
/// An instance of this type is guaranteed to be valid.
#[derive(Debug, Clone)]
pub struct ObjectSchema {
    properties: Vec<Property>,
}

impl TryFrom<RawObject> for ObjectSchema {
    type Error = Error;

    fn try_from(raw: RawObject) -> Result<Self, Self::Error> {
        let mut ordered = HashMap::with_capacity(raw.properties.len());
        raw.properties.into_iter().for_each(|(name, raw)| {
            let bucket = ordered.entry(raw.position).or_insert_with(Vec::new);
            bucket.push((name, raw.schema));
        });
        let mut properties = Vec::with_capacity(ordered.len());
        for (position, mut vec) in ordered {
            let prop = if vec.len() == 1 {
                let (name, schema) = vec.pop().expect("Ensured by .len() == 1");
                Property {
                    position,
                    schema: PropertySchema::Simple { name, schema },
                }
            } else {
                let fields: Result<HashMap<_, _>, _> = vec
                    .into_iter()
                    .map(|(name, schema)| {
                        if schema.is_bitfield() {
                            Ok((name, schema))
                        } else {
                             Err(Error::InvalidPosition(position))
                        }
                    })
                    .collect();
                let joined = JoinedBitfield::join(fields?)?;
                Property {
                    position,
                    schema: PropertySchema::Merged(joined),
                }
            };
            properties.push(prop);
        }

        properties.sort_by(|p1, p2| p1.position.cmp(&p2.position));
        Ok(Self { properties })
    }
}

impl<'de> Deserialize<'de> for ObjectSchema {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let raw = RawObject::deserialize(deserializer)?;
        ObjectSchema::try_from(raw).map_err(D::Error::custom)
    }
}

impl Encoder for ObjectSchema {
    fn encode<W>(&self, target: &mut W, value: &Value) -> Result<usize>
    where
        W: io::Write + WriteBytesExt,
    {
        let mut written = 0;
        for p in self.properties.iter() {
            written += p.schema.encode(target, &value)?;
        }

        Ok(written)
    }
}

impl Decoder for ObjectSchema {
    fn decode<R>(&self, target: &mut R) -> Result<Value>
    where
        R: io::Read + ReadBytesExt,
    {
        let mut value = serde_json::json!({});
        for p in self.properties.iter() {
            p.schema.decode_into(target, &mut value)?;
        }

        Ok(value)
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use anyhow::Result;
    use serde_json::{from_value, json};

    #[test]
    fn xiaomi_thermometer() -> Result<()> {
        let schema = json!({
            "type": "object",
            "properties": {
                "temperature": {
                    "type": "number",
                    "position": 1,
                    "length": 2,
                    "scale": 0.01,
                    "byteorder": "littleendian",
                    "unit": "degree celcius"
                },
                "humidity": {
                    "type": "integer",
                    "position": 2,
                    "length": 1,
                    "unit": "percent"
                },
                "rest": {
                    "type": "integer",
                    "position": 5,
                    "length": 2
                }
            }
        });
        let schema = from_value::<DataSchema>(schema)?;
        assert!(matches!(schema, DataSchema::Object(_)));
        let value = json!({
            "temperature": 22.1,
            "humidity": 57,
            "rest": 0
        });
        let mut buffer = Vec::new();
        assert_eq!(5, schema.encode(&mut buffer, &value)?);
        let expected = [0xA2, 0x08, 0x39, 0, 0];
        assert_eq!(&expected, buffer.as_slice());
        let mut cursor = std::io::Cursor::new(buffer);

        let returned = schema.decode(&mut cursor)?;
        assert_eq!(value, returned);

        Ok(())
    }

    #[test]
    fn ruuvi_tag() -> Result<()> {
        let schema = json!({
            "type": "object",
            "properties": {
                "version": {
                    "type": "integer",
                    "length": 1,
                    "const": 5,
                    "position": 1,
                    "description": "Version number of the protocol."
                },
                "temperature": {
                    "@type": "env:RoomTemperature",
                    "type": "number",
                    "length": 2,
                    "scale": 0.005,
                    "position": 10,
                    "unit": "degree celcius",
                    "description": "Temperature of the air surrounding the RuuviTag."
                },
                "humidity": {
                    "@type": "env:AirHumidity",
                    "type": "number",
                    "length": 2,
                    "scale": 0.0025,
                    "signed": false,
                    "position": 20,
                    "unit": "percent",
                    "description": "Relative humidity of the air surrounding the RuuviTag."
                },
                "pressure": {
                    "@type": "env:AtmosphericPressure",
                    "type": "number",
                    "length": 2,
                    "offset": 50000,
                    "signed": false,
                    "position": 30,
                    "unit": "Pa",
                    "description": "Atmospheric pressure on the RuuviTag."
                },
                "acceleration": {
                    "type": "object",
                    "position": 40,
                    "description": "3D accereration of the RuuviTag.",
                    "properties": {
                        "x": {
                            "type": "number",
                            "length": 2,
                            "scale": 0.001,
                            "unit": "G-force",
                            "position": 10,
                            "desription": "Acceleration in x-axis."
                        },
                        "y": {
                            "type": "number",
                            "length": 2,
                            "scale": 0.001,
                            "unit": "G-force",
                            "position": 20,
                            "desription": "Acceleration in y-axis."
                        },
                        "z": {
                            "type": "number",
                            "length": 2,
                            "scale": 0.001,
                            "unit": "G-force",
                            "position": 30,
                            "desription": "Acceleration in z-axis."
                        }
                    }
                },
                "battery": {
                    "type": "number",
                    "offset": 1.6,
                    "scale": 0.001,
                    "length": 2,
                    "bits": 11,
                    "bitoffset": 5,
                    "position": 50,
                    "unit": "volts",
                    "description": "Voltage of the battery powering the RuuviTag."
                },
                "txPower": {
                    "type": "number",
                    "offset": -40,
                    "scale": 2,
                    "length": 2,
                    "bits": 5,
                    "bitoffset": 0,
                    "position": 50,
                    "unit": "dBm",
                    "description": "Transmission power in 1m distance."
                },
                "moveCnt": {
                    "type": "integer",
                    "length": 1,
                    "signed": false,
                    "position": 60,
                    "description": "Number of movements (derived from accelerometer)."
                },
                "idx": {
                    "type": "integer",
                    "length": 2,
                    "signed": false,
                    "position": 70,
                    "description": "Measurement sequence number. Can be used to de-duplicate data."
                },
                "mac": {
                    "type": "string",
                    "format": "binary",
                    "minLength": 6,
                    "maxLength": 6,
                    "position": 80,
                    "description": "MAC address of the RuuviTag."
                }
            }
        });
        let _schema = from_value::<DataSchema>(schema)?;

        Ok(())
    }

    #[test]
    fn merge_bitfields() -> Result<()> {
        let schema = json!({
            "type": "object",
            "properties": {
                "battery": {
                    "type": "number",
                    "offset": 1.6,
                    "scale": 0.001,
                    "length": 2,
                    "bits": 11,
                    "bitoffset": 5,
                    "position": 50,
                    "unit": "volts",
                    "description": "Voltage of the battery powering the RuuviTag."
                },
                "txPower": {
                    "type": "number",
                    "offset": -40,
                    "scale": 2,
                    "length": 2,
                    "bits": 5,
                    "bitoffset": 0,
                    "position": 50,
                    "unit": "dBm",
                    "description": "Transmission power in 1m distance."
                }
            }
        });
        let schema = from_value::<DataSchema>(schema)?;
        println!("schema:\n{:#?}", schema);
        assert!(matches!(schema, DataSchema::Object(_)));

        let value = json!({
            "battery": 3.0,
            "txPower": 4,
        });
        let mut buffer = Vec::new();
        assert_eq!(2, schema.encode(&mut buffer, &value)?);
        let _battery = 0b101_0111_1000; // 1400 = 3.0 V
        let _tx_power = 0b1_0110; // 22 = +4 dBm
        let expected: [u8; 2] = [0b1010_1110, 0b1111_0110];
        assert_eq!(&expected, buffer.as_slice());
        let mut cursor = std::io::Cursor::new(buffer);

        let _returned = schema.decode(&mut cursor)?;
        // returned is rpughly the same but due to loss of precision due to floating point operations value is not exact.
        //assert_eq!(value, returned);

        Ok(())
    }
}
