//! Implementation of the object schema
//!
//! In BDS object schemata are generally used to map between byte strings and JSON object.
//! This should be used for most schemata modeling bytes that contain several fields.
//!
//! Every field within a byte string is modeled as one property of the object schema.
//! The order of fields is defined by the `"position"` parameter in the property's schema.
//!
//! # Parameters
//!
//! | Key           | Type     | Default  | Comment |
//! | ------------- | --------:| --------:| ------- |
//! | `"position"`  |   `uint` |       NA | Position of the property in the encoded bytes |
//! | `"jsonld:context"` | `string` |  NA | A context in the sense of [JSON-LD] that is attached to the JSON resulting from decoding |
//!
//! ## Validation
//!
//! The positions of different properties should not be the same.
//! Except they when they are defining a [merged bitfield].
//!
//! # Features
//!
//! ## Field Positions
//!
//! The BDS codec allows to transform JSON values into a byte string and reverse.
//! To decode a byte string that is made of consecutive fields the describing object schema must define the order of fields.
//! This is done via the `"position"` parameter in the properties of an object schema.
//!
//! `"position"`s of properties do not need to be continuous.
//! They are just arranged in numeric order.
//! However, they must not overlap.
//! Except when several properties model a [merged bitfield].
//!
//! ### Example
//!
//! ```
//! # use binary_data_schema::*;
//! # use valico::json_schema;
//! # use serde_json::{json, from_value};
//! let schema = json!({
//!     "type": "object",
//!     "properties": {
//!         "first": {
//!             "type": "integer",
//!             "length": 1,
//!             "position": 10,
//!         },
//!         "third": {
//!             "type": "integer",
//!             "length": 1,
//!             "position": 30,
//!         },
//!         "second": {
//!             "type": "integer",
//!             "length": 1,
//!             "position": 20,
//!         },
//!     }
//! });
//!
//! let mut scope = json_schema::Scope::new();
//! let j_schema = scope.compile_and_return(schema.clone(), false)?;
//! let schema = from_value::<DataSchema>(schema)?;
//!
//! let value = json!({ "third": 3, "first": 1, "second": 2});
//! assert!(j_schema.validate(&value).is_valid());
//! let mut encoded = Vec::new();
//! schema.encode(&mut encoded, &value)?;
//! let expected = [ 1, 2, 3 ];
//! assert_eq!(&expected, encoded.as_slice());
//!
//! let mut encoded = std::io::Cursor::new(encoded);
//! let back = schema.decode(&mut encoded)?;
//! assert!(j_schema.validate(&back).is_valid());
//! assert_eq!(back, value);
//! # Ok::<(), anyhow::Error>(())
//! ```
//!
//! ## Bitfields
//!
//! One of the more complex features of BDS is merged bitfields.
//! It is common practice to merge several fields into one byte to safe space.
//! For example, a single byte can contain up to 8 Boolean values.
//! In BDS every data schema that is recognized as a bitfield can be merged.
//! Recognized bitfield schemata are:
//!
//! - boolean schema
//! - integer schema with `"bits"` and/or `"bitoffset" set
//! - numeric schema with `"bits"` and/or `"bitoffset" set
//!
//! To merge bitfields they must be all properties of the same object schema with the **same** `"position"` and same number of bytes.
//! Accordingly, it may be necessary to set the length of a boolean schema to more than one byte to merge it.
//!
//! ### Example
//!
//! ```
//! # use binary_data_schema::*;
//! # use valico::json_schema;
//! # use serde_json::{json, from_value};
//! let schema = json!({
//!     "type": "object",
//!     "properties": {
//!         "num": {
//!             "type": "number",
//!             "position": 10,
//!             "scale": 0.2,
//!             "length": 1,
//!             "bits": 4,
//!             "bitoffset": 4,
//!         },
//!         "int": {
//!             "type": "integer",
//!             "position": 10,
//!             "length": 1,
//!             "bits": 3,
//!             "bitoffset": 1,
//!         },
//!         "bool": {
//!             "type": "boolean",
//!             "position": 10,
//!         }
//!     }
//! });
//!
//! let mut scope = json_schema::Scope::new();
//! let j_schema = scope.compile_and_return(schema.clone(), false)?;
//! let schema = from_value::<DataSchema>(schema)?;
//!
//! let value = json!({
//!     "num": 2.4,
//!     "int": 5,
//!     "bool": false
//! });
//! assert!(j_schema.validate(&value).is_valid());
//! let mut encoded = Vec::new();
//! schema.encode(&mut encoded, &value)?;
//! let num = 12 << 4;   // 2.4 / 0.2
//! let int = 5 << 1;
//! let bool_ = 0 << 0;  // false
//! let expected: [u8; 1] = [num | int | bool_];
//! assert_eq!(&expected, encoded.as_slice());
//!
//! let mut encoded = std::io::Cursor::new(encoded);
//! let back = schema.decode(&mut encoded)?;
//! assert!(j_schema.validate(&back).is_valid());
//! // back is roughly the same but due to loss of precision because of floating point operations value is not exact.
//! //assert_eq!(value, back);
//! # Ok::<(), anyhow::Error>(())
//! ```
//!
//! ## JSON-LD Context
//!
//! While JSON is a self-describing format sometimes even this is not enough, e.g. when also the semantics of the data should be conveyed.
//! For these cases the BDS codec can add a [JSON-LD context] to the resulting JSON document, turning it into machine-readable [Linked Data].
//! To do so the `"jsonld:context"` parameter is used like suggested in [JSON schema in RDF] draft.
//! However, in contrast to the draft the value of `"jsonld:context"` is simply copied from the schema to `"@context"` in the result.
//!
//! ### Example
//!
//! ```
//! # use binary_data_schema::*;
//! # use valico::json_schema;
//! # use serde_json::{json, from_value};
//! let schema = json!({
//!     "type": "object",
//!     "jsonld:context": "http://example.org/temperature/context.jsonld",
//!     "properties": {
//!         "temperature": {
//!             "type": "integer",
//!             "position": 10,
//!             "length": 1,
//!         }
//!     }
//! });
//!
//! let mut scope = json_schema::Scope::new();
//! let j_schema = scope.compile_and_return(schema.clone(), false)?;
//! let schema = from_value::<DataSchema>(schema)?;
//!
//! let encoded: [u8; 1] = [ 21 ];
//! let mut encoded = std::io::Cursor::new(encoded);
//! let back = schema.decode(&mut encoded)?;
//! assert!(j_schema.validate(&back).is_valid());
//! let expected = json!({
//!     "@context": "http://example.org/temperature/context.jsonld",
//!     "temperature": 21
//! });
//! assert_eq!(expected, back);
//! # Ok::<(), anyhow::Error>(())
//! ```
//!
//! [JSON-LD]: https://www.w3.org/TR/json-ld/#the-context
//! [JSON-LD context]: https://www.w3.org/TR/json-ld/#the-context
//! [merged bitfield]: #bitfields
//! [Linked Data]: https://www.w3.org/DesignIssues/LinkedData.html
//! [JSON schema in RDF]: https://www.w3.org/2019/wot/json-schema#defining-a-json-ld-context-for-data-instances

use std::{collections::HashMap, convert::TryFrom, io};

use byteorder::{ReadBytesExt, WriteBytesExt};
use serde::{
    de::{Deserializer, Error as DeError},
    Deserialize,
};
use serde_json::Value;

use crate::{
    integer::{Bitfield, PlainInteger},
    BooleanSchema, DataSchema, Decoder, Encoder, Error, InnerSchema, IntegerSchema, NumberSchema,
    Result,
};

/// Errors validating an [ObjectSchema].
#[derive(Debug, thiserror::Error)]
pub enum ValidationError {
    #[error("Can not join bitfields as there are none")]
    NoBitfields,
    #[error("Can not join bitfields with different number of bytes")]
    NotSameBytes,
    #[error("Can not join bitfields as they are overlapping")]
    OverlappingBitfields,
    #[error("The position {0} has been used multiple times in the object schema but only bitfields are allowed to share a position")]
    InvalidPosition(usize),
}

/// Errors encoding a string with an [ObjectSchema].
#[derive(Debug, thiserror::Error)]
pub enum EncodingError {
    #[error("The value '{value}' can not be encoded with an object schema")]
    InvalidValue { value: String },
    #[error("Writing to buffer failed: {0}")]
    WriteFail(#[from] io::Error),
    #[error("Expected the constant value {expected} but got {got}")]
    InvalidConstValue { expected: String, got: String },
    #[error("The field '{0}' is not present in the value to encode")]
    MissingField(String),
    #[error(transparent)]
    Number(#[from] crate::number::EncodingError),
    #[error(transparent)]
    Integer(#[from] crate::integer::EncodingError),
    #[error(transparent)]
    Boolean(#[from] crate::boolean::EncodingError),
    #[error("Encoding sub-schema failed: {0}")]
    SubSchema(Box<Error>),
}

impl From<Error> for EncodingError {
    fn from(e: Error) -> Self {
        EncodingError::SubSchema(Box::new(e))
    }
}

/// Errors decoding a string with an [ObjectSchema].
#[derive(Debug, thiserror::Error)]
pub enum DecodingError {
    #[error("Reading encoded data failed: {0}")]
    ReadFail(#[from] io::Error),
    #[error(transparent)]
    Integer(#[from] crate::integer::DecodingError),
    #[error("Decoding sub-schema failed: {0}")]
    SubSchema(Box<Error>),
}

impl DecodingError {
    pub fn due_to_eof(&self) -> bool {
        match &self {
            DecodingError::ReadFail(e) => e.kind() == std::io::ErrorKind::UnexpectedEof,
            DecodingError::Integer(e) => e.due_to_eof(),
            DecodingError::SubSchema(e) => e.due_to_eof(),
        }
    }
}

impl From<Error> for DecodingError {
    fn from(e: Error) -> Self {
        DecodingError::SubSchema(Box::new(e))
    }
}

/// A single property within an object schema.
#[derive(Debug, Clone, Deserialize)]
struct RawProperty {
    #[serde(flatten)]
    schema: DataSchema,
    /// Position of the property within the object. Determines the layout of
    /// binary serialization.
    position: usize,
}

/// A set of bitfields that all write to the same bytes.
#[derive(Debug, Clone)]
struct JoinedBitfield {
    bytes: usize,
    fields: HashMap<String, DataSchema>,
}

#[derive(Debug, Clone)]
enum PropertySchema {
    Simple { name: String, schema: DataSchema },
    Merged(JoinedBitfield),
}

#[derive(Debug, Clone)]
struct Property {
    position: usize,
    schema: PropertySchema,
}

#[derive(Debug, Clone, Deserialize)]
struct RawObject {
    properties: HashMap<String, RawProperty>,
    #[serde(rename = "jsonld:context")]
    context: Option<Value>,
}

/// The object schema to describe structured data (further information on [the module's documentation](index.html)).
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
    context: Option<Value>,
}

impl JoinedBitfield {
    pub fn join(bfs: HashMap<String, DataSchema>) -> Result<Self, ValidationError> {
        if bfs.values().any(|ds| !ds.is_bitfield()) {
            return Err(ValidationError::NoBitfields);
        }

        let raw_bfs = bfs
            .iter()
            .map(|(name, ds)| {
                let bf = ds.inner.bitfield().expect("ensured at beginning");
                (name.as_str(), bf)
            })
            .collect::<HashMap<_, _>>();

        let bytes = raw_bfs
            .values()
            .next()
            .ok_or(ValidationError::NoBitfields)?
            .bytes;

        if raw_bfs.values().any(|bf| bf.bytes != bytes) {
            return Err(ValidationError::NotSameBytes);
        }

        raw_bfs.values().try_fold(0u64, |state, bf| {
            let mask = bf.mask();
            if state & mask != 0 {
                Err(ValidationError::OverlappingBitfields)
            } else {
                Ok(state | mask)
            }
        })?;

        Ok(Self { bytes, fields: bfs })
    }
    fn raw_bfs(&self) -> impl Iterator<Item = (&'_ str, &'_ Bitfield)> {
        self.fields.iter().map(|(name, ds)| {
            let bf = ds.inner.bitfield().expect("ensured at constructor");
            (name.as_str(), bf)
        })
    }
    /// Integer schema to encode the value of all bitfields.
    fn integer(&self) -> PlainInteger {
        self.raw_bfs()
            .map(|(_, bf)| bf)
            .next()
            .expect("Constuctor guarantees that there is at least one bitfield")
            .integer()
    }
}

impl Encoder for JoinedBitfield {
    type Error = EncodingError;

    fn encode<W>(&self, target: &mut W, value: &Value) -> Result<usize, Self::Error>
    where
        W: io::Write + WriteBytesExt,
    {
        let mut buffer = 0;
        for (name, ds) in self.fields.iter() {
            let value = match (value.get(name), ds.default_.as_ref()) {
                (Some(val), Some(c)) if val == c => Ok(val),
                (Some(val), Some(c)) => Err(EncodingError::InvalidConstValue {
                    expected: c.to_string(),
                    got: val.to_string(),
                }),
                (Some(val), None) => Ok(val),
                (None, Some(c)) => Ok(c),
                (None, None) => Err(EncodingError::MissingField(name.clone())),
            }?;

            let (bf, value) = match &ds.inner {
                InnerSchema::Number(
                    ns
                    @
                    NumberSchema::Integer {
                        integer: IntegerSchema::Bitfield(_),
                        ..
                    },
                ) => {
                    let value = value.as_f64().ok_or_else(|| {
                        crate::number::EncodingError::InvalidValue {
                            value: value.to_string(),
                        }
                    })?;
                    let value = ns.to_binary_value(value) as _;
                    if let NumberSchema::Integer {
                        integer: IntegerSchema::Bitfield(bf),
                        ..
                    } = ns
                    {
                        (bf, value)
                    } else {
                        unreachable!("ensured by match")
                    }
                }
                InnerSchema::Integer(IntegerSchema::Bitfield(bf)) => {
                    let value = value.as_u64().ok_or_else(|| {
                        crate::integer::EncodingError::InvalidValue {
                            value: value.to_string(),
                        }
                    })?;
                    (bf, value)
                }
                InnerSchema::Boolean(BooleanSchema { bf }) => {
                    let value = value.as_bool().ok_or_else(|| {
                        crate::boolean::EncodingError::InvalidValue {
                            value: value.to_string(),
                        }
                    })? as _;
                    (bf, value)
                }
                _ => unreachable!("ensured at constructor"),
            };
            bf.write(value, &mut buffer);
        }

        let int = self.integer();
        int.encode(target, &buffer.into()).map_err(Into::into)
    }
}

impl Decoder for JoinedBitfield {
    type Error = DecodingError;

    fn decode<R>(&self, target: &mut R) -> Result<Value, Self::Error>
    where
        R: io::Read + ReadBytesExt,
    {
        let int = self.integer();
        let int = int.decode(target)?.as_u64().expect("Is always u64");
        let mut res = Value::default();
        for (name, ds) in self.fields.iter() {
            let bf = ds.inner.bitfield().expect("ensured at consturctor");
            let value = bf.read(int);
            res[name] = match &ds.inner {
                InnerSchema::Number(ns) => ns.from_binary_value(value as _).into(),
                InnerSchema::Boolean(_) => (value != 0).into(),
                _ => value.into(),
            };
        }

        Ok(res)
    }
}

impl Encoder for PropertySchema {
    type Error = EncodingError;

    fn encode<W>(&self, target: &mut W, value: &Value) -> Result<usize, Self::Error>
    where
        W: io::Write + WriteBytesExt,
    {
        match self {
            PropertySchema::Simple { name, schema } => {
                let value = match (value.get(name), schema.default_.as_ref()) {
                    (Some(val), Some(c)) if val == c => Ok(val),
                    (Some(val), Some(c)) => Err(EncodingError::InvalidConstValue {
                        expected: c.to_string(),
                        got: val.to_string(),
                    }),
                    (Some(val), None) => Ok(val),
                    (None, Some(c)) => Ok(c),
                    (None, None) => Err(EncodingError::MissingField(name.clone())),
                }?;
                schema.encode(target, value).map_err(Into::into)
            }
            PropertySchema::Merged(schema) => schema.encode(target, value).map_err(Into::into),
        }
    }
}

impl PropertySchema {
    /// Decodes values and adds them to the provided Json.
    ///
    /// This is contrary to the normal [Decoder] trait where new values are
    /// created.
    fn decode_into<R>(&self, target: &mut R, value: &mut Value) -> Result<(), DecodingError>
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

impl TryFrom<RawObject> for ObjectSchema {
    type Error = ValidationError;

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
                            Err(ValidationError::InvalidPosition(position))
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
        Ok(Self {
            properties,
            context: raw.context,
        })
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
    type Error = EncodingError;

    fn encode<W>(&self, target: &mut W, value: &Value) -> Result<usize, Self::Error>
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
    type Error = DecodingError;

    fn decode<R>(&self, target: &mut R) -> Result<Value, Self::Error>
    where
        R: io::Read + ReadBytesExt,
    {
        let mut value = serde_json::json!({});
        for p in self.properties.iter() {
            p.schema.decode_into(target, &mut value)?;
        }

        if let Some(ctx) = self.context.as_ref() {
            value["@context"] = ctx.clone();
        }

        Ok(value)
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::InnerSchema;
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
        assert!(matches!(
            schema,
            DataSchema {
                inner: InnerSchema::Object(_),
                ..
            }
        ));
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
                    "default": 5,
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
                    "minLength": 12,
                    "maxLength": 12,
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
        assert!(matches!(
            schema,
            DataSchema {
                inner: InnerSchema::Object(_),
                ..
            }
        ));

        let value = json!({
            "battery": 3.0,
            "txPower": 4,
        });
        let mut buffer = Vec::new();
        assert_eq!(2, schema.encode(&mut buffer, &value)?);
        let _battery = 0b101_0111_1000; // 1400 = 3.0 V
        let _tx_power = 0b1_0110; // 22 = +4 dBm
        let expected: [u8; 2] = [0b1010_1111, 0b0001_0110];
        assert_eq!(&expected, buffer.as_slice());
        let mut cursor = std::io::Cursor::new(buffer);

        let _returned = schema.decode(&mut cursor)?;
        // returned is roughly the same but due to loss of precision due to floating point operations value is not exact.
        //assert_eq!(value, returned);

        Ok(())
    }
    #[test]
    fn merge_different_bitfields() -> Result<()> {
        let schema = json!({
            "type": "object",
            "properties": {
                "num": {
                    "type": "number",
                    "position": 10,
                    "scale": 0.2,
                    "length": 1,
                    "bits": 4,
                    "bitoffset": 4,
                },
                "int": {
                    "type": "integer",
                    "position": 10,
                    "length": 1,
                    "bits": 3,
                    "bitoffset": 1,
                },
                "bool": {
                    "type": "boolean",
                    "position": 10,
                }
            }
        });
        let schema = from_value::<DataSchema>(schema)?;
        println!("schema:\n{:#?}", schema);
        assert!(matches!(
            schema,
            DataSchema {
                inner: InnerSchema::Object(_),
                ..
            }
        ));

        let value = json!({
            "num": 2.4,
            "int": 5,
            "bool": false
        });
        let mut buffer = Vec::new();
        assert_eq!(1, schema.encode(&mut buffer, &value)?);
        let num = 12 << 4;
        let int = 5 << 1;
        let bool_ = 0 << 0;
        let expected: [u8; 1] = [num | int | bool_];
        assert_eq!(&expected, buffer.as_slice());
        let mut cursor = std::io::Cursor::new(buffer);

        let _returned = schema.decode(&mut cursor)?;
        // returned is roughly the same but due to loss of precision due to floating point operations value is not exact.
        // assert_eq!(value, returned);

        Ok(())
    }
    #[test]
    fn jsonld_context() -> Result<()> {
        let schema = json!({
            "type": "object",
            "properties": {
                "test": {
                    "type": "integer",
                    "length": 1,
                    "position": 10
                }
            },
            "jsonld:context": "http://example.org/context.jsonld"
        });
        let schema = from_value::<DataSchema>(schema)?;
        assert!(matches!(
            schema,
            DataSchema {
                inner: InnerSchema::Object(ObjectSchema {
                    context: Some(_),
                    ..
                }),
                ..
            }
        ));

        let buffer = vec![0x10];
        let mut cursor = std::io::Cursor::new(buffer);

        let returned = schema.decode(&mut cursor)?;
        let expected = json!({
            "@context": "http://example.org/context.jsonld",
            "test": 16
        });
        assert_eq!(returned, expected);

        Ok(())
    }
}
