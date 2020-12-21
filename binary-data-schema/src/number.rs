//! Implementation of the number schema

use crate::{BinarySchema, ByteOrder, Error, IntegerSchema, Length, RawIntegerSchema, Result};
use byteorder::WriteBytesExt;
use serde::de::{Deserializer, Error as DeError};
use serde::Deserialize;
use std::convert::TryFrom;
use std::io;

/// Raw version of a number schema. May hold invalid invariants.
#[derive(Debug, Clone, Deserialize)]
#[serde(rename_all = "lowercase")]
struct RawNumber {
    #[serde(flatten, default)]
    raw_int: RawIntegerSchema,
    scale: Option<f64>,
    offset: Option<f64>,
}

/// The number schema describes a numeric value.
#[derive(Debug, Clone)]
pub enum NumberSchema {
    Integer {
        integer: IntegerSchema,
        scale: f64,
        offset: f64,
    },
    Float {
        byteorder: ByteOrder,
    },
    Double {
        byteorder: ByteOrder,
    },
}

const DEFAULT_LENGTH: usize = 4;
const DEFAULT_SCALE: f64 = 1_f64;
const DEFAULT_OFFSET: f64 = 0_f64;

impl NumberSchema {
    /// Default length is 8 bytes like a double-precision floating-point number.
    pub fn default_length() -> usize {
        DEFAULT_LENGTH
    }
    /// Default scale is neutral, i.e. `1.0`.
    pub fn default_scale() -> f64 {
        DEFAULT_SCALE
    }
    /// Default offset is neutral, i.e. `0.0`.
    pub fn default_offset() -> f64 {
        DEFAULT_OFFSET
    }
    pub fn new_integer(integer: IntegerSchema, scale: f64, offset: f64) -> Self {
        NumberSchema::Integer {
            integer,
            scale,
            offset,
        }
    }
    pub fn new_raw(length: usize, byteorder: ByteOrder) -> Result<Self> {
        match length {
            4 => Ok(NumberSchema::Float { byteorder }),
            8 => Ok(NumberSchema::Double { byteorder }),
            _ => Err(Error::InvalidFloatingLength { requested: length }),
        }
    }
    /// Apply scale and offset to the value.
    pub fn to_binary_value(&self, value: f64) -> f64 {
        match self {
            NumberSchema::Integer { scale, offset, .. } => {
                let res = (value - *offset) / *scale;
                // println!("to_binary: ({} - {}) / {} = {}", value, *offset, *scale, res);
                res
            }
            _ => value,
        }
    }
    /// Apply scale and offset to the value.
    pub fn from_binary_value(&self, value: f64) -> f64 {
        match self {
            NumberSchema::Integer { scale, offset, .. } => {
                let res = value * *scale + *offset;
                // println!("from_binary: {} * {} + {} = {}", value, *scale, *offset, res);
                res
            }
            _ => value,
        }
    }
    pub fn length(&self) -> usize {
        match self {
            NumberSchema::Integer { integer, .. } => integer.length(),
            NumberSchema::Float { .. } => 4,
            NumberSchema::Double { .. } => 8,
        }
    }
}

impl TryFrom<RawNumber> for NumberSchema {
    type Error = Error;

    fn try_from(value: RawNumber) -> Result<Self, Self::Error> {
        if value.scale.is_some() || value.offset.is_some() {
            let integer = IntegerSchema::try_from(value.raw_int)?;
            Ok(NumberSchema::Integer {
                integer,
                scale: value.scale.unwrap_or(DEFAULT_SCALE),
                offset: value.offset.unwrap_or(DEFAULT_OFFSET),
            })
        } else {
            match value.raw_int.length {
                4 => Ok(NumberSchema::Float {
                    byteorder: value.raw_int.byteorder,
                }),
                8 => Ok(NumberSchema::Double {
                    byteorder: value.raw_int.byteorder,
                }),
                _ => Err(Error::InvalidFloatingLength {
                    requested: value.raw_int.length,
                }),
            }
        }
    }
}

impl<'de> Deserialize<'de> for NumberSchema {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let raw = RawNumber::deserialize(deserializer)?;
        NumberSchema::try_from(raw).map_err(|e| D::Error::custom(e))
    }
}

impl BinarySchema for NumberSchema {
    type Value = f64;

    fn length_encoded(&self) -> Length {
        match self {
            NumberSchema::Integer { integer, .. } => integer.length_encoded(),
            NumberSchema::Float { .. } => Length::Fixed(4),
            NumberSchema::Double { .. } => Length::Fixed(8),
        }
    }
    fn encoded_size(&self, value: &Self::Value) -> usize {
        match self {
            NumberSchema::Integer { integer, .. } => integer.encoded_size(&(*value as _)),
            NumberSchema::Float { .. } => 4,
            NumberSchema::Double { .. } => 8,
        }
    }
    fn encode<W>(&self, target: W, value: &Self::Value) -> Result<usize>
    where
        W: io::Write + WriteBytesExt,
    {
        let mut target = target;
        let length = match self {
            NumberSchema::Integer { integer, .. } => {
                let value = self.to_binary_value(*value) as _;
                integer.encode(target, &value)?
            }
            NumberSchema::Float { byteorder } => {
                let value = *value as f32;
                let bytes = if *byteorder == ByteOrder::BigEndian {
                    value.to_be_bytes()
                } else {
                    value.to_le_bytes()
                };
                target.write_all(&bytes)?;
                4
            }
            NumberSchema::Double { byteorder } => {
                let value = *value;
                let bytes = if *byteorder == ByteOrder::BigEndian {
                    value.to_be_bytes()
                } else {
                    value.to_le_bytes()
                };
                target.write_all(&bytes)?;
                8
            }
        };

        Ok(length)
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use anyhow::Result;
    use serde_json::{from_value, json};

    /// Enough for this tests.
    fn eq_floaf(f1: f64, f2: f64) -> bool {
        (f1 - f2).abs() < 0.000_001
    }

    #[test]
    fn encode() -> Result<()> {
        let schema = json!({
            "scale": 0.01,
            "offset": 10,
            "byteorder": "littleendian",
            "length": 2
        });
        let number2int: NumberSchema = from_value(schema)?;
        let schema = json!({});
        let number2float: NumberSchema = from_value(schema)?;
        let schema = json!({
            "length": 8,
            "byteorder": "littleendian"
        });
        let number2double: NumberSchema = from_value(schema)?;
        let schema = json!({"length": 3});
        assert!(from_value::<NumberSchema>(schema).is_err());

        let value = 22.5;
        let value_as_bin = 1250_f64;
        assert!(eq_floaf(value_as_bin, number2int.to_binary_value(value)));
        let value_int_le = (value_as_bin as i16).to_le_bytes();
        let value_float_be = (value as f32).to_be_bytes();
        let value_double_le = value.to_le_bytes();
        let expected: Vec<u8> = value_int_le
            .iter()
            .chain(value_float_be.iter())
            .chain(value_double_le.iter())
            .copied()
            .collect();

        let mut buffer: Vec<u8> = vec![];
        assert_eq!(2, number2int.encode(&mut buffer, &value)?);
        assert_eq!(2, buffer.len());
        assert_eq!(4, number2float.encode(&mut buffer, &value)?);
        assert_eq!(2 + 4, buffer.len());
        assert_eq!(8, number2double.encode(&mut buffer, &value)?);
        assert_eq!(2 + 4 + 8, buffer.len());

        assert_eq!(buffer, expected);

        Ok(())
    }

    /// This example is the battery voltage from Ruuvi's RAWv2 protocol.
    #[test]
    fn bitfield() -> Result<()> {
        let schema = json!({
            "offset": 1.6,
            "scale": 0.001,
            "length": 2,
            "bits": 11,
            "bitoffset": 5
        });
        let voltage = from_value::<NumberSchema>(schema)?;
        let value1 = 1.6;
        let value2 = 3.0;
        let mut buffer = [0; 2];

        assert_eq!(2, voltage.encode(buffer.as_mut(), &value1)?);
        let res = u16::from_be_bytes(buffer);
        assert_eq!(0, res);

        assert_eq!(2, voltage.encode(buffer.as_mut(), &value2)?);
        let res = u16::from_be_bytes(buffer);
        let diff = 1400 - ((res >> 5) as i16);
        assert!(diff < 3 && diff > -3);

        Ok(())
    }
}
