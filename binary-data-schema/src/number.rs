//! Implementation of the number schema

use crate::{BinarySchema, ByteOrder, Error, Integer, Length, Result};
use byteorder::WriteBytesExt;
use serde::de::{Deserializer, Error as DeError};
use serde::Deserialize;
use std::convert::TryFrom;
use std::io;

#[derive(Debug, Copy, Clone, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum EncodedType {
    Integer,
    Raw,
}

impl Default for EncodedType {
    fn default() -> Self {
        EncodedType::Raw
    }
}

/// Raw version of a number schema. May hold invalid invariants.
#[derive(Debug, Copy, Clone, Deserialize)]
#[serde(rename_all = "lowercase")]
struct RawNumber {
    #[serde(default)]
    binary_encoded: EncodedType,
    #[serde(default)]
    byteorder: ByteOrder,
    #[serde(default = "Number::default_length")]
    length: usize,
    #[serde(default = "Number::default_scale")]
    scale: f64,
    #[serde(default = "Number::default_offset")]
    offset: f64,
}

/// The number schema describes a numeric value.
#[derive(Debug, Clone, Copy)]
pub enum Number {
    Integer {
        integer: Integer,
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

const DEFAULT_LENGTH: usize = 8;
const DEFAULT_SCALE: f64 = 1_f64;
const DEFAULT_OFFSET: f64 = 0_f64;

impl Number {
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
    pub fn new_integer(integer: Integer, scale: f64, offset: f64) -> Self {
        Number::Integer {
            integer, scale, offset
        }
    }
    pub fn new_raw(length: usize, byteorder: ByteOrder) -> Result<Self> {
        match length {
            4 => Ok(Number::Float { byteorder }),
            8 => Ok(Number::Double { byteorder }),
            _ => Err(Error::InvalidFloatingLength {requested: length})
        }
    }
    /// Apply scale and offset to the value.
    pub fn to_binary_value(&self, value: f64) -> f64 {
        match self {
            Number::Integer {scale, offset, .. } => (value - *offset) / *scale,
            _ => value
        }
    }
    /// Apply scale and offset to the value.
    pub fn from_binary_value(&self, value: f64) -> f64 {
        match self {
            Number::Integer {scale, offset, .. } => value * scale + offset,
            _ => value
        }
    }
}

impl TryFrom<RawNumber> for Number {
    type Error = Error;

    fn try_from(value: RawNumber) -> Result<Self, Self::Error> {
        match value.binary_encoded {
            EncodedType::Integer => {
                let integer = Integer::new(value.byteorder, value.length, true)?;
                Ok(Number::Integer {
                    integer,
                    scale: value.scale,
                    offset: value.offset,
                })
            }
            EncodedType::Raw => match value.length {
                4 => Ok(Number::Float {
                    byteorder: value.byteorder,
                }),
                8 => Ok(Number::Double {
                    byteorder: value.byteorder,
                }),
                _ => Err(Error::InvalidFloatingLength {
                    requested: value.length,
                }),
            },
        }
    }
}

impl<'de> Deserialize<'de> for Number {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let raw = RawNumber::deserialize(deserializer)?;
        Number::try_from(raw).map_err(|e| D::Error::custom(e))
    }
}

impl BinarySchema for Number {
    type Value = f64;

    fn length_encoded(&self) -> Length {
        match self {
            Number::Integer { integer, .. } => integer.length_encoded(),
            Number::Float { .. } => Length::Fixed(4),
            Number::Double { .. } => Length::Fixed(8),
        }
    }
    fn encoded_size(&self, value: &Self::Value) -> usize {
        match self {
            Number::Integer { integer, .. } => integer.encoded_size(&(*value as _)),
            Number::Float { .. } => 4,
            Number::Double { .. } => 8,
        }
    }
    fn encode<W>(&self, target: W, value: &Self::Value) -> Result<usize>
    where
        W: io::Write + WriteBytesExt,
    {
        let mut target = target;
        let length = match self {
            Number::Integer {
                integer,
                ..
            } => {
                let value = self.to_binary_value(*value) as _;
                integer.encode(target, &value)?
            }
            Number::Float { byteorder } => {
                let value = *value as f32;
                let bytes = if *byteorder == ByteOrder::BigEndian {
                    value.to_be_bytes()
                } else {
                    value.to_le_bytes()
                };
                target.write_all(&bytes)?;
                4
            }
            Number::Double { byteorder } => {
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

    /// Enough for this tests.
    fn eq_floaf(f1: f64, f2: f64) -> bool {
        (f1 - f2).abs() < 0.000_001
    }

    #[test]
    fn encode() -> Result<()> {
        let int = Integer::new(ByteOrder::LittleEndian, 2, true)?;
        let number2int = Number::new_integer(int, 0.01, 10.0);
        let number2float = Number::new_raw(4, ByteOrder::BigEndian)?;
        let number2double = Number::new_raw(8, ByteOrder::LittleEndian)?;
        assert!(Number::new_raw(3, ByteOrder::BigEndian).is_err());

        let value = 22.5;
        let value_as_bin = 1250_f64;
        assert!(eq_floaf(value_as_bin, number2int.to_binary_value(value)));
        let value_int_le = (value_as_bin as i16).to_le_bytes();
        let value_float_be = (value as f32).to_be_bytes();
        let value_double_le = value.to_le_bytes();
        let expected: Vec<u8> = value_int_le.iter()
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
}
