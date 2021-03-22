//! Implementation of the number schema
//!
//! When binary data is exchanged a common objective is to reduce the data size.
//! For this the IEEE-754 formats are not suited.
//! To reduce the size of a floating-point number linear interpolation can be used to fit a number into a smaller integer.
//!
//! # Parameters
//!
//! | Key           | Type     | Default  | Comment |
//! | ------------- | --------:| --------:| ------- |
//! | `"byteorder"` | `string` | "bigendian" | The order in which the bytes are encoded. |
//! | `"length"`    |   `uint` |        4 | Number of bytes of the encoded number |
//! | `"signed"`    |   `bool` |     true | Whether the number is signed or not |
//! | `"bits"`      |   `uint` | optional | Number of bits the [bitfield] covers |
//! | `"bitoffset"` |   `uint` | optional | Number of bits the [bitfield] is shifted |
//! | `"scale"`     | `double` | optional | Factor to scale the encoded value |
//! | `"offset"`    | `double` | optional | Offset for the encoded value |
//!
//! ## Validation
//!
//! If none of the optional parameters are provided only a length of 4 or 8 byte are valid (single and double precision floating-point numbers, IEEE 754).
//! However, there are methods to [encode floating-point numbers as integers](#linear-interpolation).
//!
//! # Features
//!
//! ## Linear Interpolation
//!
//! Linear interpolation is performed if `"bits"`, `"bitoffset"`, `"scale"` or `"offset"` are set.
//! If `"scale"` is not set the default value 1.0 is assumed.
//! If `"offset"` is not set the default value 0.0 is assumed.
//!
//! For linear interpolation the parameters `"scale"` and `"offset"` are used.
//! The formulae are as follows:
//!
//! - Encoding: `encoded_value = (json_value - offset) / scale`
//! - Decoding: `json_value = scale * encoded_value + offset`
//!
//! An interpolated value is encoded with the integer schema defined by the number schema.
//! Accordingly, it is also possible to encode an interpolated value in a [bitfield].
//!
//! ### Example
//!
//! The schema describes that a floating-point JSON value is encoded in the lower 11 bits (max 2047) with a scale of 0.001 and an offset of 1.6.
//! The calculation to encode 3.0:
//!
//! ```text
//! (3.0 - 1.6) / 0.001 = 1400 = 0b0000_0101_0111_1000 = encoded_value
//! ```
//!
//! ```
//! # use binary_data_schema::*;
//! # use valico::json_schema;
//! # use serde_json::{json, from_value};
//! let schema = json!({
//!     "type": "number",
//!     "offset": 1.6,
//!     "scale": 0.001,
//!     "length": 2,
//!     "bits": 11
//! });
//!
//! let mut scope = json_schema::Scope::new();
//! let j_schema = scope.compile_and_return(schema.clone(), false)?;
//! let schema = from_value::<DataSchema>(schema)?;
//!
//! let value = json!(3.0);
//! assert!(j_schema.validate(&value).is_valid());
//! let mut encoded = Vec::new();
//! schema.encode(&mut encoded, &value)?;
//! let expected = [ 0b0000_0101, 0b0111_1000 ];
//! assert_eq!(&expected, encoded.as_slice());
//!
//! let mut encoded = std::io::Cursor::new(encoded);
//! let back = schema.decode(&mut encoded)?;
//! assert!(j_schema.validate(&back).is_valid());
//! // This would fail due to rounding errors
//! // assert_eq!(back, value);
//! # Ok::<(), anyhow::Error>(())
//! ```
//!
//! [bitfield]: ../object/index.html#bitfields

use std::{convert::TryFrom, io};

use byteorder::{ReadBytesExt, WriteBytesExt, BE, LE};
use serde::{
    de::{Deserializer, Error as DeError},
    Deserialize,
};
use serde_json::Value;

use crate::{integer::RawIntegerSchema, ByteOrder, Decoder, Encoder, IntegerSchema};

/// Errors validating a [NumberSchema].
#[derive(Debug, thiserror::Error)]
pub enum ValidationError {
    #[error("Invalid bitfield: {0}")]
    InvalidIntergerSchema(#[from] crate::integer::ValidationError),
    #[error("The requsted length of {requested} is invalid for floating-point serialization. Either use 4 or 8 or use an integer-based encoding")]
    InvalidFloatingLength { requested: usize },
}

/// Errors encoding a string with a [NumberSchema].
#[derive(Debug, thiserror::Error)]
pub enum EncodingError {
    #[error("The value '{value}' can not be encoded with a number schema")]
    InvalidValue { value: String },
    #[error("Writing to buffer failed: {0}")]
    WriteFail(#[from] io::Error),
    #[error("Failed to encode as integer: {0}")]
    Integer(#[from] crate::integer::EncodingError),
}

/// Errors decoding a string with a [NumberSchema].
#[derive(Debug, thiserror::Error)]
pub enum DecodingError {
    #[error("Reading encoded data failed: {0}")]
    ReadFail(#[from] io::Error),
    #[error("Failed to decode underlying integer: {0}")]
    Integer(#[from] crate::integer::DecodingError),
}

impl DecodingError {
    pub fn due_to_eof(&self) -> bool {
        matches!(self, Self::ReadFail(e) if e.kind() == std::io::ErrorKind::UnexpectedEof)
    }
}

/// Raw version of a number schema. May hold invalid invariants.
#[derive(Debug, Clone, Deserialize)]
#[serde(rename_all = "lowercase")]
struct RawNumber {
    #[serde(flatten, default)]
    int: RawIntegerSchema,
    scale: Option<f64>,
    offset: Option<f64>,
}

/// The number schema describes a numeric value (further information on [the module's documentation](index.html)).
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
    /// Apply scale and offset to the value.
    pub fn to_binary_value(&self, value: f64) -> i64 {
        match self {
            NumberSchema::Integer { scale, offset, .. } => {
                ((value - *offset) / *scale).round() as _
            }
            _ => value as _,
        }
    }
    /// Apply scale and offset to the value.
    pub fn from_binary_value(&self, value: f64) -> f64 {
        match self {
            NumberSchema::Integer { scale, offset, .. } => value * *scale + *offset,
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
    type Error = ValidationError;

    fn try_from(raw: RawNumber) -> Result<Self, Self::Error> {
        if raw.scale.is_some()
            || raw.offset.is_some()
            || raw.int.bit_offset.is_some()
            || raw.int.bits.is_some()
        {
            let integer = IntegerSchema::try_from(raw.int)?;
            Ok(NumberSchema::Integer {
                integer,
                scale: raw.scale.unwrap_or(DEFAULT_SCALE),
                offset: raw.offset.unwrap_or(DEFAULT_OFFSET),
            })
        } else {
            match raw.int.length {
                4 => Ok(NumberSchema::Float {
                    byteorder: raw.int.byteorder,
                }),
                8 => Ok(NumberSchema::Double {
                    byteorder: raw.int.byteorder,
                }),
                _ => Err(ValidationError::InvalidFloatingLength {
                    requested: raw.int.length,
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
        NumberSchema::try_from(raw).map_err(D::Error::custom)
    }
}

impl Encoder for NumberSchema {
    type Error = EncodingError;

    fn encode<W>(&self, target: &mut W, value: &Value) -> Result<usize, Self::Error>
    where
        W: io::Write + WriteBytesExt,
    {
        let value = value.as_f64().ok_or_else(|| EncodingError::InvalidValue {
            value: value.to_string(),
        })?;
        let length = match self {
            NumberSchema::Integer { integer, .. } => {
                let value = self.to_binary_value(value).into();
                integer.encode(target, &value)?
            }
            NumberSchema::Float { byteorder } => {
                let value = value as f32;
                match byteorder {
                    ByteOrder::LittleEndian => target.write_f32::<LE>(value)?,
                    ByteOrder::BigEndian => target.write_f32::<BE>(value)?,
                }
                4
            }
            NumberSchema::Double { byteorder } => {
                let value = value;
                match byteorder {
                    ByteOrder::LittleEndian => target.write_f64::<LE>(value)?,
                    ByteOrder::BigEndian => target.write_f64::<BE>(value)?,
                }
                8
            }
        };

        Ok(length)
    }
}

impl Decoder for NumberSchema {
    type Error = DecodingError;

    fn decode<R>(&self, target: &mut R) -> Result<Value, Self::Error>
    where
        R: io::Read + ReadBytesExt,
    {
        let value = match self {
            NumberSchema::Integer { integer, .. } => {
                let int = integer
                    .decode(target)?
                    .as_f64()
                    .expect("always works on integer schemata");
                self.from_binary_value(int).into()
            }
            NumberSchema::Float { byteorder } => match byteorder {
                ByteOrder::LittleEndian => target.read_f32::<LE>()?.into(),
                ByteOrder::BigEndian => target.read_f32::<BE>()?.into(),
            },
            NumberSchema::Double { byteorder } => match byteorder {
                ByteOrder::LittleEndian => target.read_f64::<LE>()?.into(),
                ByteOrder::BigEndian => target.read_f64::<BE>()?.into(),
            },
        };

        Ok(value)
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use anyhow::Result;
    use serde_json::{from_value, json};

    /// Enough for this tests.
    fn eq_fload(f1: f64, f2: f64) -> bool {
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
        let json: Value = value.into();
        let value_as_bin = 1250_f64;
        assert!(eq_fload(
            value_as_bin,
            number2int.to_binary_value(value) as f64
        ));
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
        assert_eq!(2, number2int.encode(&mut buffer, &json)?);
        assert_eq!(2, buffer.len());
        assert_eq!(4, number2float.encode(&mut buffer, &json)?);
        assert_eq!(2 + 4, buffer.len());
        assert_eq!(8, number2double.encode(&mut buffer, &json)?);
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
        let json1: Value = value1.into();
        let value2 = 3.0;
        let json2: Value = value2.into();
        let mut buffer = [0; 2];

        assert_eq!(2, voltage.encode(&mut buffer.as_mut(), &json1)?);
        let res = u16::from_be_bytes(buffer);
        assert_eq!(0, res);

        assert_eq!(2, voltage.encode(&mut buffer.as_mut(), &json2)?);
        let res = u16::from_be_bytes(buffer);
        let diff = 1400 - ((res >> 5) as i16);
        assert!(diff < 3 && diff > -3);

        Ok(())
    }

    #[test]
    fn bitfield2() -> Result<()> {
        let schema = json!({
            "type": "number",
            "offset": -40,
            "scale": 2,
            "length": 2,
            "bits": 5,
            "bitoffset": 0,
            "position": 50,
            "unit": "dBm",
            "description": "Transmission power in 1m distance."
        });
        let schema = from_value::<NumberSchema>(schema)?;

        println!("schema:\n{:#?}", schema);

        Ok(())
    }

    #[test]
    fn bitfield3() -> Result<()> {
        let schema = json!({
            "type": "number",
            "offset": 1.6,
            "scale": 0.001,
            "length": 2,
            "bits": 11,
            "bitoffset": 5,
            "position": 50,
            "unit": "volts",
            "description": "Voltage of the battery powering the RuuviTag."
        });
        let schema = from_value::<NumberSchema>(schema)?;

        println!("schema:\n{:#?}", schema);

        Ok(())
    }
}
