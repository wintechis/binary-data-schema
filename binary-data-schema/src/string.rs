//! Implementation of the string schema
//!
//! Strings are not used very often when handling binary data.
//! However, strings have by nature no fixed length.
//! Accordingly, there are several ways to [encode the length](crate::LengthEncoding) of a string.
//!
//! Furthermore, hex-encoded strings can be used to give a more human-readable version of raw bytes.
//! Accordingly, the BDS codec allows to map between hex-encoded strings and raw bytes.
//!
//! # Length of a String
//!
//! The length of a string is the number of bytes required to store the UTF-8 string, e.g. `"ß"` is UTF-8 encoded as `0xC39F` so the length of `"ß"` is 2.
//!
//! # Parameters
//!
//! | Key           | Type     | Default  | Comment |
//! | ------------- | --------:| --------:| ------- |
//! | `"lengthEncoding"` | `object` | `{ "type": "tillend" }` | The way the length of the string is communicated |
//! | `"minLength"` |   `uint` | optional | Minimal length of the string |
//! | `"maxLength"` |   `uint` | optional | Maximal length of the string |
//! | `"format"`    | `string` | optional | Special format of the string |
//!
//! ## Validation
//!
//! `"lengthEncoding"` has its own validation rules (see [`LengthEncoding`](crate::LengthEncoding)).
//! This also includes the validity of the values of `"minLength"` and `"maxLength"`.
//!
//! At the moment the only value for `"format"` that is recognized by BDS is [`"binary"`] (cf. [binary format](#binary-format)).
//!
//! # Features
//!
//! ## Binary Format
//!
//! When handling binary data it is common to display such data as hex-encoded strings, e.g. `0d7f`.
//! BDS supports the format `"binary"` to simulate this.
//! Format `"binary"` means that a JSON string is limited to lower-case hex-digits `[a-f0-9]*` with an even length.
//! While encoding two hex-digits are encoded as one byte.
//! Accordingly, when decoding one byte in the byte string results in two hex-digits in the representing JSON string.
//!
//! ### Example
//!
//! ```
//! # use binary_data_schema::*;
//! # use valico::json_schema;
//! # use serde_json::{json, from_value};
//! let schema = json!({
//!     "type": "string",
//!     "format": "binary"
//! });
//!
//! let mut scope = json_schema::Scope::new();
//! let j_schema = scope.compile_and_return(schema.clone(), false)?;
//! let schema = from_value::<DataSchema>(schema)?;
//!
//! let value = json!("deadbeaf42");
//! assert!(j_schema.validate(&value).is_valid());
//! let mut encoded = Vec::new();
//! schema.encode(&mut encoded, &value)?;
//! let expected = [ 0xde, 0xad, 0xbe, 0xaf, 0x42 ];
//! assert_eq!(&expected, encoded.as_slice());
//!
//! let mut encoded = std::io::Cursor::new(encoded);
//! let back = schema.decode(&mut encoded)?;
//! assert!(j_schema.validate(&back).is_valid());
//! assert_eq!(back, value);
//! # Ok::<(), anyhow::Error>(())
//! ```
//!
//! ## Encoding Magic Bytes
//!
//! When binary data is used for communication less secure protocols tend to use special bytes to mark beginning and end of a message.
//! We recommend to model this kind of _magic_ bytes by combining the [`"default"` feature] and `"binary"`-format strings.
//!
//! ### Example
//!
//! ```
//! # use binary_data_schema::*;
//! # use valico::json_schema;
//! # use serde_json::{json, from_value};
//! let schema = json!({
//!     "type": "string",
//!     "format": "binary",
//!     "minLength": 4,
//!     "maxLength": 4,
//!     "default": "be42",
//! });
//!
//! let mut scope = json_schema::Scope::new();
//! let j_schema = scope.compile_and_return(schema.clone(), false)?;
//! let schema = from_value::<DataSchema>(schema)?;
//!
//! // Note: '{}' is not a valid value for the JSON schema
//! // but the interface requires a value.
//! let value = json!({});
//! let mut encoded = Vec::new();
//! schema.encode(&mut encoded, &value)?;
//! let expected = [ 0xbe, 0x42 ];
//! assert_eq!(&expected, encoded.as_slice());
//!
//! let mut encoded = std::io::Cursor::new(encoded);
//! let back = schema.decode(&mut encoded)?;
//! assert!(j_schema.validate(&back).is_valid());
//! let expected = json!("be42");
//! assert_eq!(back, expected);
//! # Ok::<(), anyhow::Error>(())
//! ```
//!
//! [`"binary"`]: https://swagger.io/docs/specification/data-models/data-types/#string
//! [`"default"` feature]: ../index.html#default

use std::{convert::TryFrom, io};

use bstr::{ByteSlice, ByteVec as _};
use byteorder::{ReadBytesExt, WriteBytesExt};
use serde::{
    de::{Deserializer, Error as DeError},
    Deserialize,
};
use serde_json::Value;

use crate::{
    util::{LengthEncoding, RawLengthEncoding},
    ArraySchema, Decoder, Encoder, IntegerSchema,
};

/// Errors validating a [StringSchema].
#[derive(Debug, thiserror::Error)]
pub enum ValidationError {
    #[error("The given end 'sentinel' or 'padding' is not of binary format: {0}")]
    NotHexPattern(#[from] hex::FromHexError),
    #[error("'sentinel' and 'padding' are limited to one byte but '{sentinel}' is longer")]
    InvalidPattern { sentinel: String },
    #[error("A fixed length string schema requires both 'maxLength' and 'minLength' given and having the same value")]
    IncompleteFixedLength,
    #[error("Length encoding 'capacity' requires 'maxLength'")]
    MissingCapacity,
    #[error("The provided sentinel or padding '{sentinel}' is not a string")]
    NotAString { sentinel: Value },
    #[error("Requested a fixed length or a capacity of {0}: Binary format strings have always an even length as bytes are mapped to two characters")]
    OddLimit(usize),
}

/// Errors encoding a string with a [StringSchema].
#[derive(Debug, thiserror::Error)]
pub enum EncodingError {
    #[error("The value '{value}' can not be encoded with a string schema")]
    InvalidValue { value: String },
    #[error("Writing to buffer failed: {0}")]
    WriteFail(#[from] io::Error),
    #[error("The provided string is not of 'binary' format: {0}")]
    InvalidHexString(#[from] hex::FromHexError),
    #[error("Encoding the value length failed: {0}")]
    LengthSchema(#[from] crate::integer::EncodingError),
    #[error("Encoding the binary data failed: {0}")]
    BinaryEncoding(#[from] crate::array::EncodingError),
    #[error("Length of {len} bytes but only a fixed length of {fixed} is supported")]
    NotFixedLength { len: usize, fixed: usize },
    #[error("Contains the end sequence {0}")]
    ContainsEndSequence(String),
    #[error("Length of {len} bytes but only values up to a length of {cap} are valid")]
    ExceedsCapacity { len: usize, cap: usize },
    #[error("Length of {len} bytes but only a length up to {max} bytes can be encoded")]
    ExceedsLengthEncoding { len: usize, max: usize },
}

/// Errors decoding a string with a [StringSchema].
#[derive(Debug, thiserror::Error)]
pub enum DecodingError {
    #[error("Reading encoded data failed: {0}")]
    ReadFail(#[from] io::Error),
    #[error("The encoded string is not valid UTF-8: {0}")]
    NonUtf8STring(#[from] std::string::FromUtf8Error),
    #[error("The encoded string is not valid UTF-8: {0}")]
    NonUtf8Bstr(#[from] bstr::FromUtf8Error),
    #[error("Decoding the value length failed: {0}")]
    LengthSchema(#[from] crate::integer::DecodingError),
    #[error("Decoding the binary data failed: {0}")]
    BinaryDecoding(#[from] crate::array::DecodingError),
    #[error("The encoded value '{read}' does not contain the endpattern '{pattern}'")]
    NoPattern { read: String, pattern: String },
}

impl DecodingError {
    pub fn due_to_eof(&self) -> bool {
        match &self {
            DecodingError::ReadFail(e) => e.kind() == std::io::ErrorKind::UnexpectedEof,
            DecodingError::LengthSchema(e) => e.due_to_eof(),
            _ => false,
        }
    }
}

#[derive(Debug, Clone, Copy, Deserialize, PartialEq, Eq)]
#[serde(rename_all = "lowercase")]
enum Format {
    #[serde(skip)]
    Utf8,
    Binary,
}

/// How is the length of variable sized data encoded.
#[derive(Debug, Clone, Deserialize)]
#[serde(rename_all = "camelCase")]
struct RawString {
    #[serde(default)]
    length_encoding: RawLengthEncoding,
    max_length: Option<usize>,
    min_length: Option<usize>,
    format: Option<Format>,
}

/// The string schema to describe string values (further information on [the module's documentation](index.html)).
#[derive(Debug, Clone)]
pub enum StringSchema {
    Utf8 { length: LengthEncoding<String> },
    Binary { inner: ArraySchema },
}

impl Format {
    fn validate_pattern(&self, sentinel: &str) -> Result<(), ValidationError> {
        match self {
            Format::Utf8 => {
                if sentinel.len() == 1 {
                    Ok(())
                } else {
                    Err(ValidationError::InvalidPattern {
                        sentinel: sentinel.to_owned(),
                    })
                }
            }
            Format::Binary => {
                let encoded = hex::decode(sentinel)?;
                if encoded.len() == 1 {
                    Ok(())
                } else {
                    Err(ValidationError::InvalidPattern {
                        sentinel: sentinel.to_owned(),
                    })
                }
            }
        }
    }
}

impl Default for Format {
    fn default() -> Self {
        Format::Utf8
    }
}

impl TryFrom<RawString> for StringSchema {
    type Error = ValidationError;

    fn try_from(raw: RawString) -> Result<Self, Self::Error> {
        let format = raw.format.unwrap_or_default();
        let length = match (raw.min_length, raw.max_length) {
            (Some(min), Some(max)) if min == max => Ok(LengthEncoding::Fixed(max)),
            _ => match raw.length_encoding {
                RawLengthEncoding::Fixed => Err(ValidationError::IncompleteFixedLength),
                RawLengthEncoding::ExplicitLength(schema) => {
                    Ok(LengthEncoding::LengthEncoded(schema))
                }
                RawLengthEncoding::EndPattern { sentinel } => {
                    let pat_str = sentinel
                        .as_str()
                        .ok_or_else(|| ValidationError::NotAString {
                            sentinel: sentinel.clone(),
                        })?;
                    format.validate_pattern(pat_str)?;
                    Ok(LengthEncoding::EndPattern { sentinel })
                }
                RawLengthEncoding::Capacity { padding } => {
                    let capacity = raw.max_length.ok_or(ValidationError::MissingCapacity)?;
                    let pad_str = padding
                        .as_str()
                        .ok_or_else(|| ValidationError::NotAString {
                            sentinel: padding.clone(),
                        })?;
                    format.validate_pattern(pad_str)?;
                    Ok(LengthEncoding::Capacity { capacity, padding })
                }
                RawLengthEncoding::TillEnd => Ok(LengthEncoding::TillEnd),
            },
        }?;
        let schema = match format {
            Format::Utf8 => Self::Utf8 {
                length: length
                    .map(|v| v.as_str().expect("ensured at length validation").to_owned()),
            },
            Format::Binary => {
                let mut length = length.map(|v| {
                    let string = v.as_str().expect("ensured at length validation");
                    let byte = hex::decode(string).expect("ensured at length validation");
                    byte[0].into()
                });
                match &mut length {
                    LengthEncoding::Fixed(number)
                    | LengthEncoding::Capacity {
                        capacity: number, ..
                    } => {
                        if *number % 2 == 1 {
                            return Err(ValidationError::OddLimit(*number));
                        }
                        *number /= 2;
                    }
                    _ => {}
                };
                Self::Binary {
                    inner: ArraySchema::byte_array(length).expect("ensured at length validation"),
                }
            }
        };
        Ok(schema)
    }
}

impl<'de> Deserialize<'de> for StringSchema {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let raw = RawString::deserialize(deserializer)?;
        Self::try_from(raw).map_err(D::Error::custom)
    }
}

impl Encoder for StringSchema {
    type Error = EncodingError;

    fn encode<W>(&self, target: &mut W, value: &Value) -> Result<usize, Self::Error>
    where
        W: io::Write + WriteBytesExt,
    {
        let value = value.as_str().ok_or_else(|| EncodingError::InvalidValue {
            value: value.to_string(),
        })?;
        let written = match &self {
            StringSchema::Utf8 { length } => match length {
                LengthEncoding::Fixed(len) => {
                    matches_fixed_len(value, *len)?;
                    target.write_all(value.as_bytes())?;
                    *len
                }
                LengthEncoding::LengthEncoded(int) => {
                    exceeds_length(value, int)?;
                    int.encode(target, &value.len().into())?;
                    target.write_all(value.as_bytes())?;
                    value.len() + int.length()
                }
                LengthEncoding::EndPattern { sentinel } => {
                    contains_end_sequencs(value, sentinel)?;
                    target.write_all(value.as_bytes())?;
                    target.write_all(sentinel.as_bytes())?;
                    value.len() + sentinel.len()
                }
                LengthEncoding::Capacity {
                    padding, capacity, ..
                } => {
                    exceeds_cap(value, *capacity)?;
                    target.write_all(value.as_bytes())?;
                    fill_rest(target, *capacity, value.len(), padding)?;
                    *capacity
                }
                LengthEncoding::TillEnd => {
                    target.write_all(value.as_bytes())?;
                    value.len()
                }
            },
            StringSchema::Binary { inner } => {
                let value = hex::decode(value)?;
                inner.encode(target, &(value.into()))?
            }
        };
        Ok(written)
    }
}

fn matches_fixed_len(value: &str, len: usize) -> Result<(), EncodingError> {
    if value.len() != len {
        Err(EncodingError::NotFixedLength {
            len: value.len(),
            fixed: len,
        })
    } else {
        Ok(())
    }
}

fn exceeds_length(value: &str, schema: &IntegerSchema) -> Result<(), EncodingError> {
    if value.len() > schema.max_value() {
        Err(EncodingError::ExceedsLengthEncoding {
            len: value.len(),
            max: schema.max_value(),
        })
    } else {
        Ok(())
    }
}

fn contains_end_sequencs(value: &str, pattern: &str) -> Result<(), EncodingError> {
    if value.contains(&pattern) {
        Err(EncodingError::ContainsEndSequence(pattern.to_owned()))
    } else {
        Ok(())
    }
}

fn exceeds_cap(value: &str, cap: usize) -> Result<(), EncodingError> {
    if value.len() > cap {
        Err(EncodingError::ExceedsCapacity {
            len: value.len(),
            cap,
        })
    } else {
        Ok(())
    }
}

fn fill_rest<W: io::Write>(
    target: W,
    cap: usize,
    filled: usize,
    filler: &str,
) -> Result<usize, EncodingError> {
    let mut target = target;
    let to_fill = cap - filled;
    for _ in 0..to_fill {
        // At validation it is ensured that `filler.len() == 1`
        target.write_all(filler.as_bytes())?;
    }
    Ok(to_fill)
}

impl Decoder for StringSchema {
    type Error = DecodingError;

    fn decode<R>(&self, target: &mut R) -> Result<Value, Self::Error>
    where
        R: io::Read + ReadBytesExt,
    {
        let value = match self {
            StringSchema::Utf8 { length } => {
                let bytes = match length {
                    LengthEncoding::Fixed(length) => read_with_length(target, *length)?,
                    LengthEncoding::LengthEncoded(schema) => {
                        let length = schema
                            .decode(target)?
                            .as_u64()
                            .expect("length is always u64");
                        read_with_length(target, length as _)?
                    }
                    LengthEncoding::EndPattern { sentinel: pattern } => {
                        read_with_pattern(target, pattern, usize::MAX)?
                    }
                    LengthEncoding::Capacity { padding, capacity } => {
                        read_with_pattern(target, padding, *capacity)?
                    }
                    LengthEncoding::TillEnd => {
                        let mut buf = Vec::new();
                        target.read_to_end(&mut buf)?;
                        buf
                    }
                };
                String::from_utf8(bytes)?.into()
            }
            StringSchema::Binary { inner } => {
                let array = inner.decode(target)?;
                let bytes = array
                    .as_array()
                    .expect("Is an array schema")
                    .iter()
                    .map(|v| v.as_u64().expect("elements are u8") as _)
                    .collect::<Vec<_>>();
                let hex_string = hex::encode(bytes);
                hex_string.into()
            }
        };

        Ok(value)
    }
}

fn read_with_length<R>(mut reader: R, length: usize) -> Result<Vec<u8>, DecodingError>
where
    R: io::Read,
{
    let mut buf = vec![0; length];
    reader.read_exact(buf.as_mut_slice())?;
    Ok(buf)
}

fn read_with_pattern<R>(reader: R, pattern: &str, max: usize) -> Result<Vec<u8>, DecodingError>
where
    R: io::Read,
{
    let mut buf = Vec::new();
    for b in reader.bytes() {
        let b = b?;
        buf.push(b);
        if buf.ends_with_str(pattern) {
            buf.pop();
            return Ok(buf);
        }
        if buf.len() == max {
            return Ok(buf);
        }
    }

    Err(DecodingError::NoPattern {
        read: buf.into_string()?,
        pattern: pattern.to_owned(),
    })
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::array::ArraySchema;
    use anyhow::Result;
    use serde_json::{from_value, json};

    #[test]
    fn fixed() -> Result<()> {
        let schema = json!({
            "minLength": 4,
            "maxLength": 4,
            "lengthEncoding": {
                "@type": "endpattern",
                "sentinel": "!"
            }
        });
        let schema: StringSchema = from_value(schema)?;
        // Length encoding gets ignored if 'min-' and 'maxLength' are present.
        assert!(matches!(
            schema,
            StringSchema::Utf8 {
                length: LengthEncoding::Fixed(4)
            }
        ));

        let mut buffer = vec![];
        let value = "Hans".to_string();
        let json: Value = value.clone().into();
        assert_eq!(4, schema.encode(&mut buffer, &json)?);
        assert_eq!(value.as_bytes(), buffer.as_slice());

        let invalid = json!("Berta");
        assert!(schema.encode(&mut buffer, &invalid).is_err());

        Ok(())
    }

    #[test]
    fn incomplete_fixed() -> Result<()> {
        let schema = json!({
            "maxLength": 5,
            "lengthEncoding": { "@type": "fixed" }
        });
        assert!(from_value::<StringSchema>(schema).is_err());

        Ok(())
    }

    #[test]
    fn length() -> Result<()> {
        let schema = json!({
            "lengthEncoding": {
                "@type": "explicitlength",
                "length": 1
            }
        });
        let schema: StringSchema = from_value(schema)?;
        assert!(matches!(
            schema,
            StringSchema::Utf8 {
                length: LengthEncoding::LengthEncoded(_)
            }
        ));
        println!("schema: {:#?}", schema);

        let mut buffer = vec![];
        let value = "Hans".to_string();
        let json: Value = value.clone().into();
        assert_eq!(5, schema.encode(&mut buffer, &json)?);
        let expected = [4, b'H', b'a', b'n', b's'];
        assert_eq!(&expected, buffer.as_slice());

        Ok(())
    }

    #[test]
    fn simple_pattern() -> Result<()> {
        let schema = json!({
            "lengthEncoding": {
                "@type": "endpattern",
                "sentinel": "\0"
            }
        });
        let schema: StringSchema = from_value(schema)?;
        assert!(matches!(
            schema,
            StringSchema::Utf8 {
                length: LengthEncoding::EndPattern { .. }
            }
        ));

        let mut buffer = vec![];
        let value = "Hans".to_string();
        let json: Value = value.clone().into();
        assert_eq!(5, schema.encode(&mut buffer, &json)?);
        let expected = [b'H', b'a', b'n', b's', 0x00];
        assert_eq!(&expected, buffer.as_slice());

        Ok(())
    }

    #[test]
    fn simple_pattern_binary() -> Result<()> {
        println!("entry");
        let schema = json!({
            "lengthEncoding": {
                "@type": "endpattern",
                "sentinel": "00"
            },
            "format": "binary",
        });
        let schema: StringSchema = from_value(schema)?;
        assert!(matches!(
            schema,
            StringSchema::Binary {
                inner: ArraySchema {
                    length: LengthEncoding::EndPattern { .. },
                    ..
                }
            }
        ));

        let mut buffer = vec![];
        let value = "6911dead".to_string();
        let json: Value = value.clone().into();
        assert_eq!(5, schema.encode(&mut buffer, &json)?);
        let expected = [0x69, 0x11, 0xde, 0xad, 0x00];
        assert_eq!(&expected, buffer.as_slice());

        Ok(())
    }

    #[test]
    fn default() -> Result<()> {
        let schema = json!({});
        let schema: StringSchema = from_value(schema)?;
        assert!(matches!(
            schema,
            StringSchema::Utf8 {
                length: LengthEncoding::TillEnd
            }
        ));

        let mut buffer = vec![];
        let value = "Hans".to_string();
        let json: Value = value.clone().into();
        assert_eq!(4, schema.encode(&mut buffer, &json)?);
        let expected = [b'H', b'a', b'n', b's'];
        assert_eq!(&expected, buffer.as_slice());

        Ok(())
    }

    #[test]
    fn invalid_pattern() -> Result<()> {
        // ß is UTF8 encoded as the two bytes `0xC3_9F` but patterns are only
        // allowed to have one byte.
        let schema = json!({
            "lengthEncoding": {
                "@type": "endpattern",
                "sentinel": "ß"
            }
        });
        assert!(from_value::<StringSchema>(schema).is_err());

        Ok(())
    }

    #[test]
    fn other_pattern() -> Result<()> {
        let schema = json!({
            "lengthEncoding": {
                "@type": "endpattern",
                "sentinel": "!"
            }
        });
        let schema: StringSchema = from_value(schema)?;
        assert!(matches!(
            schema,
            StringSchema::Utf8 {
                length: LengthEncoding::EndPattern { .. }
            }
        ));

        let mut buffer = vec![];
        let value = "Hans".to_string();
        let json: Value = value.clone().into();
        assert_eq!(5, schema.encode(&mut buffer, &json)?);
        let expected = [b'H', b'a', b'n', b's', b'!'];
        assert_eq!(&expected, buffer.as_slice());

        Ok(())
    }

    #[test]
    fn pattern_included() -> Result<()> {
        let schema = json!({
            "lengthEncoding": {
                "@type": "endpattern",
                "sentinel": "a"
            }
        });
        let schema: StringSchema = from_value(schema)?;
        assert!(matches!(
            schema,
            StringSchema::Utf8 {
                length: LengthEncoding::EndPattern { .. }
            }
        ));

        let mut buffer = vec![];
        let value = "Hans".to_string();
        let json: Value = value.clone().into();
        // Endpattern `a` is in `Hans` included
        assert!(schema.encode(&mut buffer, &json).is_err());

        Ok(())
    }

    #[test]
    fn invalid_padding() -> Result<()> {
        let schema = json!({
            "lengthEncoding": {
                "@type": "capacity",
                "padding": "µ"
            },
            "maxLength": 10
        });
        // Fails, default char must UTF8 encode as one byte.
        assert!(from_value::<StringSchema>(schema).is_err());

        Ok(())
    }

    #[test]
    fn missing_capacity() -> Result<()> {
        let schema = json!({
            "lengthEncoding": {
                "@type": "capacity",
                "padding": "\0"
            }
        });
        // Capacity encoding requires `"maxLength"`.
        assert!(from_value::<StringSchema>(schema).is_err());

        Ok(())
    }

    #[test]
    fn capacity() -> Result<()> {
        let schema = json!({
            "lengthEncoding": {
                "@type": "capacity",
                "padding": "!"
            },
            "maxLength": 10
        });
        let schema: StringSchema = from_value(schema)?;
        assert!(matches!(
            schema,
            StringSchema::Utf8 {
                length: LengthEncoding::Capacity { .. }
            }
        ));

        let mut buffer = vec![];
        let value = "Hans".to_string();
        let json: Value = value.clone().into();
        assert_eq!(10, schema.encode(&mut buffer, &json)?);
        let expected: [u8; 10] = [b'H', b'a', b'n', b's', b'!', b'!', b'!', b'!', b'!', b'!'];
        //  ^ value                 ^ filler
        assert_eq!(&expected, buffer.as_slice());

        Ok(())
    }

    #[test]
    fn binary_capacity() -> Result<()> {
        println!("entry");
        let schema = json!({
            "lengthEncoding": {
                "@type": "capacity",
                "padding": "00"
            },
            "maxLength": 10,
            "format": "binary"
        });
        let schema: StringSchema = from_value(schema)?;
        assert!(matches!(
            schema,
            StringSchema::Binary {
                inner: ArraySchema {
                    length: LengthEncoding::Capacity { capacity: 5, .. },
                    ..
                }
            }
        ));

        let mut buffer = vec![];
        let value = "6911dead".to_string();
        let json: Value = value.clone().into();
        assert_eq!(5, schema.encode(&mut buffer, &json)?);
        let expected = [0x69, 0x11, 0xde, 0xad, 0x00];
        assert_eq!(&expected, buffer.as_slice());

        let mut read = std::io::Cursor::new(buffer);
        let decoded = schema.decode(&mut read)?;
        assert_eq!(json, decoded);

        Ok(())
    }
}
