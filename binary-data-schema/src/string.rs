//! Implementation of the string schema

use std::{convert::TryFrom, io};

use bstr::ByteVec as _;
use byteorder::{ReadBytesExt, WriteBytesExt};
use serde::{
    de::{Deserializer, Error as DeError},
    Deserialize,
};
use serde_json::Value;

use crate::{Decoder, Encoder, Error, IntegerSchema, Result};

/// Character `\0` is used as default end and padding.
pub const DEFAULT_CHAR: char = '\0';

#[derive(Debug, thiserror::Error)]
pub enum EncodingError {
    #[error("Encoding length failed: {0}")]
    LengthEncoding(#[from] Box<Error>),
    #[error("Writing to buffer failed: {0}")]
    WriteFail(#[from] io::Error),
    #[error("Length of {len} bytes but only a fixed length of {fixed} is supported")]
    NotFixedLength { len: usize, fixed: usize },
    #[error("Contains the end sequence {0}")]
    ContainsEndSequence(String),
    #[error("Length of {len} bytes but only values up to a length of {cap} are valid")]
    ExceedsCapacity { len: usize, cap: usize },
    #[error("Length of {len} bytes but only a length up to {max} bytes can be encoded")]
    ExceedsLengthEncoding { len: usize, max: usize },
}

impl EncodingError {
    pub fn context(self, value: String) -> Error {
        Error::StringEncoding {
            value,
            source: self,
        }
    }
}

/// Length encoding like present in the JSON description.
#[derive(Debug, Clone, Deserialize)]
#[serde(tag = "type", rename_all = "lowercase")]
enum RawLengthEncoding {
    /// Requires `"maxLength"`. All values are that long. If the to-encode-value
    /// has a different size an error is raised.
    /// An alternative way to specify a fixed length is setting `"minLength"`
    /// and `"maxLength"` to the same value. This version has priority above all
    /// other length encodings.
    Fixed,
    /// The length of a string is stored at the beginning of the field with the
    /// given integer schema.
    ExplicitLength(IntegerSchema),
    /// The end of the string is marked by a certain sequence.
    EndPattern {
        /// The sequence marking the end of the string (default: [DEFAULT_CHAR]).
        #[serde(default = "RawLengthEncoding::default_char")]
        sequence: String,
    },
    /// A capacity of `"maxLength"` is reserved for the string. Unused place is
    /// filled with `"padding"`.
    Capacity {
        /// The characters used to fill unused capacity (default: [DEFAULT_CHAR]).
        #[serde(default = "RawLengthEncoding::default_char")]
        padding: String,
    },
    /// The encoded string reaches to the end of the data. This is the default
    /// encoding in order to be compatible to the `octet-stream` encoder of
    /// `node-wot`.
    TillEnd,
}

#[derive(Debug, Clone, Copy, Deserialize, PartialEq, Eq)]
#[serde(rename_all = "lowercase")]
enum Format {
    #[serde(skip)]
    Utf8,
    Binary,
}

/// The string schema to describe string values.
#[derive(Debug, Clone)]
enum StringEncoding {
    Fixed(usize),
    LengthEncoded(IntegerSchema),
    EndPattern {
        encoded: u8,
        decoded: String,
    },
    Capacity {
        encoded: u8,
        decoded: String,
        capacity: usize,
    },
    TillEnd,
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

/// The string schema to describe string values.
#[derive(Debug, Clone)]
pub struct StringSchema {
    encoding: StringEncoding,
    format: Format,
}

impl RawLengthEncoding {
    fn default_char() -> String {
        DEFAULT_CHAR.into()
    }
}

impl Default for RawLengthEncoding {
    fn default() -> Self {
        RawLengthEncoding::TillEnd
    }
}

impl Format {
    fn encode(&self, value: String) -> Result<Vec<u8>> {
        match self {
            Format::Utf8 => Ok(value.into_bytes()),
            Format::Binary if value == "\0" => Ok(vec![0]),
            Format::Binary => hex::decode(value).map_err(Into::into),
        }
    }
    fn decode(&self, value: Vec<u8>) -> Result<String> {
        match self {
            Format::Utf8 => String::from_utf8(value).map_err(Into::into),
            Format::Binary => Ok(hex::encode(value)),
        }
    }
    fn validate_pattern(&self, pattern: String) -> Result<u8> {
        let encoded = self.encode(pattern.clone())?;
        if encoded.len() == 1 {
            Ok(encoded[0])
        } else {
            Err(Error::InvalidPattern { pattern })
        }
    }
    /// State how much bytes are written for a given length string.
    fn bytes_written(&self, len: usize) -> usize {
        match self {
            Format::Utf8 => len,
            Format::Binary => len / 2,
        }
    }
    /// State how much characters are written for a given number of bytes.
    fn chars_written(&self, bytes: usize) -> usize {
        match self {
            Format::Utf8 => bytes,
            Format::Binary => bytes * 2,
        }
    }
}

impl Default for Format {
    fn default() -> Self {
        Format::Utf8
    }
}

impl StringEncoding {
    fn encoded_length(&self, value_len: usize) -> usize {
        match self {
            StringEncoding::Fixed(_) => value_len,
            StringEncoding::LengthEncoded(int) => int.length() + value_len,
            StringEncoding::EndPattern { .. } => 1 + value_len,
            StringEncoding::Capacity { capacity, .. } => *capacity,
            StringEncoding::TillEnd => value_len,
        }
    }
    fn encode<W>(&self, target: &mut W, value: Vec<u8>) -> Result<(), EncodingError>
    where
        W: io::Write + WriteBytesExt,
    {
        match self {
            StringEncoding::Fixed(len) => {
                matches_fixed_len(value.len(), *len)?;
                target.write_all(&value)?;
            }
            StringEncoding::LengthEncoded(int) => {
                exceeds_length(value.len(), int)?;
                int.encode(target, &value.len().into()).map_err(Box::new)?;
                target.write_all(&value)?;
            }
            StringEncoding::EndPattern { encoded, decoded } => {
                contains_end_sequencs(&value, *encoded, decoded)?;
                target.write_all(&value)?;
                target.write_all(&[*encoded])?;
            }
            StringEncoding::Capacity {
                encoded, capacity, ..
            } => {
                let len_value = value.len();
                let capacity = *capacity;
                exceeds_cap(len_value, capacity)?;
                target.write_all(&value)?;
                fill_rest(target, capacity, len_value, *encoded)?;
            }
            StringEncoding::TillEnd => {
                target.write_all(&value)?;
            }
        }

        Ok(())
    }
    fn decode<R>(&self, target: &mut R) -> Result<Vec<u8>>
    where
        R: io::Read + ReadBytesExt,
    {
        let value = match self {
            StringEncoding::Fixed(length) => read_str_with_length(target, *length)?,
            StringEncoding::LengthEncoded(schema) => {
                let length = schema
                    .decode(target)?
                    .as_u64()
                    .expect("length is always u64");
                read_str_with_length(target, length as _)?
            }
            StringEncoding::EndPattern { encoded, decoded } => {
                read_str_with_pattern(target, *encoded, usize::MAX, decoded)?
            }
            StringEncoding::Capacity {
                encoded,
                capacity,
                decoded,
                ..
            } => read_str_with_pattern(target, *encoded, *capacity, decoded)?,
            StringEncoding::TillEnd => {
                let mut buf = Vec::new();
                target.read_to_end(&mut buf)?;
                buf
            }
        };

        Ok(value)
    }
}

impl TryFrom<RawString> for StringEncoding {
    type Error = Error;

    fn try_from(raw: RawString) -> Result<Self, Self::Error> {
        let fmt = raw.format.unwrap_or_default();
        match (raw.min_length, raw.max_length) {
            (Some(min), Some(max)) if min == max => return Ok(StringEncoding::Fixed(max)),
            _ => {}
        }
        match raw.length_encoding {
            RawLengthEncoding::Fixed => Err(Error::IncompleteFixedLength),
            RawLengthEncoding::ExplicitLength(schema) => Ok(StringEncoding::LengthEncoded(schema)),
            RawLengthEncoding::Capacity { padding } => {
                let encoded = fmt.validate_pattern(padding.clone())?;
                let capacity = if let Some(cap) = raw.max_length {
                    Ok(cap)
                } else {
                    Err(Error::MissingCapacity)
                }?;
                Ok(StringEncoding::Capacity {
                    capacity,
                    encoded,
                    decoded: padding,
                })
            }
            RawLengthEncoding::EndPattern { sequence } => {
                let encoded = fmt.validate_pattern(sequence.clone())?;
                Ok(StringEncoding::EndPattern {
                    encoded,
                    decoded: sequence,
                })
            }
            RawLengthEncoding::TillEnd => Ok(StringEncoding::TillEnd),
        }
    }
}

impl<'de> Deserialize<'de> for StringSchema {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let raw = RawString::deserialize(deserializer)?;
        let format = raw.format.as_ref().copied().unwrap_or(Format::Utf8);
        let encoding = StringEncoding::try_from(raw).map_err(D::Error::custom)?;
        Ok(StringSchema { encoding, format })
    }
}

impl Encoder for StringSchema {
    fn encode<W>(&self, target: &mut W, value: &Value) -> Result<usize>
    where
        W: io::Write + WriteBytesExt,
    {
        let value = value.as_str().ok_or_else(|| Error::InvalidValue {
            value: value.to_string(),
            type_: "string",
        })?;
        let value_encoded = self.format.encode(value.to_owned())?;
        let len = value_encoded.len();
        self.encoding
            .encode(target, value_encoded)
            .map_err(|e| e.context(value.to_owned()))?;

        let written = self.format.chars_written(self.encoding.encoded_length(len));
        Ok(written)
    }
}

impl Decoder for StringSchema {
    fn decode<R>(&self, target: &mut R) -> Result<Value>
    where
        R: io::Read + ReadBytesExt,
    {
        let encoded = self.encoding.decode(target)?;
        let decoded = self.format.decode(encoded)?;
        Ok(decoded.into())
    }
}

fn matches_fixed_len(value_len: usize, len: usize) -> Result<(), EncodingError> {
    if value_len != len {
        Err(EncodingError::NotFixedLength {
            len: value_len,
            fixed: len,
        })
    } else {
        Ok(())
    }
}

fn contains_end_sequencs(
    value: &[u8],
    pattern: u8,
    decoded_pattern: &str,
) -> Result<(), EncodingError> {
    if value.contains(&pattern) {
        Err(EncodingError::ContainsEndSequence(
            decoded_pattern.to_owned(),
        ))
    } else {
        Ok(())
    }
}

fn exceeds_cap(value_len: usize, cap: usize) -> Result<(), EncodingError> {
    if value_len > cap {
        Err(EncodingError::ExceedsCapacity {
            len: value_len,
            cap,
        })
    } else {
        Ok(())
    }
}

fn exceeds_length(value_len: usize, schema: &IntegerSchema) -> Result<(), EncodingError> {
    if value_len > schema.max_value() {
        Err(EncodingError::ExceedsLengthEncoding {
            len: value_len,
            max: schema.max_value(),
        })
    } else {
        Ok(())
    }
}

fn fill_rest<W: io::Write>(
    target: W,
    cap: usize,
    filled: usize,
    filler: u8,
) -> Result<usize, EncodingError> {
    let mut target = target;
    let to_fill = cap - filled;
    let filler = [filler];
    for _ in 0..to_fill {
        target.write_all(&filler)?;
    }
    Ok(to_fill)
}

fn read_str_with_length<R>(mut reader: R, length: usize) -> Result<Vec<u8>>
where
    R: io::Read,
{
    let mut buf = vec![0; length];
    reader.read_exact(buf.as_mut_slice())?;
    Ok(buf)
}

fn read_str_with_pattern<R>(
    reader: R,
    pattern: u8,
    max: usize,
    decoded_pattern: &str,
) -> Result<Vec<u8>>
where
    R: io::Read,
{
    let mut buf = Vec::new();
    for b in reader.bytes() {
        let b = b?;
        if b == pattern {
            return Ok(buf);
        }
        buf.push(b);
        if buf.len() == max {
            return Ok(buf);
        }
    }

    Err(Error::NoPattern {
        read: buf.into_string()?,
        pattern: decoded_pattern.to_owned(),
    })
}

#[cfg(test)]
mod test {
    use super::*;
    use anyhow::Result;
    use serde_json::{from_value, json};

    #[test]
    fn fixed() -> Result<()> {
        let schema = json!({
            "minLength": 4,
            "maxLength": 4,
            "lengthEncoding": {
                "type": "endpattern",
                "sequence": "!"
            }
        });
        let schema: StringSchema = from_value(schema)?;
        // Length encoding gets ignored if 'min-' and 'maxLength' are present.
        assert!(matches!(schema.encoding, StringEncoding::Fixed(4)));

        let mut buffer = vec![];
        let value = "Hans".to_string();
        let json: Value = value.clone().into();
        assert_eq!(4, schema.encode(&mut buffer, &json)?);
        assert_eq!(value.as_bytes(), buffer.as_slice());
        let invalid = "Berta".to_string();
        let invalid_json: Value = invalid.clone().into();
        assert!(schema.encode(&mut buffer, &invalid_json).is_err());

        let schema = json!({
            "maxLength": 5,
            "lengthEncoding": { "type": "fixed" }
        });
        let schema: StringSchema = from_value(schema)?;
        let value = invalid;
        let json = invalid_json;
        buffer.clear();
        assert_eq!(5, schema.encode(&mut buffer, &json)?);
        assert_eq!(value.as_bytes(), buffer.as_slice());

        Ok(())
    }

    #[test]
    fn length() -> Result<()> {
        let schema = json!({
            "lengthEncoding": {
                "type": "explicitlength",
                "length": 1
            }
        });
        let schema: StringSchema = from_value(schema)?;
        assert!(matches!(schema.encoding, StringEncoding::LengthEncoded(_)));

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
    fn default_pattern() -> Result<()> {
        let schema = json!({
            "lengthEncoding": { "type": "endpattern" }
        });
        let schema: StringSchema = from_value(schema)?;
        assert!(matches!(schema.encoding, StringEncoding::EndPattern { .. }));

        let mut buffer = vec![];
        let value = "Hans".to_string();
        let json: Value = value.clone().into();
        assert_eq!(5, schema.encode(&mut buffer, &json)?);
        let expected = [b'H', b'a', b'n', b's', 0x00];
        assert_eq!(&expected, buffer.as_slice());

        Ok(())
    }

    #[test]
    fn default_pattern_binary() -> Result<()> {
        println!("entry");
        let schema = json!({
            "lengthEncoding": { "type": "endpattern" },
            "format": "binary",
        });
        println!("parse");
        let schema: StringSchema = from_value(schema)?;
        println!("ensure type");
        assert!(matches!(schema.encoding, StringEncoding::EndPattern { .. }));

        println!("encode");
        let mut buffer = vec![];
        let value = "6911dead".to_string();
        let json: Value = value.clone().into();
        assert_eq!(5, schema.encode(&mut buffer, &json)?);
        println!("ensure result");
        let expected = [0x69, 0x11, 0xde, 0xad, 0x00];
        assert_eq!(&expected, buffer.as_slice());

        Ok(())
    }

    #[test]
    fn default() -> Result<()> {
        let schema = json!({});
        let schema: StringSchema = from_value(schema)?;
        assert!(matches!(schema.encoding, StringEncoding::TillEnd));

        let mut buffer = vec![];
        let value = "Hans".to_string();
        let json: Value = value.clone().into();
        assert_eq!(4, schema.encode(&mut buffer, &json)?);
        let expected = [b'H', b'a', b'n', b's'];
        assert_eq!(&expected, buffer.as_slice());

        Ok(())
    }

    #[test]
    fn pattern_utf8() -> Result<()> {
        let schema = json!({
            "lengthEncoding": {
                "type": "endpattern",
                "sequence": "ß"
            }
        });
        let schema: StringSchema = from_value(schema)?;
        assert!(matches!(schema.encoding, StringEncoding::EndPattern { .. }));

        let mut buffer = vec![];
        let value = "Hans".to_string();
        let json: Value = value.clone().into();
        // ß is UTF8 encoded as the two bytes `0xC3_9F`
        assert_eq!(6, schema.encode(&mut buffer, &json)?);
        let expected = [b'H', b'a', b'n', b's', 0xC3, 0x9F];
        assert_eq!(&expected, buffer.as_slice());

        Ok(())
    }

    #[test]
    fn pattern_and_capacity() -> Result<()> {
        let schema = json!({
            "lengthEncoding": {
                "type": "endpattern",
                "sequence": "?"
            },
            "defaultChar": "µ"
        });
        // Fails, default char must UTF8 encode as one byte.
        assert!(from_value::<StringSchema>(schema).is_err());
        let schema = json!({
            "lengthEncoding": {
                "type": "endpattern",
                "sequence": "?"
            },
            "defaultChar": "!",
            "maxLength": 10
        });
        let schema: StringSchema = from_value(schema)?;
        assert!(matches!(schema.encoding, StringEncoding::Capacity { .. }));

        let mut buffer = vec![];
        let value = "Hans".to_string();
        let json: Value = value.clone().into();
        assert_eq!(11, schema.encode(&mut buffer, &json)?);
        let expected: [u8; 11] = [
            b'H', b'a', b'n', b's', b'?', b'!', b'!', b'!', b'!', b'!', b'!',
        ];
        //  ^ value                 ^ end ^ filler
        assert_eq!(&expected, buffer.as_slice());

        Ok(())
    }
}
