//! Implementation of the string schema

use crate::{Decoder, Encoder, Error, IntegerSchema, Result};
use bstr::{ByteSlice, ByteVec as _};
use byteorder::{ReadBytesExt, WriteBytesExt};
use serde::de::{Deserializer, Error as DeError};
use serde::Deserialize;
use serde_json::Value;
use std::convert::TryFrom;
use std::io;

#[derive(Debug, thiserror::Error)]
pub enum EncodingError {
    #[error("Encoding length failed: {0}")]
    LengthEncoding(#[from] Box<Error>),
    #[error("Writing to buffer failed: {0}")]
    WriteFail(#[from] io::Error),
    #[error("Length of {len} bytes but only a fixed length of {fixed} is supported.")]
    NotFixedLength { len: usize, fixed: usize },
    #[error("Contains the end sequence {0}.")]
    ContainsEndSequence(String),
    #[error("Length of {len} bytes but only values up to a length of {cap} are valid.")]
    ExceedsCapacity { len: usize, cap: usize },
    #[error("Length of {len} bytes but only a length up to {max} bytes can be encoded.")]
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

#[derive(Debug, Clone, Deserialize)]
#[serde(tag = "type", rename_all = "lowercase")]
enum LengthEncoding {
    /// Requires `"maxLength"`. All values are that long. If the to-encode-value
    /// has a different size an error is raised.
    /// An alternative way to specify a fixed length is setting `"minLength"`
    /// and `"maxLength"` to the same value. This version has priority above all
    /// other length encodings.
    Fixed,
    /// The length of a string is stored at the beginning of the field with the
    /// given integer schema.
    ///
    /// This does not require `"maxLength"`. However, if it is present remaining
    /// space is filled with `"defaultChar"`.
    ExplicitLength(IntegerSchema),
    /// The end of the string is marked by a certain sequence.
    ///
    /// This does not require `"maxLength"`. However, if it is present remaining
    /// space is filled with `"defaultChar"`.
    EndPattern {
        #[serde(default = "LengthEncoding::default_end")]
        sequence: String,
    },
    /// The encoded string reaches to the end of the data. This is the default
    /// encoding in order to be compatible to the `octet-stream` encoder of
    /// `node-wot`.
    TillEnd,
}

pub const DEFAULT_END: char = '\0';

impl LengthEncoding {
    fn default_end() -> String {
        DEFAULT_END.into()
    }
}

impl Default for LengthEncoding {
    fn default() -> Self {
        LengthEncoding::TillEnd
    }
}

#[derive(Debug, Clone, Copy, Deserialize)]
#[serde(rename_all = "lowercase")]
enum Format {
    #[serde(skip)]
    Utf8,
    Binary,
}

impl Format {
    fn encode(&self, value: String) -> Result<Vec<u8>> {
        match self {
            Format::Utf8 => Ok(value.into_bytes()),
            Format::Binary => hex::decode(value).map_err(Into::into),
        }
    }
    fn decode(&self, value: Vec<u8>) -> Result<String> {
        match self {
            Format::Utf8 => String::from_utf8(value).map_err(Into::into),
            Format::Binary => Ok(hex::encode(value)),
        }
    }
}

impl Default for Format {
    fn default() -> Self {
        Format::Utf8
    }
}

/// How is the length of variable sized data encoded.
#[derive(Debug, Clone, Deserialize)]
#[serde(rename_all = "camelCase")]
struct RawString {
    #[serde(default)]
    length_encoding: LengthEncoding,
    max_length: Option<usize>,
    min_length: Option<usize>,
    #[serde(default = "StringEncoding::default_char")]
    default_char: char,
    format: Option<Format>,
}

impl RawString {
    fn valid_default_char(&self) -> Result<char> {
        if self.default_char.len_utf8() == 1 {
            Ok(self.default_char)
        } else {
            Err(Error::InvalidDefaultChar(self.default_char))
        }
    }
}

/// The string schema to describe string values.
#[derive(Debug, Clone)]
pub enum StringEncoding {
    Fixed(usize),
    LengthEncoded(IntegerSchema),
    EndPattern {
        encoded: Vec<u8>,
        decoded: String,
    },
    LenAndCap {
        length: IntegerSchema,
        capacity: usize,
        default_char: char,
    },
    PatternAndCap {
        encoded: Vec<u8>,
        decoded: String,
        capacity: usize,
        default_char: char,
    },
    TillEnd,
}

pub const DEFAULT_CHAR: char = '\0';

impl StringEncoding {
    pub fn default_char() -> char {
        DEFAULT_CHAR
    }
    fn encoded_length(&self, value_len: usize) -> usize {
        match self {
            StringEncoding::Fixed(_) => value_len,
            StringEncoding::LengthEncoded(int) => int.length() + value_len,
            StringEncoding::EndPattern { encoded, .. } => encoded.len() + value_len,
            StringEncoding::LenAndCap {
                capacity, length, ..
            } => *capacity + length.length(),
            StringEncoding::PatternAndCap {
                capacity, encoded, ..
            } => *capacity + encoded.len(),
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
                contains_end_sequencs(&value, encoded, decoded)?;
                target.write_all(&value)?;
                target.write_all(&encoded)?;
            }
            StringEncoding::LenAndCap {
                length,
                capacity,
                default_char,
            } => {
                let len_length = length.length();
                let len_value = value.len();
                let capacity = *capacity;
                exceeds_cap(len_value, capacity)?;
                exceeds_length(len_value, length)?;
                length.encode(target, &len_value.into()).map_err(Box::new)?;
                target.write_all(&value)?;
                fill_rest(
                    target,
                    capacity + len_length,
                    len_length + len_value,
                    *default_char,
                )?;
            }
            StringEncoding::PatternAndCap {
                encoded,
                decoded,
                capacity,
                default_char,
            } => {
                let len_pattern = encoded.len();
                let len_value = value.len();
                let capacity = *capacity;
                exceeds_cap(len_value, capacity)?;
                contains_end_sequencs(&value, encoded, decoded)?;
                target.write_all(&value)?;
                target.write_all(&encoded)?;
                fill_rest(
                    target,
                    capacity + len_pattern,
                    len_pattern + len_value,
                    *default_char,
                )?;
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
                read_str_with_pattern(target, encoded, usize::MAX, decoded)?
            }
            StringEncoding::LenAndCap {
                length, capacity, ..
            } => {
                let length = length
                    .decode(target)?
                    .as_u64()
                    .expect("length is always u64") as _;
                if length > *capacity {
                    return Err(Error::EncodedValueExceedsCapacity {
                        len: length,
                        cap: *capacity,
                    });
                }
                read_str_with_length(target, length)?
            }
            StringEncoding::PatternAndCap {
                encoded,
                capacity,
                decoded,
                ..
            } => read_str_with_pattern(target, encoded, *capacity, decoded)?,
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
        let default_char = raw.valid_default_char()?;
        match (raw.min_length, raw.max_length) {
            (Some(min), Some(max)) if min == max => return Ok(StringEncoding::Fixed(max)),
            _ => {}
        }
        match raw.length_encoding {
            LengthEncoding::Fixed => {
                if let Some(len) = raw.max_length {
                    Ok(StringEncoding::Fixed(len))
                } else {
                    Err(Error::MissingCapacity)
                }
            }
            LengthEncoding::ExplicitLength(schema) => {
                if let Some(cap) = raw.max_length {
                    Ok(StringEncoding::LenAndCap {
                        length: schema,
                        capacity: cap,
                        default_char,
                    })
                } else {
                    Ok(StringEncoding::LengthEncoded(schema))
                }
            }
            LengthEncoding::EndPattern { sequence } => {
                let encoded = fmt.encode(sequence.clone())?;
                if let Some(cap) = raw.max_length {
                    Ok(StringEncoding::PatternAndCap {
                        encoded,
                        decoded: sequence,
                        capacity: cap,
                        default_char,
                    })
                } else {
                    Ok(StringEncoding::EndPattern {
                        encoded,
                        decoded: sequence,
                    })
                }
            }
            LengthEncoding::TillEnd => Ok(StringEncoding::TillEnd),
        }
    }
}

/// The string schema to describe string values.
#[derive(Debug, Clone)]
pub struct StringSchema {
    encoding: StringEncoding,
    format: Format,
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

        Ok(self.encoding.encoded_length(len))
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
    pattern: &[u8],
    decoded_pattern: &str,
) -> Result<(), EncodingError> {
    if value.contains_str(pattern) {
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
    filler: char,
) -> Result<usize, EncodingError> {
    let mut target = target;
    let to_fill = cap - filled;
    let mut c = [0];
    filler.encode_utf8(&mut c);
    for _ in 0..to_fill {
        target.write_all(&c)?;
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
    pattern: &[u8],
    max: usize,
    decoded_pattern: &str,
) -> Result<Vec<u8>>
where
    R: io::Read,
{
    let mut buf = Vec::new();
    let pattern_len = pattern.len() as isize;
    let max = max as isize;
    for b in reader.bytes() {
        let b = b?;
        buf.push(b);
        if buf.ends_with(pattern.as_bytes()) {
            (0..pattern_len).for_each(|_| {
                buf.pop_byte();
            });
            return Ok(buf);
        } else if buf.len() as isize - pattern_len >= max {
            return Err(Error::NoPattern {
                read: buf.into_string()?,
                pattern: decoded_pattern.to_owned(),
            });
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
        assert!(matches!(schema.encoding, StringEncoding::EndPattern {..}));

        let mut buffer = vec![];
        let value = "Hans".to_string();
        let json: Value = value.clone().into();
        assert_eq!(5, schema.encode(&mut buffer, &json)?);
        let expected = [b'H', b'a', b'n', b's', 0x00];
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
        assert!(matches!(schema.encoding, StringEncoding::EndPattern{..}));

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
    fn length_and_capacity() -> Result<()> {
        let schema = json!({
            "lengthEncoding": {
                "type": "explicitlength",
                "length": 2,
                "byteorder": "littleendian",
            },
            "maxLength": 10,
        });
        let schema: StringSchema = from_value(schema)?;
        assert!(matches!(schema.encoding, StringEncoding::LenAndCap {..} ));

        let mut buffer = vec![];
        let value = "Hans".to_string();
        let json: Value = value.clone().into();
        assert_eq!(12, schema.encode(&mut buffer, &json)?);
        let expected: [u8; 12] = [
            0x04, 0x00, b'H', b'a', b'n', b's', 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        ];
        //  ^ len in LE ^ value                 ^ filler
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
        assert!(matches!(schema.encoding, StringEncoding::PatternAndCap {..}));

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
