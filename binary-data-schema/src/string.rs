//! Implementation of the string schema

use crate::{BinaryCodec, Error, IntegerSchema, Length, Result};
use byteorder::WriteBytesExt;
use serde::de::{Deserializer, Error as DeError};
use serde::Deserialize;
use std::convert::TryFrom;
use std::io;

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
}

pub const DEFAULT_END: char = '\0';

impl LengthEncoding {
    fn default_end() -> String {
        DEFAULT_END.into()
    }
}

impl Default for LengthEncoding {
    fn default() -> Self {
        LengthEncoding::EndPattern {
            sequence: DEFAULT_END.to_string(),
        }
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
    #[serde(default = "StringSchema::default_char")]
    default_char: char,
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

/// The number schema describes a numeric value.
#[derive(Debug, Clone)]
pub enum StringSchema {
    Fixed(usize),
    LengthEncoded(IntegerSchema),
    EndPattern(String),
    LenAndCap {
        length: IntegerSchema,
        capacity: usize,
        default_char: char,
    },
    PatternAndCap {
        pattern: String,
        capacity: usize,
        default_char: char,
    },
}

pub const DEFAULT_CHAR: char = '\0';

impl StringSchema {
    pub fn default_char() -> char {
        DEFAULT_CHAR
    }
    fn encoded_size(&self, value: &str) -> Result<usize> {
        let size = match self {
            StringSchema::Fixed(size) => {
                matches_fixed_len(value, *size)?;
                *size
            }
            StringSchema::LengthEncoded(int) => value.len() + int.length(),
            StringSchema::EndPattern(pattern) => {
                contains_end_sequencs(value, pattern)?;
                value.len() + pattern.len()
            }
            StringSchema::LenAndCap {
                capacity, length, ..
            } => {
                exceeds_cap(value, *capacity)?;
                *capacity + length.length()
            }
            StringSchema::PatternAndCap {
                capacity, pattern, ..
            } => {
                exceeds_cap(value, *capacity)?;
                contains_end_sequencs(value, pattern)?;
                *capacity + pattern.len()
            }
        };
        Ok(size)
    }
}

impl TryFrom<RawString> for StringSchema {
    type Error = Error;

    fn try_from(raw: RawString) -> Result<Self, Self::Error> {
        let default_char = raw.valid_default_char()?;
        match (raw.min_length, raw.max_length) {
            (Some(min), Some(max)) if min == max => return Ok(StringSchema::Fixed(max)),
            _ => {}
        }
        match raw.length_encoding {
            LengthEncoding::Fixed => {
                if let Some(len) = raw.max_length {
                    Ok(StringSchema::Fixed(len))
                } else {
                    Err(Error::MissingCapacity)
                }
            }
            LengthEncoding::ExplicitLength(int) => {
                if let Some(cap) = raw.max_length {
                    Ok(StringSchema::LenAndCap {
                        length: int,
                        capacity: cap,
                        default_char,
                    })
                } else {
                    Ok(StringSchema::LengthEncoded(int))
                }
            }
            LengthEncoding::EndPattern { sequence } => {
                if let Some(cap) = raw.max_length {
                    Ok(StringSchema::PatternAndCap {
                        pattern: sequence,
                        capacity: cap,
                        default_char,
                    })
                } else {
                    Ok(StringSchema::EndPattern(sequence))
                }
            }
        }
    }
}

impl<'de> Deserialize<'de> for StringSchema {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let raw = RawString::deserialize(deserializer)?;
        StringSchema::try_from(raw).map_err(D::Error::custom)
    }
}

impl BinaryCodec for StringSchema {
    type Value = str;

    fn length_encoded(&self) -> Length {
        match self {
            StringSchema::Fixed(len) => Length::Fixed(*len),
            StringSchema::LengthEncoded(_) | StringSchema::EndPattern(_) => Length::Variable,
            StringSchema::LenAndCap {
                length, capacity, ..
            } => Length::Fixed(*capacity + length.length()),
            StringSchema::PatternAndCap {
                pattern, capacity, ..
            } => Length::Fixed(*capacity + pattern.len()),
        }
    }
    fn encode<W>(&self, target: W, value: &Self::Value) -> Result<usize>
    where
        W: io::Write + WriteBytesExt,
    {
        let mut target = target;
        let written = self.encoded_size(value)?;
        match self {
            StringSchema::Fixed(len) => {
                matches_fixed_len(value, *len)?;
                target.write_all(value.as_bytes())?;
                Ok(written)
            }
            StringSchema::LengthEncoded(int) => {
                let len = value.len();
                int.encode(&mut target, &(len as _))?;
                target.write_all(value.as_bytes())?;
                Ok(written)
            }
            StringSchema::EndPattern(seq) => {
                contains_end_sequencs(value, seq)?;
                target.write_all(value.as_bytes())?;
                target.write_all(seq.as_bytes())?;
                Ok(written)
            }
            StringSchema::LenAndCap {
                length,
                capacity,
                default_char,
            } => {
                let len_length = length.length();
                let len_value = value.len();
                let capacity = *capacity;
                exceeds_cap(value, capacity)?;
                length.encode(&mut target, &(len_value as _))?;
                target.write_all(value.as_bytes())?;
                fill_rest(
                    target,
                    capacity + len_length,
                    len_length + len_value,
                    *default_char,
                )?;
                Ok(written)
            }
            StringSchema::PatternAndCap {
                pattern,
                capacity,
                default_char,
            } => {
                let len_pattern = pattern.len();
                let len_value = value.len();
                let capacity = *capacity;
                exceeds_cap(value, capacity)?;
                contains_end_sequencs(value, pattern)?;
                target.write_all(value.as_bytes())?;
                target.write_all(pattern.as_bytes())?;
                fill_rest(
                    target,
                    capacity + len_pattern,
                    len_pattern + len_value,
                    *default_char,
                )?;
                Ok(written)
            }
        }
    }
}

fn matches_fixed_len(value: &str, len: usize) -> Result<()> {
    if value.len() != len {
        Err(Error::NotMatchFixedLength {
            value: value.to_owned(),
            fixed: len,
        })
    } else {
        Ok(())
    }
}

fn contains_end_sequencs(value: &str, pattern: &str) -> Result<()> {
    if value.contains(pattern) {
        Err(Error::ContainsEndSequence {
            value: value.to_owned(),
            sequence: pattern.to_owned(),
        })
    } else {
        Ok(())
    }
}

fn exceeds_cap(value: &str, cap: usize) -> Result<()> {
    if value.len() > cap {
        Err(Error::ToLongString {
            value: value.to_owned(),
            cap,
        })
    } else {
        Ok(())
    }
}

fn fill_rest<W: io::Write>(target: W, cap: usize, filled: usize, filler: char) -> Result<usize> {
    let mut target = target;
    let to_fill = cap - filled;
    let mut c = [0];
    filler.encode_utf8(&mut c);
    for _ in 0..to_fill {
        target.write_all(&c)?;
    }
    Ok(to_fill)
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
        assert!(matches!(schema, StringSchema::Fixed(4)));

        let mut buffer = vec![];
        let value = "Hans".to_string();
        assert_eq!(4, schema.encode(&mut buffer, &value)?);
        assert_eq!(value.as_bytes(), buffer.as_slice());
        let invalid = "Berta".to_string();
        assert!(schema.encode(&mut buffer, &invalid).is_err());

        let schema = json!({
            "maxLength": 5,
            "lengthEncoding": { "type": "fixed" }
        });
        let schema: StringSchema = from_value(schema)?;
        let value = invalid;
        buffer.clear();
        assert_eq!(5, schema.encode(&mut buffer, &value)?);
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
        assert!(matches!(schema, StringSchema::LengthEncoded(_)));

        let mut buffer = vec![];
        let value = "Hans".to_string();
        assert_eq!(5, schema.encode(&mut buffer, &value)?);
        let expected = [4, b'H', b'a', b'n', b's'];
        assert_eq!(&expected, buffer.as_slice());

        Ok(())
    }

    #[test]
    fn pattern() -> Result<()> {
        let schema = json!({});
        let schema: StringSchema = from_value(schema)?;
        assert!(matches!(schema, StringSchema::EndPattern(_)));

        let mut buffer = vec![];
        let value = "Hans".to_string();
        assert_eq!(5, schema.encode(&mut buffer, &value)?);
        let expected = [b'H', b'a', b'n', b's', 0x00];
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
        assert!(matches!(schema, StringSchema::EndPattern(_)));

        let mut buffer = vec![];
        let value = "Hans".to_string();
        // ß is UTF8 encoded as the two bytes `0xC3_9F`
        assert_eq!(6, schema.encode(&mut buffer, &value)?);
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
        assert!(matches!(schema, StringSchema::LenAndCap {..} ));

        let mut buffer = vec![];
        let value = "Hans".to_string();
        assert_eq!(12, schema.encode(&mut buffer, &value)?);
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
        assert!(matches!(schema, StringSchema::PatternAndCap {..}));

        let mut buffer = vec![];
        let value = "Hans".to_string();
        assert_eq!(11, schema.encode(&mut buffer, &value)?);
        let expected: [u8; 11] = [
            b'H', b'a', b'n', b's', b'?', b'!', b'!', b'!', b'!', b'!', b'!',
        ];
        //  ^ value                 ^ end ^ filler
        assert_eq!(&expected, buffer.as_slice());

        Ok(())
    }
}
