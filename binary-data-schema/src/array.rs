//! Implementation of the string schema

use std::{convert::TryFrom, io};

use byteorder::{ReadBytesExt, WriteBytesExt};
use serde::{
    de::{Deserializer, Error as DeError},
    Deserialize,
};
use serde_json::Value;

use crate::{
    integer::{self as int, IntegerSchema},
    util::*,
    DataSchema, Decoder, Encoder, Error, Result,
};

/// Errors validating an [ArraySchema].
#[derive(Debug, thiserror::Error)]
pub enum ValidationError {
    #[error("A fixed length array schema requires both 'maxItems' and 'minItems' given and having the same value")]
    IncompleteFixedLength,
    #[error("Patterns and/or paddings must be encodable with the given schema: '{value}' can not be encoded with a {type_} schema: {error}")]
    InvalidPatternOrPadding {
        value: Value,
        type_: &'static str,
        error: Box<Error>,
    },
    #[error("Length encoding 'capacity' requires 'maxItems'")]
    MissingCapacity,
}

/// Errors encoding a string with an [ArraySchema].
#[derive(Debug, thiserror::Error)]
pub enum EncodingError {
    #[error("The value '{value}' can not be encoded with an array schema")]
    InvalidValue { value: String },
    #[error("Writing to buffer failed: {0}")]
    WriteFail(#[from] io::Error),
    #[error("Could not encode length: {0}")]
    EncodingLength(#[from] int::EncodingError),
    #[error("Encoding sub-schema failed: {0}")]
    SubSchema(Box<Error>),
    #[error("{len} elements in array but only a fixed number of {fixed} elements is supported")]
    NotFixedLength { len: usize, fixed: usize },
    #[error("{len} elements in the array but only a length up to {max} elementy can be encoded")]
    ExceedsLengthEncoding { len: usize, max: usize },
    #[error("Array contains the end pattern or the padding {0}")]
    ContainsPatternOrPadding(Value),
    #[error("{len} elements in array but only values up to {cap} elements are valid")]
    ExceedsCapacity { len: usize, cap: usize },
}

impl From<Error> for EncodingError {
    fn from(e: Error) -> Self {
        EncodingError::SubSchema(Box::new(e))
    }
}

/// Errors decoding a string with an [ArraySchema].
#[derive(Debug, thiserror::Error)]
pub enum DecodingError {
    #[error("Reading encoded data failed: {0}")]
    ReadFail(#[from] io::Error),
    #[error("Decoding sub-schema failed: {0}")]
    SubSchema(Box<Error>),
    #[error("Could not deencode length: {0}")]
    DecodingLength(#[from] int::DecodingError),
}

impl From<Error> for DecodingError {
    fn from(e: Error) -> Self {
        DecodingError::SubSchema(Box::new(e))
    }
}

impl DecodingError {
    pub fn due_to_eof(&self) -> bool {
        matches!(self, Self::ReadFail(e) if e.kind() == std::io::ErrorKind::UnexpectedEof)
    }
}

/// How is the length of variable sized data encoded.
#[derive(Debug, Clone, Deserialize)]
#[serde(rename_all = "camelCase")]
struct RawArray {
    #[serde(default)]
    length_encoding: RawLengthEncoding,
    max_items: Option<usize>,
    min_items: Option<usize>,
    items: DataSchema,
}

/// The array schema to describe arrays of homogeneous elements.
///
/// Contrary to the JSON schema's array schema tuples are not supported.
#[derive(Debug, Clone)]
pub struct ArraySchema {
    pub(crate) length: LengthEncoding<Value>,
    items: DataSchema,
}

fn validate_value(value: &Value, schema: &DataSchema) -> Result<(), ValidationError> {
    let mut buf = Vec::new();
    match schema.encode(&mut buf, value) {
        Ok(_) => Ok(()),
        Err(e) => Err(ValidationError::InvalidPatternOrPadding {
            value: value.clone(),
            type_: schema.type_(),
            error: Box::new(e),
        }),
    }
}

impl TryFrom<RawArray> for ArraySchema {
    type Error = ValidationError;

    fn try_from(raw: RawArray) -> Result<Self, Self::Error> {
        let schema = raw.items;
        let length = match (raw.min_items, raw.max_items) {
            (Some(min), Some(max)) if min == max => Ok(LengthEncoding::Fixed(min)),
            _ => match raw.length_encoding {
                RawLengthEncoding::Fixed => Err(ValidationError::IncompleteFixedLength),
                RawLengthEncoding::ExplicitLength(schema) => {
                    Ok(LengthEncoding::LengthEncoded(schema))
                }
                RawLengthEncoding::EndPattern { pattern } => {
                    validate_value(&pattern, &schema)?;
                    Ok(LengthEncoding::EndPattern { pattern })
                }
                RawLengthEncoding::Capacity { padding } => {
                    let capacity = raw.max_items.ok_or(ValidationError::MissingCapacity)?;
                    validate_value(&padding, &schema)?;
                    Ok(LengthEncoding::Capacity { padding, capacity })
                }
                RawLengthEncoding::TillEnd => Ok(LengthEncoding::TillEnd),
            },
        }?;

        Ok(Self {
            length,
            items: schema,
        })
    }
}

impl<'de> Deserialize<'de> for ArraySchema {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let raw = RawArray::deserialize(deserializer)?;
        ArraySchema::try_from(raw).map_err(D::Error::custom)
    }
}

impl ArraySchema {
    pub fn byte_array(length: LengthEncoding<Value>) -> Result<Self, ValidationError> {
        let byte_schema = IntegerSchema::unsigned_byte().into();
        match &length {
            LengthEncoding::EndPattern { pattern: value }
            | LengthEncoding::Capacity { padding: value, .. } => {
                validate_value(value, &byte_schema)?;
            }
            _ => {}
        }

        Ok(Self {
            length,
            items: byte_schema,
        })
    }
    fn valid_slice(&self, slice: &[Value]) -> Result<(), EncodingError> {
        match &self.length {
            LengthEncoding::Fixed(length) => {
                if slice.len() != *length {
                    Err(EncodingError::NotFixedLength {
                        len: slice.len(),
                        fixed: *length,
                    })
                } else {
                    Ok(())
                }
            }
            LengthEncoding::LengthEncoded(schema) => {
                if schema.max_value() < slice.len() {
                    Err(EncodingError::ExceedsLengthEncoding {
                        len: slice.len(),
                        max: schema.max_value(),
                    })
                } else {
                    Ok(())
                }
            }
            LengthEncoding::EndPattern { pattern } => {
                if slice.iter().any(|v| v == pattern) {
                    Err(EncodingError::ContainsPatternOrPadding(pattern.clone()))
                } else {
                    Ok(())
                }
            }
            LengthEncoding::Capacity { padding, capacity } => {
                if *capacity < slice.len() {
                    Err(EncodingError::ExceedsCapacity {
                        len: slice.len(),
                        cap: *capacity,
                    })
                } else if slice.iter().any(|v| v == padding) {
                    Err(EncodingError::ContainsPatternOrPadding(padding.clone()))
                } else {
                    Ok(())
                }
            }
            LengthEncoding::TillEnd => Ok(()),
        }
    }
}

impl Encoder for ArraySchema {
    type Error = EncodingError;

    fn encode<W>(&self, target: &mut W, value: &Value) -> Result<usize, Self::Error>
    where
        W: io::Write + WriteBytesExt,
    {
        let value = value
            .as_array()
            .ok_or_else(|| EncodingError::InvalidValue {
                value: value.to_string(),
            })?;
        let len = value.len();
        self.valid_slice(value)?;

        let mut written = 0;
        // pre-value
        if let LengthEncoding::LengthEncoded(schema) = &self.length {
            let len = len as u64;
            written += schema.encode(target, &(len.into()))?;
        }
        // write array
        for v in value.iter() {
            written += self.items.encode(target, v)?;
        }
        // post-value
        match &self.length {
            LengthEncoding::EndPattern { pattern } => {
                written += self.items.encode(target, pattern)?;
            }
            LengthEncoding::Capacity { padding, capacity } => {
                let left = *capacity - len;
                for _ in 0..left {
                    written += self.items.encode(target, padding)?;
                }
            }
            _ => {}
        }

        Ok(written)
    }
}

impl Decoder for ArraySchema {
    type Error = DecodingError;

    fn decode<R>(&self, target: &mut R) -> Result<Value, Self::Error>
    where
        R: io::Read + ReadBytesExt,
    {
        let elements = match &self.length {
            LengthEncoding::Fixed(len) => (0..*len)
                .map(|_| self.items.decode(target))
                .collect::<Result<Vec<_>, _>>()?,
            LengthEncoding::LengthEncoded(schema) => {
                let len = schema
                    .decode(target)?
                    .as_u64()
                    .expect("counts are always unsigned ints");
                (0..len)
                    .map(|_| self.items.decode(target))
                    .collect::<Result<Vec<_>, _>>()?
            }
            LengthEncoding::EndPattern { pattern } => {
                let mut elements = Vec::new();
                loop {
                    let element = self.items.decode(target)?;
                    if element != *pattern {
                        elements.push(element);
                    } else {
                        break;
                    }
                }
                elements
            }
            LengthEncoding::Capacity {
                padding: pattern,
                capacity,
            } => {
                let mut elements = Vec::new();
                for _ in 0..*capacity {
                    let element = self.items.decode(target)?;
                    if element != *pattern {
                        elements.push(element);
                    } else {
                        break;
                    }
                }
                elements
            }
            LengthEncoding::TillEnd => {
                let mut elements = Vec::new();
                loop {
                    match self.items.decode(target) {
                        Ok(element) => elements.push(element),
                        Err(e) if e.due_to_eof() => break,
                        Err(e) => return Err(e.into()),
                    }
                }
                elements
            }
        };

        Ok(elements.into())
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use anyhow::Result;
    use serde_json::{from_value, json};

    #[test]
    fn default() -> Result<()> {
        let schema = json!({});
        let schema = from_value::<ArraySchema>(schema);
        assert!(schema.is_err());
        Ok(())
    }
    #[test]
    fn schema_only() -> Result<()> {
        let schema = json!({
            "items": {
                "type": "boolean"
            }
        });
        let schema = from_value::<ArraySchema>(schema)?;
        assert!(matches!(
            schema,
            ArraySchema {
                length: LengthEncoding::TillEnd,
                ..
            }
        ));
        Ok(())
    }
    #[test]
    fn fixed() -> Result<()> {
        let schema = json!({
            "minItems": 2,
            "maxItems": 2,
            "items": {
                "type": "boolean"
            }
        });
        let schema = from_value::<ArraySchema>(schema)?;
        assert!(matches!(
            schema,
            ArraySchema {
                length: LengthEncoding::Fixed { .. },
                ..
            }
        ));

        let value = json!([false, true]);
        let mut buffer = vec![];
        assert_eq!(2, schema.encode(&mut buffer, &value)?);
        let expected: [u8; 2] = [0, 1];
        assert_eq!(&expected, buffer.as_slice());

        Ok(())
    }
    #[test]
    fn length() -> Result<()> {
        let schema = json!({
            "lengthEncoding": {
                "type": "explicitlength",
                "length": 1,
                "signed": false
            },
            "items": {
                "type": "boolean"
            }
        });
        let schema = from_value::<ArraySchema>(schema)?;
        assert!(matches!(
            schema,
            ArraySchema {
                length: LengthEncoding::LengthEncoded(_),
                ..
            }
        ));

        let value = json!([false, true]);
        let mut buffer = vec![];
        assert_eq!(3, schema.encode(&mut buffer, &value)?);
        let expected: [u8; 3] = [2, 0, 1];
        assert_eq!(&expected, buffer.as_slice());

        Ok(())
    }
}
