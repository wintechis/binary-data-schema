//! Implementation of the string schema

use crate::{BinaryCodec, DataSchema, Error, IntegerSchema, Length, Result};
use byteorder::WriteBytesExt;
use serde::de::{Deserializer, Error as DeError};
use serde::Deserialize;
use serde_json::Value;
use std::convert::TryFrom;
use std::io;

#[derive(Debug, Clone, Deserialize)]
#[serde(tag = "type", rename_all = "lowercase")]
enum LengthEncoding {
    /// Requires `"minItems"` and  `"maxItems"` set to the same value.
    /// All array have that many entries. If there are more or less entries to
    /// encode an error is raised.
    Fixed { length: usize },
    /// The number of entries in the array is stored as the beginning using the
    /// given integer schema.
    ExplicitLength(IntegerSchema),
}

/// How is the length of variable sized data encoded.
#[derive(Debug, Clone, Deserialize)]
#[serde(rename_all = "camelCase")]
struct RawArray {
    length_encoding: Option<IntegerSchema>,
    max_items: Option<usize>,
    min_items: Option<usize>,
    items: DataSchema,
}

/// The array schema to describe arrays of homogeneous elements.
///
/// Contrary to the JSON schema's array schema tuples are not supported.
#[derive(Debug, Clone)]
pub struct ArraySchema {
    length: LengthEncoding,
    items: DataSchema,
}

impl ArraySchema {
    fn valid_slice<T: std::fmt::Debug>(&self, slice: &[T]) -> Result<()> {
        match &self.length {
            LengthEncoding::Fixed { length } => {
                if slice.len() != *length {
                    Err(Error::NotMatchFixedLength {
                        value: format!("{:?}", slice),
                        len: slice.len(),
                        fixed: *length,
                    })
                } else {
                    Ok(())
                }
            }
            LengthEncoding::ExplicitLength(schema) => {
                if schema.max_value() < slice.len() {
                    Err(Error::ExceededLengthEncoding {
                        value: format!("{:?}", slice),
                        len: slice.len(),
                        max: schema.max_value(),
                    })
                } else {
                    Ok(())
                }
            }
        }
    }
}

impl TryFrom<RawArray> for ArraySchema {
    type Error = Error;

    fn try_from(raw: RawArray) -> Result<Self, Self::Error> {
        let length = match (raw.min_items, raw.max_items, raw.length_encoding) {
            (Some(min), Some(max), None) if min == max => LengthEncoding::Fixed {length: max},
            (_, _, None) => return Err(Error::MissingArrayLength),
            (_, _, Some(schema)) => LengthEncoding::ExplicitLength(schema),
        };

        Ok(Self {
            length,
            items: raw.items,
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

impl BinaryCodec for ArraySchema {
    type Value = [Value];

    fn length_encoded(&self) -> Length {
        match self.length {
            LengthEncoding::Fixed { length } => Length::Fixed(length),
            LengthEncoding::ExplicitLength(_) => Length::Variable,
        }
    }
    fn encode<W>(&self, target: W, value: &Self::Value) -> Result<usize>
    where
        W: io::Write + WriteBytesExt,
    {
        let mut target = target;
        self.valid_slice(value)?;

        match &self.length {
            LengthEncoding::Fixed { .. } => {
                let mut written = 0;
                for v in value.iter() {
                    written += self.items.encode_value(&mut target, v)?;
                }
                Ok(written)
            }
            LengthEncoding::ExplicitLength(len_schema) => {
                let len = value.len() as _;
                let mut written = len_schema.encode(&mut target, &len)?;
                for v in value.iter() {
                    written += self.items.encode_value(&mut target, v)?;
                }
                Ok(written)
            }
        }
    }
    fn encode_value<W>(&self, target: W, value: &Value) -> Result<usize>
    where
        W: io::Write + WriteBytesExt,
    {
        if let Some(array) = value.as_array() {
            self.encode(target, array)
        } else {
            Err(Error::InvalidValue {
                value: value.to_string(),
                type_: "array",
            })
        }
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
        let schema = from_value::<ArraySchema>(schema);
        assert!(schema.is_err());
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
        assert!(matches!(schema, ArraySchema {length: LengthEncoding::Fixed {..}, ..}));

        let value = json!([false, true]);
        let mut buffer = vec![];
        assert_eq!(2, schema.encode_value(&mut buffer, &value)?);
        let expected: [u8; 2] = [0, 1];
        assert_eq!(&expected, buffer.as_slice());

        Ok(())
    }
    #[test]
    fn length() -> Result<()> {
        let schema = json!({
            "lengthEncoding": {
                "length": 1,
                "signed": false
            },
            "items": {
                "type": "boolean"
            }
        });
        let schema = from_value::<ArraySchema>(schema)?;
        assert!(matches!(schema, ArraySchema {length: LengthEncoding::ExplicitLength(_), ..}));

        let value = json!([false, true]);
        let mut buffer = vec![];
        assert_eq!(3, schema.encode_value(&mut buffer, &value)?);
        let expected: [u8; 3] = [2, 0, 1];
        assert_eq!(&expected, buffer.as_slice());

        Ok(())
    }
}
