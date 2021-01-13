//! Implementation of the string schema

use crate::{BinaryCodec, DataSchema, Error, IntegerSchema, JoinedBitfield, Length, Result};
use byteorder::WriteBytesExt;
use serde::de::{Deserializer, Error as DeError};
use serde::Deserialize;
use serde_json::Value;
use std::io;
use std::{collections::HashMap, convert::TryFrom};

/// A single property within an object schema.
#[derive(Debug, Clone, Deserialize)]
struct RawProperty {
    #[serde(flatten)]
    schema: DataSchema,
    /// Position of the property within the object. Determines the layout of
    /// binary serialization.
    position: usize,
}

#[derive(Debug, Clone, Deserialize)]
struct RawObject {
    properties: HashMap<String, RawProperty>,
}

#[derive(Debug, Clone)]
pub enum PropertySchema {
    Simple { name: String, schema: DataSchema },
    Merged(JoinedBitfield),
}

impl BinaryCodec for PropertySchema {
    type Value = Value;

    fn length_encoded(&self) -> Length {
        match self {
            PropertySchema::Simple { schema, .. } => schema.length_encoded(),
            PropertySchema::Merged(schema) => schema.length_encoded(),
        }
    }
    fn encode<W>(&self, target: W, value: &Self::Value) -> Result<usize>
    where
        W: io::Write + WriteBytesExt,
    {
        self.encode_value(target, value)
    }
    fn encode_value<W>(&self, target: W, value: &Value) -> Result<usize>
    where
        W: io::Write + WriteBytesExt,
    {
        match self {
            PropertySchema::Simple { name, schema } => {
                let value = value
                    .get(name)
                    .ok_or_else(|| Error::MissingField(name.clone()))?;
                schema.encode_value(target, value)
            }
            PropertySchema::Merged(schema) => schema.encode_value(target, value),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Property {
    position: usize,
    schema: PropertySchema,
}

/// The object schema to describe structured data.
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
}

impl TryFrom<RawObject> for ObjectSchema {
    type Error = Error;

    fn try_from(raw: RawObject) -> Result<Self, Self::Error> {
        let mut ordered = HashMap::with_capacity(raw.properties.len());
        raw.properties.into_iter().for_each(|(name, raw)| {
            let bucket = ordered.entry(raw.position).or_insert_with(|| vec![]);
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
                    .map(|(name, schema)| match schema {
                        DataSchema::Integer(IntegerSchema::Bitfield(bf)) => Ok((name, bf)),
                        _ => Err(Error::InvalidPosition(position)),
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
        Ok(Self { properties })
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

impl BinaryCodec for ObjectSchema {
    type Value = Value;

    fn length_encoded(&self) -> Length {
        self.properties
            .iter()
            .map(|p| p.schema.length_encoded())
            .sum()
    }
    fn encode<W>(&self, target: W, value: &Self::Value) -> Result<usize>
    where
        W: io::Write + WriteBytesExt,
    {
        let mut target = target;
        let mut written = 0;
        for p in self.properties.iter() {
            written += p.schema.encode_value(&mut target, &value)?;
        }

        Ok(written)
    }
    fn encode_value<W>(&self, target: W, value: &Value) -> Result<usize>
    where
        W: io::Write + WriteBytesExt,
    {
        self.encode(target, value)
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use anyhow::Result;
    use serde_json::{from_value, json};
}
