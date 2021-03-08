//! Implementation of the Boolean schema

use std::{convert::TryFrom, io};

use byteorder::{ReadBytesExt, WriteBytesExt};
use serde::{
    de::{Deserializer, Error as DeError},
    Deserialize,
};
use serde_json::Value;

use crate::{integer::Bitfield, Decoder, Encoder};

/// Default length of boolean schemata.
pub const DEFAULT_LENGTH: usize = 1;

/// Errors validating a [BooleanSchema].
#[derive(Debug, thiserror::Error)]
pub enum ValidationError {
    #[error(transparent)]
    InvalidBitAddressing(#[from] crate::integer::ValidationError),
}

/// Errors encoding a string with a [BooleanSchema].
#[derive(Debug, thiserror::Error)]
pub enum EncodingError {
    #[error("The value '{value}' can not be encoded with a boolean schema")]
    InvalidValue { value: String },
    #[error("Failed to encode flag: {0}")]
    Bitfield(#[from] crate::integer::EncodingError),
}

/// Errors decoding a string with a [BooleanSchema].
#[derive(Debug, thiserror::Error)]
pub enum DecodingError {
    #[error("Failed to decode flag: {0}")]
    Bitfield(#[from] crate::integer::DecodingError),
}

impl DecodingError {
    pub fn due_to_eof(&self) -> bool {
        match &self {
            Self::Bitfield(e) => e.due_to_eof(),
        }
    }
}

/// Raw version of a Boolean schema. May hold invalid invariants.
#[derive(Debug, Copy, Clone, Deserialize)]
#[serde(rename_all = "lowercase")]
struct RawBoolean {
    #[serde(default = "BooleanSchema::default_length")]
    length: usize,
    #[serde(default, rename = "bitoffset")]
    offset: usize,
}

/// The Boolean schema describes a Boolean value.
///
/// Boolean schemata always describe a bitfield with a width of 1 bit.
#[derive(Debug, Clone)]
pub struct BooleanSchema {
    bf: Bitfield,
}

impl BooleanSchema {
    pub fn default_length() -> usize {
        DEFAULT_LENGTH
    }
    pub fn length(&self) -> usize {
        self.bf.bytes()
    }
}

impl From<Bitfield> for BooleanSchema {
    fn from(bf: Bitfield) -> Self {
        Self { bf }
    }
}

impl TryFrom<RawBoolean> for BooleanSchema {
    type Error = ValidationError;

    fn try_from(raw: RawBoolean) -> Result<Self, Self::Error> {
        let bf = Bitfield::new(raw.length, 1, raw.offset)?;

        Ok(bf.into())
    }
}

impl<'de> Deserialize<'de> for BooleanSchema {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let raw = RawBoolean::deserialize(deserializer)?;
        BooleanSchema::try_from(raw).map_err(D::Error::custom)
    }
}

impl Encoder for BooleanSchema {
    type Error = EncodingError;

    fn encode<W>(&self, target: &mut W, value: &Value) -> Result<usize, Self::Error>
    where
        W: io::Write + WriteBytesExt,
    {
        let value = value.as_bool().ok_or_else(|| EncodingError::InvalidValue {
            value: value.to_string(),
        })?;
        let int = if value { 1 } else { 0 };
        let written = self.bf.encode(target, &int.into())?;

        Ok(written)
    }
}

impl Decoder for BooleanSchema {
    type Error = DecodingError;

    fn decode<R>(&self, target: &mut R) -> Result<Value, Self::Error>
    where
        R: io::Read + ReadBytesExt,
    {
        let int = self
            .bf
            .decode(target)?
            .as_u64()
            .expect("always u64 from BF");
        let b = int != 0;
        Ok(b.into())
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
        let b = from_value::<BooleanSchema>(schema)?;
        let mut buf = [0];
        assert_eq!(1, b.encode(&mut buf.as_mut(), &json!(true))?);
        assert_eq!(1, buf[0]);

        Ok(())
    }

    #[test]
    fn bit3_byte2() -> Result<()> {
        let schema = json!({
            "bitoffset": 11,
            "length": 2
        });
        let b = from_value::<BooleanSchema>(schema)?;
        let mut buf = [0, 0];
        assert_eq!(2, b.encode(&mut buf.as_mut(), &json!(true))?);
        assert_eq!(1 << 3, buf[0]);
        assert_eq!(0, buf[1]);
        assert_eq!(2, b.encode(&mut buf.as_mut(), &json!(false))?);
        assert_eq!(0, buf[0]);
        assert_eq!(0, buf[1]);

        Ok(())
    }
}
