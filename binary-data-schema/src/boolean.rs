//! Implementation of the Boolean schema

use crate::{BinaryCodec, Bitfield, Error, Length, Result};
use byteorder::WriteBytesExt;
use serde::de::{Deserializer, Error as DeError};
use serde::Deserialize;
use serde_json::Value;
use std::convert::TryFrom;
use std::io;

/// Raw version of a Boolean schema. May hold invalid invariants.
#[derive(Debug, Copy, Clone, Deserialize)]
#[serde(rename_all = "lowercase")]
pub(crate) struct RawBoolean {
    #[serde(default = "BooleanSchema::default_length")]
    pub(crate) length: usize,
    #[serde(default, rename = "bitoffset")]
    pub(crate) offset: usize,
}

/// The Boolean schema describes a Boolean value.
///
/// Boolean schemata always describe a bitfield with a width of 1 bit.
#[derive(Debug, Clone)]
pub struct BooleanSchema {
    bf: Bitfield,
}

pub const DEFAULT_LENGTH: usize = 1;

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
    type Error = Error;

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

impl BinaryCodec for BooleanSchema {
    type Value = bool;

    fn length_encoded(&self) -> Length {
        self.bf.length_encoded()
    }
    fn encode<W>(&self, target: W, value: &Self::Value) -> Result<usize>
    where
        W: io::Write + WriteBytesExt,
    {
        let int = if *value { 1 } else { 0 };
        let written = self.bf.encode(target, &int)?;

        Ok(written)
    }
    fn encode_value<W>(&self, target: W, value: &Value) -> Result<usize>
    where
        W: io::Write + WriteBytesExt,
    {
        if let Some(value) = value.as_bool() {
            self.encode(target, &value)
        } else {
            Err(Error::InvalidValue {
                value: value.to_string(),
                type_: "boolean",
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
        let b = from_value::<BooleanSchema>(schema)?;
        let mut buf = [0];
        assert_eq!(1, b.encode(buf.as_mut(), &true)?);
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
        assert_eq!(2, b.encode(buf.as_mut(), &true)?);
        assert_eq!(1 << 3, buf[0]);
        assert_eq!(0, buf[1]);
        assert_eq!(2, b.encode(buf.as_mut(), &false)?);
        assert_eq!(0, buf[0]);
        assert_eq!(0, buf[1]);

        Ok(())
    }
}
