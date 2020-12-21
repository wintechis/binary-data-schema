//! Implementation of the string schema

use crate::{
    BinarySchema, Bitfield, Error, Length, Result,
};
use byteorder::WriteBytesExt;
use serde::de::{Deserializer, Error as DeError};
use serde::Deserialize;
use std::convert::TryFrom;
use std::io;

/// How is the length of variable sized data encoded.
#[derive(Debug, Clone, Deserialize)]
#[serde(rename_all = "lowercase")]
pub struct LengthEncoding {
    pattern: Option<String>,
    length: Option<IntegerSchema>,
    capacity: Option<usize>,
}

/// Raw version of a Boolean schema. May hold invalid invariants.
#[derive(Debug, Copy, Clone, Deserialize)]
#[serde(rename_all = "lowercase")]
pub(crate) struct RawString {
    #[serde(default = "Boolean::default_length")]
    pub(crate) length: usize,
    #[serde(default, rename = "bitoffset")]
    pub(crate) offset: usize,
}

/// The number schema describes a numeric value.
#[derive(Debug, Clone)]
pub struct Boolean {
    bf: Bitfield,
}

pub const DEFAULT_LENGTH: usize = 1;

impl Boolean {
    pub fn default_length() -> usize {
        DEFAULT_LENGTH
    }
    pub fn length(&self) -> usize {
        self.bf.bytes()
    }
}

impl From<Bitfield> for Boolean {
    fn from(bf: Bitfield) -> Self {
        Self { bf }
    }
}

impl TryFrom<RawBoolean> for Boolean {
    type Error = Error;

    fn try_from(raw: RawBoolean) -> Result<Self, Self::Error> {
        let bf = Bitfield::new(raw.length, 1, raw.offset)?;

        Ok(bf.into())
    }
}

impl<'de> Deserialize<'de> for Boolean {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let raw = RawBoolean::deserialize(deserializer)?;
        Boolean::try_from(raw).map_err(|e| D::Error::custom(e))
    }
}

impl BinarySchema for Boolean {
    type Value = bool;

    fn length_encoded(&self) -> Length {
        self.bf.length_encoded()
    }
    fn encoded_size(&self, _: &Self::Value) -> usize {
        self.bf.encoded_size(&0)
    }
    fn encode<W>(&self, target: W, value: &Self::Value) -> Result<usize>
    where
        W: io::Write + WriteBytesExt,
    {
        let int = if *value { 1 } else { 0 };
        let written = self.bf.encode(target, &int)?;

        Ok(written)
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
        let b = from_value::<Boolean>(schema)?;
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
        let b = from_value::<Boolean>(schema)?;
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
