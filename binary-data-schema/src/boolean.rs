//! Implementation of the Boolean schema

use crate::{Bitfield, Encoder, Error, Result};
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

impl Encoder for BooleanSchema {
    fn encode<W>(&self, target: &mut W, value: &Value) -> Result<usize>
    where
        W: io::Write + WriteBytesExt,
    {
        let value = value.as_bool().ok_or_else(|| Error::InvalidValue {
            value: value.to_string(),
            type_: "boolean",
        })?;
        let int = if value { 1 } else { 0 };
        let written = self.bf.encode(target, &int.into())?;

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
