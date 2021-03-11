//! Implementation of the boolean schema.
//!
//! By default [BooleanSchema] encode the Boolean values `true` and `false` to
//! one byte. However, internally it only targets a single bit. This allows BDS
//! to recognize [BooleanSchema] as [bitfields].
//!
//! # Parameters
//!
//! | Key           | Type   | Default | Comment |
//! | ------------- | ------:| -------:| ------- |
//! | `"bitoffset"` | `uint` |       0 | Number of bits the [bitfield] is shifted |
//! | `"length"`    | `uint` |       1 | Number of bytes the field encodes |
//!
//! _Note:_ If the [BooleanSchema] is part of a merged [bitfield] its length
//! must match the other bitfields.
//!
//! ## Validation
//!
//! Like for every other [bitfield] the `"bitoffset"` must not exceed the field's
//! length, i.e.:
//!
//! ```text
//! "bitoffset" + 1 <= "length" * 8
//! ```
//!
//! _Note:_ The width ([`"bits"`]) of a [BooleanSchema] is always 1.
//!
//! # Example
//!
//! All parameters of [BooleanSchema] has default values. Accordingly, an empty
//! schema is valid.
//!
//! ```
//! # use binary_data_schema::*;
//! # use valico::json_schema;
//! # use serde_json::{json, from_value};
//! let schema = json!({ "type": "boolean" });
//!
//! let mut scope = json_schema::Scope::new();
//! let j_schema = scope.compile_and_return(schema.clone(), false)?;
//! let schema = from_value::<DataSchema>(schema)?;
//!
//! let value = json!(true);
//! assert!(j_schema.validate(&value).is_valid());
//! let mut encoded = Vec::new();
//! schema.encode(&mut encoded, &value)?;
//! # let expected = [1];
//! # assert_eq!(&expected, encoded.as_slice());
//!
//! let mut encoded = std::io::Cursor::new(encoded);
//! let back = schema.decode(&mut encoded)?;
//! assert!(j_schema.validate(&back).is_valid());
//! assert_eq!(back, value);
//! # Ok::<(), anyhow::Error>(())
//! ```
//!
//! [bitfields]: ../object/index.html#bitfields
//! [bitfield]: ../object/index.html#bitfields
//! [`"bits"`]: ../integer/index.html#parameters

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

/// The Boolean schema describes a Boolean value (further information on [the module's documentation](index.html)).
///
/// Boolean schemata always describe a bitfield with a width of 1 bit.
#[derive(Debug, Clone)]
pub struct BooleanSchema {
    pub(crate) bf: Bitfield,
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
