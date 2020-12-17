//! Implementation of the integer schema

use crate::{BinarySchema, ByteOrder, Error, Length, Result};
use byteorder::{WriteBytesExt, BE, LE};
use serde::Deserialize;
use std::io;

const MAX_INTEGER_SIZE: usize = 8;
const DEFAULT_LENGTH: usize = 4;
const DEFAULT_SIGNED: bool = false;

#[derive(Debug, Copy, Clone, Deserialize)]
pub struct Integer {
    #[serde(default)]
    pub(crate) byteorder: ByteOrder,
    #[serde(default = "Integer::default_length")]
    pub(crate) length: usize,
    #[serde(default = "Integer::default_signed")]
    pub(crate) signed: bool,
}

impl Integer {
    pub fn new(byteorder: ByteOrder, length: usize, signed: bool) -> Result<Self> {
        if length > MAX_INTEGER_SIZE {
            Err(Error::MaxLength {
                max: MAX_INTEGER_SIZE,
                requested: length,
            })
        } else {
            Ok(Self {
                byteorder,
                length,
                signed,
            })
        }
    }
    pub fn default_length() -> usize {
        DEFAULT_LENGTH
    }
    pub fn default_signed() -> bool {
        DEFAULT_SIGNED
    }
}

impl Default for Integer {
    fn default() -> Self {
        Self {
            length: Integer::default_length(),
            signed: Integer::default_signed(),
            byteorder: Default::default(),
        }
    }
}

impl BinarySchema for Integer {
    type Value = i64;

    fn length_encoded(&self) -> Length {
        Length::Fixed(self.length)
    }
    fn encoded_size(&self, _: &Self::Value) -> usize {
        self.length
    }
    fn encode<W>(&self, target: W, value: &Self::Value) -> Result<usize>
    where
        W: io::Write + WriteBytesExt,
    {
        let mut target = target;
        match (self.byteorder, self.signed) {
            (ByteOrder::BigEndian, true) => target.write_int::<BE>(*value, self.length)?,
            (ByteOrder::BigEndian, false) => target.write_uint::<BE>(*value as _, self.length)?,
            (ByteOrder::LittleEndian, true) => target.write_int::<LE>(*value, self.length)?,
            (ByteOrder::LittleEndian, false) => {
                target.write_uint::<LE>(*value as _, self.length)?
            }
        };

        Ok(self.length)
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use anyhow::Result;

    #[test]
    fn encode_integer_4_signed() -> Result<()> {
        let schema_msb = Integer::default();
        let schema_lsb = Integer {
            byteorder: ByteOrder::LittleEndian,
            ..schema_msb
        };
        let value: i32 = 0x1234_5678;
        let mut buffer = [0; 4];

        assert_eq!(4, schema_msb.encode(buffer.as_mut(), &(value as _))?);
        let buf_value = i32::from_be_bytes(buffer);
        assert_eq!(value, buf_value);

        assert_eq!(4, schema_lsb.encode(buffer.as_mut(), &(value as _))?);
        let buf_value = i32::from_le_bytes(buffer);
        assert_eq!(value, buf_value);

        Ok(())
    }

    #[test]
    fn encode_integer_3_unsigned() -> Result<()> {
        let schema_msb = Integer {
            length: 3,
            signed: false,
            byteorder: ByteOrder::BigEndian,
        };
        let schema_lsb = Integer {
            byteorder: ByteOrder::LittleEndian,
            ..schema_msb
        };
        let value: i64 = 0x123456;
        let mut buffer: Vec<u8> = vec![];

        assert_eq!(3, schema_msb.encode(&mut buffer, &(value))?);
        assert!(matches!(buffer.as_slice(), [0x12, 0x34, 0x56, ..]));

        assert_eq!(3, schema_lsb.encode(&mut buffer, &(value))?);
        assert!(matches!(
            buffer.as_slice(),
            [0x12, 0x34, 0x56, 0x56, 0x34, 0x12, ..]
        ));

        Ok(())
    }
}
