//! Implementation of the binary data schema

use byteorder::{ReadBytesExt, WriteBytesExt, BE, LE};
use serde::Deserialize;
use std::io;

pub type Result<T, E = Error> = std::result::Result<T, E>;

/// Errors from binary serialization.
#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error(
        "The provided buffer only provides {available} bytes but {required} bytes are required."
    )]
    InsufficientBuffer { required: usize, available: usize },
    #[error("Encoding a value failed: {0}")]
    WriteFail(#[from] io::Error),
}

/// Length in bytes when binary serialized.
pub enum Length {
    /// Always fixed.
    Fixed(usize),
    /// Fixed size but partially filled.
    PartiallyFilled { capacity: usize, end: u8 },
    /// Length is encoded in the field.
    ///
    /// The length is the first integer in the field, e.g. the first byte.
    Dynamic(Integer),
}

/// A chema to serialze a value to bytes.
pub trait BinarySchema {
    /// The type of values that can be serialized.
    type Value;

    /// General length a serialization of this schema.
    fn length_encoded(&self) -> Length;
    /// Concrete size in bytes the value will have serialized.
    fn encoded_size(&self, value: &Self::Value) -> usize;
    /// Write the value according to the schema.
    fn encode<W>(&self, target: W, value: &Self::Value) -> Result<usize>
    where
        W: io::Write + WriteBytesExt;
}

#[derive(Debug, Copy, Clone, Deserialize)]
pub struct Integer {
    #[serde(default)]
    byteorder: ByteOrder,
    #[serde(default = "Integer::default_length")]
    length: usize,
    #[serde(default = "Integer::default_signed")]
    signed: bool,
}

impl Integer {
    pub fn default_length() -> usize {
        4
    }
    pub fn default_signed() -> bool {
        true
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

/// The different schema types.
#[derive(Debug, Deserialize)]
#[serde(tag = "type")]
pub enum Schema {
    /// NumberSchema
    #[serde(rename = "number")]
    Number,
    #[serde(rename = "integer")]
    Integer(Integer),
    /// BooleanSchema
    #[serde(rename = "boolean")]
    Boolean,
    /// StringSchema
    #[serde(rename = "string")]
    String,
    /// ArraySchema
    #[serde(rename = "array")]
    Array,
    /// ObjectSchema
    #[serde(rename = "object")]
    Object,
    /// NullSchema
    #[serde(rename = "null")]
    Null,
}

#[derive(Debug, Copy, Clone, Deserialize, Eq, PartialEq)]
pub enum ByteOrder {
    #[serde(rename = "littleendian")]
    LittleEndian,
    #[serde(rename = "bigendian")]
    BigEndian,
}

impl Default for ByteOrder {
    fn default() -> Self {
        ByteOrder::BigEndian
    }
}

/// Base for each schema.
pub struct DataSchema {
    base: Schema,
    position: usize,
}

#[cfg(test)]
mod test {
    use super::*;
    use anyhow::Result;
    use serde_json::{from_reader, to_string_pretty};
    use std::fs::File;
    use valico::json_schema;

    #[test]
    fn validate_1() -> Result<()> {
        let schema = from_reader(File::open("test-files/1/schema.json")?)?;
        let mut scope = json_schema::Scope::new();
        let schema = scope.compile_and_return(schema, false)?;

        let valid = from_reader(File::open("test-files/1/valid.json")?)?;
        let invalid = from_reader(File::open("test-files/1/invalid.json")?)?;

        let expect_valid = schema.validate(&valid);
        let expect_invalid = schema.validate(&invalid);

        println!("Valid test result:\n{}", to_string_pretty(&expect_valid)?);
        println!(
            "Invalid test result:\n{}",
            to_string_pretty(&expect_invalid)?
        );

        assert!(expect_valid.is_valid());
        assert!(!expect_invalid.is_valid());
        Ok(())
    }

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
