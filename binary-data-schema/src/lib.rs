//! Implementation of the binary data schema

#![warn(missing_debug_implementations)]

use byteorder::WriteBytesExt;
use serde::Deserialize;
use std::io;

mod integer;
pub use self::integer::*;
mod number;
pub use self::number::*;

pub type Result<T, E = Error> = std::result::Result<T, E>;

/// Errors from binary serialization.
#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("Encoding a value failed: {0}")]
    WriteFail(#[from] io::Error),
    #[error("Invalid length requested: Maximum allowed is {max} but {requested} where requested")]
    MaxLength { max: usize, requested: usize },
    #[error("The requsted length of {requested} is invalid for floating-point serialization. Either use 4 or 8 or use an integer encoding")]
    InvalidFloatingLength { requested: usize },
    #[error("The requested offset of {requested} bits is invalid as only {max} bits are available in the field.")]
    BitOffset { max: usize, requested: usize },
    #[error("The requested fieldsize of {requested} bits is insufficient. It must be between 1 and {max} bits.")]
    InvalidBitWidth { max: usize, requested: usize },
    #[error("Can not join no bitfields.")]
    NoBitfields,
    #[error("Can not join bitfields as they are overlapping.")]
    OverlappingBitfields,
    #[error("Can not join bitfields with varing number of bytes.")]
    NotSameBytes,
    #[error("Invalid integer schema. Not a bitfield: {bf}; nor an integer: {int}")]
    InvalidIntegerSchema { bf: Box<Error>, int: Box<Error> },
    #[error("The given value can not be encoded with the given schema.")]
    InvalidValue,
}

/// Length in bytes when binary serialized.
#[derive(Debug, Clone)]
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

/// A schema to serialize a value to bytes.
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

// /// The different schema types.
// #[derive(Debug, Deserialize)]
// #[serde(tag = "type")]
// #[serde(rename_all = "lowercase")]
// pub enum Schema {
//     /// NumberSchema
//     Number,
//     /// IntegerSchema
//     Integer(Integer),
//     /// BooleanSchema
//     Boolean,
//     /// StringSchema
//     String,
//     /// ArraySchema
//     Array,
//     /// ObjectSchema
//     Object,
//     /// NullSchema
//     Null,
// }

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

#[cfg(test)]
mod test {
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
}
