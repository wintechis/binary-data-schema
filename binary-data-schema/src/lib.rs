//! Implementation of the binary data schema

#![warn(missing_debug_implementations)]

use byteorder::WriteBytesExt;
use serde::Deserialize;
use serde_json::Value;
use std::io;
use std::ops::Add;

mod integer;
pub use self::integer::*;
mod number;
pub use self::number::*;
mod boolean;
pub use self::boolean::*;
mod string;
pub use self::string::*;
mod array;
pub use self::array::*;

pub type Result<T, E = Error> = std::result::Result<T, E>;

/// Errors from binary serialization.
#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error(transparent)]
    Serialization(#[from] serde_json::Error),
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
    #[error("The value '{}' can not be encoded with a {type_} schema.")]
    InvalidValue { value: String, type_: &'static str },
    #[error("A fixed length string schema requires a 'maxLength' property.")]
    MissingCapacity,
    #[error(
        "The value '{value}' can not be encoded as it contains the end sequence '{sequence}'."
    )]
    ContainsEndSequence { value: String, sequence: String },
    #[error("The value '{value}' has a length of {len} but only values with a length of {fixed} are supported.")]
    NotMatchFixedLength {
        value: String,
        len: usize,
        fixed: usize,
    },
    #[error("The value '{value}' has a length of {} but only values up to a length of {cap} are valid.", value.len())]
    ToLongString { value: String, cap: usize },
    #[error("The default character has to be UTF8 encoded as one byte but '{0}' is encoded in {} bytes", .0.len_utf8())]
    InvalidDefaultChar(char),
    #[error("'{0}' is not a field in the schema.")]
    NotAField(String),
    #[error("A Json object was expected but got: {0}")]
    NotAnObject(String),
    #[error("The value '{value}' has a length of {len} but the length encoding can only handle a length of up to {max}.")]
    ExceededLengthEncoding {
        value: String,
        len: usize,
        max: usize,
    },
    #[error("The length of an array must be encoded in some way.")]
    MissingArrayLength,
    #[error("There are contrary specifications for a fixed-length array")]
    InconsitentFixedLength,
}

/// Length in bytes when binary serialized.
#[derive(Debug, Clone, Copy)]
pub enum Length {
    /// Always fixed.
    Fixed(usize),
    /// Varies depending on the value.
    Variable,
}

impl Add for Length {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Length::Fixed(lhs), Length::Fixed(rhs)) => Length::Fixed(lhs + rhs),
            (Length::Fixed(_), Length::Variable)
            | (Length::Variable, Length::Fixed(_))
            | (Length::Variable, Length::Variable) => Length::Variable,
        }
    }
}

/// A schema to serialize a value to bytes.
pub trait BinaryCodec {
    /// The type of values that can be serialized.
    type Value: ?Sized;

    /// General length a serialization of this schema.
    fn length_encoded(&self) -> Length;
    /// Write the value according to the schema.
    fn encode<W>(&self, target: W, value: &Self::Value) -> Result<usize>
    where
        W: io::Write + WriteBytesExt;
    /// Write the a Json value according to the schema.
    fn encode_value<W>(&self, target: W, value: &Value) -> Result<usize>
    where
        W: io::Write + WriteBytesExt;
}

#[derive(Debug, Copy, Clone, Deserialize, Eq, PartialEq)]
#[serde(rename_all = "lowercase")]
pub enum ByteOrder {
    LittleEndian,
    BigEndian,
}

impl Default for ByteOrder {
    fn default() -> Self {
        ByteOrder::BigEndian
    }
}

#[derive(Debug, Clone, Deserialize)]
#[serde(tag = "type", rename_all = "lowercase")]
pub enum DataSchema {
    Boolean(BooleanSchema),
    Integer(IntegerSchema),
    Number(NumberSchema),
    String(StringSchema),
}

impl From<BooleanSchema> for DataSchema {
    fn from(v: BooleanSchema) -> Self {
        DataSchema::Boolean(v)
    }
}

impl From<IntegerSchema> for DataSchema {
    fn from(v: IntegerSchema) -> Self {
        DataSchema::Integer(v)
    }
}

impl From<NumberSchema> for DataSchema {
    fn from(v: NumberSchema) -> Self {
        DataSchema::Number(v)
    }
}

impl From<StringSchema> for DataSchema {
    fn from(v: StringSchema) -> Self {
        DataSchema::String(v)
    }
}

impl BinaryCodec for DataSchema {
    type Value = Value;

    fn length_encoded(&self) -> Length {
        match self {
            DataSchema::Boolean(schema) => schema.length_encoded(),
            DataSchema::Integer(schema) => schema.length_encoded(),
            DataSchema::Number(schema) => schema.length_encoded(),
            DataSchema::String(schema) => schema.length_encoded(),
        }
    }
    fn encode<W>(&self, target: W, value: &Self::Value) -> Result<usize>
    where
        W: io::Write + WriteBytesExt,
    {
        match self {
            DataSchema::Boolean(schema) => {
                let value = value.as_bool().ok_or_else(|| Error::InvalidValue {
                    value: value.to_string(),
                    type_: "boolean",
                })?;
                schema.encode(target, &value)
            }
            DataSchema::Integer(schema) => {
                let value = value.as_i64().ok_or_else(|| Error::InvalidValue {
                    value: value.to_string(),
                    type_: "integer",
                })?;
                schema.encode(target, &value)
            }
            DataSchema::Number(schema) => {
                let value = value.as_f64().ok_or_else(|| Error::InvalidValue {
                    value: value.to_string(),
                    type_: "number",
                })?;
                schema.encode(target, &value)
            }
            DataSchema::String(schema) => {
                let value = value.as_str().ok_or_else(|| Error::InvalidValue {
                    value: value.to_string(),
                    type_: "string",
                })?;
                schema.encode(target, value)
            }
        }
    }
    fn encode_value<W>(&self, target: W, value: &Value) -> Result<usize>
    where
        W: io::Write + WriteBytesExt,
    {
        match self {
            DataSchema::Boolean(schema) => schema.encode_value(target, value),
            DataSchema::Integer(schema) => schema.encode_value(target, value),
            DataSchema::Number(schema) => schema.encode_value(target, value),
            DataSchema::String(schema) => schema.encode_value(target, value),
        }
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
