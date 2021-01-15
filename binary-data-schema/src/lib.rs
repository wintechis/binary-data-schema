//! Implementation of the binary data schema

#![warn(missing_debug_implementations)]

use byteorder::{ReadBytesExt, WriteBytesExt};
use serde::Deserialize;
use serde_json::Value;
use std::{io, string::FromUtf8Error};

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
mod object;
pub use self::object::*;

pub type Result<T, E = Error> = std::result::Result<T, E>;

/// Errors from binary serialization.
#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error(transparent)]
    Serialization(#[from] serde_json::Error),
    #[error("Encoding a value failed: {0}")]
    WriteFail(#[from] io::Error),
    #[error("The encoded string is invalid: {0}")]
    InvalidString(#[from] FromUtf8Error),
    #[error("The encoded string is invalid: {0}")]
    InvalidBString(#[from] bstr::FromUtf8Error),
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
    #[error("The value '{value}' can not be encoded with a {type_} schema.")]
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
    #[error("The field '{0}' is not present in the value to encode.")]
    MissingField(String),
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
    #[error("The position {0} has been used multiple times in the object schema but only bitfields are allowed to share a position.")]
    InvalidPosition(usize),
    #[error("Can not decode value as the encoded lenght is {len} but capcacity is only {cap}.")]
    EncodedValueExceedsCapacity {
        len: usize,
        cap: usize
    },
    #[error("The encoded value '{read}' does not contain the endpattern '{pattern}'.")]
    NoPattern {
        read: String,
        pattern: String,
    }
}

/// A schema to serialize a value to bytes.
pub trait Encoder {
    /// Write a Json value according to the schema.
    fn encode<W>(&self, target: &mut W, value: &Value) -> Result<usize>
    where
        W: io::Write + WriteBytesExt;
}

/// A schema to de-serialize a value from bytes.
pub trait Decoder {
    fn decode<R>(&self, target: &mut R) -> Result<Value>
    where
        R: io::Read + ReadBytesExt;
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
    Array(Box<ArraySchema>),
    Object(ObjectSchema),
}

impl From<ObjectSchema> for DataSchema {
    fn from(v: ObjectSchema) -> Self {
        DataSchema::Object(v)
    }
}

impl From<ArraySchema> for DataSchema {
    fn from(v: ArraySchema) -> Self {
        DataSchema::Array(Box::new(v))
    }
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

impl Encoder for DataSchema {
    fn encode<W>(&self, target: &mut W, value: &Value) -> Result<usize>
    where
        W: io::Write + WriteBytesExt,
    {
        match self {
            DataSchema::Boolean(schema) => schema.encode(target, value),
            DataSchema::Integer(schema) => schema.encode(target, value),
            DataSchema::Number(schema) => schema.encode(target, value),
            DataSchema::String(schema) => schema.encode(target, value),
            DataSchema::Array(schema) => schema.encode(target, value),
            DataSchema::Object(schema) => schema.encode(target, value),
        }
    }
}

impl Decoder for DataSchema {
    fn decode<R>(&self, target: &mut R) -> Result<Value>
    where
        R: io::Read + ReadBytesExt,
    {
        match self {
            DataSchema::Boolean(dec) => dec.decode(target),
            DataSchema::Integer(dec) => dec.decode(target),
            DataSchema::Number(dec) => dec.decode(target),
            DataSchema::String(_) => todo!(),
            DataSchema::Array(dec) => dec.decode(target),
            DataSchema::Object(dec) => dec.decode(target),
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
