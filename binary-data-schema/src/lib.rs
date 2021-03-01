//! Implementation of the binary data schema

#![warn(missing_debug_implementations)]

use byteorder::{ReadBytesExt, WriteBytesExt};
use serde::{de::Error as DeError, Deserialize, Deserializer};
use serde_json::Value;
use std::{convert::TryFrom, io, string::FromUtf8Error};

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

/// Errors from binary serialization.
#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error(transparent)]
    Serialization(#[from] serde_json::Error),
    #[error("IO Error: {0}")]
    IoFail(#[from] io::Error),
    #[error("The encoded string is invalid: {0}")]
    InvalidString(#[from] FromUtf8Error),
    #[error("The encoded string is invalid: {0}")]
    InvalidBString(#[from] bstr::FromUtf8Error),
    #[error("Binary format string is invalid: {0}")]
    BinaryEncoding(#[from] hex::FromHexError),
    #[error("Can't encode '{value}' as string: {source}")]
    StringEncoding {
        value: String,
        source: self::string::EncodingError,
    },
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
    #[error("The default character has to be UTF8 encoded as one byte but '{0}' is encoded in {} bytes", .0.len_utf8())]
    InvalidDefaultChar(char),
    #[error("'{0}' is not a field in the schema.")]
    NotAField(String),
    #[error("The field '{0}' is not present in the value to encode.")]
    MissingField(String),
    #[error("A Json object was expected but got: {0}")]
    NotAnObject(String),
    #[error("The length of an array must be encoded in some way.")]
    MissingArrayLength,
    #[error(
        "Can not encode array {value} as its length is {len} but only length {fixed} is supported."
    )]
    NotMatchFixedLength {
        value: String,
        len: usize,
        fixed: usize,
    },
    #[error("Can not encode array {value} as its length is {len} but only length up to {max} can be encoded.")]
    ExceededLengthEncoding {
        value: String,
        len: usize,
        max: usize,
    },
    #[error("There are contrary specifications for a fixed-length array")]
    InconsitentFixedLength,
    #[error("The position {0} has been used multiple times in the object schema but only bitfields are allowed to share a position.")]
    InvalidPosition(usize),
    #[error("Can not decode value as the encoded lenght is {len} but capcacity is only {cap}.")]
    EncodedValueExceedsCapacity { len: usize, cap: usize },
    #[error("The encoded value '{read}' does not contain the endpattern '{pattern}'.")]
    NoPattern { read: String, pattern: String },
    #[error("The value '{value}' is not of type '{type_}' because of {source}.")]
    InvalidConst {
        value: String,
        type_: &'static str,
        #[source]
        source: Box<Error>,
    },
    #[error("Expected the constant value {expected} but got {got}")]
    InvalidConstValue { expected: String, got: String },
}

#[derive(Debug, Clone)]
pub struct DataSchema {
    inner: InnerSchema,
    const_: Option<Value>,
    context: Option<Value>,
}

/// Raw data schema to catch constant values.
#[derive(Debug, Clone, Deserialize)]
pub struct RawDataSchema {
    #[serde(flatten)]
    inner: InnerSchema,
    #[serde(rename = "const")]
    const_: Option<Value>,
    #[serde(rename = "jsonld:context")]
    context: Option<Value>,
}

/// The inner data schema without special features like `"const"` or
/// `"jsonld:context"`.
#[derive(Debug, Clone, Deserialize)]
#[serde(tag = "type", rename_all = "lowercase")]
pub enum InnerSchema {
    Boolean(BooleanSchema),
    Integer(IntegerSchema),
    Number(NumberSchema),
    String(StringSchema),
    Array(Box<ArraySchema>),
    Object(ObjectSchema),
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

impl InnerSchema {
    fn type_(&self) -> &'static str {
        match self {
            InnerSchema::Boolean(_) => "boolean",
            InnerSchema::Integer(_) => "integer",
            InnerSchema::Number(_) => "number",
            InnerSchema::String(_) => "string",
            InnerSchema::Array(_) => "array",
            InnerSchema::Object(_) => "object",
        }
    }
    fn is_bitfield(&self) -> bool {
        matches!(
            self,
            Self::Number(NumberSchema::Integer {
                integer: IntegerSchema::Bitfield(_),
                ..
            }) | Self::Integer(IntegerSchema::Bitfield(_))
        )
    }
}

impl Encoder for InnerSchema {
    fn encode<W>(&self, target: &mut W, value: &Value) -> Result<usize>
    where
        W: io::Write + WriteBytesExt,
    {
        match self {
            InnerSchema::Boolean(schema) => schema.encode(target, value),
            InnerSchema::Integer(schema) => schema.encode(target, value),
            InnerSchema::Number(schema) => schema.encode(target, value),
            InnerSchema::String(schema) => schema.encode(target, value),
            InnerSchema::Array(schema) => schema.encode(target, value),
            InnerSchema::Object(schema) => schema.encode(target, value),
        }
    }
}

impl Decoder for InnerSchema {
    fn decode<R>(&self, target: &mut R) -> Result<Value>
    where
        R: io::Read + ReadBytesExt,
    {
        match self {
            InnerSchema::Boolean(schema) => schema.decode(target),
            InnerSchema::Integer(schema) => schema.decode(target),
            InnerSchema::Number(schema) => schema.decode(target),
            InnerSchema::String(schema) => schema.decode(target),
            InnerSchema::Array(schema) => schema.decode(target),
            InnerSchema::Object(schema) => schema.decode(target),
        }
    }
}

impl DataSchema {
    /// The `"type"` tags value.
    pub fn type_(&self) -> &'static str {
        self.inner.type_()
    }
    /// Check whether the data schema encodes to/from a bitfield.
    pub fn is_bitfield(&self) -> bool {
        self.inner.is_bitfield()
    }
}

impl TryFrom<RawDataSchema> for DataSchema {
    type Error = Error;

    fn try_from(raw: RawDataSchema) -> Result<Self, Self::Error> {
        if let Some(value) = &raw.const_ {
            let mut dummy = Vec::new();
            if let Err(e) = raw.inner.encode(&mut dummy, value) {
                return Err(Error::InvalidConst {
                    value: value.to_string(),
                    type_: raw.inner.type_(),
                    source: Box::new(e),
                });
            }
        }

        Ok(Self {
            inner: raw.inner,
            const_: raw.const_,
            context: raw.context,
        })
    }
}

impl<'de> Deserialize<'de> for DataSchema {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let raw = RawDataSchema::deserialize(deserializer)?;
        DataSchema::try_from(raw).map_err(D::Error::custom)
    }
}

impl From<ObjectSchema> for InnerSchema {
    fn from(v: ObjectSchema) -> Self {
        Self::Object(v)
    }
}

impl From<ArraySchema> for InnerSchema {
    fn from(v: ArraySchema) -> Self {
        Self::Array(Box::new(v))
    }
}

impl From<Box<ArraySchema>> for InnerSchema {
    fn from(v: Box<ArraySchema>) -> Self {
        Self::Array(v)
    }
}

impl From<BooleanSchema> for InnerSchema {
    fn from(v: BooleanSchema) -> Self {
        Self::Boolean(v)
    }
}

impl From<IntegerSchema> for InnerSchema {
    fn from(v: IntegerSchema) -> Self {
        Self::Integer(v)
    }
}

impl From<NumberSchema> for InnerSchema {
    fn from(v: NumberSchema) -> Self {
        Self::Number(v)
    }
}

impl From<StringSchema> for InnerSchema {
    fn from(v: StringSchema) -> Self {
        Self::String(v)
    }
}

impl<S> From<S> for DataSchema
where
    S: Into<InnerSchema>,
{
    fn from(schema: S) -> Self {
        let inner = schema.into();
        Self {
            inner,
            const_: None,
            context: None,
        }
    }
}

impl Encoder for DataSchema {
    fn encode<W>(&self, target: &mut W, value: &Value) -> Result<usize>
    where
        W: io::Write + WriteBytesExt,
    {
        if let Some(c) = &self.const_ {
            self.inner.encode(target, c)
        } else {
            self.inner.encode(target, value)
        }
    }
}

impl Decoder for DataSchema {
    fn decode<R>(&self, target: &mut R) -> Result<Value>
    where
        R: io::Read + ReadBytesExt,
    {
        let mut res = self.inner.decode(target)?;

        if let Some(ctx) = &self.context {
            res["@context"] = ctx.clone();
        }

        Ok(res)
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use anyhow::Result;
    use serde_json::{from_value, json};

    #[test]
    fn const_schema() -> Result<()> {
        let schema = json!({
            "type": "object",
            "properties": {
                "start": {
                    "type": "integer",
                    "length": 1,
                    "const": 100,
                    "position": 1
                },
                "is_on": {
                    "type": "boolean",
                    "position": 5
                },
                "end": {
                    "type": "integer",
                    "length": 1,
                    "signed": false,
                    "const": 200,
                    "position": 10,
                }
            }
        });
        let schema = from_value::<DataSchema>(schema)?;
        println!("schema: {:#?}", schema);
        let value = json!({ "is_on": true });
        let expected = [100, 1, 200];
        let mut buffer = Vec::new();

        assert_eq!(3, schema.encode(&mut buffer, &value)?);
        assert_eq!(&expected, buffer.as_slice());

        let mut cursor = std::io::Cursor::new(buffer);
        let returned = schema.decode(&mut cursor)?;
        let expected = json!({
            "start": 100,
            "is_on": true,
            "end": 200
        });
        assert_eq!(returned, expected);

        Ok(())
    }

    #[test]
    fn led_rgb() -> Result<()> {
        let schema = json!({
            "type": "object",
            "properties": {
                "start": {
                    "type": "string",
                    "format": "binary",
                    "minLength": 4,
                    "maxLength": 4,
                    "const": "7e000503",
                    "position": 1
                },
                "red": {
                    "type": "integer",
                    "signed": false,
                    "length": 1,
                    "position": 3,
                    "description": "Red value of the color [0 - 255]."
                },
                "green": {
                    "type": "integer",
                    "signed": false,
                    "length": 1,
                    "position": 4,
                    "description": "Green value of the color [0 - 255]."
                },
                "blue": {
                    "type": "integer",
                    "signed": false,
                    "length": 1,
                    "position": 5,
                    "description": "Blue value of the color [0 - 255]."
                },
                "end": {
                    "type": "string",
                    "format": "binary",
                    "minLength": 2,
                    "maxLength": 2,
                    "const": "00ef",
                    "position": 10
                }
            }
        });
        let schema = from_value::<DataSchema>(schema)?;
        let value = json!({ "red": 255, "green": 16, "blue": 255 });
        let expected = [0x7e, 0, 0x05, 0x03, 0xff, 0x10, 0xff, 0, 0xef];

        let mut buffer = Vec::new();
        assert_eq!(9, schema.encode(&mut buffer, &value)?);
        assert_eq!(&expected, buffer.as_slice());

        let mut input = std::io::Cursor::new(expected);
        let returned = schema.decode(&mut input)?;
        assert_eq!(returned["red"], 255);
        assert_eq!(returned["green"], 16);
        assert_eq!(returned["blue"], 255);

        Ok(())
    }

    #[test]
    fn led_power() -> Result<()> {
        let schema = json!({
            "type": "object",
            "properties": {
                "start": {
                    "type": "string",
                    "format": "binary",
                    "minLength": 3,
                    "maxLength": 3,
                    "const": "7e0004",
                    "position": 1
                },
                "is_on": {
                    "type": "boolean",
                    "position": 5
                },
                "end": {
                    "type": "string",
                    "format": "binary",
                    "minLength": 5,
                    "maxLength": 5,
                    "const": "00000000ef",
                    "position": 10
                }
            }
        });
        let schema = from_value::<DataSchema>(schema)?;
        let value = json!({ "is_on": true });
        let expected = [0x7e, 0, 0x04, 1, 0, 0, 0, 0, 0xef];

        let mut buffer = Vec::new();
        assert_eq!(9, schema.encode(&mut buffer, &value)?);
        assert_eq!(&expected, buffer.as_slice());

        Ok(())
    }
}
