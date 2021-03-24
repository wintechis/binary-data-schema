//! Binary Data Schema (BDS) is an extension of [JSON schema].
//! With this extension it is possible to convert JSON documents into raw bytes and
//! reverse.
//!
//! The intention is to use BDS in [WoT Thing Descriptions] in order to allow `application/octet-stream` as a [content type for forms].
//!
//! # Features
//!
//! The specific features for each schema are explained in their sub module:
//!
//! - [boolean schema](boolean)
//! - [integer schema](integer)
//! - [number schema](number)
//! - [string schema](string)
//! - [array schema](array)
//! - [object schema](object)
//!
//! Each feature is explained with an example. The examples follow the same structure as the (commented) [`default` example](#example) below.
//!
//! BDS is by far not feature complete. If you do not find a feature described it is probably safe to assume that it is not yet implemented.
//! If you require a specific feature [file an issue], please.
//! PRs are also welcome.
//!
//! ## `default`
//!
//! The only feature described on this level is `default`.
//!
//! In general binary protocols often have some kind of _magic_ start and end bytes.
//! To simulate those BDS uses the [`default` keyword].
//! When encoding a JSON document fields whose schema has a `default` value those do not have to be provided.
//!
//! - Fields with `default` are not required for encoding but included when
//!   decoded.
//! - To keep BDS aligned with [JSON schema] it is recommended to add
//!   [`"required"`] to object schemata.
//!
//! ### Example
//!
//! ```
//! # use binary_data_schema::*;
//! # use valico::json_schema;
//! # use serde_json::{json, from_value};
//! let schema = json!({
//!     "type": "object",
//!     "properties": {
//!         "start": {
//!             "type": "string",
//!             "format": "binary",
//!             "minLength": 2,
//!             "maxLength": 2,
//!             "default": "fe",
//!             "position": 1
//!         },
//!         "is_on": {
//!             "type": "boolean",
//!             "position": 5
//!         },
//!         "end": {
//!             "type": "string",
//!             "format": "binary",
//!             "minLength": 2,
//!             "maxLength": 2,
//!             "default": "ef",
//!             "position": 10
//!         }
//!     },
//!     "required": ["is_on"]
//! });
//! let mut scope = json_schema::Scope::new();
//! // Valid JSON schema
//! let j_schema = scope.compile_and_return(schema.clone(), false)?;
//! // Valid Binary Data schema
//! let schema = from_value::<DataSchema>(schema)?;
//!
//! let value = json!({ "is_on": true });
//! // 'value' is valid for the JSON schema
//! assert!(j_schema.validate(&value).is_valid());
//! let mut encoded = Vec::new();
//! // 'value' is valid for the Binary Data schema
//! schema.encode(&mut encoded, &value)?;
//! # let expected = [0xfe, 1, 0xef];
//! # assert_eq!(&expected, encoded.as_slice());
//!
//! let mut encoded = std::io::Cursor::new(encoded);
//! let back = schema.decode(&mut encoded)?;
//! let expected = json!({
//!     "start": "fe",
//!     "is_on": true,
//!     "end": "ef"
//! });
//! // The retrieved value is valid for the JSON schema
//! assert!(j_schema.validate(&back).is_valid());
//! // The retrieved value is as expected
//! assert_eq!(back, expected);
//! # Ok::<(), anyhow::Error>(())
//! ```
//!
//! [JSON schema]: https://json-schema.org/
//! [WoT Thing Descriptions]: https://www.w3.org/TR/wot-thing-description
//! [content type for forms]: https://www.w3.org/TR/2020/NOTE-wot-binding-templates-20200130/#content-types
//! [file an issue]: https://github.com/wintechis/binary-data-schema/issues
//! [`default` keyword]: http://json-schema.org/understanding-json-schema/reference/generic.html#annotations
//! [`"required"`]: http://json-schema.org/understanding-json-schema/reference/object.html#required-properties

#![warn(missing_debug_implementations)]

pub mod array;
pub mod boolean;
pub mod integer;
pub mod number;
pub mod object;
pub mod string;
pub(crate) mod util;
pub use self::util::LengthEncoding;

use std::{convert::TryFrom, io, string::FromUtf8Error};

use byteorder::{ReadBytesExt, WriteBytesExt};
use integer::Bitfield;
use serde::{de::Error as DeError, Deserialize, Deserializer};
use serde_json::Value;

use crate::{
    array::ArraySchema, boolean::BooleanSchema, integer::IntegerSchema, number::NumberSchema,
    object::ObjectSchema, string::StringSchema,
};

pub type Result<T, E = Error> = std::result::Result<T, E>;

/// A schema to serialize a value to bytes.
pub trait Encoder {
    /// Error encoding a value.
    type Error;

    /// Write a Json value according to the schema.
    fn encode<W>(&self, target: &mut W, value: &Value) -> Result<usize, Self::Error>
    where
        W: io::Write + WriteBytesExt;
}

/// A schema to de-serialize a value from bytes.
pub trait Decoder {
    /// Error decoding a value.
    type Error;

    /// Decode a value from a target with the given schema.
    fn decode<R>(&self, target: &mut R) -> Result<Value, Self::Error>
    where
        R: io::Read + ReadBytesExt;
}

/// Errors from binary serialization.
#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("Invalid string schema: {0}")]
    ValidateString(#[from] string::ValidationError),
    #[error("Encoding with string schema failed: {0}")]
    EncodeString(#[from] string::EncodingError),
    #[error("Decoding with string schema failed: {0}")]
    DecodeString(#[from] string::DecodingError),
    #[error("Invalid boolean schema: {0}")]
    ValidateBoolean(#[from] boolean::ValidationError),
    #[error("Encoding with boolean schema failed: {0}")]
    EncodeBoolean(#[from] boolean::EncodingError),
    #[error("Decoding with boolean schema failed: {0}")]
    DecodeBoolean(#[from] boolean::DecodingError),
    #[error("Invalid number schema: {0}")]
    ValidateNumber(#[from] number::ValidationError),
    #[error("Encoding with number schema failed: {0}")]
    EncodeNumber(#[from] number::EncodingError),
    #[error("Decoding with number schema failed: {0}")]
    DecodeNumber(#[from] number::DecodingError),
    #[error("Invalid integer schema: {0}")]
    ValidateInteger(#[from] integer::ValidationError),
    #[error("Encoding with integer schema failed: {0}")]
    EncodeInteger(#[from] integer::EncodingError),
    #[error("Decoding with integer schema failed: {0}")]
    DecodeInteger(#[from] integer::DecodingError),
    #[error("Invalid object schema: {0}")]
    ValidateObject(#[from] object::ValidationError),
    #[error("Encoding with object schema failed: {0}")]
    EncodeObject(#[from] object::EncodingError),
    #[error("Decoding with object schema failed: {0}")]
    DecodeObject(#[from] object::DecodingError),
    #[error("Invalid array schema: {0}")]
    ValidateArray(#[from] array::ValidationError),
    #[error("Encoding with array schema failed: {0}")]
    EncodeArray(#[from] array::EncodingError),
    #[error("Decoding with array schema failed: {0}")]
    DecodeArray(#[from] array::DecodingError),

    #[error("Invalid JSON: {0}")]
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
        source: crate::string::EncodingError,
    },

    #[error("The value '{value}' can not be encoded with a {type_} schema")]
    InvalidValue { value: String, type_: &'static str },

    #[error("The default character has to be UTF8 encoded as one byte but '{0}' is encoded in {} bytes", .0.len_utf8())]
    InvalidDefaultChar(char),
    #[error("'{0}' is not a field in the schema")]
    NotAField(String),

    #[error("A Json object was expected but got: {0}")]
    NotAnObject(String),
    #[error("The length of an array must be encoded in some way")]
    MissingArrayLength,
    #[error(
        "Can not encode array {value} as its length is {len} but only length {fixed} is supported"
    )]
    NotMatchFixedLength {
        value: String,
        len: usize,
        fixed: usize,
    },
    #[error("Can not encode array {value} as its length is {len} but only length up to {max} can be encoded")]
    ExceededLengthEncoding {
        value: String,
        len: usize,
        max: usize,
    },
    #[error("There are contrary specifications for a fixed-length array")]
    InconsitentFixedLength,

    #[error("Can not decode value as the encoded lenght is {len} but capcacity is only {cap}")]
    EncodedValueExceedsCapacity { len: usize, cap: usize },

    #[error("The value '{value}' is invalid as 'default' for a {type_} schema: {source}")]
    InvalidDefault {
        value: String,
        type_: &'static str,
        #[source]
        source: Box<Error>,
    },
    #[error("Expected the constant value {expected} but got {got}")]
    InvalidConstValue { expected: String, got: String },
}

impl Error {
    pub fn due_to_eof(&self) -> bool {
        match &self {
            Error::DecodeString(e) => e.due_to_eof(),
            Error::DecodeBoolean(e) => e.due_to_eof(),
            Error::DecodeNumber(e) => e.due_to_eof(),
            Error::DecodeInteger(e) => e.due_to_eof(),
            Error::DecodeObject(e) => e.due_to_eof(),
            Error::DecodeArray(e) => e.due_to_eof(),
            _ => false,
        }
    }
}

/// Order of bytes within a field.
#[derive(Debug, Copy, Clone, Deserialize, Eq, PartialEq)]
#[serde(rename_all = "lowercase")]
pub enum ByteOrder {
    /// LSB first.
    LittleEndian,
    /// MSB first.
    BigEndian,
}

/// Raw data schema to catch constant values.
#[derive(Debug, Clone, Deserialize)]
struct RawDataSchema {
    #[serde(flatten)]
    inner: InnerSchema,
    #[serde(rename = "default")]
    default_: Option<Value>,
}

/// The inner data schema without special features like `"default"`.
#[derive(Debug, Clone, Deserialize)]
#[serde(tag = "type", rename_all = "lowercase")]
pub enum InnerSchema {
    Boolean(BooleanSchema),
    Integer(IntegerSchema),
    Number(NumberSchema),
    String(Box<StringSchema>),
    Array(Box<ArraySchema>),
    Object(ObjectSchema),
}

/// The data schema is the typical type users will interact with.
#[derive(Debug, Clone)]
pub struct DataSchema {
    inner: InnerSchema,
    default_: Option<Value>,
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
        self.bitfield().is_some()
    }
    /// Return the inner bitfield if there is some.
    fn bitfield(&self) -> Option<&Bitfield> {
        match &self {
            Self::Number(NumberSchema::Integer {
                integer: IntegerSchema::Bitfield(bf),
                ..
            })
            | Self::Integer(IntegerSchema::Bitfield(bf))
            | Self::Boolean(BooleanSchema { bf }) => Some(bf),
            _ => None,
        }
    }
}

impl Encoder for InnerSchema {
    type Error = Error;

    fn encode<W>(&self, target: &mut W, value: &Value) -> Result<usize, Self::Error>
    where
        W: io::Write + WriteBytesExt,
    {
        let written = match self {
            InnerSchema::Boolean(schema) => schema.encode(target, value)?,
            InnerSchema::Integer(schema) => schema.encode(target, value)?,
            InnerSchema::Number(schema) => schema.encode(target, value)?,
            InnerSchema::String(schema) => schema.encode(target, value)?,
            InnerSchema::Array(schema) => schema.encode(target, value)?,
            InnerSchema::Object(schema) => schema.encode(target, value)?,
        };
        Ok(written)
    }
}

impl Decoder for InnerSchema {
    type Error = Error;

    fn decode<R>(&self, target: &mut R) -> Result<Value, Self::Error>
    where
        R: io::Read + ReadBytesExt,
    {
        let value = match self {
            InnerSchema::Boolean(schema) => schema.decode(target)?,
            InnerSchema::Integer(schema) => schema.decode(target)?,
            InnerSchema::Number(schema) => schema.decode(target)?,
            InnerSchema::String(schema) => schema.decode(target)?,
            InnerSchema::Array(schema) => schema.decode(target)?,
            InnerSchema::Object(schema) => schema.decode(target)?,
        };
        Ok(value)
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
        if let Some(value) = &raw.default_ {
            let mut dummy = Vec::new();
            if let Err(e) = raw.inner.encode(&mut dummy, value) {
                return Err(Error::InvalidDefault {
                    value: value.to_string(),
                    type_: raw.inner.type_(),
                    source: Box::new(e),
                });
            }
        }

        Ok(Self {
            inner: raw.inner,
            default_: raw.default_,
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
        Self::String(Box::new(v))
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
            default_: None,
        }
    }
}

impl Encoder for DataSchema {
    type Error = Error;

    fn encode<W>(&self, target: &mut W, value: &Value) -> Result<usize, Self::Error>
    where
        W: io::Write + WriteBytesExt,
    {
        if let Some(c) = &self.default_ {
            self.inner.encode(target, c)
        } else {
            self.inner.encode(target, value)
        }
    }
}

impl Decoder for DataSchema {
    type Error = Error;

    fn decode<R>(&self, target: &mut R) -> Result<Value, Self::Error>
    where
        R: io::Read + ReadBytesExt,
    {
        self.inner.decode(target)
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
                    "default": 100,
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
                    "default": 200,
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
                    "minLength": 8,
                    "maxLength": 8,
                    "default": "7e000503",
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
                    "minLength": 4,
                    "maxLength": 4,
                    "default": "00ef",
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
                    "minLength": 6,
                    "maxLength": 6,
                    "default": "7e0004",
                    "position": 1
                },
                "is_on": {
                    "type": "boolean",
                    "position": 5
                },
                "end": {
                    "type": "string",
                    "format": "binary",
                    "minLength": 10,
                    "maxLength": 10,
                    "default": "00000000ef",
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

    #[test]
    fn doc() -> anyhow::Result<()> {
        use super::*;
        use serde_json::{from_value, json};
        use valico::json_schema;
        let schema = json!({
            "type": "object",
            "properties": {
                "start": {
                    "type": "string",
                    "format": "binary",
                    "minLength": 2,
                    "maxLength": 2,
                    "default": "fe",
                    "position": 1
                },
                "is_on": {
                    "type": "boolean",
                    "position": 5
                },
                "end": {
                    "type": "string",
                    "format": "binary",
                    "minLength": 2,
                    "maxLength": 2,
                    "default": "ef",
                    "position": 10
                }
            },
            "required": ["is_on"]
        });
        let mut scope = json_schema::Scope::new();
        let j_schema = scope.compile_and_return(schema.clone(), false)?;
        let schema = from_value::<DataSchema>(schema)?;
        let value = json!({ "is_on": true });
        assert!(j_schema.validate(&value).is_valid());
        let mut encoded = Vec::new();
        schema.encode(&mut encoded, &value)?;
        let expected = [0xfe, 1, 0xef];
        assert_eq!(&expected, encoded.as_slice());
        let mut encoded = std::io::Cursor::new(encoded);
        let back = schema.decode(&mut encoded)?;
        let expected = json!({
            "start": "fe",
            "is_on": true,
            "end": "ef"
        });
        assert!(j_schema.validate(&back).is_valid());
        assert_eq!(back, expected);
        Ok::<(), anyhow::Error>(())
    }
}
