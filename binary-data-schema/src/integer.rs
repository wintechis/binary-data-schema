//! Implementation of the integer schema
//!
//! The integer schema allows en- and decoding of integers.
//! BDS supports integers up to a length of 8 bytes.
//! In order to support more dense encoding, BDS's integer schema allow for addressing [bitfields].
//! This means that an encoded integer does not have to align with bytes.
//!
//! # Parameters
//!
//! | Key           | Type     | Default  | Comment |
//! | ------------- | --------:| --------:| ------- |
//! | `"byteorder"` | `string` | "bigendian" | The order in which the bytes are encoded. |
//! | `"length"`    |   `uint` |        4 | Number of bytes of the encoded integer |
//! | `"signed"`    |   `bool` |     true | Whether the integer is signed or not |
//! | `"bits"`      |   `uint` | optional | Number of bits the [bitfield] covers |
//! | `"bitoffset"` |   `uint` | optional | Number of bits the [bitfield] is shifted |
//!
//! ## Validation
//!
//! BDS only supports integers up to 64 bit.
//! Accordingly, a `"length"` bigger than 8 is invalid.
//!
//! # Features
//!
//! ## Endianness
//!
//! Valid values for `"byteorder"` are `"bigendian"` and `"littleendian"`.
//! Endianness defines the order the bytes of an integer are encoded.
//! With big endian the Most Significant Byte (MSB) is placed first.
//! Contrary, little endian encodes the Least Significant Byte (LSB) first.
//!
//! ### Example
//!
//! ```
//! # use binary_data_schema::*;
//! # use valico::json_schema;
//! # use serde_json::{json, from_value};
//! let schema = json!({
//!     "type": "integer",
//!     "byteorder": "littleendian",
//!     "length": 2
//! });
//!
//! let mut scope = json_schema::Scope::new();
//! let j_schema = scope.compile_and_return(schema.clone(), false)?;
//! let schema = from_value::<DataSchema>(schema)?;
//!
//! let value = json!(0x01_02);
//! assert!(j_schema.validate(&value).is_valid());
//! let mut encoded = Vec::new();
//! schema.encode(&mut encoded, &value)?;
//! let expected = [0x02, 0x01];
//! assert_eq!(&expected, encoded.as_slice());
//!
//! let mut encoded = std::io::Cursor::new(encoded);
//! let back = schema.decode(&mut encoded)?;
//! assert!(j_schema.validate(&back).is_valid());
//! assert_eq!(back, value);
//! # Ok::<(), anyhow::Error>(())
//! ```
//!
//! ## Bitfield
//!
//! Integer can not only be encoded into full bytes but also into a number of bits.
//! It is common practice to save space by [merging several bitfields] into a number of bytes if the individual values do not need a whole byte.
//! For example, when a thermometer can only display 0 °C through 60 °C with a precision of 1 °C all values fit into 6 bits (max 63).
//!
//! ### Recognizing an Integer Schema as Bitfield
//!
//! An integer schema is recognized as bitfield when either `"bits"` or `"bitoffset"` are given.
//!
//! Bitfields are always _unsigned_ and _big endian_.
//! When a bitfield is recognized the values for `"signed"` and `"byteorder"` are **ignored**.
//!
//! ### Example
//!
//! ```
//! # use binary_data_schema::*;
//! # use valico::json_schema;
//! # use serde_json::{json, from_value};
//! let schema = json!({
//!     "type": "integer",
//!     "bits": 6,
//!     "bitoffset": 2,
//!     "length": 1
//! });
//!
//! let mut scope = json_schema::Scope::new();
//! let j_schema = scope.compile_and_return(schema.clone(), false)?;
//! let schema = from_value::<DataSchema>(schema)?;
//!
//! let value = json!(0b0011_1001); // decimal: 57
//! assert!(j_schema.validate(&value).is_valid());
//! let mut encoded = Vec::new();
//! schema.encode(&mut encoded, &value)?;
//! let expected = [ 0b1110_0100 ];
//! assert_eq!(&expected, encoded.as_slice());
//!
//! let mut encoded = std::io::Cursor::new(encoded);
//! let back = schema.decode(&mut encoded)?;
//! assert!(j_schema.validate(&back).is_valid());
//! assert_eq!(back, value);
//! # Ok::<(), anyhow::Error>(())
//! ```
//!
//! ## Range and Multiples
//!
//! BDS does not check for [`"minimum"`, `"maximum"`] or [`"multiples"`], yet.
//!
//! [bitfields]: ../object/index.html#bitfields
//! [bitfield]: ../object/index.html#bitfields
//! [merging several bitfields]: ../object/index.html#bitfields
//! [`"minimum"`, `"maximum"`]: https://json-schema.org/understanding-json-schema/reference/numeric.html#range
//! [`"multiples"`]: https://json-schema.org/understanding-json-schema/reference/numeric.html#multiples

use std::{convert::TryFrom, io};

use byteorder::{ReadBytesExt, WriteBytesExt, BE, LE};
use serde::{de::Error as _, Deserialize, Deserializer};
use serde_json::Value;

use crate::{ByteOrder, Decoder, Encoder};

const MAX_INTEGER_SIZE: usize = 8;
const DEFAULT_LENGTH: usize = 4;
const DEFAULT_SIGNED: bool = true;

/// Errors validating an [IntegerSchema].
#[derive(Debug, thiserror::Error)]
pub enum ValidationError {
    #[error("Invalid length requested: Maximum allowed is {max} but {requested} where requested")]
    MaxLength { max: usize, requested: usize },
    #[error("A bitfield with offset {offset} and a width of {width} bits does not fit into a {bytes} bytes field")]
    BitOffset {
        bytes: usize,
        offset: usize,
        width: usize,
    },
    #[error("The requested field size of {requested} bits is insufficient. It must be between 1 and {max} bits")]
    InvalidBitWidth { max: usize, requested: usize },
    #[error("Invalid integer schema. Not a bitfield: {bf}; nor an integer: {int}")]
    InvalidIntegerSchema {
        bf: Box<ValidationError>,
        int: Box<ValidationError>,
    },
    #[error(
        "Tried to build a bitfield from a description that has neither 'bits' nor 'bitoffset'"
    )]
    NotABitfield,
}

/// Errors encoding a string with an [IntegerSchema].
#[derive(Debug, thiserror::Error)]
pub enum EncodingError {
    #[error("The value '{value}' can not be encoded with an integer schema")]
    InvalidValue { value: String },
    #[error("Writing to buffer failed: {0}")]
    WriteFail(#[from] io::Error),
}

/// Errors decoding a string with an [IntegerSchema].
#[derive(Debug, thiserror::Error)]
pub enum DecodingError {
    #[error("Reading encoded data failed: {0}")]
    ReadFail(#[from] io::Error),
}

impl DecodingError {
    pub fn due_to_eof(&self) -> bool {
        matches!(self, Self::ReadFail(e) if e.kind() == std::io::ErrorKind::UnexpectedEof)
    }
}

#[derive(Debug, Clone, Deserialize)]
#[serde(rename_all = "lowercase")]
pub(crate) struct RawIntegerSchema {
    #[serde(default)]
    pub(crate) byteorder: ByteOrder,
    #[serde(default = "IntegerSchema::default_length")]
    pub(crate) length: usize,
    #[serde(default = "IntegerSchema::default_signed")]
    pub(crate) signed: bool,
    pub(crate) bits: Option<usize>,
    #[serde(rename = "bitoffset")]
    pub(crate) bit_offset: Option<usize>,
}

/// A simple integer schema.
#[derive(Debug, Clone, Copy)]
pub struct PlainInteger {
    byteorder: ByteOrder,
    length: usize,
    signed: bool,
}

/// A schema referencing some bits within a chunk of bytes.
#[derive(Debug, Clone, Copy)]
pub struct Bitfield {
    pub(crate) bytes: usize,
    pub(crate) width: usize,
    pub(crate) offset: usize,
}

/// An integer schema. May refer to a bitfield (further information on [the module's documentation](index.html)).
#[derive(Debug, Clone)]
pub enum IntegerSchema {
    Integer(PlainInteger),
    Bitfield(Bitfield),
}

impl Default for Bitfield {
    fn default() -> Self {
        Self {
            bytes: 1,
            width: 1,
            offset: 0,
        }
    }
}

impl Bitfield {
    /// Create a new bitfield.
    pub fn new(bytes: usize, width: usize, offset: usize) -> Result<Self, ValidationError> {
        let max_bits = bytes * 8;
        if bytes > MAX_INTEGER_SIZE {
            Err(ValidationError::MaxLength {
                max: MAX_INTEGER_SIZE,
                requested: bytes,
            })
        } else if width + offset > max_bits {
            Err(ValidationError::BitOffset {
                bytes,
                width,
                offset,
            })
        } else if width == 0 || width > max_bits {
            Err(ValidationError::InvalidBitWidth {
                max: max_bits,
                requested: width,
            })
        } else {
            Ok(Self {
                bytes,
                width,
                offset,
            })
        }
    }
    /// Width of the bitfield in bits.
    pub fn bits(&self) -> usize {
        self.width
    }
    pub fn bytes(&self) -> usize {
        self.bytes
    }
    /// Mask to select the bits covered by the bitfield.
    pub fn mask(&self) -> u64 {
        ((1 << self.width) - 1) << self.offset
    }
    /// Read the value of the bitfield from bytes.
    pub fn read(&self, value: u64) -> u64 {
        (value & self.mask()) >> self.offset
    }
    /// Write the value to the described bitfield.
    pub fn write(&self, value: u64, target: &mut u64) {
        let value = (value << self.offset) & self.mask();
        *target |= value;
    }
    /// An integer schema to encode the bytes of the bitfield.
    pub(crate) fn integer(&self) -> PlainInteger {
        PlainInteger::new(ByteOrder::BigEndian, self.bytes, false)
            .expect("Invariants of bitfield match those of integer")
    }
}

impl Default for PlainInteger {
    fn default() -> Self {
        PlainInteger {
            length: IntegerSchema::default_length(),
            signed: IntegerSchema::default_signed(),
            byteorder: Default::default(),
        }
    }
}

impl PlainInteger {
    pub fn new(byteorder: ByteOrder, length: usize, signed: bool) -> Result<Self, ValidationError> {
        if length > MAX_INTEGER_SIZE {
            Err(ValidationError::MaxLength {
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
    /// Default schema with a length of 8 bytes.
    pub fn default_long() -> Self {
        Self {
            length: 8,
            ..Default::default()
        }
    }
    /// Default schema with a length of 1 byte.
    pub fn signed_byte() -> Self {
        Self {
            length: 1,
            ..Default::default()
        }
    }
    pub fn unsigned_byte() -> Self {
        Self {
            length: 1,
            signed: false,
            ..Default::default()
        }
    }
}

impl IntegerSchema {
    pub fn default_length() -> usize {
        DEFAULT_LENGTH
    }
    pub fn default_signed() -> bool {
        DEFAULT_SIGNED
    }
    /// Default integer schema with 8 bytes length.
    pub fn long_int() -> Self {
        PlainInteger::default_long().into()
    }
    /// Default integer schema with 1 byte length.
    pub fn signed_byte() -> Self {
        PlainInteger::signed_byte().into()
    }
    /// Default integer schema with 1 byte length.
    pub fn unsigned_byte() -> Self {
        PlainInteger::unsigned_byte().into()
    }
    /// Encoded length in bytes.
    pub fn length(&self) -> usize {
        match self {
            IntegerSchema::Integer(PlainInteger { length, .. }) => *length,
            IntegerSchema::Bitfield(Bitfield { bytes, .. }) => *bytes,
        }
    }
    /// The theoretical maximal value that can be encoded.
    pub fn max_value(&self) -> usize {
        match self {
            IntegerSchema::Integer(PlainInteger { length, signed, .. }) => {
                let mut max = (1 << (length * 8)) - 1;
                if *signed {
                    max >>= 1;
                }
                max
            }
            IntegerSchema::Bitfield(Bitfield { width: bits, .. }) => (1 << bits) - 1,
        }
    }
}

impl Default for IntegerSchema {
    fn default() -> Self {
        PlainInteger::default().into()
    }
}

impl From<Bitfield> for IntegerSchema {
    fn from(bits: Bitfield) -> Self {
        IntegerSchema::Bitfield(bits)
    }
}

impl From<PlainInteger> for IntegerSchema {
    fn from(integer: PlainInteger) -> Self {
        IntegerSchema::Integer(integer)
    }
}

impl TryFrom<RawIntegerSchema> for Bitfield {
    type Error = ValidationError;

    fn try_from(raw: RawIntegerSchema) -> Result<Self, Self::Error> {
        if raw.bits.is_some() || raw.bit_offset.is_some() {
            let bits = raw.bits.unwrap_or_else(|| raw.length * 8);
            let offset = raw.bit_offset.unwrap_or_default();
            Self::new(raw.length, bits, offset)
        } else {
            Err(ValidationError::NotABitfield)
        }
    }
}

impl TryFrom<RawIntegerSchema> for PlainInteger {
    type Error = ValidationError;

    fn try_from(raw: RawIntegerSchema) -> Result<Self, Self::Error> {
        Self::new(raw.byteorder, raw.length, raw.signed)
    }
}

impl TryFrom<RawIntegerSchema> for IntegerSchema {
    type Error = ValidationError;

    fn try_from(raw: RawIntegerSchema) -> Result<Self, Self::Error> {
        match Bitfield::try_from(raw.clone()) {
            Ok(bf) => Ok(bf.into()),
            Err(ValidationError::NotABitfield) => PlainInteger::try_from(raw).map(Into::into),
            Err(e) => Err(e),
        }
    }
}

impl<'de> Deserialize<'de> for IntegerSchema {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let raw = RawIntegerSchema::deserialize(deserializer)?;
        IntegerSchema::try_from(raw).map_err(D::Error::custom)
    }
}

impl Encoder for Bitfield {
    type Error = EncodingError;

    fn encode<W>(&self, target: &mut W, value: &Value) -> Result<usize, Self::Error>
    where
        W: io::Write + WriteBytesExt,
    {
        let value = value.as_u64().ok_or_else(|| EncodingError::InvalidValue {
            value: value.to_string(),
        })?;
        let mut buffer = 0;
        self.write(value, &mut buffer);
        let int = self.integer();
        int.encode(target, &buffer.into())
    }
}

impl Encoder for PlainInteger {
    type Error = EncodingError;

    fn encode<W>(&self, target: &mut W, value: &Value) -> Result<usize, Self::Error>
    where
        W: io::Write + WriteBytesExt,
    {
        let value = value.as_i64().ok_or_else(|| EncodingError::InvalidValue {
            value: value.to_string(),
        })?;
        match (self.byteorder, self.signed) {
            (ByteOrder::BigEndian, true) => target.write_int::<BE>(value, self.length)?,
            (ByteOrder::BigEndian, false) => target.write_uint::<BE>(value as _, self.length)?,
            (ByteOrder::LittleEndian, true) => target.write_int::<LE>(value, self.length)?,
            (ByteOrder::LittleEndian, false) => target.write_uint::<LE>(value as _, self.length)?,
        };

        Ok(self.length)
    }
}

impl Encoder for IntegerSchema {
    type Error = EncodingError;

    fn encode<W>(&self, target: &mut W, value: &Value) -> Result<usize, Self::Error>
    where
        W: io::Write + WriteBytesExt,
    {
        match self {
            IntegerSchema::Integer(schema) => schema.encode(target, value),
            IntegerSchema::Bitfield(schema) => schema.encode(target, value),
        }
    }
}

impl Decoder for Bitfield {
    type Error = DecodingError;

    fn decode<R>(&self, target: &mut R) -> Result<Value, Self::Error>
    where
        R: io::Read + ReadBytesExt,
    {
        let int = self.integer();
        let int = int.decode(target)?.as_u64().expect("Is always u64.");
        Ok(self.read(int).into())
    }
}

impl Decoder for PlainInteger {
    type Error = DecodingError;

    fn decode<R>(&self, target: &mut R) -> Result<Value, Self::Error>
    where
        R: io::Read + ReadBytesExt,
    {
        Ok(match (self.byteorder, self.signed) {
            (ByteOrder::BigEndian, true) => target.read_int::<BE>(self.length)?.into(),
            (ByteOrder::BigEndian, false) => target.read_uint::<BE>(self.length)?.into(),
            (ByteOrder::LittleEndian, true) => target.read_int::<LE>(self.length)?.into(),
            (ByteOrder::LittleEndian, false) => target.read_uint::<LE>(self.length)?.into(),
        })
    }
}

impl Decoder for IntegerSchema {
    type Error = DecodingError;

    fn decode<R>(&self, target: &mut R) -> Result<Value, Self::Error>
    where
        R: io::Read + ReadBytesExt,
    {
        match self {
            IntegerSchema::Integer(dec) => dec.decode(target),
            IntegerSchema::Bitfield(dec) => dec.decode(target),
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use anyhow::Result;
    use serde_json::{from_value, json};

    #[test]
    fn encode_integer_4_signed() -> Result<()> {
        let schema_msb = json!({});
        let schema_msb: IntegerSchema = from_value(schema_msb)?;
        let schema_lsb = json!({"byteorder": "littleendian"});
        let schema_lsb: IntegerSchema = from_value(schema_lsb)?;
        let value: i32 = 0x1234_5678;
        let json: Value = value.into();
        let mut buffer = [0; 4];

        assert_eq!(4, schema_msb.encode(&mut buffer.as_mut(), &json)?);
        let buf_value = i32::from_be_bytes(buffer);
        assert_eq!(value, buf_value);

        assert_eq!(4, schema_lsb.encode(&mut buffer.as_mut(), &json)?);
        let buf_value = i32::from_le_bytes(buffer);
        assert_eq!(value, buf_value);

        Ok(())
    }

    #[test]
    fn encode_integer_3_unsigned() -> Result<()> {
        let schema = json!({
            "length": 3,
            "signed": false
        });
        let schema_msb: IntegerSchema = from_value(schema)?;
        let schema = json!({
            "length": 3,
            "signed": false,
            "byteorder": "littleendian"
        });
        let schema_lsb: IntegerSchema = from_value(schema)?;
        let value: i64 = 0x123456;
        let json: Value = value.into();
        let mut buffer: Vec<u8> = vec![];

        assert_eq!(3, schema_msb.encode(&mut buffer, &json)?);
        assert!(matches!(buffer.as_slice(), [0x12, 0x34, 0x56, ..]));

        assert_eq!(3, schema_lsb.encode(&mut buffer, &json)?);
        assert!(matches!(
            buffer.as_slice(),
            [0x12, 0x34, 0x56, 0x56, 0x34, 0x12, ..]
        ));

        Ok(())
    }

    #[test]
    fn masks() -> Result<()> {
        let b1 = Bitfield::new(1, 1, 0)?;
        let m1 = 1;
        assert_eq!(b1.mask(), m1);
        let b2 = Bitfield::new(2, 3, 9)?;
        let m2 = 0b1110_0000_0000;
        assert_eq!(b2.mask(), m2);
        let b3 = Bitfield::new(4, 32, 0)?;
        let m3 = 0xFFFF_FFFF;
        assert_eq!(b3.mask(), m3);
        Ok(())
    }
}
