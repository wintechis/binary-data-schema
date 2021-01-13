//! Implementation of the integer schema

use crate::{BinaryCodec, ByteOrder, Error, Length, Result};
use byteorder::{WriteBytesExt, BE, LE};
use serde::{de::Error as _, Deserialize, Deserializer};
use serde_json::Value;
use std::collections::HashMap;
use std::convert::TryFrom;
use std::io;

const MAX_INTEGER_SIZE: usize = 8;
const DEFAULT_LENGTH: usize = 4;
const DEFAULT_SIGNED: bool = true;

#[derive(Debug, Clone, Deserialize)]
#[serde(rename_all = "lowercase")]
pub(crate) struct RawIntegerSchema {
    #[serde(default)]
    pub(crate) byteorder: ByteOrder,
    #[serde(default = "IntegerSchema::default_length")]
    pub(crate) length: usize,
    #[serde(default = "IntegerSchema::default_signed")]
    pub(crate) signed: bool,
    #[serde(default)]
    pub(crate) bits: usize,
    #[serde(default, rename = "bitoffset")]
    pub(crate) bit_offset: usize,
}

/// A set of bitfields that all write to the same bytes.
#[derive(Debug, Clone)]
pub struct JoinedBitfield {
    bytes: usize,
    fields: HashMap<String, Bitfield>,
}

impl JoinedBitfield {
    pub fn join(bfs: HashMap<String, Bitfield>) -> Result<Self> {
        let bytes = if let Some(bf) = bfs.values().next() {
            bf.bytes
        } else {
            // Empty map
            return Err(Error::NoBitfields);
        };

        if bfs.values().any(|bf| bf.bytes != bytes) {
            return Err(Error::NotSameBytes);
        }

        bfs.values().try_fold(0u64, |state, bf| {
            let mask = bf.mask();
            if state & mask != 0 {
                Err(Error::OverlappingBitfields)
            } else {
                Ok(state | mask)
            }
        })?;

        Ok(Self { bytes, fields: bfs })
    }
    /// Integer schema to encode the value of all bitfields.
    fn integer(&self) -> Integer {
        self.fields
            .values()
            .next()
            .expect("Constuctor guarantees that there is at least one bitfield")
            .integer()
    }
}

impl BinaryCodec for JoinedBitfield {
    type Value = Value;

    fn length_encoded(&self) -> Length {
        Length::Fixed(self.bytes)
    }
    fn encode<W>(&self, target: W, value: &Self::Value) -> Result<usize>
    where
        W: io::Write + WriteBytesExt,
    {
        let mut buffer = 0;
        for (name, bf) in self.fields.iter() {
            let value = value
                .get(name)
                .ok_or_else(|| Error::MissingField(name.clone()))?;
            let value = value.as_u64().ok_or_else(|| Error::InvalidValue {
                value: value.to_string(),
                type_: "integer",
            })?;
            bf.write(value, &mut buffer);
        }

        let int = self.integer();
        int.encode(target, &(buffer as _))
    }
    fn encode_value<W>(&self, target: W, value: &Value) -> Result<usize>
    where
        W: io::Write + WriteBytesExt,
    {
        self.encode(target, value)
    }
}

/// A schema referencing some bits within a chunk of bytes.
#[derive(Debug, Clone, Copy)]
pub struct Bitfield {
    bytes: usize,
    bits: usize,
    offset: usize,
}

impl Default for Bitfield {
    fn default() -> Self {
        Self {
            bytes: 1,
            bits: 1,
            offset: 0,
        }
    }
}

impl Bitfield {
    /// Create a new bitfield.
    pub fn new(bytes: usize, bits: usize, offset: usize) -> Result<Self> {
        let max_bits = bytes * 8;
        if bytes > 8 {
            Err(Error::MaxLength {
                max: 8,
                requested: bytes,
            })
        } else if bits + offset > max_bits {
            Err(Error::BitOffset {
                max: max_bits,
                requested: bits + offset,
            })
        } else if bits == 0 || bits > max_bits {
            Err(Error::InvalidBitWidth {
                max: max_bits,
                requested: bits,
            })
        } else {
            Ok(Self {
                bytes,
                bits,
                offset,
            })
        }
    }
    /// Width of the bitfield in bits.
    pub fn bits(&self) -> usize {
        self.bits
    }
    pub fn bytes(&self) -> usize {
        self.bytes
    }
    /// Mask to select the bits covered by the bitfield.
    pub fn mask(&self) -> u64 {
        ((1 << self.bits) - 1) << self.offset
    }
    /// Read the value of the bitfield from bytes.
    pub fn read(&self, value: u64) -> u64 {
        (value & self.mask()) >> self.offset
    }
    /// Write the value to the described bitfield.
    ///
    /// # Example
    ///
    /// ```
    /// use binary_data_schema::Bitfield;
    ///
    /// let bf = Bitfield::new(2, 3, 7)?;
    /// let mut buffer = 0;
    /// bf.write(21, &mut buffer);
    /// // 21 & 0b111 = 5;
    /// assert_eq!(buffer, (5 << 7));
    /// # Ok::<(), anyhow::Error>(())
    /// ```
    pub fn write(&self, value: u64, target: &mut u64) {
        let value = (value << self.offset) & self.mask();
        *target |= value;
    }
    /// An integer schema to encode the bytes of the bitfield.
    fn integer(&self) -> Integer {
        Integer::new(ByteOrder::BigEndian, self.bytes, false)
            .expect("Invariants of bitfield match those of integer")
    }
}

impl BinaryCodec for Bitfield {
    type Value = u64;

    fn length_encoded(&self) -> Length {
        Length::Fixed(self.bytes)
    }
    fn encode<W>(&self, target: W, value: &Self::Value) -> Result<usize>
    where
        W: io::Write + WriteBytesExt,
    {
        let mut buffer = 0;
        self.write(*value, &mut buffer);
        let int = self.integer();
        int.encode(target, &(buffer as _))
    }
    fn encode_value<W>(&self, target: W, value: &Value) -> Result<usize>
    where
        W: io::Write + WriteBytesExt,
    {
        if let Some(value) = value.as_u64() {
            self.encode(target, &value)
        } else {
            Err(Error::InvalidValue {
                value: value.to_string(),
                type_: "integer",
            })
        }
    }
}

impl TryFrom<RawIntegerSchema> for Bitfield {
    type Error = Error;

    /// Try to build a bitfield schema from a raw definition.
    ///
    /// # Defaults
    ///
    /// If only `"bitoffset"` is present a bit width of 1 is assumed.
    ///
    /// # Errors
    ///
    /// Fails if `"bits"` and `"bitoffset"` are not present. If you want
    /// to default in such case use [Bitfield::from_raw].
    ///
    /// Furthermore, errors are raised when:
    /// - The `"length"` exceeds the maximum integer schema length, i.e. 8.
    /// - If `"bits"` and `"bitoffset"` together are bigger than the bitfield.
    /// - If `"bits"` is 0.
    fn try_from(raw: RawIntegerSchema) -> Result<Self, Self::Error> {
        if raw.bit_offset != 0 && raw.bits == 0 {
            Bitfield::new(raw.length, 1, raw.bit_offset)
        } else {
            Bitfield::new(raw.length, raw.bits, raw.bit_offset)
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Integer {
    byteorder: ByteOrder,
    length: usize,
    signed: bool,
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
    /// Default schema with a length of 8 bytes.
    pub fn default_long() -> Self {
        Self {
            length: 8,
            ..Default::default()
        }
    }
    /// Default schema with a length of 1 byte.
    pub fn default_short() -> Self {
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

impl Default for Integer {
    fn default() -> Self {
        Integer {
            length: IntegerSchema::default_length(),
            signed: IntegerSchema::default_signed(),
            byteorder: Default::default(),
        }
    }
}

impl BinaryCodec for Integer {
    type Value = i64;

    fn length_encoded(&self) -> Length {
        Length::Fixed(self.length)
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
    fn encode_value<W>(&self, target: W, value: &Value) -> Result<usize>
    where
        W: io::Write + WriteBytesExt,
    {
        if let Some(value) = value.as_i64() {
            self.encode(target, &value)
        } else {
            Err(Error::InvalidValue {
                value: value.to_string(),
                type_: "integer",
            })
        }
    }
}

impl TryFrom<RawIntegerSchema> for Integer {
    type Error = Error;

    fn try_from(raw: RawIntegerSchema) -> Result<Self, Self::Error> {
        Self::new(raw.byteorder, raw.length, raw.signed)
    }
}

/// An integer schema. May refer to a bitfield.
#[derive(Debug, Clone)]
pub enum IntegerSchema {
    Integer(Integer),
    Bitfield(Bitfield),
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
        Integer::default_long().into()
    }
    /// Default integer schema with 1 byte length.
    pub fn short_int() -> Self {
        Integer::default_short().into()
    }
    /// Encoded length in bytes.
    pub fn length(&self) -> usize {
        match self {
            IntegerSchema::Integer(Integer { length, .. }) => *length,
            IntegerSchema::Bitfield(Bitfield { bytes, .. }) => *bytes,
        }
    }
    /// The theoretical maximal value that can be encoded.
    pub fn max_value(&self) -> usize {
        match self {
            IntegerSchema::Integer(Integer { length, signed, .. }) => {
                let mut max = (1 << (length * 8)) - 1;
                if *signed {
                    max >>= 1;
                }
                max
            }
            IntegerSchema::Bitfield(Bitfield { bits, .. }) => (1 << bits) - 1,
        }
    }
}

impl Default for IntegerSchema {
    fn default() -> Self {
        Integer::default().into()
    }
}

impl From<Bitfield> for IntegerSchema {
    fn from(bits: Bitfield) -> Self {
        IntegerSchema::Bitfield(bits)
    }
}

impl From<Integer> for IntegerSchema {
    fn from(integer: Integer) -> Self {
        IntegerSchema::Integer(integer)
    }
}

impl TryFrom<RawIntegerSchema> for IntegerSchema {
    type Error = Error;

    fn try_from(raw: RawIntegerSchema) -> Result<Self, Self::Error> {
        match Bitfield::try_from(raw.clone()) {
            Ok(bf) => Ok(bf.into()),
            Err(e_bf) => match Integer::try_from(raw) {
                Ok(int) => Ok(int.into()),
                Err(e_int) => Err(Error::InvalidIntegerSchema {
                    bf: Box::new(e_bf),
                    int: Box::new(e_int),
                }),
            },
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

impl BinaryCodec for IntegerSchema {
    type Value = i64;

    fn length_encoded(&self) -> Length {
        Length::Fixed(self.length())
    }
    fn encode<W>(&self, target: W, value: &Self::Value) -> Result<usize>
    where
        W: io::Write + WriteBytesExt,
    {
        match self {
            IntegerSchema::Integer(schema) => schema.encode(target, value),
            IntegerSchema::Bitfield(schema) => schema.encode(target, &(*value as _)),
        }
    }
    fn encode_value<W>(&self, target: W, value: &Value) -> Result<usize>
    where
        W: io::Write + WriteBytesExt,
    {
        match self {
            IntegerSchema::Integer(schema) => schema.encode_value(target, value),
            IntegerSchema::Bitfield(schema) => schema.encode_value(target, value),
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
