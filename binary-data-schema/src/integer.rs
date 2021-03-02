//! Implementation of the integer schema

use crate::{ByteOrder, DataSchema, Decoder, Encoder, Error, InnerSchema, NumberSchema, Result};
use byteorder::{ReadBytesExt, WriteBytesExt, BE, LE};
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
    bytes: usize,
    bits: usize,
    offset: usize,
}

/// An integer schema. May refer to a bitfield.
#[derive(Debug, Clone)]
pub enum IntegerSchema {
    Integer(PlainInteger),
    Bitfield(Bitfield),
}

/// A set of bitfields that all write to the same bytes.
#[derive(Debug, Clone)]
pub struct JoinedBitfield {
    bytes: usize,
    fields: HashMap<String, DataSchema>,
}

impl JoinedBitfield {
    pub fn join(bfs: HashMap<String, DataSchema>) -> Result<Self> {
        if bfs.values().any(|ds| !ds.is_bitfield()) {
            return Err(Error::NoBitfields);
        }

        let raw_bfs = bfs
            .iter()
            .map(|(name, ds)| {
                let bf = match ds.inner {
                    InnerSchema::Number(NumberSchema::Integer {
                        integer: IntegerSchema::Bitfield(bf),
                        ..
                    })
                    | InnerSchema::Integer(IntegerSchema::Bitfield(bf)) => bf,
                    _ => unreachable!("ensured at beginning"),
                };
                (name.as_str(), bf)
            })
            .collect::<HashMap<_, _>>();

        let bytes = if let Some(bf) = raw_bfs.values().next() {
            bf.bytes
        } else {
            // Empty map
            return Err(Error::NoBitfields);
        };

        if raw_bfs.values().any(|bf| bf.bytes != bytes) {
            return Err(Error::NotSameBytes);
        }

        raw_bfs.values().try_fold(0u64, |state, bf| {
            let mask = bf.mask();
            if state & mask != 0 {
                Err(Error::OverlappingBitfields)
            } else {
                Ok(state | mask)
            }
        })?;

        Ok(Self { bytes, fields: bfs })
    }
    fn raw_bfs(&self) -> impl Iterator<Item = (&'_ str, &'_ Bitfield)> {
        self.fields.iter().map(|(name, ds)| {
            let bf = match &ds.inner {
                InnerSchema::Number(NumberSchema::Integer {
                    integer: IntegerSchema::Bitfield(bf),
                    ..
                })
                | InnerSchema::Integer(IntegerSchema::Bitfield(bf)) => bf,
                _ => unreachable!("ensured at constructor"),
            };
            (name.as_str(), bf)
        })
    }
    /// Integer schema to encode the value of all bitfields.
    fn integer(&self) -> PlainInteger {
        self.raw_bfs()
            .map(|(_, bf)| bf)
            .next()
            .expect("Constuctor guarantees that there is at least one bitfield")
            .integer()
    }
}

impl Encoder for JoinedBitfield {
    fn encode<W>(&self, target: &mut W, value: &Value) -> Result<usize>
    where
        W: io::Write + WriteBytesExt,
    {
        let mut buffer = 0;
        for (name, ds) in self.fields.iter() {
            let value = match (value.get(name), ds.const_.as_ref()) {
                (Some(val), Some(c)) if val == c => Ok(val),
                (Some(val), Some(c)) => Err(Error::InvalidConstValue {
                    expected: c.to_string(),
                    got: val.to_string(),
                }),
                (Some(val), None) => Ok(val),
                (None, Some(c)) => Ok(c),
                (None, None) => Err(Error::MissingField(name.clone())),
            }?;

            let bf = match &ds.inner {
                InnerSchema::Number(NumberSchema::Integer {
                    integer: IntegerSchema::Bitfield(bf),
                    ..
                })
                | InnerSchema::Integer(IntegerSchema::Bitfield(bf)) => bf,
                _ => unreachable!("ensured at constructor"),
            };
            let value = if let InnerSchema::Number(ns) = &ds.inner {
                let value = value.as_f64().ok_or_else(|| Error::InvalidValue {
                    value: value.to_string(),
                    type_: "number",
                })?;
                ns.to_binary_value(value) as _
            } else {
                value.as_u64().ok_or_else(|| Error::InvalidValue {
                    value: value.to_string(),
                    type_: "integer",
                })?
            };
            bf.write(value, &mut buffer);
        }

        let int = self.integer();
        int.encode(target, &buffer.into())
    }
}

impl Decoder for JoinedBitfield {
    fn decode<R>(&self, target: &mut R) -> Result<Value>
    where
        R: io::Read + ReadBytesExt,
    {
        let int = self.integer();
        let int = int.decode(target)?.as_u64().expect("Is always u64");
        let mut res = Value::default();
        for (name, ds) in self.fields.iter() {
            let bf = match &ds.inner {
                InnerSchema::Number(NumberSchema::Integer {
                    integer: IntegerSchema::Bitfield(bf),
                    ..
                })
                | InnerSchema::Integer(IntegerSchema::Bitfield(bf)) => bf,
                _ => unreachable!("ensured at constructor"),
            };
            let value = bf.read(int);
            if let InnerSchema::Number(ns) = &ds.inner {
                let value = ns.from_binary_value(value as _);
                res[name] = value.into();
            } else {
                res[name] = value.into();
            }
        }

        Ok(res)
    }
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
    fn integer(&self) -> PlainInteger {
        PlainInteger::new(ByteOrder::BigEndian, self.bytes, false)
            .expect("Invariants of bitfield match those of integer")
    }
}

impl Encoder for Bitfield {
    fn encode<W>(&self, target: &mut W, value: &Value) -> Result<usize>
    where
        W: io::Write + WriteBytesExt,
    {
        let value = value.as_u64().ok_or_else(|| Error::InvalidValue {
            value: value.to_string(),
            type_: "integer",
        })?;
        let mut buffer = 0;
        self.write(value, &mut buffer);
        let int = self.integer();
        int.encode(target, &buffer.into())
    }
}

impl Decoder for Bitfield {
    fn decode<R>(&self, target: &mut R) -> Result<Value>
    where
        R: io::Read + ReadBytesExt,
    {
        let int = self.integer();
        let int = int.decode(target)?.as_u64().expect("Is always u64.");
        Ok(self.read(int).into())
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

impl PlainInteger {
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

impl Default for PlainInteger {
    fn default() -> Self {
        PlainInteger {
            length: IntegerSchema::default_length(),
            signed: IntegerSchema::default_signed(),
            byteorder: Default::default(),
        }
    }
}

impl Encoder for PlainInteger {
    fn encode<W>(&self, target: &mut W, value: &Value) -> Result<usize>
    where
        W: io::Write + WriteBytesExt,
    {
        let value = value.as_i64().ok_or_else(|| Error::InvalidValue {
            value: value.to_string(),
            type_: "integer",
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

impl Decoder for PlainInteger {
    fn decode<R>(&self, target: &mut R) -> Result<Value>
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

impl TryFrom<RawIntegerSchema> for PlainInteger {
    type Error = Error;

    fn try_from(raw: RawIntegerSchema) -> Result<Self, Self::Error> {
        Self::new(raw.byteorder, raw.length, raw.signed)
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
    pub fn short_int() -> Self {
        PlainInteger::default_short().into()
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
            IntegerSchema::Bitfield(Bitfield { bits, .. }) => (1 << bits) - 1,
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

impl TryFrom<RawIntegerSchema> for IntegerSchema {
    type Error = Error;

    fn try_from(raw: RawIntegerSchema) -> Result<Self, Self::Error> {
        match Bitfield::try_from(raw.clone()) {
            Ok(bf) => Ok(bf.into()),
            Err(e_bf) => match PlainInteger::try_from(raw) {
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

impl Encoder for IntegerSchema {
    fn encode<W>(&self, target: &mut W, value: &Value) -> Result<usize>
    where
        W: io::Write + WriteBytesExt,
    {
        match self {
            IntegerSchema::Integer(schema) => schema.encode(target, value),
            IntegerSchema::Bitfield(schema) => schema.encode(target, value),
        }
    }
}

impl Decoder for IntegerSchema {
    fn decode<R>(&self, target: &mut R) -> Result<Value>
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
