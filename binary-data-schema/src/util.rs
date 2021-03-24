//! Utility stuff.

use serde::Deserialize;
use serde_json::Value;

use crate::integer::IntegerSchema;

/// Length encoding like present in the JSON description.
#[derive(Debug, Clone, Deserialize)]
#[serde(tag = "@type", rename_all = "lowercase")]
pub enum RawLengthEncoding {
    Fixed,
    ExplicitLength(IntegerSchema),
    EndPattern {
        sentinel: Value,
    },
    Capacity {
        padding: Value,
    },
    TillEnd,
}

/// The way the length of a variable length field is specified in an [array](crate::ArraySchema) or [string schema](crate::StringSchema).
///
/// The length encoding is usually given explicit with the parameter `"lengthEncoding"` in a schema.
/// A length encoding is always a JSON object with a field `"type"`.
/// The default length encoding is `"tillend"`.
///
/// # Length of Array and String
///
/// Depending on the surrounding schema the length encoded refers to different things.
/// In an array schema the length denotes the number of elements in the encoded array.
/// In a string array the length means the number of bytes required to store the UTF-8 encoded string.
///
///
///
#[derive(Debug, Clone)]
pub enum LengthEncoding<T> {
    /// Fixed Size &rarr; `"@type": "fixed"`.
    ///
    /// The encoded length is always the same.
    /// Only values with the fixed length are valid for this schema.
    ///
    /// # Definition in the Schema
    ///
    /// In contrast to other length encodings `fixed` is may not be given explicitly.
    /// Instead it requires `"minLength"` and `"maxLength"` for string and `"minItems"` and `"maxItems"` for array schemata be set to the same value.
    ///
    /// If `fixed` is given explicitly but `min` and `max` are different or one is missing the length encoding is not valid.
    ///
    /// ## Examples
    ///
    /// ```
    /// # use binary_data_schema::{LengthEncoding, string::StringSchema};
    /// # use serde_json::{json, from_value};
    /// let schema = json!({
    ///     "type": "string",
    ///     "minLength": 2,
    ///     "maxLength": 2
    /// });
    /// let schema = from_value::<StringSchema>(schema)?;
    /// assert!(matches!(schema, StringSchema::Utf8 { length: LengthEncoding::Fixed(2) }));
    /// # Ok::<(), anyhow::Error>(())
    /// ```
    ///
    /// ```
    /// # use binary_data_schema::{LengthEncoding, array::ArraySchema};
    /// # use serde_json::{json, from_value};
    /// let schema = json!({
    ///     "type": "array",
    ///     "minItems": 2,
    ///     "lengthEncoding": { "@type": "fixed" },
    ///     "items": { "type": "boolean" }
    /// });
    /// assert!(from_value::<ArraySchema>(schema).is_err());
    /// # Ok::<(), anyhow::Error>(())
    /// ```
    Fixed(usize),
    /// Length Encoded &rarr; `"@type": "explicitlength"`.
    ///
    /// Length encoded means that the length of the value encoded is encoded at the beginning of the field.
    /// How the length is encoded is defined via an integer schema.
    ///
    /// # Example
    ///
    /// ```
    /// # use binary_data_schema::*;
    /// # use valico::json_schema;
    /// # use serde_json::{json, from_value};
    /// let schema = json!({
    ///     "type": "string",
    ///     "lengthEncoding": {
    ///         "@type": "explicitlength",
    ///         "length": 1
    ///     }
    /// });
    ///
    /// let mut scope = json_schema::Scope::new();
    /// let j_schema = scope.compile_and_return(schema.clone(), false)?;
    /// let schema = from_value::<DataSchema>(schema)?;
    ///
    /// let value = json!("tree");
    /// assert!(j_schema.validate(&value).is_valid());
    /// let mut encoded = Vec::new();
    /// schema.encode(&mut encoded, &value)?;
    /// let expected = [ 4, b't', b'r', b'e', b'e' ];
    /// assert_eq!(&expected, encoded.as_slice());
    ///
    /// let mut encoded = std::io::Cursor::new(encoded);
    /// let back = schema.decode(&mut encoded)?;
    /// assert!(j_schema.validate(&back).is_valid());
    /// assert_eq!(back, value);
    /// # Ok::<(), anyhow::Error>(())
    /// ```
    LengthEncoded(IntegerSchema),
    /// End Pattern &rarr; `"@type": "endpattern"`.
    ///
    /// With End Pattern the end of a variable length string or array is marked by a sentinel value.
    /// The sentinel must be valid for the surrounding schema, i.e. for string schema it must adhere to the `"format"` and for array schema it must be valid for the `"items"` schema.
    /// Furthermore, for string schema the sentinel must be encoded as one byte, e.g. an ASCII letter or if `"format": "binary"` two hex-digits.
    ///
    /// **Note:** The sentinel value is not allowed to be included in an encoded value!
    ///
    /// **Note2:** The sentinel values should bot be used with arrays with numeric items. The sentinel is tested for equality so rounding errors may result in not being able to identify the sentinel!
    ///
    /// # Examples
    ///
    /// ```
    /// # use binary_data_schema::*;
    /// # use valico::json_schema;
    /// # use serde_json::{json, from_value};
    /// let schema = json!({
    ///     "type": "string",
    ///     "lengthEncoding": {
    ///         "@type": "endpattern",
    ///         "sentinel": "!"
    ///     }
    /// });
    ///
    /// let mut scope = json_schema::Scope::new();
    /// let j_schema = scope.compile_and_return(schema.clone(), false)?;
    /// let schema = from_value::<DataSchema>(schema)?;
    ///
    /// let value = json!("tree");
    /// assert!(j_schema.validate(&value).is_valid());
    /// let mut encoded = Vec::new();
    /// schema.encode(&mut encoded, &value)?;
    /// let expected = [ b't', b'r', b'e', b'e', b'!' ];
    /// assert_eq!(&expected, encoded.as_slice());
    ///
    /// let mut encoded = std::io::Cursor::new(encoded);
    /// let back = schema.decode(&mut encoded)?;
    /// assert!(j_schema.validate(&back).is_valid());
    /// assert_eq!(back, value);
    /// # Ok::<(), anyhow::Error>(())
    /// ```
    ///
    /// ```
    /// # use binary_data_schema::*;
    /// # use valico::json_schema;
    /// # use serde_json::{json, from_value};
    /// let schema = json!({
    ///     "type": "string",
    ///     "format": "binary",
    ///     "lengthEncoding": {
    ///         "@type": "endpattern",
    ///         "sentinel": "00"
    ///     }
    /// });
    ///
    /// let mut scope = json_schema::Scope::new();
    /// let j_schema = scope.compile_and_return(schema.clone(), false)?;
    /// let schema = from_value::<DataSchema>(schema)?;
    ///
    /// let value = json!("beef");
    /// assert!(j_schema.validate(&value).is_valid());
    /// let mut encoded = Vec::new();
    /// schema.encode(&mut encoded, &value)?;
    /// let expected = [ 0xbe, 0xef, 0x00 ];
    /// assert_eq!(&expected, encoded.as_slice());
    ///
    /// let mut encoded = std::io::Cursor::new(encoded);
    /// let back = schema.decode(&mut encoded)?;
    /// assert!(j_schema.validate(&back).is_valid());
    /// assert_eq!(back, value);
    /// # Ok::<(), anyhow::Error>(())
    /// ```
    ///
    /// ```
    /// # use binary_data_schema::*;
    /// # use valico::json_schema;
    /// # use serde_json::{json, from_value};
    /// let schema = json!({
    ///     "type": "array",
    ///     "lengthEncoding": {
    ///         "@type": "endpattern",
    ///         "sentinel": false
    ///     },
    ///     "items": { "type": "boolean" }
    /// });
    ///
    /// let mut scope = json_schema::Scope::new();
    /// let j_schema = scope.compile_and_return(schema.clone(), false)?;
    /// let schema = from_value::<DataSchema>(schema)?;
    ///
    /// let value = json!([ true, true, false ]);
    /// let mut encoded = Vec::new();
    /// // value contains the sentinel value `false`
    /// assert!(schema.encode(&mut encoded, &value).is_err());
    /// # Ok::<(), anyhow::Error>(())
    /// ```
    EndPattern { sentinel: T },
    /// Capacity &rarr; `"@type": "capacity"`.
    ///
    /// With capacity there is always a certain capacity reserved for the field in the encoded byte string.
    /// For string schemata the capacity is defined by `"maxLength"` where it refers to the maximal length of string that can be encoded.
    /// For array schemata `"maxItems"` defines the capacity.
    ///
    /// Unused space is filled with `"padding"`.
    /// The padding must fulfill the same requirements as the `"sentinel"` of end pattern.
    ///
    /// A value may consume the whole reserved space.
    /// In this case no padding is inserted.
    ///
    /// # Example
    ///
    /// ```
    /// # use binary_data_schema::*;
    /// # use valico::json_schema;
    /// # use serde_json::{json, from_value};
    /// let schema = json!({
    ///     "type": "string",
    ///     "format": "binary",
    ///     "maxLength": 8,
    ///     "lengthEncoding": {
    ///         "@type": "capacity",
    ///         "padding": "00"
    ///     }
    /// });
    ///
    /// let mut scope = json_schema::Scope::new();
    /// let j_schema = scope.compile_and_return(schema.clone(), false)?;
    /// let schema = from_value::<DataSchema>(schema)?;
    ///
    /// let value = json!("beef");
    /// assert!(j_schema.validate(&value).is_valid());
    /// let mut encoded = Vec::new();
    /// schema.encode(&mut encoded, &value)?;
    /// let expected = [ 0xbe, 0xef, 0x00, 0x00 ];
    /// assert_eq!(&expected, encoded.as_slice());
    ///
    /// let mut encoded = std::io::Cursor::new(encoded);
    /// let back = schema.decode(&mut encoded)?;
    /// assert!(j_schema.validate(&back).is_valid());
    /// assert_eq!(back, value);
    /// # Ok::<(), anyhow::Error>(())
    /// ```
    Capacity { padding: T, capacity: usize },
    /// Till End &rarr; `"@type": "tillend"`.
    ///
    /// Till End is the default length encoding.
    /// It simply means that the end of an array or string is determined by the end of the byte string.
    /// Accordingly, schemata with `"tillend"` length encoding are only allowed as the last property in an object schema.
    TillEnd,
}

impl Default for RawLengthEncoding {
    fn default() -> Self {
        RawLengthEncoding::TillEnd
    }
}

impl<T> LengthEncoding<T> {
    pub fn fixed(fixed: usize) -> Self {
        Self::Fixed(fixed)
    }
    pub fn length_encoded(schema: IntegerSchema) -> Self {
        Self::LengthEncoded(schema)
    }
    pub fn end_pattern(pattern: T) -> Self {
        Self::EndPattern { sentinel: pattern }
    }
    pub fn capacity(padding: T, capacity: usize) -> Self {
        Self::Capacity { padding, capacity }
    }
    pub fn map<R, F>(self, f: F) -> LengthEncoding<R>
    where
        F: FnOnce(T) -> R,
    {
        match self {
            Self::Fixed(fixed) => LengthEncoding::Fixed(fixed),
            Self::LengthEncoded(schema) => LengthEncoding::LengthEncoded(schema),
            Self::EndPattern { sentinel: pattern } => LengthEncoding::EndPattern {
                sentinel: f(pattern),
            },
            Self::Capacity { padding, capacity } => LengthEncoding::Capacity {
                padding: f(padding),
                capacity,
            },
            Self::TillEnd => LengthEncoding::TillEnd,
        }
    }
    pub fn try_map<R, E, F>(self, f: F) -> Result<LengthEncoding<R>, E>
    where
        F: FnOnce(T) -> Result<R, E>,
    {
        let new = match self {
            Self::Fixed(fixed) => LengthEncoding::Fixed(fixed),
            Self::LengthEncoded(schema) => LengthEncoding::LengthEncoded(schema),
            Self::EndPattern { sentinel: pattern } => LengthEncoding::EndPattern {
                sentinel: f(pattern)?,
            },
            Self::Capacity { padding, capacity } => LengthEncoding::Capacity {
                padding: f(padding)?,
                capacity,
            },
            Self::TillEnd => LengthEncoding::TillEnd,
        };
        Ok(new)
    }
}
