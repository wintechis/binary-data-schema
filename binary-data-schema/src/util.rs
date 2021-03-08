//! Utility stuff.

use serde::Deserialize;
use serde_json::Value;

use crate::integer::IntegerSchema;

/// Length encoding like present in the JSON description.
#[derive(Debug, Clone, Deserialize)]
#[serde(tag = "type", rename_all = "lowercase")]
pub enum RawLengthEncoding {
    /// A fixed length array is defiened by setting `"minItems"` and
    /// `"maxItems"` to the same value.
    Fixed,
    /// The number of elements in the array is stored at the beginning of the
    /// field with the given integer schema.
    ExplicitLength(IntegerSchema),
    /// The end of the array is marked by a certain value.
    EndPattern {
        /// The value marking the end of the array. The value must be valid for
        /// the `"items"` schema.
        pattern: Value,
    },
    /// A capacity of `"maxItems"` is reserved for the array. Unused place is
    /// filled with `"padding"`.
    Capacity {
        /// The value filling the unused slots of the array. The value must be
        /// valid for the `"items"` schema.
        padding: Value,
    },
    /// The array reaches up to the end of the data. This is the default
    /// encoding.
    TillEnd,
}
#[derive(Debug, Clone)]
pub enum LengthEncoding<T> {
    Fixed(usize),
    LengthEncoded(IntegerSchema),
    EndPattern { pattern: T },
    Capacity { padding: T, capacity: usize },
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
        Self::EndPattern { pattern }
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
            Self::EndPattern { pattern } => LengthEncoding::EndPattern {
                pattern: f(pattern),
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
            Self::EndPattern { pattern } => LengthEncoding::EndPattern {
                pattern: f(pattern)?,
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
