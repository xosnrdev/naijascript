use crate::arena::{Arena, ArenaString};
use crate::builtins::Builtin;
use crate::helper::ValueType;

/// Built-in string methods
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum StringBuiltin {
    /// Get the length of a string
    Len,
    /// Get a substring from a string
    Slice,
    /// Convert a string to uppercase
    ToUppercase,
    /// Convert a string to lowercase
    ToLowercase,
    /// Find substring in string
    Find,
    /// Replace substring in string
    Replace,
    /// Trim whitespace from string
    Trim,
    /// Convert a string to a number
    ToNumber,
    /// Split a string into an array of substrings
    Split,
}

impl Builtin for StringBuiltin {
    fn arity(&self) -> usize {
        match self {
            StringBuiltin::Len
            | StringBuiltin::ToUppercase
            | StringBuiltin::ToLowercase
            | StringBuiltin::Trim
            | StringBuiltin::ToNumber => 0,
            StringBuiltin::Find | StringBuiltin::Split => 1,
            StringBuiltin::Slice | StringBuiltin::Replace => 2,
        }
    }

    fn return_type(&self) -> ValueType {
        match self {
            StringBuiltin::Len | StringBuiltin::Find | StringBuiltin::ToNumber => ValueType::Number,
            StringBuiltin::Slice
            | StringBuiltin::ToUppercase
            | StringBuiltin::ToLowercase
            | StringBuiltin::Replace
            | StringBuiltin::Trim => ValueType::String,
            StringBuiltin::Split => ValueType::Array,
        }
    }

    fn from_name(name: &str) -> Option<Self> {
        match name {
            "len" => Some(StringBuiltin::Len),
            "slice" => Some(StringBuiltin::Slice),
            "to_uppercase" => Some(StringBuiltin::ToUppercase),
            "to_lowercase" => Some(StringBuiltin::ToLowercase),
            "find" => Some(StringBuiltin::Find),
            "replace" => Some(StringBuiltin::Replace),
            "trim" => Some(StringBuiltin::Trim),
            "to_number" => Some(StringBuiltin::ToNumber),
            "split" => Some(StringBuiltin::Split),
            _ => None,
        }
    }
}

impl StringBuiltin {
    #[inline]
    #[must_use]
    #[allow(clippy::cast_precision_loss)]
    pub fn len(s: &str) -> f64 {
        s.chars().count() as f64
    }

    #[inline]
    pub fn slice<'arena>(
        s: &str,
        start: f64,
        end: f64,
        arena: &'arena Arena,
    ) -> ArenaString<'arena> {
        let len = s.chars().count().cast_signed();
        #[allow(clippy::cast_precision_loss, clippy::cast_possible_truncation)]
        let (mut start, mut end) = (start.floor() as isize, end.floor() as isize);

        if start < 0 {
            start += len;
        }
        if end < 0 {
            end += len;
        }

        (start, end) = (start.clamp(0, len), end.clamp(0, len));
        if start >= end {
            return ArenaString::new_in(arena);
        }

        if start == 0 && end == len {
            return ArenaString::from_str(arena, s);
        }

        let mut buffer = ArenaString::with_capacity_in((end - start).cast_unsigned(), arena);
        s.chars()
            .skip(start.cast_unsigned())
            .take((end - start).cast_unsigned())
            .for_each(|ch| buffer.push(ch));
        buffer
    }

    #[inline]
    pub fn to_uppercase<'arena>(s: &str, arena: &'arena Arena) -> ArenaString<'arena> {
        let mut buffer = ArenaString::with_capacity_in(s.len(), arena);
        s.chars().flat_map(char::to_uppercase).for_each(|ch| buffer.push(ch));
        buffer
    }

    #[inline]
    pub fn to_lowercase<'arena>(s: &str, arena: &'arena Arena) -> ArenaString<'arena> {
        let mut buffer = ArenaString::with_capacity_in(s.len(), arena);
        s.chars().flat_map(char::to_lowercase).for_each(|ch| buffer.push(ch));
        buffer
    }

    #[inline]
    #[must_use]
    pub fn find(haystack: &str, needle: &str) -> f64 {
        #[allow(clippy::cast_precision_loss)]
        super::find(haystack, needle).map_or(-1.0, |v| v as f64)
    }

    #[inline]
    pub fn replace<'arena>(
        s: &str,
        old: &str,
        new: &str,
        arena: &'arena Arena,
    ) -> ArenaString<'arena> {
        super::replace(arena, s, old, new)
    }

    #[inline]
    pub fn trim<'arena>(s: &str, arena: &'arena Arena) -> ArenaString<'arena> {
        ArenaString::from_str(arena, s.trim())
    }

    #[inline]
    #[must_use]
    pub fn to_number(s: &str) -> f64 {
        s.parse::<f64>().unwrap_or(f64::NAN)
    }

    #[inline]
    pub fn split<'arena>(
        s: &str,
        pat: &str,
        arena: &'arena Arena,
    ) -> impl Iterator<Item = ArenaString<'arena>> {
        s.split(pat).map(|s| ArenaString::from_str(arena, s))
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::KIBI;
    use crate::arena::Arena;

    #[test]
    fn test_string_slice() {
        let arena = Arena::new(KIBI).unwrap();
        let s = StringBuiltin::slice("Hello, 世界! 🌎", 0.0, 5.0, &arena);
        assert_eq!(s, "Hello");
    }

    #[test]
    fn test_string_uppercase() {
        let arena = Arena::new(KIBI).unwrap();
        let s = StringBuiltin::to_uppercase("abcdefghijklmnopqrstuvwxyz", &arena);
        assert_eq!(s, "ABCDEFGHIJKLMNOPQRSTUVWXYZ");
    }

    #[test]
    fn test_string_lowercase() {
        let arena = Arena::new(KIBI).unwrap();
        let s = StringBuiltin::to_lowercase("ABCDEFGHIJKLMNOPQRSTUVWXYZ", &arena);
        assert_eq!(s, "abcdefghijklmnopqrstuvwxyz");
    }
}
