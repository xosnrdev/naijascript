use std::fmt::Write;

use crate::arena::{Arena, ArenaString};
use crate::builtins::Builtin;
use crate::helpers::ValueType;
use crate::runtime::Value;

/// Built-in array methods
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ArrayBuiltin {
    /// Get the length of an array
    Len,
    /// Push a value to the end of an array
    Push,
    /// Pop a value from the end of an array
    Pop,
    /// Reverse an array
    Reverse,
    /// Join an array of strings with a separator
    Join,
}

impl Builtin for ArrayBuiltin {
    fn arity(&self) -> usize {
        match self {
            ArrayBuiltin::Push | ArrayBuiltin::Join => 1,
            ArrayBuiltin::Len | ArrayBuiltin::Pop | ArrayBuiltin::Reverse => 0,
        }
    }

    fn return_type(&self) -> ValueType {
        match self {
            ArrayBuiltin::Len => ValueType::Number,
            ArrayBuiltin::Pop => ValueType::Dynamic,
            ArrayBuiltin::Push | ArrayBuiltin::Reverse => ValueType::Null,
            ArrayBuiltin::Join => ValueType::String,
        }
    }

    fn from_name(name: &str) -> Option<Self> {
        match name {
            "len" => Some(ArrayBuiltin::Len),
            "push" => Some(ArrayBuiltin::Push),
            "pop" => Some(ArrayBuiltin::Pop),
            "reverse" => Some(ArrayBuiltin::Reverse),
            "join" => Some(ArrayBuiltin::Join),
            _ => None,
        }
    }

    fn requires_mut_receiver(&self) -> bool {
        matches!(self, ArrayBuiltin::Push | ArrayBuiltin::Pop | ArrayBuiltin::Reverse)
    }
}

impl ArrayBuiltin {
    #[inline]
    #[allow(clippy::cast_precision_loss)]
    pub fn len<T>(array: &[T]) -> f64 {
        array.len() as f64
    }

    #[inline]
    pub fn push<'a>(array: &mut Vec<Value<'a>, &'a Arena>, value: Value<'a>) {
        array.push(value);
    }

    #[inline]
    pub fn pop<'a>(array: &mut Vec<Value<'a>, &'a Arena>) -> Option<Value<'a>> {
        array.pop()
    }

    #[inline]
    pub fn reverse<'a>(array: &mut Vec<Value<'a>, &'a Arena>) {
        array.reverse();
    }

    #[inline]
    pub fn join<'a>(
        array: &Vec<Value<'a>, &'a Arena>,
        sep: &str,
        arena: &'a Arena,
    ) -> ArenaString<'a> {
        let mut buffer =
            ArenaString::with_capacity_in(sep.len() * array.len().saturating_sub(1), arena);
        for (i, value) in array.iter().enumerate() {
            if i > 0 {
                buffer.push_str(sep);
            }
            match value {
                Value::Str(string) => buffer.push_str(string),
                Value::Array(arr) => buffer.push_str(&Self::join(arr, sep, arena)),
                value => write!(buffer, "{value}").unwrap(),
            }
        }
        buffer
    }
}
