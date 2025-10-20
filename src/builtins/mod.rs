mod array;
mod number;
mod string;
mod tw;

use std::{fmt, io};

pub use array::*;
pub use number::*;
pub use string::*;
pub use tw::*;

use crate::arena::{Arena, ArenaString};
use crate::arena_format;
use crate::runtime::Value;
use crate::sys::{self, Stdin};

/// Trait representing a built-in function in NaijaScript
pub trait Builtin {
    fn arity(&self) -> usize;
    fn return_type(&self) -> BuiltinReturnType;
    fn from_name(name: &str) -> Option<Self>
    where
        Self: Sized;
}

/// Return types for built-in functions.
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum BuiltinReturnType {
    Number,
    String,
    Bool,
    Array,
}

/// Enumeration of global built-in functions available in NaijaScript.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum GlobalBuiltin {
    /// Input/output function that prints values to console
    Shout,
    /// Get the type of a value as a string
    TypeOf,
    /// Read a line of input from stdin
    ReadLine,
    /// Convert a value to a string
    ToString,
}

impl Builtin for GlobalBuiltin {
    fn arity(&self) -> usize {
        match self {
            GlobalBuiltin::Shout
            | GlobalBuiltin::TypeOf
            | GlobalBuiltin::ReadLine
            | GlobalBuiltin::ToString => 1,
        }
    }

    fn return_type(&self) -> BuiltinReturnType {
        match self {
            GlobalBuiltin::Shout => BuiltinReturnType::Number,
            GlobalBuiltin::TypeOf | GlobalBuiltin::ReadLine | GlobalBuiltin::ToString => {
                BuiltinReturnType::String
            }
        }
    }

    fn from_name(name: &str) -> Option<Self> {
        match name {
            "shout" => Some(GlobalBuiltin::Shout),
            "typeof" => Some(GlobalBuiltin::TypeOf),
            "read_line" => Some(GlobalBuiltin::ReadLine),
            "to_string" => Some(GlobalBuiltin::ToString),
            _ => None,
        }
    }
}

impl GlobalBuiltin {
    #[inline]
    pub fn shout<T: fmt::Display>(value: T) {
        println!("{value}");
    }

    #[inline]
    pub fn to_string<'arena, T: fmt::Display>(
        arena: &'arena Arena,
        value: &T,
    ) -> ArenaString<'arena> {
        arena_format!(arena, "{value}")
    }

    #[inline]
    pub fn type_of(value: &Value) -> &'static str {
        match value {
            Value::Number(..) => "number",
            Value::Str(..) => "string",
            Value::Bool(..) => "boolean",
            Value::Array(..) => "array",
        }
    }

    #[inline]
    pub fn read_line<'arena>(
        prompt: &str,
        arena: &'arena Arena,
    ) -> Result<ArenaString<'arena>, io::Error> {
        sys::stdin::read_line(prompt, arena)
    }
}

/// Unified enum for all member builtin methods
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MemberBuiltin {
    String(StringBuiltin),
    Array(ArrayBuiltin),
    Number(NumberBuiltin),
}

impl Builtin for MemberBuiltin {
    fn arity(&self) -> usize {
        match self {
            MemberBuiltin::String(b) => b.arity(),
            MemberBuiltin::Array(b) => b.arity(),
            MemberBuiltin::Number(b) => b.arity(),
        }
    }

    fn return_type(&self) -> BuiltinReturnType {
        match self {
            MemberBuiltin::String(b) => b.return_type(),
            MemberBuiltin::Array(b) => b.return_type(),
            MemberBuiltin::Number(b) => b.return_type(),
        }
    }

    fn from_name(name: &str) -> Option<Self> {
        StringBuiltin::from_name(name)
            .map(MemberBuiltin::String)
            .or_else(|| ArrayBuiltin::from_name(name).map(MemberBuiltin::Array))
            .or_else(|| NumberBuiltin::from_name(name).map(MemberBuiltin::Number))
    }
}
