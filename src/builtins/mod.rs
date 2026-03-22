mod array;
mod number;
mod process;
mod replace;
mod string;
mod tw;

use std::{fmt, io};

pub use array::*;
pub use number::*;
pub use process::*;
pub use replace::*;
pub use string::*;
pub use tw::*;

use crate::arena::{Arena, ArenaString};
use crate::arena_format;
use crate::helpers::ValueType;
use crate::process::HostValue;
use crate::runtime::Value;
use crate::sys::{self, Stdin};

/// Trait representing a built-in function in `NaijaScript`
pub trait Builtin {
    fn arity(&self) -> usize;
    fn return_type(&self) -> ValueType;
    fn from_name(name: &str) -> Option<Self>
    where
        Self: Sized;
    fn requires_mut_receiver(&self) -> bool {
        false
    }
}

/// Enumeration of global built-in functions available in `NaijaScript`.
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
    /// Construct a process command builder
    Command,
}

impl Builtin for GlobalBuiltin {
    fn arity(&self) -> usize {
        match self {
            GlobalBuiltin::Shout
            | GlobalBuiltin::TypeOf
            | GlobalBuiltin::ReadLine
            | GlobalBuiltin::ToString
            | GlobalBuiltin::Command => 1,
        }
    }

    fn return_type(&self) -> ValueType {
        match self {
            GlobalBuiltin::Shout => ValueType::Null,
            GlobalBuiltin::TypeOf | GlobalBuiltin::ReadLine | GlobalBuiltin::ToString => {
                ValueType::String
            }
            GlobalBuiltin::Command => ValueType::ProcessCommand,
        }
    }

    fn from_name(name: &str) -> Option<Self> {
        match name {
            "shout" => Some(GlobalBuiltin::Shout),
            "typeof" => Some(GlobalBuiltin::TypeOf),
            "read_line" => Some(GlobalBuiltin::ReadLine),
            "to_string" => Some(GlobalBuiltin::ToString),
            "command" => Some(GlobalBuiltin::Command),
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
    #[must_use]
    pub fn type_of(value: &Value) -> &'static str {
        match value {
            Value::Number(..) => "number",
            Value::Str(..) => "string",
            Value::Bool(..) => "boolean",
            Value::Array(..) => "array",
            Value::Host(host) => match host.get() {
                HostValue::ProcessCommand(..) => "process_command",
                HostValue::ProcessResult(..) => "process_result",
            },
            Value::Null => "null",
        }
    }

    #[inline]
    pub fn read_line<'a>(
        prompt: &Value<'a>,
        arena: &'a Arena,
    ) -> Result<ArenaString<'a>, io::Error> {
        sys::stdin::read_line(prompt, arena)
    }
}

/// Unified enum for all member builtin methods
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MemberBuiltin {
    String(StringBuiltin),
    Array(ArrayBuiltin),
    Number(NumberBuiltin),
    ProcessCommand(ProcessCommandBuiltin),
    ProcessResult(ProcessResultBuiltin),
}

impl Builtin for MemberBuiltin {
    fn arity(&self) -> usize {
        match self {
            MemberBuiltin::String(b) => b.arity(),
            MemberBuiltin::Array(b) => b.arity(),
            MemberBuiltin::Number(b) => b.arity(),
            MemberBuiltin::ProcessCommand(b) => b.arity(),
            MemberBuiltin::ProcessResult(b) => b.arity(),
        }
    }

    fn return_type(&self) -> ValueType {
        match self {
            MemberBuiltin::String(b) => b.return_type(),
            MemberBuiltin::Array(b) => b.return_type(),
            MemberBuiltin::Number(b) => b.return_type(),
            MemberBuiltin::ProcessCommand(b) => b.return_type(),
            MemberBuiltin::ProcessResult(b) => b.return_type(),
        }
    }

    fn from_name(name: &str) -> Option<Self> {
        StringBuiltin::from_name(name)
            .map(MemberBuiltin::String)
            .or_else(|| ArrayBuiltin::from_name(name).map(MemberBuiltin::Array))
            .or_else(|| NumberBuiltin::from_name(name).map(MemberBuiltin::Number))
            .or_else(|| ProcessCommandBuiltin::from_name(name).map(MemberBuiltin::ProcessCommand))
            .or_else(|| ProcessResultBuiltin::from_name(name).map(MemberBuiltin::ProcessResult))
    }

    fn requires_mut_receiver(&self) -> bool {
        match self {
            MemberBuiltin::String(b) => b.requires_mut_receiver(),
            MemberBuiltin::Array(b) => b.requires_mut_receiver(),
            MemberBuiltin::Number(b) => b.requires_mut_receiver(),
            MemberBuiltin::ProcessCommand(b) => b.requires_mut_receiver(),
            MemberBuiltin::ProcessResult(b) => b.requires_mut_receiver(),
        }
    }
}
