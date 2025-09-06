mod string;
mod tw;

pub use string::*;
pub use tw::*;

/// Built-in functions available in NaijaScript.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Builtin {
    /// Output function that prints values to console
    Shout,
    /// Absolute value function
    Abs,
    /// Square root function
    Sqrt,
    /// Floor function (round down)
    Floor,
    /// Ceiling function (round up)
    Ceil,
    /// Round to nearest integer
    Round,
    /// String length function
    Len,
    /// String slice function
    Slice,
    /// String to uppercase function
    Upper,
    /// String to lowercase function
    Lower,
    /// Find substring in string function
    Find,
    /// Replace substring in string function
    Replace,
    /// Trim whitespace from string function
    Trim,
    /// Value to string function
    ToString,
    /// String to number function
    ToNumber,
    /// Type of value function
    TypeOf,
    /// Read line from input function
    #[cfg(not(target_family = "wasm"))]
    ReadLine,
}

/// Represents the type of a value or return type in NaijaScript
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum BuiltinReturnType {
    Number,
    String,
    Bool,
}

impl Builtin {
    /// Get the expected parameter count for the built-in function
    #[inline]
    pub const fn arity(self) -> usize {
        match self {
            Builtin::Shout
            | Builtin::Abs
            | Builtin::Sqrt
            | Builtin::Floor
            | Builtin::Ceil
            | Builtin::Round
            | Builtin::Len
            | Builtin::Upper
            | Builtin::Lower
            | Builtin::Trim
            | Builtin::ToString
            | Builtin::ToNumber
            | Builtin::TypeOf => 1,
            #[cfg(not(target_family = "wasm"))]
            Builtin::ReadLine => 1,
            Builtin::Find => 2,
            Builtin::Slice => 3,
            Builtin::Replace => 3,
        }
    }

    /// Get the return type of the built-in function
    #[inline]
    pub const fn return_type(self) -> BuiltinReturnType {
        match self {
            Builtin::Shout
            | Builtin::Abs
            | Builtin::Sqrt
            | Builtin::Floor
            | Builtin::Ceil
            | Builtin::Round
            | Builtin::Len
            | Builtin::Find
            | Builtin::ToNumber => BuiltinReturnType::Number,
            Builtin::Slice
            | Builtin::Upper
            | Builtin::Lower
            | Builtin::Replace
            | Builtin::Trim
            | Builtin::ToString
            | Builtin::TypeOf => BuiltinReturnType::String,
            #[cfg(not(target_family = "wasm"))]
            Builtin::ReadLine => BuiltinReturnType::String,
        }
    }

    /// Check if a function name refers to a built-in
    #[inline]
    pub fn from_name(name: &str) -> Option<Self> {
        match name {
            "shout" => Some(Builtin::Shout),
            "abs" => Some(Builtin::Abs),
            "sqrt" => Some(Builtin::Sqrt),
            "floor" => Some(Builtin::Floor),
            "ceil" => Some(Builtin::Ceil),
            "round" => Some(Builtin::Round),
            "len" => Some(Builtin::Len),
            "slice" => Some(Builtin::Slice),
            "upper" => Some(Builtin::Upper),
            "lower" => Some(Builtin::Lower),
            "find" => Some(Builtin::Find),
            "replace" => Some(Builtin::Replace),
            "trim" => Some(Builtin::Trim),
            "to_string" => Some(Builtin::ToString),
            "to_number" => Some(Builtin::ToNumber),
            "typeof" => Some(Builtin::TypeOf),
            #[cfg(not(target_family = "wasm"))]
            "read_line" => Some(Builtin::ReadLine),
            _ => None,
        }
    }
}
