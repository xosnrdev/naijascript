mod string;

pub use string::*;

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
            | Builtin::Lower => 1,
            Builtin::Slice => 3,
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
            | Builtin::Len => BuiltinReturnType::Number,
            Builtin::Slice | Builtin::Upper | Builtin::Lower => BuiltinReturnType::String,
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
            _ => None,
        }
    }
}
