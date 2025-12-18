use crate::builtins::Builtin;
use crate::helpers::ValueType;

/// Built-in number methods
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum NumberBuiltin {
    /// Get absolute value
    Abs,
    /// Get square root
    Sqrt,
    /// Get floor value (round down)
    Floor,
    /// Get ceiling value (round up)
    Ceil,
    /// Get rounded value to nearest integer
    Round,
}

impl Builtin for NumberBuiltin {
    fn arity(&self) -> usize {
        match self {
            NumberBuiltin::Abs
            | NumberBuiltin::Sqrt
            | NumberBuiltin::Floor
            | NumberBuiltin::Ceil
            | NumberBuiltin::Round => 0,
        }
    }

    fn return_type(&self) -> ValueType {
        ValueType::Number
    }

    fn from_name(name: &str) -> Option<Self> {
        match name {
            "abs" => Some(NumberBuiltin::Abs),
            "sqrt" => Some(NumberBuiltin::Sqrt),
            "floor" => Some(NumberBuiltin::Floor),
            "ceil" => Some(NumberBuiltin::Ceil),
            "round" => Some(NumberBuiltin::Round),
            _ => None,
        }
    }
}

impl NumberBuiltin {
    #[inline]
    #[must_use]
    pub const fn abs(n: f64) -> f64 {
        n.abs()
    }

    #[inline]
    #[must_use]
    pub fn sqrt(n: f64) -> f64 {
        n.sqrt()
    }

    #[inline]
    #[must_use]
    pub const fn floor(n: f64) -> f64 {
        n.floor()
    }

    #[inline]
    #[must_use]
    pub const fn ceil(n: f64) -> f64 {
        n.ceil()
    }

    #[inline]
    #[must_use]
    pub const fn round(n: f64) -> f64 {
        n.round()
    }
}
