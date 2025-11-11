use crate::builtins::Builtin;
use crate::helper::ValueType;

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
    pub const fn abs(n: f64) -> f64 {
        n.abs()
    }

    #[inline]
    pub fn sqrt(n: f64) -> f64 {
        n.sqrt()
    }

    #[inline]
    pub const fn floor(n: f64) -> f64 {
        n.floor()
    }

    #[inline]
    pub const fn ceil(n: f64) -> f64 {
        n.ceil()
    }

    #[inline]
    pub const fn round(n: f64) -> f64 {
        n.round()
    }
}
