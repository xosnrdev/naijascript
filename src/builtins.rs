/// Built-in functions available in NaijaScript.
///
/// These are globally available functions implemented in the interpreter
/// rather than defined by user code.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Builtin {
    /// Output function that prints values to console
    Shout,
}

impl Builtin {
    /// Get the name of the built-in function as it appears in source code
    #[inline]
    pub const fn name(self) -> &'static str {
        match self {
            Builtin::Shout => "shout",
        }
    }

    /// Get the expected parameter count for the built-in function
    #[inline]
    pub const fn arity(self) -> usize {
        match self {
            Builtin::Shout => 1,
        }
    }

    /// Check if a function name refers to a built-in
    #[inline]
    pub fn from_name(name: &str) -> Option<Self> {
        match name {
            "shout" => Some(Builtin::Shout),
            _ => None,
        }
    }
}
