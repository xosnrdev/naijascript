use std::fmt;

pub struct LenWriter(pub usize);

impl fmt::Write for LenWriter {
    fn write_str(&mut self, s: &str) -> fmt::Result {
        self.0 += s.len();
        Ok(())
    }
}

// Represents the value types in NaijaScript
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum ValueType {
    Number,
    String,
    Bool,
    Array,
    Dynamic,
    Null,
}

impl fmt::Display for ValueType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ValueType::Number => write!(f, "number"),
            ValueType::String => write!(f, "string"),
            ValueType::Bool => write!(f, "boolean"),
            ValueType::Array => write!(f, "array"),
            ValueType::Dynamic => write!(f, "dynamic"),
            ValueType::Null => write!(f, "null"),
        }
    }
}

pub const KIBI: usize = 1024;
pub const MEBI: usize = KIBI * KIBI;
pub const GIBI: usize = MEBI * KIBI;
