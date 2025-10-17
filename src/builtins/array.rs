use crate::arena::Arena;
use crate::builtins::{Builtin, BuiltinReturnType};
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
}

impl Builtin for ArrayBuiltin {
    fn arity(&self) -> usize {
        match self {
            ArrayBuiltin::Len => 0,
            ArrayBuiltin::Push => 1,
            ArrayBuiltin::Pop => 0,
        }
    }

    fn return_type(&self) -> BuiltinReturnType {
        match self {
            ArrayBuiltin::Len => BuiltinReturnType::Number,
            ArrayBuiltin::Push | ArrayBuiltin::Pop => BuiltinReturnType::Array,
        }
    }

    fn from_name(name: &str) -> Option<Self> {
        match name {
            "len" => Some(ArrayBuiltin::Len),
            "push" => Some(ArrayBuiltin::Push),
            "pop" => Some(ArrayBuiltin::Pop),
            _ => None,
        }
    }
}

impl ArrayBuiltin {
    #[inline]
    pub fn len<T>(array: &[T]) -> f64 {
        array.len() as f64
    }

    #[inline]
    pub fn push<'arena, 'src>(
        array: &mut Vec<Value<'arena, 'src>, &'arena Arena>,
        value: Value<'arena, 'src>,
    ) {
        array.push(value);
    }

    #[inline]
    pub fn pop<'arena, 'src>(
        array: &mut Vec<Value<'arena, 'src>, &'arena Arena>,
    ) -> Option<Value<'arena, 'src>> {
        array.pop()
    }
}
