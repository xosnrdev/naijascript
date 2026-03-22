//! Effect and trap classification for analysis passes.

use crate::builtins::{
    ArrayBuiltin, GlobalBuiltin, MemberBuiltin, ProcessCommandBuiltin, ProcessResultBuiltin,
};

/// Conservative effect class for an expression or statement.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum ExprClass {
    PureNoTrap,
    PureMayTrap,
    Impure,
}

impl ExprClass {
    /// Joins two classes conservatively.
    #[must_use]
    pub const fn join(self, other: Self) -> Self {
        match (self, other) {
            (Self::Impure, ..) | (.., Self::Impure) => Self::Impure,
            (Self::PureMayTrap, ..) | (.., Self::PureMayTrap) => Self::PureMayTrap,
            (Self::PureNoTrap, Self::PureNoTrap) => Self::PureNoTrap,
        }
    }
}

/// Returns the effect class of a global builtin call.
#[must_use]
pub const fn global_builtin_class(builtin: GlobalBuiltin) -> ExprClass {
    match builtin {
        GlobalBuiltin::TypeOf | GlobalBuiltin::ToString | GlobalBuiltin::Command => {
            ExprClass::PureNoTrap
        }
        GlobalBuiltin::ReadLine | GlobalBuiltin::Shout => ExprClass::Impure,
    }
}

/// Returns the effect class of a member builtin call.
#[must_use]
pub const fn member_builtin_class(builtin: MemberBuiltin) -> ExprClass {
    match builtin {
        MemberBuiltin::Array(array_builtin) => array_builtin_class(array_builtin),
        MemberBuiltin::String(..) | MemberBuiltin::Number(..) => ExprClass::PureNoTrap,
        MemberBuiltin::ProcessCommand(command_builtin) => {
            process_command_builtin_class(command_builtin)
        }
        MemberBuiltin::ProcessResult(result_builtin) => {
            process_result_builtin_class(result_builtin)
        }
    }
}

const fn array_builtin_class(builtin: ArrayBuiltin) -> ExprClass {
    match builtin {
        ArrayBuiltin::Len | ArrayBuiltin::Join => ExprClass::PureNoTrap,
        ArrayBuiltin::Push | ArrayBuiltin::Pop | ArrayBuiltin::Reverse => ExprClass::Impure,
    }
}

const fn process_command_builtin_class(builtin: ProcessCommandBuiltin) -> ExprClass {
    match builtin {
        ProcessCommandBuiltin::Run
        | ProcessCommandBuiltin::Arg
        | ProcessCommandBuiltin::Cwd
        | ProcessCommandBuiltin::Env
        | ProcessCommandBuiltin::StdinText
        | ProcessCommandBuiltin::StdinInherit
        | ProcessCommandBuiltin::StdinNull
        | ProcessCommandBuiltin::StdoutCapture
        | ProcessCommandBuiltin::StdoutInherit
        | ProcessCommandBuiltin::StdoutNull
        | ProcessCommandBuiltin::StderrCapture
        | ProcessCommandBuiltin::StderrInherit
        | ProcessCommandBuiltin::StderrNull
        | ProcessCommandBuiltin::TimeoutMs => ExprClass::Impure,
    }
}

const fn process_result_builtin_class(builtin: ProcessResultBuiltin) -> ExprClass {
    match builtin {
        ProcessResultBuiltin::Success
        | ProcessResultBuiltin::ExitCode
        | ProcessResultBuiltin::Stdout
        | ProcessResultBuiltin::Stderr => ExprClass::PureNoTrap,
    }
}
