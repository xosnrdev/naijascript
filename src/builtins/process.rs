use crate::builtins::Builtin;
use crate::helpers::ValueType;

/// Built-in methods for `process_command`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ProcessCommandBuiltin {
    Arg,
    Cwd,
    Env,
    StdinText,
    StdinInherit,
    StdinNull,
    StdoutCapture,
    StdoutInherit,
    StdoutNull,
    StderrCapture,
    StderrInherit,
    StderrNull,
    TimeoutMs,
    Run,
}

impl Builtin for ProcessCommandBuiltin {
    fn arity(&self) -> usize {
        match self {
            ProcessCommandBuiltin::Arg
            | ProcessCommandBuiltin::Cwd
            | ProcessCommandBuiltin::StdinText
            | ProcessCommandBuiltin::TimeoutMs => 1,
            ProcessCommandBuiltin::Env => 2,
            ProcessCommandBuiltin::StdinInherit
            | ProcessCommandBuiltin::StdinNull
            | ProcessCommandBuiltin::StdoutCapture
            | ProcessCommandBuiltin::StdoutInherit
            | ProcessCommandBuiltin::StdoutNull
            | ProcessCommandBuiltin::StderrCapture
            | ProcessCommandBuiltin::StderrInherit
            | ProcessCommandBuiltin::StderrNull
            | ProcessCommandBuiltin::Run => 0,
        }
    }

    fn return_type(&self) -> ValueType {
        match self {
            ProcessCommandBuiltin::Run => ValueType::ProcessResult,
            ProcessCommandBuiltin::Arg
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
            | ProcessCommandBuiltin::TimeoutMs => ValueType::Null,
        }
    }

    fn from_name(name: &str) -> Option<Self> {
        match name {
            "arg" => Some(Self::Arg),
            "cwd" => Some(Self::Cwd),
            "env" => Some(Self::Env),
            "stdin_text" => Some(Self::StdinText),
            "stdin_inherit" => Some(Self::StdinInherit),
            "stdin_null" => Some(Self::StdinNull),
            "stdout_capture" => Some(Self::StdoutCapture),
            "stdout_inherit" => Some(Self::StdoutInherit),
            "stdout_null" => Some(Self::StdoutNull),
            "stderr_capture" => Some(Self::StderrCapture),
            "stderr_inherit" => Some(Self::StderrInherit),
            "stderr_null" => Some(Self::StderrNull),
            "timeout_ms" => Some(Self::TimeoutMs),
            "run" => Some(Self::Run),
            _ => None,
        }
    }

    fn requires_mut_receiver(&self) -> bool {
        !matches!(self, Self::Run)
    }
}

/// Built-in methods for `process_result`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ProcessResultBuiltin {
    Success,
    ExitCode,
    Stdout,
    Stderr,
}

impl Builtin for ProcessResultBuiltin {
    fn arity(&self) -> usize {
        0
    }

    fn return_type(&self) -> ValueType {
        match self {
            ProcessResultBuiltin::Success => ValueType::Bool,
            ProcessResultBuiltin::ExitCode
            | ProcessResultBuiltin::Stdout
            | ProcessResultBuiltin::Stderr => ValueType::Dynamic,
        }
    }

    fn from_name(name: &str) -> Option<Self> {
        match name {
            "success" => Some(Self::Success),
            "exit_code" => Some(Self::ExitCode),
            "stdout" => Some(Self::Stdout),
            "stderr" => Some(Self::Stderr),
            _ => None,
        }
    }
}
