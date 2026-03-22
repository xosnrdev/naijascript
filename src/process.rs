//! Process execution types, limits, and host policy.

use std::ptr::NonNull;
use std::{fmt, io};

use crate::arena::{Arena, ArenaString, PoolSet};

/// Hard limits for host process execution.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ProcessCaps {
    pub max_program_bytes: u32,
    pub max_cwd_bytes: u32,
    pub max_args: u32,
    pub max_arg_bytes: u32,
    pub max_total_arg_bytes: u32,
    pub max_env_pairs: u32,
    pub max_env_key_bytes: u32,
    pub max_env_value_bytes: u32,
    pub max_total_env_bytes: u32,
    pub max_stdin_bytes: u32,
    pub max_capture_bytes_per_stream: u32,
    pub default_timeout_ms: u32,
    pub max_timeout_ms: u32,
    pub wait_poll_ms: u32,
}

impl ProcessCaps {
    #[must_use]
    pub const fn defaults() -> Self {
        Self {
            max_program_bytes: 4_096,
            max_cwd_bytes: 4_096,
            max_args: 256,
            max_arg_bytes: 65_536,
            max_total_arg_bytes: 262_144,
            max_env_pairs: 128,
            max_env_key_bytes: 256,
            max_env_value_bytes: 16_384,
            max_total_env_bytes: 131_072,
            max_stdin_bytes: 1_048_576,
            max_capture_bytes_per_stream: 1_048_576,
            default_timeout_ms: 900_000,
            max_timeout_ms: 3_600_000,
            wait_poll_ms: 10,
        }
    }
}

/// Host capability policy injected by the embedding runtime.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct HostPolicy {
    pub allow_process: bool,
    pub process: ProcessCaps,
}

impl HostPolicy {
    #[must_use]
    pub const fn native_default() -> Self {
        Self { allow_process: true, process: ProcessCaps::defaults() }
    }

    #[must_use]
    pub const fn wasm_default() -> Self {
        Self { allow_process: false, process: ProcessCaps::defaults() }
    }

    #[must_use]
    pub const fn target_default() -> Self {
        #[cfg(target_family = "wasm")]
        {
            Self::wasm_default()
        }
        #[cfg(not(target_family = "wasm"))]
        {
            Self::native_default()
        }
    }
}

impl Default for HostPolicy {
    fn default() -> Self {
        Self::target_default()
    }
}

/// Child stdin configuration.
#[derive(Debug, Clone)]
pub enum StdinPolicy<'a> {
    Inherit,
    Null,
    Text(ArenaString<'a>),
}

/// Child stdout or stderr configuration.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum OutputPolicy {
    Inherit,
    Null,
    Capture,
}

/// One environment variable override.
#[derive(Debug, Clone)]
pub struct EnvPair<'a> {
    pub key: ArenaString<'a>,
    pub value: ArenaString<'a>,
}

impl EnvPair<'_> {
    #[must_use]
    pub fn clone_into<'b>(&self, arena: &'b Arena) -> EnvPair<'b> {
        EnvPair {
            key: ArenaString::from_str(arena, self.key.as_str()),
            value: ArenaString::from_str(arena, self.value.as_str()),
        }
    }
}

/// User-visible process command builder.
#[derive(Debug, Clone)]
pub struct ProcessCommand<'a> {
    pub program: ArenaString<'a>,
    pub args: Vec<ArenaString<'a>, &'a Arena>,
    pub cwd: Option<ArenaString<'a>>,
    pub env: Vec<EnvPair<'a>, &'a Arena>,
    pub stdin: StdinPolicy<'a>,
    pub stdout: OutputPolicy,
    pub stderr: OutputPolicy,
    pub timeout_ms: Option<u32>,
}

impl<'a> ProcessCommand<'a> {
    #[must_use]
    pub fn new(program: &str, arena: &'a Arena) -> Self {
        Self {
            program: ArenaString::from_str(arena, program),
            args: Vec::new_in(arena),
            cwd: None,
            env: Vec::new_in(arena),
            stdin: StdinPolicy::Inherit,
            stdout: OutputPolicy::Inherit,
            stderr: OutputPolicy::Inherit,
            timeout_ms: None,
        }
    }

    pub fn push_arg(&mut self, arg: ArenaString<'a>) {
        self.args.push(arg);
    }

    pub fn set_cwd(&mut self, cwd: ArenaString<'a>) {
        self.cwd = Some(cwd);
    }

    pub fn set_env(&mut self, key: ArenaString<'a>, value: ArenaString<'a>) {
        if let Some(pair) = self.env.iter_mut().rev().find(|pair| pair.key == key.as_str()) {
            pair.value = value;
        } else {
            self.env.push(EnvPair { key, value });
        }
    }

    pub fn set_stdin_text(&mut self, text: ArenaString<'a>) {
        self.stdin = StdinPolicy::Text(text);
    }

    pub fn set_stdin_policy(&mut self, policy: StdinPolicy<'a>) {
        self.stdin = policy;
    }

    pub fn set_stdout_policy(&mut self, policy: OutputPolicy) {
        self.stdout = policy;
    }

    pub fn set_stderr_policy(&mut self, policy: OutputPolicy) {
        self.stderr = policy;
    }

    pub fn set_timeout_ms(&mut self, timeout_ms: u32) {
        self.timeout_ms = Some(timeout_ms);
    }

    #[must_use]
    pub fn clone_into<'b>(&self, arena: &'b Arena) -> ProcessCommand<'b> {
        let mut args = Vec::with_capacity_in(self.args.len(), arena);
        for arg in &self.args {
            args.push(ArenaString::from_str(arena, arg.as_str()));
        }

        let mut env = Vec::with_capacity_in(self.env.len(), arena);
        for pair in &self.env {
            env.push(pair.clone_into(arena));
        }

        let stdin = match &self.stdin {
            StdinPolicy::Inherit => StdinPolicy::Inherit,
            StdinPolicy::Null => StdinPolicy::Null,
            StdinPolicy::Text(text) => StdinPolicy::Text(ArenaString::from_str(arena, text)),
        };

        ProcessCommand {
            program: ArenaString::from_str(arena, self.program.as_str()),
            args,
            cwd: self.cwd.as_ref().map(|cwd| ArenaString::from_str(arena, cwd.as_str())),
            env,
            stdin,
            stdout: self.stdout,
            stderr: self.stderr,
            timeout_ms: self.timeout_ms,
        }
    }

    pub fn validate<'b>(&'b self, caps: &ProcessCaps) -> Result<ProcessSpec<'b>, ProcessError> {
        validate_named_text(
            "program",
            self.program.as_str(),
            caps.max_program_bytes,
            false,
            false,
        )?;
        validate_count(self.args.len(), caps.max_args, "argument count")?;
        validate_count(self.env.len(), caps.max_env_pairs, "environment pair count")?;

        let mut total_arg_bytes = 0u32;
        for arg in &self.args {
            let len =
                validate_named_text("argument", arg.as_str(), caps.max_arg_bytes, true, false)?;
            total_arg_bytes = total_arg_bytes
                .checked_add(len)
                .ok_or(ProcessError::SpecInvalid("Argument bytes pass configured limit"))?;
        }
        if total_arg_bytes > caps.max_total_arg_bytes {
            return Err(ProcessError::SpecInvalid("Argument bytes pass configured limit"));
        }

        let cwd = match &self.cwd {
            Some(cwd) => {
                validate_named_text("cwd", cwd.as_str(), caps.max_cwd_bytes, false, false)?;
                Some(cwd.as_str())
            }
            None => None,
        };

        let mut total_env_bytes = 0u32;
        for pair in &self.env {
            let key_len = validate_named_text(
                "environment key",
                pair.key.as_str(),
                caps.max_env_key_bytes,
                false,
                true,
            )?;
            let value_len = validate_named_text(
                "environment value",
                pair.value.as_str(),
                caps.max_env_value_bytes,
                true,
                false,
            )?;
            total_env_bytes = total_env_bytes
                .checked_add(key_len)
                .and_then(|value| value.checked_add(value_len))
                .ok_or(ProcessError::SpecInvalid("Environment bytes pass configured limit"))?;
        }
        if total_env_bytes > caps.max_total_env_bytes {
            return Err(ProcessError::SpecInvalid("Environment bytes pass configured limit"));
        }

        match &self.stdin {
            StdinPolicy::Text(text) => {
                validate_named_text(
                    "stdin text",
                    text.as_str(),
                    caps.max_stdin_bytes,
                    true,
                    false,
                )?;
            }
            StdinPolicy::Inherit | StdinPolicy::Null => {}
        }

        let timeout_ms = self.timeout_ms.unwrap_or(caps.default_timeout_ms);
        if timeout_ms == 0 {
            return Err(ProcessError::SpecInvalid("Timeout must be positive"));
        }
        if timeout_ms > caps.max_timeout_ms {
            return Err(ProcessError::SpecInvalid("Timeout pass configured limit"));
        }

        Ok(ProcessSpec {
            program: self.program.as_str(),
            args: &self.args,
            cwd,
            env: &self.env,
            stdin: &self.stdin,
            stdout: self.stdout,
            stderr: self.stderr,
            timeout_ms,
        })
    }
}

/// Host-backed runtime values exposed to scripts.
#[derive(Debug, Clone, PartialEq)]
pub enum HostValue<'a> {
    ProcessCommand(ProcessCommand<'a>),
    ProcessResult(ProcessResult<'a>),
}

/// Arena-backed indirection that keeps host values out of the hot interpreter value layout.
#[derive(Debug)]
pub struct HostHandle<'a>(NonNull<HostValue<'a>>);

impl HostValue<'_> {
    #[must_use]
    pub fn clone_into<'b>(&self, arena: &'b Arena) -> HostValue<'b> {
        match self {
            HostValue::ProcessCommand(command) => {
                HostValue::ProcessCommand(command.clone_into(arena))
            }
            HostValue::ProcessResult(result) => HostValue::ProcessResult(result.clone_into(arena)),
        }
    }
}

impl fmt::Display for HostValue<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            HostValue::ProcessCommand(command) => {
                write!(
                    f,
                    "<process_command program=\"{}\" args={}>",
                    command.program,
                    command.args.len()
                )
            }
            HostValue::ProcessResult(result) => {
                if let Some(code) = result.exit_code {
                    write!(f, "<process_result success={} exit={code}>", result.success)
                } else {
                    write!(f, "<process_result success={} exit=null>", result.success)
                }
            }
        }
    }
}

impl<'a> HostHandle<'a> {
    #[must_use]
    pub(crate) fn new_in(arena: &'a Arena, value: HostValue<'a>) -> Self {
        let slot = arena.alloc_uninit::<HostValue<'a>>();
        Self(NonNull::from(slot.write(value)))
    }

    #[must_use]
    pub(crate) fn get(&self) -> &HostValue<'a> {
        unsafe { self.0.as_ref() }
    }

    #[must_use]
    pub(crate) fn get_mut(&mut self) -> &mut HostValue<'a> {
        unsafe { self.0.as_mut() }
    }

    #[must_use]
    pub(crate) fn clone_into<'b>(&self, arena: &'b Arena) -> HostHandle<'b> {
        HostHandle::new_in(arena, self.get().clone_into(arena))
    }

    #[must_use]
    pub(crate) fn promote(self, pool: &PoolSet<'a>, frame: &Arena) -> HostHandle<'a> {
        if !frame.contains_ptr(self.0.as_ptr().cast::<u8>().cast_const()) {
            return self;
        }
        HostHandle::new_in(pool.arena(), self.get().clone_into(pool.arena()))
    }
}

impl PartialEq for HostHandle<'_> {
    fn eq(&self, other: &Self) -> bool {
        self.get() == other.get()
    }
}

/// User-visible process result.
#[derive(Debug, Clone)]
pub struct ProcessResult<'a> {
    pub success: bool,
    pub exit_code: Option<i32>,
    pub stdout: Option<ArenaString<'a>>,
    pub stderr: Option<ArenaString<'a>>,
}

impl PartialEq for StdinPolicy<'_> {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (StdinPolicy::Inherit, StdinPolicy::Inherit)
            | (StdinPolicy::Null, StdinPolicy::Null) => true,
            (StdinPolicy::Text(lhs), StdinPolicy::Text(rhs)) => lhs.as_str() == rhs.as_str(),
            _ => false,
        }
    }
}

impl PartialEq for EnvPair<'_> {
    fn eq(&self, other: &Self) -> bool {
        self.key.as_str() == other.key.as_str() && self.value.as_str() == other.value.as_str()
    }
}

impl PartialEq for ProcessCommand<'_> {
    fn eq(&self, other: &Self) -> bool {
        self.program.as_str() == other.program.as_str()
            && self.args.len() == other.args.len()
            && self.args.iter().zip(&other.args).all(|(lhs, rhs)| lhs.as_str() == rhs.as_str())
            && self.cwd.as_ref().map(ArenaString::as_str)
                == other.cwd.as_ref().map(ArenaString::as_str)
            && self.env == other.env
            && self.stdin == other.stdin
            && self.stdout == other.stdout
            && self.stderr == other.stderr
            && self.timeout_ms == other.timeout_ms
    }
}

impl PartialEq for ProcessResult<'_> {
    fn eq(&self, other: &Self) -> bool {
        self.success == other.success
            && self.exit_code == other.exit_code
            && self.stdout.as_ref().map(ArenaString::as_str)
                == other.stdout.as_ref().map(ArenaString::as_str)
            && self.stderr.as_ref().map(ArenaString::as_str)
                == other.stderr.as_ref().map(ArenaString::as_str)
    }
}

impl ProcessResult<'_> {
    #[must_use]
    pub fn clone_into<'b>(&self, arena: &'b Arena) -> ProcessResult<'b> {
        ProcessResult {
            success: self.success,
            exit_code: self.exit_code,
            stdout: self.stdout.as_ref().map(|stdout| ArenaString::from_str(arena, stdout)),
            stderr: self.stderr.as_ref().map(|stderr| ArenaString::from_str(arena, stderr)),
        }
    }
}

/// Immutable command specification passed into the platform backend.
#[derive(Debug, Clone, Copy)]
pub struct ProcessSpec<'a> {
    pub program: &'a str,
    pub args: &'a [ArenaString<'a>],
    pub cwd: Option<&'a str>,
    pub env: &'a [EnvPair<'a>],
    pub stdin: &'a StdinPolicy<'a>,
    pub stdout: OutputPolicy,
    pub stderr: OutputPolicy,
    pub timeout_ms: u32,
}

/// Stream identifier for process-related diagnostics.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ProcessStream {
    Stdout,
    Stderr,
}

impl ProcessStream {
    #[must_use]
    pub const fn as_str(self) -> &'static str {
        match self {
            ProcessStream::Stdout => "stdout",
            ProcessStream::Stderr => "stderr",
        }
    }
}

/// Process execution failure reported by the backend.
#[derive(Debug)]
pub enum ProcessError {
    Unsupported,
    Denied,
    SpawnFailed(io::Error),
    Timeout,
    OutputLimitExceeded(ProcessStream),
    InvalidUtf8(ProcessStream),
    SpecInvalid(&'static str),
}

fn validate_count(len: usize, max: u32, name: &'static str) -> Result<(), ProcessError> {
    let len = u32::try_from(len).map_err(|_| ProcessError::SpecInvalid(name))?;
    if len > max { Err(ProcessError::SpecInvalid(name)) } else { Ok(()) }
}

fn validate_named_text(
    name: &'static str,
    value: &str,
    max_bytes: u32,
    allow_empty: bool,
    forbid_equals: bool,
) -> Result<u32, ProcessError> {
    if !allow_empty && value.is_empty() {
        return Err(ProcessError::SpecInvalid(name));
    }
    if value.contains('\0') {
        return Err(ProcessError::SpecInvalid(name));
    }
    if forbid_equals && value.contains('=') {
        return Err(ProcessError::SpecInvalid(name));
    }

    let len = u32::try_from(value.len()).map_err(|_| ProcessError::SpecInvalid(name))?;
    if len > max_bytes { Err(ProcessError::SpecInvalid(name)) } else { Ok(len) }
}
