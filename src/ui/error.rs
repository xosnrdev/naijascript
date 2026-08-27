//! Stable, machine-readable UI error model.
//!
//! Mirrors the categories and recoverability defined in
//! `docs/spec/UI_IR_PROTOCOL.md`. Errors are owned values so they can cross the
//! retained engine's event boundaries without borrowing arena memory.

use crate::diagnostics::Span;

/// Broad failure category. Determines host recovery behavior.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UiErrorCategory {
    /// Source failed to compile into a UI program.
    Compile,
    /// Capability/version negotiation failed before mount.
    Negotiation,
    /// A snapshot, patch, or event failed structural/type validation.
    Validation,
    /// A protocol invariant (revisions, sessions, ordering) was violated.
    Protocol,
    /// A configured resource limit was exceeded.
    Limit,
    /// A rendering failure reported by a platform SDK.
    Renderer,
    /// A security invariant was violated.
    Security,
}

impl UiErrorCategory {
    #[must_use]
    pub const fn as_str(self) -> &'static str {
        match self {
            UiErrorCategory::Compile => "compile",
            UiErrorCategory::Negotiation => "negotiation",
            UiErrorCategory::Validation => "validation",
            UiErrorCategory::Protocol => "protocol",
            UiErrorCategory::Limit => "limit",
            UiErrorCategory::Renderer => "renderer",
            UiErrorCategory::Security => "security",
        }
    }
}

/// How a host should recover from a given error.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Recoverability {
    /// The session cannot continue.
    Fatal,
    /// The last accepted tree is preserved; a fresh snapshot is required.
    Resync,
    /// The offending event is dropped; the UI is unchanged.
    DropEvent,
}

impl Recoverability {
    #[must_use]
    pub const fn as_str(self) -> &'static str {
        match self {
            Recoverability::Fatal => "fatal",
            Recoverability::Resync => "resync",
            Recoverability::DropEvent => "drop_event",
        }
    }
}

/// A structured UI error with a stable code for host branching.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct UiError {
    pub category: UiErrorCategory,
    pub code: &'static str,
    pub message: String,
    pub span: Option<Span>,
    pub recoverability: Recoverability,
}

impl UiError {
    #[must_use]
    pub fn new(
        category: UiErrorCategory,
        code: &'static str,
        message: impl Into<String>,
        recoverability: Recoverability,
    ) -> Self {
        Self { category, code, message: message.into(), span: None, recoverability }
    }

    #[must_use]
    pub fn with_span(mut self, span: Span) -> Self {
        self.span = Some(span);
        self
    }

    #[must_use]
    pub fn compile(code: &'static str, message: impl Into<String>) -> Self {
        Self::new(UiErrorCategory::Compile, code, message, Recoverability::Fatal)
    }

    #[must_use]
    pub fn validation(code: &'static str, message: impl Into<String>) -> Self {
        Self::new(UiErrorCategory::Validation, code, message, Recoverability::Resync)
    }

    #[must_use]
    pub fn limit(code: &'static str, message: impl Into<String>) -> Self {
        Self::new(UiErrorCategory::Limit, code, message, Recoverability::Resync)
    }

    #[must_use]
    pub fn protocol(code: &'static str, message: impl Into<String>) -> Self {
        Self::new(UiErrorCategory::Protocol, code, message, Recoverability::Resync)
    }

    #[must_use]
    pub fn security(code: &'static str, message: impl Into<String>) -> Self {
        Self::new(UiErrorCategory::Security, code, message, Recoverability::Fatal)
    }

    /// A dropped event is a non-fatal `Validation` outcome.
    #[must_use]
    pub fn dropped_event(code: &'static str, message: impl Into<String>) -> Self {
        Self::new(UiErrorCategory::Validation, code, message, Recoverability::DropEvent)
    }
}

/// Result alias for UI operations.
pub type UiResult<T> = Result<T, UiError>;
