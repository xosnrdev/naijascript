//! The diagnostics infrastructure engine for NaijaScript.

/// Represents a diagnostic message (error, warning, or note) produced during analysis.
///
/// Each diagnostic includes its kind, a message, a code, the relevant source code, a byte span,
/// and the line and column for precise location reporting.
pub struct Diagnostic<'src> {
    /// The severity or type of the diagnostic.
    pub kind: DiagnosticKind,
    /// Human-readable message describing the issue.
    pub message: &'static str,
    /// Short code or label for the diagnostic (e.g., "runtime error").
    pub code: &'static str,
    /// The original source code where the diagnostic applies.
    pub source: &'src str,
    /// Byte range in the source code that the diagnostic refers to.
    pub span: (usize, usize),
    /// Line number for precise location reporting (1-based).
    pub line: usize,
    /// Column number for precise location reporting (1-based).
    pub column: usize,
}

/// Enumerates the severity or type of a diagnostic message.
#[derive(Debug, Clone)]
pub enum DiagnosticKind {
    /// Indicates a fatal error that prevents further processing.
    Error,
    /// Indicates a non-fatal warning.
    Warning,
    /// Provides additional information or suggestions.
    Note,
}

impl<'src> Diagnostic<'src> {
    /// Constructs a new error diagnostic.
    pub fn error(
        code: &'static str,
        message: &'static str,
        source: &'src str,
        span: (usize, usize),
        line: usize,
        column: usize,
    ) -> Self {
        Diagnostic { kind: DiagnosticKind::Error, code, message, source, span, line, column }
    }
    /// Constructs a new warning diagnostic.
    pub fn warning(
        code: &'static str,
        message: &'static str,
        source: &'src str,
        span: (usize, usize),
        line: usize,
        column: usize,
    ) -> Self {
        Diagnostic { kind: DiagnosticKind::Warning, code, message, source, span, line, column }
    }
    /// Constructs a new note diagnostic.
    pub fn note(
        code: &'static str,
        message: &'static str,
        source: &'src str,
        span: (usize, usize),
        line: usize,
        column: usize,
    ) -> Self {
        Diagnostic { kind: DiagnosticKind::Note, code, message, source, span, line, column }
    }
}

/// Trait for reporting diagnostics.
///
/// Implementors can define custom reporting strategies (e.g., stderr, file, GUI).
pub trait DiagnosticHandler {
    /// Reports a diagnostic message.
    fn report(&mut self, diag: &Diagnostic);
}

/// Diagnostic handler that prints diagnostics to standard error.
pub struct StderrHandler;

impl DiagnosticHandler for StderrHandler {
    /// Reports a diagnostic by printing it to stderr in a human-readable format.
    fn report(&mut self, diag: &Diagnostic) {
        eprintln!(
            "[{}] {} at line {}, column {}: {}",
            diag.code,
            diag.kind.kind_str(),
            diag.line,
            diag.column,
            diag.message
        );
        let (start, end) = diag.span;
        // Print a snippet of the source code if the span is valid.
        if start < end && end <= diag.source.len() {
            let snippet = &diag.source[start..end];
            eprintln!("  --> {snippet}");
        }
    }
}

impl DiagnosticKind {
    /// Returns a string representation of the diagnostic kind.
    pub fn kind_str(&self) -> &'static str {
        match self {
            DiagnosticKind::Error => "error",
            DiagnosticKind::Warning => "warning",
            DiagnosticKind::Note => "note",
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_error_diagnostic() {
        let src = "make x get 5";
        let diag = Diagnostic::error(
            "runtime error",
            "Abeg, variable no dey defined for here o",
            src,
            (5, 6),
            1,
            6,
        );
        let mut handler = StderrHandler;
        handler.report(&diag);
    }
}
