//! The diagnostics infrastructure engine for NaijaScript.

/// Represents a diagnostic message (error, warning, or note) produced during analysis.
///
/// Each diagnostic contains relevant information such as the severity, message, and source code context.
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
    /// Line number (1-based).
    pub line: usize,
    /// Column number (1-based).
    pub column: usize,
    /// Optional suggestion or help message.
    pub suggestion: Option<&'static str>,
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
        Diagnostic {
            kind: DiagnosticKind::Error,
            code,
            message,
            source,
            span,
            line,
            column,
            suggestion: None,
        }
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
        Diagnostic {
            kind: DiagnosticKind::Warning,
            code,
            message,
            source,
            span,
            line,
            column,
            suggestion: None,
        }
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
        Diagnostic {
            kind: DiagnosticKind::Note,
            code,
            message,
            source,
            span,
            line,
            column,
            suggestion: None,
        }
    }
    /// Returns a new diagnostic with a suggestion/help message.
    pub fn with_suggestion(mut self, suggestion: &'static str) -> Self {
        self.suggestion = Some(suggestion);
        self
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
        let display = DiagnosticDisplay::new(diag);
        eprintln!("{}", display.format());
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

/// Formats diagnostics for display.
///
/// This struct extracts context lines, aligns carets, and produces user-friendly messages for
/// diagnostic handlers.
pub struct DiagnosticDisplay<'a> {
    /// The diagnostic to format.
    pub diag: &'a Diagnostic<'a>,
}

impl<'a> DiagnosticDisplay<'a> {
    /// Creates a new display formatter for a diagnostic.
    pub fn new(diag: &'a Diagnostic<'a>) -> Self {
        Self { diag }
    }

    /// Formats the diagnostic as a string, including context lines and a caret under the error column.
    /// Adds ANSI color codes for error, warning, and note labels.
    pub fn format(&self) -> String {
        let diag = self.diag;
        let mut output = String::new();
        // ANSI color codes for kind labels
        let (kind_color, reset) = match diag.kind {
            DiagnosticKind::Error => ("\x1b[31m", "\x1b[0m"), // Red
            DiagnosticKind::Warning => ("\x1b[33m", "\x1b[0m"), // Yellow
            DiagnosticKind::Note => ("\x1b[34m", "\x1b[0m"),  // Blue
        };
        // Header line
        output
            .push_str(&format!("[{}] at line {}, column {}\n", diag.code, diag.line, diag.column));
        // Extract context lines
        let lines: Vec<&str> = diag.source.lines().collect();
        let line_idx = diag.line.saturating_sub(1);
        // Previous line (if any)
        if line_idx > 0 {
            output.push_str(&format!("{:>4} | {}\n", diag.line - 1, lines[line_idx - 1]));
        }
        // Error line (highlighted with '>')
        if let Some(line) = lines.get(line_idx) {
            output.push_str(&format!(">{:>3} | {}\n", diag.line, line));
            // Caret under the error column (1-based)
            let caret_pos = diag.column.max(1);
            let caret_line = format!(
                "     | {}{}^{}\n",
                " ".repeat(caret_pos.saturating_sub(1)),
                kind_color,
                reset
            );
            output.push_str(&caret_line);
            // Diagnostic message under the caret position
            let message_line = format!(
                "     | {}{}{}{}\n",
                " ".repeat(caret_pos.saturating_sub(1)),
                kind_color,
                diag.message,
                reset
            );
            output.push_str(&message_line);
        }
        // Next line (if any)
        if line_idx + 1 < lines.len() {
            output.push_str(&format!("{:>4} | {}\n", diag.line + 1, lines[line_idx + 1]));
        }
        // Print snippet if span is valid and not already shown
        let (start, end) = diag.span;
        if start < end && end <= diag.source.len() {
            let snippet = &diag.source[start..end];
            if lines.get(line_idx).is_none_or(|l| !l.contains(snippet)) {
                output.push_str(&format!("  --> {snippet}\n"));
            }
        }
        // Print suggestion/help if present
        if let Some(suggestion) = diag.suggestion {
            output.push_str(&format!("help: {suggestion}\n"));
        }
        output
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
