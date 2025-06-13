pub struct Diagnostic<'src> {
    pub kind: DiagnosticKind,
    pub message: &'static str,
    pub code: &'static str,
    pub source: &'src str,
    pub span: (usize, usize),
    pub line: usize,
    pub column: usize,
}

#[derive(Debug, Clone)]
pub enum DiagnosticKind {
    Error,
    Warning,
    Note,
}

impl<'src> Diagnostic<'src> {
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

pub trait DiagnosticHandler {
    fn report(&mut self, diag: &Diagnostic);
}

pub struct StderrHandler;

impl DiagnosticHandler for StderrHandler {
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
        if start < end && end <= diag.source.len() {
            let snippet = &diag.source[start..end];
            eprintln!("  --> {snippet}");
        }
    }
}

impl DiagnosticKind {
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
