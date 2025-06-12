use crate::syntax::parser::ParseError;
use crate::syntax::{LexError, LexErrorKind};

#[derive(Debug, Clone, PartialEq)]
pub enum DiagnosticKind {
    Error,
    Warning,
    Info,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Diagnostic {
    pub kind: DiagnosticKind,
    pub code: &'static str,
    pub message: String,
    pub file: Option<String>,
    pub line: Option<usize>,
    pub column: Option<usize>,
    pub snippet: Option<String>,
    pub suggestion: Option<String>,
}

pub trait DiagnosticEmitter {
    fn emit(&self, diagnostic: &Diagnostic);
}

pub struct TerminalEmitter;

impl DiagnosticEmitter for TerminalEmitter {
    fn emit(&self, diagnostic: &Diagnostic) {
        let kind = match diagnostic.kind {
            DiagnosticKind::Error => "error",
            DiagnosticKind::Warning => "warning",
            DiagnosticKind::Info => "info",
        };
        let code = diagnostic.code;
        let loc = match (&diagnostic.file, diagnostic.line, diagnostic.column) {
            (Some(f), Some(l), Some(c)) => format!(" --> {f}:{l}:{c}"),
            (Some(f), Some(l), None) => format!(" --> {f}:{l}"),
            (Some(f), None, None) => format!(" --> {f}"),
            _ => String::new(),
        };
        println!("{kind}[{code}]: {}{loc}", diagnostic.message);
        if let Some(snippet) = &diagnostic.snippet {
            let mut lines = snippet.lines();
            if let Some(code_line) = lines.next() {
                println!("   | {code_line}");
            }
            if let Some(caret_line) = lines.next() {
                println!("     {caret_line}");
            }
        }
        if let Some(suggestion) = &diagnostic.suggestion {
            println!("   = suggestion: {suggestion}");
        }
    }
}

impl<'a> LexError<'a> {
    pub fn to_diagnostic(&self, input: &str, file: Option<&str>) -> Diagnostic {
        let line_num = self.line;
        let column = self.column;
        let snippet = input.lines().nth(line_num.saturating_sub(1)).map(|l| {
            let mut s = l.to_string();
            s.push('\n');
            s.push_str(&" ".repeat(column.saturating_sub(1)));
            s.push('^');
            s
        });
        let (code, suggestion): (&str, Option<&str>) = match &self.kind {
            LexErrorKind::UnexpectedCharacter(_) => ("E1001", None),
            LexErrorKind::InvalidNumber(_) => ("E1002", None),
            LexErrorKind::UnterminatedString => ("E1003", None),
            LexErrorKind::InvalidEscapeSequence(_) => ("E1004", None),
        };
        Diagnostic {
            kind: DiagnosticKind::Error,
            code,
            message: self.to_string(),
            file: file.map(|s| s.to_string()),
            line: Some(line_num),
            column: Some(column),
            snippet,
            suggestion: suggestion.map(|s| s.to_string()),
        }
    }
}

impl<'a> ParseError<'a> {
    pub fn to_diagnostic(&self, input: &str, file: Option<&str>) -> Diagnostic {
        match self {
            ParseError::UnexpectedEof => {
                let line_num = input.lines().count();
                let last_line = input.lines().last().unwrap_or("");
                let column = last_line.len() + 1;
                let snippet = if !last_line.is_empty() {
                    let mut s = last_line.to_string();
                    s.push('\n');
                    s.push_str(&" ".repeat(column.saturating_sub(1)));
                    s.push('^');
                    Some(s)
                } else {
                    None
                };
                Diagnostic {
                    kind: DiagnosticKind::Error,
                    code: "E2002",
                    message: "Omo! Your code finish for middle, e never complete".to_string(),
                    file: file.map(|s| s.to_string()),
                    line: Some(line_num),
                    column: Some(column),
                    snippet,
                    suggestion: Some("Check say you complete your statement or block".to_string()),
                }
            }
            ParseError::UnexpectedToken(tok) => {
                let token_str = format!("{tok}");
                let mut line_num = 1;
                let mut column = 1;
                let mut found = false;
                for (i, line) in input.lines().enumerate() {
                    if let Some(idx) = line.find(&token_str) {
                        line_num = i + 1;
                        column = idx + 1;
                        found = true;
                        break;
                    }
                }
                let snippet = if found {
                    let code_line = input.lines().nth(line_num - 1).unwrap_or("");
                    let mut s = code_line.to_string();
                    s.push('\n');
                    s.push_str(&" ".repeat(column.saturating_sub(1)));
                    s.push('^');
                    Some(s)
                } else {
                    None
                };
                Diagnostic {
                    kind: DiagnosticKind::Error,
                    code: "E2001",
                    message: format!("Wetin be dis token: {tok}"),
                    file: file.map(|s| s.to_string()),
                    line: Some(line_num),
                    column: Some(column),
                    snippet,
                    suggestion: Some(
                        "Check say you no misspelled or put wrong thing for here".to_string(),
                    ),
                }
            }
            ParseError::LexerError(e) => e.to_diagnostic(input, file),
        }
    }
}
