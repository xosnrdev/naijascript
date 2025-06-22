use std::ops::Range;

pub type Span = Range<usize>;

const BOLD: &str = "\x1b[1m";
const RESET: &str = "\x1b[0m";
const ERROR: &str = "\x1b[31m"; // red
const WARNING: &str = "\x1b[33m"; // yellow
const NOTE: &str = "\x1b[34m"; // blue

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Severity {
    Error,
    Warning,
    Note,
}

impl Severity {
    #[inline(always)]
    const fn label(self) -> &'static str {
        match self {
            Severity::Error => "error",
            Severity::Warning => "warning",
            Severity::Note => "note",
        }
    }
    #[inline(always)]
    const fn color_code(self) -> &'static str {
        match self {
            Severity::Error => ERROR,
            Severity::Warning => WARNING,
            Severity::Note => NOTE,
        }
    }
}

#[derive(Debug)]
pub struct Label {
    pub span: Span,
    pub message: &'static str,
}

#[derive(Debug)]
pub struct Diagnostic {
    pub span: Span,
    pub severity: Severity,
    pub code: &'static str,
    pub message: &'static str,
    pub labels: &'static [Label],
}

#[derive(Debug, Default)]
pub struct Diagnostics {
    pub diagnostics: Vec<Diagnostic>,
}

impl Diagnostics {
    #[inline(always)]
    pub fn emit(
        &mut self,
        span: Span,
        severity: Severity,
        code: &'static str,
        message: &'static str,
        labels: &'static [Label],
    ) {
        self.diagnostics.push(Diagnostic { span, severity, code, message, labels });
    }

    pub fn report(&self, src: &str, filename: &str) {
        for diag in &self.diagnostics {
            let color =  diag.severity.color_code();
            
            // 1) Compute line & column
            let before = &src[..diag.span.start];
            let line = before.chars().filter(|&c| c == '\n').count() + 1;
            let col = before.chars().rev().take_while(|&c| c != '\n').count() + 1;

            // 2) Header
            let header = format!(
                "{BOLD}{color}{}[{}]{RESET}: {BOLD}{}",
                diag.severity.label(),
                diag.code,
                diag.message
            );

            // 3) Location
            let location = format!(" -->{RESET} {filename}:{line}:{col}");

            // 4) Source line
            let line_start = before.rfind('\n').map_or(0, |i| i + 1);
            let line_end = src[line_start..].find('\n').map_or(src.len(), |i| line_start + i);
            let src_line = &src[line_start..line_end];
            let src_line_display = format!("  {BOLD}|\n{line} | {RESET}{src_line}{BOLD}{color}");

            // 5) Caret underline
            let mut caret_line = String::new();
            for _ in 0..(col + 2) {
                caret_line.push(' ');
            }
            let caret_count = (diag.span.end.saturating_sub(diag.span.start)).max(1);
            for _ in 0..caret_count {
                caret_line.push('^');
            }

            // 6) Additional labels
            let mut label_lines = Vec::new();
            for label in diag.labels {
                let lbl_col = label.span.start.saturating_sub(line_start) + 1;
                let mut label_line = String::new();
                for _ in 0..(lbl_col + 5) {
                    label_line.push(' ');
                }
                let dash_count = (label.span.end.saturating_sub(label.span.start)).max(1);
                for _ in 0..dash_count {
                    label_line.push('-');
                }
                label_line.push(' ');
                label_line.push_str(label.message);
                label_lines.push(label_line);
            }

            // 7) Print using macros
            match diag.severity {
                Severity::Error => {
                    eprintln!("{header}");
                    eprintln!("{location}");
                    for l in src_line_display.lines() {
                        eprintln!("{l}");
                    }
                    eprintln!("{caret_line}");
                    for l in &label_lines {
                        eprintln!("{l}");
                    }
                }
                _ => {
                    println!("{header}");
                    println!("{location}");
                    for l in src_line_display.lines() {
                        println!("{l}");
                    }
                    println!("{caret_line}");
                    for l in &label_lines {
                        println!("{l}");
                    }
                }
            }
        }
    }
}

pub trait AsStr: 'static {
    fn as_str(&self) -> &'static str;
}
