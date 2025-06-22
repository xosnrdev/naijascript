use std::ops::Range;

pub type Span = Range<usize>;

pub const BOLD: &str = "\x1b[1m";
pub const RESET: &str = "\x1b[0m";
pub const ERROR: &str = "\x1b[31m"; // red
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
    pub labels: Vec<Label>,
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
        labels: Vec<Label>,
    ) {
        self.diagnostics.push(Diagnostic { span, severity, code, message, labels });
    }

    pub fn report(&self, src: &str, filename: &str) {
        for diag in &self.diagnostics {
            let color = diag.severity.color_code();

            // 1) Compute line & column
            let (line, col, line_start, line_end) = Self::line_col_from_span(src, diag.span.start);

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
            let src_line = &src[line_start..line_end];
            let gutter = format!("{line} | ");
            let plain_gutter = "  | ";

            // 5) Caret underline
            let mut caret_line = String::new();
            caret_line.push_str(plain_gutter);
            for _ in 0..(col - 1) {
                caret_line.push(' ');
            }
            let caret_count = (diag.span.end.saturating_sub(diag.span.start)).max(1);
            for _ in 0..caret_count {
                caret_line.push('^');
            }

            // 6) Additional labels
            let main_line = line;
            let (same_line_labels, cross_line_labels): (Vec<_>, Vec<_>) =
                diag.labels.iter().partition(|label| {
                    let (label_line, _, _, _) = Self::line_col_from_span(src, label.span.start);
                    label_line == main_line
                });

            // Render same-line labels
            let mut label_lines = Vec::new();
            for label in same_line_labels {
                let lbl_col = label.span.start.saturating_sub(line_start) + 1;
                let mut label_line = String::new();
                label_line.push_str(plain_gutter);
                for _ in 0..(lbl_col - 1) {
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

            // Render cross-line labels
            let mut cross_line_displays = Vec::new();
            for label in cross_line_labels {
                let (label_line, label_col, label_line_start, label_line_end) =
                    Self::line_col_from_span(src, label.span.start);

                let label_src_line = &src[label_line_start..label_line_end];
                let label_gutter = format!("{label_line} | ");
                let line_display = format!("{label_gutter}{RESET}{label_src_line}{BOLD}{color}");

                let mut label_underline = String::new();
                label_underline.push_str(plain_gutter);
                for _ in 0..(label_col - 1) {
                    label_underline.push(' ');
                }
                let dash_count = (label.span.end.saturating_sub(label.span.start)).max(1);
                for _ in 0..dash_count {
                    label_underline.push('-');
                }
                label_underline.push(' ');
                label_underline.push_str(label.message);

                cross_line_displays.push((line_display, label_underline));
            }

            // 7) Pretty print the diagnostic
            match diag.severity {
                Severity::Error => {
                    eprintln!("{header}");
                    eprintln!("{location}");
                    eprintln!("  {BOLD}|");

                    // Show cross-line labels first
                    for (line_display, label_underline) in &cross_line_displays {
                        eprintln!("{line_display}");
                        eprintln!("{label_underline}");
                        eprintln!("  {BOLD}|");
                    }

                    // Show main diagnostic line
                    eprintln!("{gutter}{RESET}{src_line}{BOLD}{color}");
                    eprintln!("{caret_line}");

                    // Show same-line labels
                    for l in &label_lines {
                        eprintln!("{l}");
                    }
                }
                _ => {
                    println!("{header}");
                    println!("{location}");
                    println!("  {BOLD}|");

                    // Show cross-line labels first
                    for (line_display, label_underline) in &cross_line_displays {
                        println!("{line_display}");
                        println!("{label_underline}");
                        println!("  {BOLD}|");
                    }

                    // Show main diagnostic line
                    println!("{gutter}{RESET}{src_line}{BOLD}{color}");
                    println!("{caret_line}");

                    // Show same-line labels
                    for l in &label_lines {
                        println!("{l}");
                    }
                }
            }
        }
    }

    #[inline(always)]
    fn line_col_from_span(src: &str, span_start: usize) -> (usize, usize, usize, usize) {
        let before = &src[..span_start];
        let line = before.chars().filter(|&c| c == '\n').count() + 1;
        let col = before.chars().rev().take_while(|&c| c != '\n').count() + 1;
        let line_start = before.rfind('\n').map_or(0, |i| i + 1);
        let line_end = src[line_start..].find('\n').map_or(src.len(), |i| line_start + i);
        (line, col, line_start, line_end)
    }
}

pub trait AsStr: 'static {
    fn as_str(&self) -> &'static str;
}
