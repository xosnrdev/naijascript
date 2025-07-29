//! The diagnostics system for NaijaScript.

use std::borrow::Cow;
use std::ops::Range;

/// Byte-range span within source text.
pub type Span = Range<usize>;

const BOLD: &str = "\x1b[1m";
const RESET: &str = "\x1b[0m";

// Maximum length for carets and dashes in diagnostics output.
const MAX_CD_LEN: usize = 8;

/// Diagnostic severity levels that determine display style and output routing.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Severity {
    Error,
    Warning,
    Note,
}

impl Severity {
    #[inline]
    const fn label(self) -> &'static str {
        match self {
            Severity::Error => "error",
            Severity::Warning => "warning",
            Severity::Note => "note",
        }
    }
    #[inline]
    const fn color_code(self) -> &'static str {
        match self {
            Severity::Error => "\x1b[31m",   // red
            Severity::Warning => "\x1b[33m", // yellow
            Severity::Note => "\x1b[34m",    // blue
        }
    }

    #[inline]
    // Writes a diagnostic line to the appropriate output stream or buffer.
    fn write_to_stream_or_buf(&self, s: &str, buf: Option<&mut String>) {
        if let Some(buf) = buf {
            buf.push_str(s);
            buf.push('\n');
        } else {
            match self {
                Severity::Error => eprintln!("{s}"),
                _ => println!("{s}"),
            }
        }
    }
}

/// Secondary annotation attached to a diagnostic.
#[derive(Debug)]
pub struct Label {
    pub span: Span,
    pub message: Cow<'static, str>,
}

#[derive(Debug)]
pub struct Diagnostic {
    pub span: Span,
    pub severity: Severity,
    pub code: &'static str,
    pub message: &'static str,
    pub labels: Vec<Label>,
}

/// Collection of diagnostics with batch reporting capabilities.
#[derive(Debug, Default)]
pub struct Diagnostics {
    pub diagnostics: Vec<Diagnostic>,
}

impl Diagnostics {
    /// Add a new diagnostic to the collection.
    #[inline]
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

    /// Report all collected diagnostics to the terminal with rich formatting.
    pub fn report(&self, src: &str, filename: &str) {
        let ansi = self.render_ansi(src, filename);
        print!("{ansi}");
    }

    /// Report all collected diagnostics as HTML for the web.
    #[cfg(target_arch = "wasm32")]
    pub fn report_html(&self, src: &str, filename: &str) -> String {
        let ansi = self.render_ansi(src, filename);
        ansi_to_html::convert(&ansi).unwrap()
    }

    /// Check if there are any error-level diagnostics
    #[inline]
    pub fn has_errors(&self) -> bool {
        self.diagnostics.iter().any(|d| d.severity == Severity::Error)
    }

    // Render all diagnostics as a single ANSI string
    fn render_ansi(&self, src: &str, filename: &str) -> String {
        let mut buf = String::new();
        let gutter_width = Self::compute_gutter_width(&self.diagnostics, src);
        for diag in &self.diagnostics {
            Self::render_diagnostic(diag, src, filename, gutter_width, Some(&mut buf));
        }
        buf
    }

    fn render_diagnostic(
        diag: &Diagnostic,
        src: &str,
        filename: &str,
        gutter_width: usize,
        mut buf: Option<&mut String>,
    ) {
        let color = diag.severity.color_code();
        let (line, col, line_start, line_end) = Self::line_col_from_span(src, diag.span.start);
        let header = Self::render_header(diag.severity, diag.code, diag.message);
        let location = Self::render_location(filename, line, col, color);
        let src_line = &src[line_start..line_end];
        let gutter = Self::render_gutter(line, color, gutter_width);
        let plain_gutter = Self::render_plain_gutter(color, gutter_width);
        let caret_count = (diag.span.end.saturating_sub(diag.span.start)).max(1);
        let caret_line = Self::render_caret_line(col, caret_count, color, &plain_gutter);

        // Partition labels based on whether they're on the same line as the main diagnostic.
        // This affects how we render them - same-line labels appear as underlines below
        // the source line, while cross-line labels get their own source context.
        let (same_line_labels, cross_line_labels): (Vec<_>, Vec<_>) =
            diag.labels.iter().partition(|label| {
                let (label_line, _, _, _) = Self::line_col_from_span(src, label.span.start);
                label_line == line
            });

        // Render same-line labels as underlines with dashes
        let mut label_lines = Vec::new();
        for label in same_line_labels {
            // Convert absolute span to column position relative to line start
            let lbl_col = label.span.start.saturating_sub(line_start) + 1;
            let dash_count = (label.span.end.saturating_sub(label.span.start)).max(1);
            label_lines.push(Self::render_label_line(
                lbl_col,
                dash_count,
                color,
                &label.message,
                &plain_gutter,
            ));
        }

        // Handle cross-line labels by showing their complete source context
        let mut cross_line_displays = Vec::new();
        for label in cross_line_labels {
            let (label_line, label_col, label_line_start, label_line_end) =
                Self::line_col_from_span(src, label.span.start);
            let label_src_line = &src[label_line_start..label_line_end];
            let label_gutter = Self::render_gutter(label_line, color, gutter_width);
            let line_display = format!("{label_gutter}{label_src_line}");
            let dash_count = (label.span.end.saturating_sub(label.span.start)).max(1);
            let label_underline = Self::render_label_line(
                label_col,
                dash_count,
                color,
                &label.message,
                &plain_gutter,
            );
            cross_line_displays.push((line_display, label_underline));
        }

        // Output diagnostic components in specific order to match rustc/clang format:
        // This ordering helps developers pattern match against familiar error displays
        // and enables tooling to parse our output using existing parsers
        //
        //   error[E001]: Assignment syntax no correct
        //    --> file.ns:5:10
        //     |
        //   3 | make x get y
        //     |        --- label points here
        //     |
        //   5 | make get 42
        //     |      ^^^ main error caret
        //     |      expected identifier after make
        //
        diag.severity.write_to_stream_or_buf(&header, buf.as_deref_mut());
        diag.severity.write_to_stream_or_buf(&location, buf.as_deref_mut());
        diag.severity.write_to_stream_or_buf(&plain_gutter, buf.as_deref_mut());
        for (line_display, label_underline) in &cross_line_displays {
            diag.severity.write_to_stream_or_buf(line_display, buf.as_deref_mut());
            diag.severity.write_to_stream_or_buf(label_underline, buf.as_deref_mut());
            diag.severity.write_to_stream_or_buf(&plain_gutter, buf.as_deref_mut());
        }
        diag.severity.write_to_stream_or_buf(&format!("{gutter}{src_line}"), buf.as_deref_mut());
        diag.severity.write_to_stream_or_buf(&caret_line, buf.as_deref_mut());
        for l in &label_lines {
            diag.severity.write_to_stream_or_buf(l, buf.as_deref_mut());
        }
    }

    #[inline]
    fn render_header(severity: Severity, code: &str, message: &str) -> String {
        let color = severity.color_code();
        format!("{BOLD}{color}{}[{code}]{RESET}: {BOLD}{message}{RESET}", severity.label())
    }

    #[inline]
    fn render_location(filename: &str, line: usize, col: usize, color: &str) -> String {
        format!(" {BOLD}{color}-->{RESET} {filename}:{line}:{col}")
    }

    #[inline]
    fn render_gutter(line: usize, color: &str, width: usize) -> String {
        format!("{BOLD}{color}{line:>width$} |{RESET} ")
    }

    #[inline]
    fn render_plain_gutter(color: &str, width: usize) -> String {
        format!("{BOLD}{color}{:>width$} |{RESET} ", "")
    }

    // Build the caret `^^^` line that points to the error location.
    #[inline]
    fn render_caret_line(col: usize, len: usize, color: &str, plain_gutter: &str) -> String {
        let mut caret_line = String::new();
        caret_line.push_str(plain_gutter);
        for _ in 0..(col - 1) {
            caret_line.push(' ');
        }
        caret_line.push_str(BOLD);
        caret_line.push_str(color);
        for _ in 0..len.min(MAX_CD_LEN) {
            caret_line.push('^');
        }

        if len > MAX_CD_LEN {
            caret_line.push_str("...");
        }
        caret_line.push_str(RESET);
        caret_line
    }

    // Build the label underline with dashes and explanatory text.
    #[inline]
    fn render_label_line(
        lbl_col: usize,
        dash_count: usize,
        color: &str,
        message: &str,
        plain_gutter: &str,
    ) -> String {
        let mut label_line = String::new();
        label_line.push_str(plain_gutter);
        for _ in 0..(lbl_col - 1) {
            label_line.push(' ');
        }
        label_line.push_str(BOLD);
        label_line.push_str(color);
        for _ in 0..dash_count.min(MAX_CD_LEN) {
            label_line.push('-');
        }

        if dash_count > MAX_CD_LEN {
            label_line.push_str("...");
        }
        label_line.push_str(RESET);
        label_line.push(' ');
        label_line.push_str(BOLD);
        label_line.push_str(message);
        label_line.push_str(RESET);
        label_line
    }

    #[inline]
    fn line_col_from_span(src: &str, span_start: usize) -> (usize, usize, usize, usize) {
        let before = &src[..span_start];
        let line_start = before.rfind('\n').map_or(0, |i| i + 1);
        let line_end = src[line_start..].find('\n').map_or(src.len(), |i| line_start + i);
        let line = before.bytes().filter(|&b| b == b'\n').count() + 1;
        let col = span_start - line_start + 1;
        (line, col, line_start, line_end)
    }

    // We calculate the widest line number so the gutter always lines up,
    // no matter how many digits the line numbers have.
    #[inline]
    fn compute_gutter_width(diagnostics: &[Diagnostic], src: &str) -> usize {
        let mut max_line = 1;
        for diag in diagnostics {
            let (line, _, _, _) = Self::line_col_from_span(src, diag.span.start);
            max_line = max_line.max(line);
            for label in &diag.labels {
                let (label_line, _, _, _) = Self::line_col_from_span(src, label.span.start);
                max_line = max_line.max(label_line);
            }
        }
        max_line.to_string().len()
    }
}

/// Trait for error types that can be converted to display strings.
///
/// This provides a consistent interface for all our error enums across the
/// interpreter.
pub trait AsStr: 'static {
    fn as_str(&self) -> &'static str;
}
