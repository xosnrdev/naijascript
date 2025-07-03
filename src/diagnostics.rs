//! The diagnostics system for NaijaScript.

use std::borrow::Cow;
use std::ops::Range;

/// Byte-range span within source text.
pub type Span = Range<usize>;

const BOLD: &str = "\x1b[1m";
const RESET: &str = "\x1b[0m";
const ERROR: &str = "\x1b[31m"; // red
const WARNING: &str = "\x1b[33m"; // yellow
const NOTE: &str = "\x1b[34m"; // blue

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
            Severity::Error => ERROR,
            Severity::Warning => WARNING,
            Severity::Note => NOTE,
        }
    }

    #[inline]
    /// Writes a diagnostic line to the appropriate output stream.
    fn write_to_stream(&self, s: &str) {
        match self {
            Severity::Error => eprintln!("{s}"),
            _ => println!("{s}"),
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
        let gutter_width = Self::compute_gutter_width(&self.diagnostics, src);

        for diag in &self.diagnostics {
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

            // Output the complete diagnostic in the expected order:
            // 1. Header with severity and message
            // 2. File location pointer
            // 3. Cross-line labels with their source context
            // 4. Main diagnostic line with source
            // 5. Caret line pointing to the error
            // 6. Same-line labels with explanations
            diag.severity.write_to_stream(&header);
            diag.severity.write_to_stream(&location);
            diag.severity.write_to_stream(&plain_gutter);
            for (line_display, label_underline) in &cross_line_displays {
                diag.severity.write_to_stream(line_display);
                diag.severity.write_to_stream(label_underline);
                diag.severity.write_to_stream(&plain_gutter);
            }
            diag.severity.write_to_stream(&format!("{gutter}{src_line}"));
            diag.severity.write_to_stream(&caret_line);
            for l in &label_lines {
                diag.severity.write_to_stream(l);
            }
        }
    }

    /// Report all collected diagnostics as HTML for the web.
    #[cfg(target_arch = "wasm32")]
    pub fn report_html(&self, src: &str, filename: &str) -> String {
        let gutter_width = Self::compute_gutter_width(&self.diagnostics, src);
        let mut html = String::new();
        for diag in &self.diagnostics {
            let (line, col, line_start, line_end) = Self::line_col_from_span(src, diag.span.start);
            let src_line = &src[line_start..line_end];
            let caret_count = (diag.span.end.saturating_sub(diag.span.start)).max(1);
            html.push_str(&format!(
                "<div class='diagnostic {severity}'>\
                  <div class='header'><span class='severity {severity}'>{severity}[{code}]</span>: <b>{msg}</b></div>\
                  <div class='location'>--&gt; {filename}:{line}:{col}</div>\
                  <div class='src'><span class='gutter'>{line:>gutter_width$} |</span> <span class='src-line'>{src_line}</span></div>\
                  <div class='caret'><span class='gutter'>{gutter}</span>{caret}</div>",
                severity=diag.severity.label(),
                code=diag.code,
                msg=diag.message,
                filename=html_escape::encode_safe(filename),
                src_line=html_escape::encode_safe(src_line),
                gutter=" ",
                caret="^".repeat(caret_count)
            ));
            for label in &diag.labels {
                let (lbl_line, lbl_col, lbl_line_start, lbl_line_end) =
                    Self::line_col_from_span(src, label.span.start);
                let src_line = &src[lbl_line_start..lbl_line_end];
                let dash_count = (label.span.end.saturating_sub(label.span.start)).max(1);
                html.push_str(&format!(
                    "<div class='label'><span class='gutter'>{lbl_line:>gutter_width$} |</span> <span class='src-line'>{src_line}</span></div>\
                     <div class='label-underline'><span class='gutter'>{gutter}</span>{dashes} <b>{msg}</b></div>",
                    src_line=html_escape::encode_safe(src_line),
                    gutter=" ",
                    dashes="-".repeat(dash_count),
                    msg=label.message
                ));
            }
            html.push_str("</div>");
        }
        html
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
        format!("{BOLD}{color}{:>width$} |{RESET} ", "", width = width)
    }

    /// Build the caret line that points to the error location.
    ///
    /// This creates the "^^^" line that appears under the source code.
    /// The tricky part is getting the column alignment right - we need to account
    /// for the gutter width and ensure the carets line up precisely with the
    /// problematic source text.
    #[inline]
    fn render_caret_line(col: usize, len: usize, color: &str, plain_gutter: &str) -> String {
        let mut caret_line = String::with_capacity(plain_gutter.len() + col + len + 8);
        caret_line.push_str(plain_gutter);
        for _ in 0..(col - 1) {
            caret_line.push(' ');
        }
        caret_line.push_str(BOLD);
        caret_line.push_str(color);
        for _ in 0..len {
            caret_line.push('^');
        }
        caret_line.push_str(RESET);
        caret_line
    }

    /// Build a label underline with dashes and explanatory text.
    ///
    /// Labels use dashes instead of carets to distinguish them from the main
    /// diagnostic. The horizontal alignment needs to match the source text
    /// exactly, which requires careful column calculation.
    #[inline]
    fn render_label_line(
        lbl_col: usize,
        dash_count: usize,
        color: &str,
        message: &str,
        plain_gutter: &str,
    ) -> String {
        let mut label_line =
            String::with_capacity(plain_gutter.len() + lbl_col + dash_count + message.len() + 16);
        label_line.push_str(plain_gutter);
        for _ in 0..(lbl_col - 1) {
            label_line.push(' ');
        }
        label_line.push_str(BOLD);
        label_line.push_str(color);
        for _ in 0..dash_count {
            label_line.push('-');
        }
        label_line.push_str(RESET);
        label_line.push(' ');
        label_line.push_str(BOLD);
        label_line.push_str(message);
        label_line.push_str(RESET);
        label_line
    }

    /// Convert a byte offset to line/column coordinates and line boundaries.
    ///
    /// This is the foundation of our source mapping system. We need four pieces
    /// of information: the line number (1-based), column number (1-based),
    /// and the byte offsets for the start and end of that line. The line
    /// boundaries are crucial for extracting the source text to display.
    ///
    /// The algorithm works by scanning backwards from the target position to
    /// count newlines (for line number) and find the line start. Then we scan
    /// forward to find the line end. It's not the most efficient possible
    /// approach, but it's simple and works well for typical error reporting.
    #[inline]
    fn line_col_from_span(src: &str, span_start: usize) -> (usize, usize, usize, usize) {
        let before = &src[..span_start];
        let line_start = before.rfind('\n').map_or(0, |i| i + 1);
        let line_end = src[line_start..].find('\n').map_or(src.len(), |i| line_start + i);
        let line = before.bytes().filter(|&b| b == b'\n').count() + 1;
        let col = span_start - line_start + 1;
        (line, col, line_start, line_end)
    }

    /// Computes the gutter width needed for all diagnostics and labels.
    ///
    /// This ensures that the vertical bar and code are always aligned, even for large scripts.
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
