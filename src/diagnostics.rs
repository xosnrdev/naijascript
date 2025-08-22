//! The diagnostics system for NaijaScript.

use std::ops::Range;

use crate::arena::{Arena, ArenaCow, ArenaString};
use crate::arena_format;

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
    fn write_to_stream_or_buf(&self, s: &str, buf: Option<&mut ArenaString<'_>>) {
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
pub struct Label<'arena> {
    pub span: Span,
    pub message: ArenaCow<'arena, 'static>,
}

#[derive(Debug)]
pub struct Diagnostic<'arena> {
    pub span: Span,
    pub severity: Severity,
    pub code: &'static str,
    pub message: &'static str,
    pub labels: Vec<Label<'arena>>,
}

/// Collection of diagnostics with batch reporting capabilities.
#[derive(Debug)]
pub struct Diagnostics<'arena> {
    pub diagnostics: Vec<Diagnostic<'arena>>,
    arena: &'arena Arena,
}

impl<'arena> Diagnostics<'arena> {
    /// Creates a new [`Diagnostics`] instance.
    pub fn new(arena: &'arena Arena) -> Diagnostics<'arena> {
        Diagnostics { diagnostics: Vec::new(), arena }
    }

    /// Add a new diagnostic to the collection.
    #[inline]
    pub fn emit(
        &mut self,
        span: Span,
        severity: Severity,
        code: &'static str,
        message: &'static str,
        labels: Vec<Label<'arena>>,
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
    fn render_ansi(&self, src: &str, filename: &str) -> ArenaString<'arena> {
        let mut buf = ArenaString::new_in(self.arena);
        let gutter_width = Self::compute_gutter_width(&self.diagnostics, src);
        for diag in &self.diagnostics {
            self.render_diagnostic(diag, src, filename, gutter_width, Some(&mut buf));
        }
        buf.shrink_to_fit();
        buf
    }

    fn render_diagnostic(
        &self,
        diag: &Diagnostic,
        src: &str,
        filename: &str,
        gutter_width: usize,
        mut buf: Option<&mut ArenaString<'arena>>,
    ) {
        let color = diag.severity.color_code();
        let (line, col, line_start, line_end) = Self::line_col_from_span(src, diag.span.start);
        let header = self.render_header(diag.severity, diag.code, diag.message);
        let location = self.render_location(filename, line, col, color);
        let src_line = &src[line_start..line_end];
        let gutter = self.render_gutter(line, color, gutter_width);
        let plain_gutter = self.render_plain_gutter(color, gutter_width);
        let caret_count = src[diag.span.start..diag.span.end].chars().count().max(1);
        let caret_line = self.render_caret_line(col, caret_count, color, &plain_gutter);

        // Partition labels based on whether they're on the same line as the main diagnostic.
        // This affects how we render them - same-line labels appear as underlines below
        // the source line, while cross-line labels get their own source context.
        let (same_line_labels, cross_line_labels): (Vec<_>, Vec<_>) =
            diag.labels.iter().partition(|label| {
                let (label_line, _, _, _) = Self::line_col_from_span(src, label.span.start);
                label_line == line
            });

        // Render same-line labels as underlines with dashes
        let mut label_lines = Vec::with_capacity_in(same_line_labels.len(), &self.arena);
        for label in same_line_labels {
            // Convert absolute span to column position relative to line start
            let lbl_col = label.span.start.saturating_sub(line_start) + 1;
            let dash_count = src[label.span.start..label.span.end].chars().count().max(1);
            label_lines.push(self.render_label_line(
                lbl_col,
                dash_count,
                color,
                &label.message,
                &plain_gutter,
            ));
        }

        // Handle cross-line labels by showing their complete source context
        let mut cross_line_displays = Vec::with_capacity_in(cross_line_labels.len(), &self.arena);
        for label in cross_line_labels {
            let (label_line, label_col, label_line_start, label_line_end) =
                Self::line_col_from_span(src, label.span.start);
            let label_src_line = &src[label_line_start..label_line_end];
            let label_gutter = self.render_gutter(label_line, color, gutter_width);
            let line_display = format!("{label_gutter}{label_src_line}");
            let dash_count = src[label.span.start..label.span.end].chars().count().max(1);
            let label_underline =
                self.render_label_line(label_col, dash_count, color, &label.message, &plain_gutter);
            cross_line_displays.push((line_display, label_underline));
        }
        cross_line_displays.shrink_to_fit();

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
    fn render_header(&self, severity: Severity, code: &str, message: &str) -> ArenaString<'arena> {
        let color = severity.color_code();
        arena_format!(
            &self.arena,
            "{BOLD}{color}{}[{code}]{RESET}: {BOLD}{message}{RESET}",
            severity.label()
        )
    }

    #[inline]
    fn render_location(
        &self,
        filename: &str,
        line: usize,
        col: usize,
        color: &str,
    ) -> ArenaString<'arena> {
        arena_format!(&self.arena, " {BOLD}{color}-->{RESET} {filename}:{line}:{col}")
    }

    #[inline]
    fn render_gutter(&self, line: usize, color: &str, width: usize) -> ArenaString<'arena> {
        arena_format!(&self.arena, "{BOLD}{color}{line:>width$} |{RESET} ")
    }

    #[inline]
    fn render_plain_gutter(&self, color: &str, width: usize) -> ArenaString<'arena> {
        arena_format!(&self.arena, "{BOLD}{color}{:>width$} |{RESET} ", "")
    }

    // Build the caret `^^^` line that points to the error location.
    #[inline]
    fn render_caret_line(
        &self,
        col: usize,
        len: usize,
        color: &str,
        plain_gutter: &str,
    ) -> ArenaString<'arena> {
        let mut caret_line =
            ArenaString::with_capacity_in(plain_gutter.len() + col + 4 + len, self.arena);
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
        caret_line.shrink_to_fit();
        caret_line
    }

    // Build the label underline with dashes and explanatory text.
    #[inline]
    fn render_label_line(
        &self,
        lbl_col: usize,
        dash_count: usize,
        color: &str,
        message: &str,
        plain_gutter: &str,
    ) -> ArenaString<'arena> {
        let mut label_line = ArenaString::with_capacity_in(
            plain_gutter.len() + lbl_col + 4 + dash_count,
            self.arena,
        );
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
        label_line.shrink_to_fit();
        label_line
    }

    #[inline]
    fn line_col_from_span(src: &str, start: usize) -> (usize, usize, usize, usize) {
        assert!(start <= src.len(), "Start span out of bounds");

        let bytes = src.as_bytes();
        let mut line_start = 0;
        let mut line = 1;

        // Find last '\n' before start (for line_start)
        let mut i = 0;
        while i < start {
            if unsafe { *bytes.get_unchecked(i) } == b'\n' {
                line_start = i + 1;
                line += 1;
            }
            i += 1;
        }

        // Find next '\n' after line_start (for line_end)
        let mut line_end = src.len();
        i = line_start;
        while i < src.len() {
            if unsafe { *bytes.get_unchecked(i) } == b'\n' {
                line_end = i;
                break;
            }
            i += 1;
        }

        // Count UTF-8 chars from line_start to start (for col)
        let mut col = 1;
        let mut j = line_start;
        while j < start {
            let c = unsafe { *bytes.get_unchecked(j) };
            let step = if c < 0x80 {
                1
            } else if c < 0xE0 {
                2
            } else if c < 0xF0 {
                3
            } else {
                4
            };
            col += 1;
            j += step;
        }

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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_utf8_column_calculation() {
        let src = "áé你😆"; // 2-byte, 2-byte, 3-byte, 4-byte characters

        let (_, col, ..) = Diagnostics::line_col_from_span(src, 0);
        assert_eq!(col, 1); // First character 'á'

        let (_, col, ..) = Diagnostics::line_col_from_span(src, 2);
        assert_eq!(col, 2); // Second character 'é' at byte 2

        let (_, col, ..) = Diagnostics::line_col_from_span(src, 4);
        assert_eq!(col, 3); // Third character '你' at byte 4

        let (_, col, ..) = Diagnostics::line_col_from_span(src, 7);
        assert_eq!(col, 4); // Fourth character '😆' at byte 7
    }

    #[test]
    fn test_utf8_multiline_handling() {
        let src = "foó\nbár";

        let (line, col, ..) = Diagnostics::line_col_from_span(src, 2);
        assert_eq!(line, 1);
        assert_eq!(col, 3); // 'ó' in "foó" at character position 3

        let (line, col, ..) = Diagnostics::line_col_from_span(src, 6);
        assert_eq!(line, 2);
        assert_eq!(col, 2); // 'á' in "bár" at character position 2
    }
}
