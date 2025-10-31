//! The diagnostics system for NaijaScript.

use std::range::Range;

use crate::arena::{Arena, ArenaCow, ArenaString};
use crate::arena_format;
use crate::simd::memchr2;

/// Byte-range span within source text.
pub type Span = Range<usize>;

const BOLD: &str = "\x1b[1m";
const RESET: &str = "\x1b[0m";

/// Diagnostic severity levels that determine display style and output routing.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Severity {
    Error,
    Warning,
    Note,
}

impl Severity {
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
    pub message: ArenaCow<'arena, 'static>,
    pub span: Span,
}

#[derive(Debug)]
pub struct Diagnostic<'arena> {
    pub labels: Vec<Label<'arena>>,
    pub span: Span,
    pub code: &'static str,
    pub message: &'static str,
    pub severity: Severity,
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
    #[cfg(target_family = "wasm")]
    pub fn report_html(&self, src: &str, filename: &str) -> String {
        let ansi = self.render_ansi(src, filename);
        ansi_to_html::convert(&ansi).unwrap()
    }

    /// Check if there are any error-level diagnostics
    pub fn has_errors(&self) -> bool {
        self.diagnostics.iter().any(|d| d.severity == Severity::Error)
    }

    // Render all diagnostics as a single ANSI string
    fn render_ansi(&self, src: &str, filename: &str) -> ArenaString<'arena> {
        let mut buf = ArenaString::new_in(self.arena);
        let gutter_width = self.compute_gutter_width(&self.diagnostics, src);
        for diag in &self.diagnostics {
            self.render_diagnostic(diag, src, filename, gutter_width, Some(&mut buf));
        }
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
        let (line, col, line_start, line_end) = self.line_col_from_span(src, diag.span.start);
        let header = self.render_header(diag.severity, diag.code, diag.message);
        let location = self.render_location(filename, line, col, color);
        let src_line = self.expand_tabs(&src[line_start..line_end]);
        let gutter = self.render_gutter(line, color, gutter_width);
        let plain_gutter = self.render_plain_gutter(color, gutter_width);
        let caret_count = src[diag.span.start..diag.span.end.min(line_end)].chars().count().max(1);
        let caret_line = self.render_caret_line(col, caret_count, color, &plain_gutter);

        // Separate same-line labels from cross-line labels
        let (same_line_labels, cross_line_labels): (Vec<_>, Vec<_>) =
            diag.labels.iter().partition(|label| {
                let (label_line, ..) = self.line_col_from_span(src, label.span.start);
                label_line == line
            });

        // Render same-line labels as underlines with dashes
        let mut label_lines = Vec::with_capacity_in(same_line_labels.len(), self.arena);
        for label in same_line_labels {
            // Convert absolute span to column position relative to line start
            let lbl_col = &src[line_start..label.span.start];
            let lbl_col = Self::visual_col(lbl_col) + 1;
            let dash_count = &src[label.span.start..label.span.end.min(line_end)];
            let dash_count = Self::visual_col(dash_count).max(1);
            label_lines.push(self.render_label_line(
                lbl_col,
                dash_count,
                color,
                &label.message,
                &plain_gutter,
            ));
        }

        // Handle cross-line labels by showing their complete source context
        let mut cross_line_displays = Vec::with_capacity_in(cross_line_labels.len(), self.arena);
        for label in cross_line_labels {
            let (label_line, label_col, label_line_start, label_line_end) =
                self.line_col_from_span(src, label.span.start);
            let label_src_line = self.expand_tabs(&src[label_line_start..label_line_end]);
            let label_gutter = self.render_gutter(label_line, color, gutter_width);
            let line_display = format!("{label_gutter}{label_src_line}");
            let dash_count = &src[label.span.start..label.span.end.min(label_line_end)];
            let dash_count = Self::visual_col(dash_count).max(1);
            let label_underline =
                self.render_label_line(label_col, dash_count, color, &label.message, &plain_gutter);
            cross_line_displays.push((line_display, label_underline));
        }

        // Example output:
        //
        //   error[code]: Assignment syntax no correct
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

    fn render_header(&self, severity: Severity, code: &str, message: &str) -> ArenaString<'arena> {
        let color = severity.color_code();
        arena_format!(
            self.arena,
            "{BOLD}{color}{}[{code}]{RESET}: {BOLD}{message}{RESET}",
            severity.label()
        )
    }

    fn render_location(
        &self,
        filename: &str,
        line: usize,
        col: usize,
        color: &str,
    ) -> ArenaString<'arena> {
        arena_format!(self.arena, " {BOLD}{color}-->{RESET} {filename}:{line}:{col}")
    }

    #[inline]
    fn render_gutter(&self, line: usize, color: &str, width: usize) -> ArenaString<'arena> {
        arena_format!(self.arena, "{BOLD}{color}{line:>width$} |{RESET} ")
    }

    fn render_plain_gutter(&self, color: &str, width: usize) -> ArenaString<'arena> {
        arena_format!(self.arena, "{BOLD}{color}{:>width$} |{RESET} ", "")
    }

    // Build the caret `^^^` line that points to the error location.
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
        for _ in 0..len {
            caret_line.push('^');
        }
        caret_line.push_str(RESET);
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

    #[inline]
    fn line_col_from_span(&self, src: &str, start: usize) -> (usize, usize, usize, usize) {
        let line_starts = self.compute_line_starts(src);
        let line_idx = line_starts.binary_search(&start).unwrap_or_else(|x| x - 1);
        let line_start = line_starts[line_idx];
        let line_end = if line_idx + 1 < line_starts.len() {
            line_starts[line_idx + 1] - 1
        } else {
            src.len()
        };
        let col = Self::visual_col(&src[line_start..start]) + 1;
        (line_idx + 1, col, line_start, line_end)
    }

    fn compute_line_starts(&self, src: &str) -> Vec<usize, &'arena Arena> {
        let haystack = src.as_bytes();
        let len = haystack.len();
        let mut starts = Vec::with_capacity_in(len, self.arena);
        starts.push(0);

        let mut offset = 0;
        while offset < len {
            // find either '\n' or ('\r' for windows)
            let idx = memchr2(b'\r', b'\n', haystack, offset);
            if idx == len {
                break;
            }

            // handle '\r' (Windows case)
            if haystack[idx] == b'\r' {
                if idx + 1 < len && haystack[idx + 1] == b'\n' {
                    starts.push(idx + 2);
                    offset = idx + 2;
                    continue;
                } else {
                    // lone '\r' as a newline
                    starts.push(idx + 1);
                    offset = idx + 1;
                    continue;
                }
            }

            // plain '\n' (Unix case)
            starts.push(idx + 1);
            offset = idx + 1
        }

        starts
    }

    // We calculate the widest line number so the gutter always lines up,
    // no matter how many digits the line numbers have.
    fn compute_gutter_width(&self, diagnostics: &[Diagnostic], src: &str) -> usize {
        let mut max_line = 1;
        for diag in diagnostics {
            let (line, _, _, _) = self.line_col_from_span(src, diag.span.start);
            max_line = max_line.max(line);
            for label in &diag.labels {
                let (label_line, _, _, _) = self.line_col_from_span(src, label.span.start);
                max_line = max_line.max(label_line);
            }
        }
        max_line.to_string().len()
    }

    const TAB_WIDTH: usize = 4;

    fn expand_tabs(&self, text: &str) -> ArenaString<'arena> {
        let mut result = ArenaString::with_capacity_in(text.len() * 2, self.arena);
        let mut col = 0;
        for ch in text.chars() {
            if ch == '\t' {
                let spaces = Self::TAB_WIDTH - (col % Self::TAB_WIDTH);
                for _ in 0..spaces {
                    result.push(' ');
                }
                col += spaces;
            } else {
                result.push(ch);
                col += 1;
            }
        }
        result
    }

    fn visual_col(text: &str) -> usize {
        text.chars().fold(0, |col, ch| {
            if ch == '\t' { col + (Self::TAB_WIDTH - (col % Self::TAB_WIDTH)) } else { col + 1 }
        })
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
    use crate::KIBI;

    #[test]
    fn test_utf8_column_calculation() {
        let src = "áé你😆"; // 2-byte, 2-byte, 3-byte, 4-byte characters
        let arena = Arena::new(KIBI).unwrap();
        let diagnostics = Diagnostics::new(&arena);

        let (_, col, ..) = diagnostics.line_col_from_span(src, 0);
        assert_eq!(col, 1); // First character 'á'

        let (_, col, ..) = diagnostics.line_col_from_span(src, 2);
        assert_eq!(col, 2); // Second character 'é' at byte 2

        let (_, col, ..) = diagnostics.line_col_from_span(src, 4);
        assert_eq!(col, 3); // Third character '你' at byte 4

        let (_, col, ..) = diagnostics.line_col_from_span(src, 7);
        assert_eq!(col, 4); // Fourth character '😆' at byte 7
    }

    #[test]
    fn test_utf8_multiline_handling() {
        let src = "foó\nbár";
        let arena = Arena::new(KIBI).unwrap();
        let diagnostics = Diagnostics::new(&arena);

        let (line, col, ..) = diagnostics.line_col_from_span(src, 2);
        assert_eq!(line, 1);
        assert_eq!(col, 3); // 'ó' in "foó" at character position 3

        let (line, col, ..) = diagnostics.line_col_from_span(src, 6);
        assert_eq!(line, 2);
        assert_eq!(col, 2); // 'á' in "bár" at character position 2
    }

    #[test]
    fn test_tab_expansion() {
        let src = "a\tb\tc";
        let arena = Arena::new(KIBI).unwrap();
        let diagnostics = Diagnostics::new(&arena);

        let expanded = diagnostics.expand_tabs(src);
        assert_eq!(expanded, "a   b   c");

        let (_, col, ..) = diagnostics.line_col_from_span(src, 2); // Position after 'a' and tab
        assert_eq!(col, 5); // 'b' should be at column 5 after tab expansion
    }

    #[test]
    fn test_visual_column_calculation() {
        let text = "a\tb\tc";
        let col = Diagnostics::visual_col(text);
        assert_eq!(col, 9); // 'c' should be at column 9 after tab expansion
    }
}
