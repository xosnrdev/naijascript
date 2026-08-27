//! A small, self-contained tokenizer for the `.nui` DSL.
//!
//! The UI DSL is a separate surface language from core `NaijaScript`, so it has
//! its own scanner rather than reusing the arena-backed core lexer. Alphabetic
//! words (keywords and Pidgin operator words alike) are emitted as [`Tok::Word`]
//! and interpreted by the parser; this keeps multi-word operators such as
//! `no small pass` trivial to handle.

use crate::diagnostics::Span;
use crate::ui::error::UiError;

/// A lexical token.
#[derive(Debug, Clone, PartialEq)]
pub enum Tok {
    Word(String),
    Number(f64),
    /// A numeric literal with a `px` suffix.
    Length(f64),
    /// The raw inner content of a string literal (escapes resolved except for
    /// interpolation braces, which the parser handles).
    Str(String),
    LParen,
    RParen,
    Comma,
    Colon,
    Dot,
    At,
    Slash,
    Eof,
}

/// A token with its source span.
#[derive(Debug, Clone, PartialEq)]
pub struct Spanned {
    pub tok: Tok,
    pub span: Span,
}

struct Scanner<'a> {
    src: &'a [u8],
    pos: usize,
}

/// Tokenizes `.nui` source into a flat token stream terminated by [`Tok::Eof`].
pub fn tokenize(source: &str) -> Result<Vec<Spanned>, UiError> {
    let mut scanner = Scanner { src: source.as_bytes(), pos: 0 };
    let mut out = Vec::new();
    loop {
        scanner.skip_trivia();
        let start = scanner.pos;
        let Some(&byte) = scanner.src.get(scanner.pos) else {
            out.push(Spanned { tok: Tok::Eof, span: Span::from(start..start) });
            return Ok(out);
        };
        let spanned = match byte {
            b'(' => scanner.single(Tok::LParen),
            b')' => scanner.single(Tok::RParen),
            b',' => scanner.single(Tok::Comma),
            b':' => scanner.single(Tok::Colon),
            b'.' => scanner.single(Tok::Dot),
            b'@' => scanner.single(Tok::At),
            b'/' => scanner.single(Tok::Slash),
            b'"' => scanner.string()?,
            b'0'..=b'9' => scanner.number(),
            b if is_ident_start(b) => scanner.word(),
            other => {
                return Err(UiError::compile(
                    "NUI0001",
                    format!("unexpected character {:?}", other as char),
                )
                .with_span(Span::from(start..start + 1)));
            }
        };
        out.push(spanned);
    }
}

impl Scanner<'_> {
    fn single(&mut self, tok: Tok) -> Spanned {
        let start = self.pos;
        self.pos += 1;
        Spanned { tok, span: Span::from(start..self.pos) }
    }

    fn skip_trivia(&mut self) {
        while let Some(&byte) = self.src.get(self.pos) {
            match byte {
                b' ' | b'\t' | b'\r' | b'\n' => self.pos += 1,
                b'#' => {
                    while let Some(&b) = self.src.get(self.pos) {
                        if b == b'\n' {
                            break;
                        }
                        self.pos += 1;
                    }
                }
                _ => break,
            }
        }
    }

    fn word(&mut self) -> Spanned {
        let start = self.pos;
        while let Some(&byte) = self.src.get(self.pos) {
            if is_ident_continue(byte) {
                self.pos += 1;
            } else {
                break;
            }
        }
        let text = String::from_utf8_lossy(&self.src[start..self.pos]).into_owned();
        Spanned { tok: Tok::Word(text), span: Span::from(start..self.pos) }
    }

    fn number(&mut self) -> Spanned {
        let start = self.pos;
        while let Some(&byte) = self.src.get(self.pos) {
            if byte.is_ascii_digit() || byte == b'.' {
                self.pos += 1;
            } else {
                break;
            }
        }
        let raw = String::from_utf8_lossy(&self.src[start..self.pos]).into_owned();
        let value: f64 = raw.parse().unwrap_or(0.0);
        // Optional `px` length suffix, only when not glued to more identifier text.
        if self.src.get(self.pos) == Some(&b'p') && self.src.get(self.pos + 1) == Some(&b'x') {
            let after = self.src.get(self.pos + 2).copied();
            if after.is_none_or(|b| !is_ident_continue(b)) {
                self.pos += 2;
                return Spanned { tok: Tok::Length(value), span: Span::from(start..self.pos) };
            }
        }
        Spanned { tok: Tok::Number(value), span: Span::from(start..self.pos) }
    }

    fn string(&mut self) -> Result<Spanned, UiError> {
        let start = self.pos;
        self.pos += 1; // opening quote
        let mut bytes: Vec<u8> = Vec::new();
        loop {
            let Some(&byte) = self.src.get(self.pos) else {
                return Err(UiError::compile("NUI0002", "unterminated string literal")
                    .with_span(Span::from(start..self.pos)));
            };
            match byte {
                b'"' => {
                    self.pos += 1;
                    let content = String::from_utf8_lossy(&bytes).into_owned();
                    return Ok(Spanned {
                        tok: Tok::Str(content),
                        span: Span::from(start..self.pos),
                    });
                }
                b'\\' => {
                    self.pos += 1;
                    let Some(&esc) = self.src.get(self.pos) else {
                        return Err(UiError::compile("NUI0002", "unterminated escape")
                            .with_span(Span::from(start..self.pos)));
                    };
                    match esc {
                        b'"' => bytes.push(b'"'),
                        b'\\' => bytes.push(b'\\'),
                        b'n' => bytes.push(b'\n'),
                        b't' => bytes.push(b'\t'),
                        b'{' => bytes.push(0x01), // sentinel: literal '{'
                        b'}' => bytes.push(0x02), // sentinel: literal '}'
                        other => bytes.push(other),
                    }
                    self.pos += 1;
                }
                // Preserve raw bytes so multibyte UTF-8 survives intact.
                _ => {
                    bytes.push(byte);
                    self.pos += 1;
                }
            }
        }
    }
}

fn is_ident_start(byte: u8) -> bool {
    byte.is_ascii_alphabetic() || byte == b'_'
}

fn is_ident_continue(byte: u8) -> bool {
    byte.is_ascii_alphanumeric() || byte == b'_'
}
