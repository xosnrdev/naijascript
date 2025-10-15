//! The lexer (or scanner) for NaijaScript.

use std::range::Range;

use crate::arena::{Arena, ArenaCow, ArenaString};
use crate::arena_format;
use crate::diagnostics::{AsStr, Diagnostics, Label, Severity};
use crate::simd::memchr2;
use crate::syntax::token::{SpannedToken, Token};

/// Represents errors that can occur during lexical analysis.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LexError {
    UnexpectedChar,
    InvalidNumber,
    InvalidIdentifier,
    InvalidStringEscape,
    UnterminatedString,
}

impl AsStr for LexError {
    fn as_str(&self) -> &'static str {
        match self {
            LexError::UnexpectedChar => "Unexpected character",
            LexError::InvalidNumber => "Invalid number",
            LexError::InvalidIdentifier => "Invalid identifier",
            LexError::InvalidStringEscape => "Invalid string escape",
            LexError::UnterminatedString => "Unterminated string",
        }
    }
}

/// The interface for the NaijaScript lexer.
pub struct Lexer<'arena, 'input> {
    /// Collection of errors encountered during scanning
    pub errors: Diagnostics<'arena>,
    // The original source text as bytes
    src: &'input [u8],
    arena: &'arena Arena,
    // Current position in the source text
    pos: usize,
    // Total length of the source text
    len: usize,
}

impl<'arena, 'input> Lexer<'arena, 'input> {
    /// Creates a new [`Lexer`] for the given source text.
    pub fn new(src: &'input str, arena: &'arena Arena) -> Self {
        let src = src.as_bytes();
        Self { errors: Diagnostics::new(arena), src, arena, pos: 0, len: src.len() }
    }

    fn next_token(&mut self) -> SpannedToken<'arena, 'input> {
        loop {
            // Skip over any whitespace before the next token
            self.skip_whitespace();

            // Record starting position for spans and slicing
            let start = self.pos;

            // Check for EOF first
            if self.pos >= self.len {
                return SpannedToken { token: Token::EOF, span: Range::from(start..start) };
            }

            // SAFETY: self.pos < self.len, so this is safe
            let b = unsafe { *self.src.get_unchecked(self.pos) };

            // If we see a '#' character, that means the start of a comment.
            if b == b'#' {
                self.skip_comment();
                continue;
            }

            // String literals
            if b == b'"' || b == b'\'' {
                let token = self.scan_string(start, b);
                return SpannedToken { token, span: Range::from(start..self.pos) };
            }
            if let Some(token) = self.scan_punctuation(b) {
                return SpannedToken { token, span: Range::from(start..self.pos) };
            }
            if b.is_ascii_digit() {
                let token = self.scan_number(start);
                return SpannedToken { token, span: Range::from(start..self.pos) };
            }
            if Self::is_alpha_or_underscore(b) {
                let token = self.scan_identifier_or_keyword(start);
                return SpannedToken { token, span: Range::from(start..self.pos) };
            }
            // Non-ASCII character ?
            if !b.is_ascii() {
                // SAFETY: start..self.len is valid UTF-8 because the original input is a &str
                let rest =
                    unsafe { str::from_utf8_unchecked(self.src.get_unchecked(start..self.len)) };
                if let Some(c) = rest.chars().next() {
                    let len = c.len_utf8();
                    self.emit_error(
                        Range::from(start..self.pos + len),
                        LexError::UnexpectedChar,
                        vec![Label {
                            span: Range::from(start..self.pos + len),
                            message: ArenaCow::Borrowed("I no sabi dis character"),
                        }],
                    );
                    // Avoid splitting across byte boundaries
                    self.pos += len;
                    continue;
                } else {
                    // Not a valid UTF-8 sequence ?
                    self.pos += 1;
                    continue;
                }
            }
            // Likely an unexpected character ?
            self.emit_error(
                Range::from(start..self.pos),
                LexError::UnexpectedChar,
                vec![Label {
                    span: Range::from(start..self.pos),
                    message: ArenaCow::Borrowed("I no sabi dis character"),
                }],
            );
            self.pos += 1;
        }
    }

    fn emit_error(&mut self, span: Range<usize>, error: LexError, label: Vec<Label<'arena>>) {
        self.errors.emit(span, Severity::Error, "lexical", error.as_str(), label);
    }

    // Skips all whitespace characters
    fn skip_whitespace(&mut self) {
        while self.pos < self.len
        // SAFETY: self.pos < self.len, so this is safe
            && unsafe { *self.src.get_unchecked(self.pos) }.is_ascii_whitespace()
        {
            self.pos += 1;
        }
    }

    fn skip_comment(&mut self) {
        let len = self.len;
        // SAFETY: self.pos < len, so this is safe
        let haystack = unsafe { self.src.get_unchecked(self.pos..len) };
        let index = memchr2(b'\n', b'\r', haystack, 0);

        // Advance position to the end of the comment
        self.pos += index;

        // Is it a newline or carriage return?
        if self.pos < len {
            // SAFETY: self.pos < len, so this is safe
            let ch = unsafe { *self.src.get_unchecked(self.pos) };
            if ch == b'\n' || ch == b'\r' {
                self.pos += 1;
            }
        }
    }

    // Checks if a byte is a letter (A-Z, a-z) or underscore (_)
    //
    // Used for identifier and keyword detection
    #[inline]
    const fn is_alpha_or_underscore(b: u8) -> bool {
        b.is_ascii_alphabetic() || b == b'_'
    }

    // Reads an entire word (identifier or keyword)
    fn read_word(&mut self) -> &'input str {
        let beg = self.pos;

        while self.pos < self.len {
            // SAFETY: self.pos < self.len, so this is safe
            let ch = unsafe { *self.src.get_unchecked(self.pos) };
            if !Self::is_alpha_or_underscore(ch) && !ch.is_ascii_digit() {
                break;
            }
            self.pos += 1;
        }

        // SAFETY: beg..self.pos is valid UTF-8 because we only process valid identifier characters
        unsafe { str::from_utf8_unchecked(self.src.get_unchecked(beg..self.pos)) }
    }

    // Try to consume a specific word at current position
    //
    // This is crucial for multi-word keywords like "if to say" and "small pass".
    // The function checks if the next word matches the expected one,
    // and only consumes it if:
    // 1. It's followed by a non-alphabetic character (prevents partial matches)
    // 2. We have enough input left
    #[inline]
    fn try_consume_word(&mut self, word: &str) -> bool {
        let len = self.len;
        let mut beg = self.pos;

        while beg < len &&
        // SAFETY: beg < len, so this is safe
        unsafe {
            self.src.get_unchecked(beg).is_ascii_whitespace()
        } {
            beg += 1;
        }

        let end = beg + word.len();
        if end <= len
        // SAFETY: beg..end is valid because end <= len
            && unsafe { self.src.get_unchecked(beg..end) == word.as_bytes() }
            && (end == len
                || !Self::is_alpha_or_underscore(
                    // SAFETY: end < len, so this is safe
                    unsafe { *self.src.get_unchecked(end) }))
        {
            self.pos = end;
            return true;
        }
        false
    }

    fn scan_string(&mut self, start: usize, quote: u8) -> Token<'arena, 'input> {
        self.pos += 1; // Skip the opening quote
        let beg = self.pos;

        let mut has_escape = false;
        let mut buffer = ArenaString::new_in(self.arena);

        loop {
            // SAFETY: self.pos..self.len is valid because self.pos < self.len
            let bytes = unsafe { self.src.get_unchecked(self.pos..) };

            // Find the next quote/escape OR newline sequence
            let quote_or_escape = memchr2(quote, b'\\', bytes, 0);
            let newline = memchr2(b'\n', b'\r', bytes, 0);

            // Is the first occurrence a newline ?
            if newline < quote_or_escape {
                let line_end = self.pos + newline;
                self.emit_error(
                    Range::from(start..line_end),
                    LexError::UnterminatedString,
                    vec![Label {
                        span: Range::from(start..line_end),
                        message: ArenaCow::Owned(arena_format!(
                            self.arena,
                            "Dis string no get ending quote `{}`",
                            quote as char
                        )),
                    }],
                );
                self.pos = line_end;
                if has_escape {
                    return Token::String(ArenaCow::Owned(buffer));
                } else {
                    // SAFETY: beg..line_end is valid UTF-8 because we only process valid string content
                    let s =
                        unsafe { str::from_utf8_unchecked(self.src.get_unchecked(beg..line_end)) };
                    return Token::String(ArenaCow::Borrowed(s));
                }
            }

            // We don't have a closing quote or escape sequence
            if quote_or_escape == bytes.len() {
                break;
            }

            let pos = self.pos + quote_or_escape;
            // SAFETY: pos < self.len because quote_or_escape < bytes.len()
            let c = unsafe { *self.src.get_unchecked(pos) };
            if c == quote {
                // Closing quote
                if has_escape {
                    // Copy any remaining content before the quote
                    if self.pos < pos {
                        // SAFETY: self.pos..pos is valid UTF-8 because we only process valid string content
                        let string = unsafe {
                            str::from_utf8_unchecked(self.src.get_unchecked(self.pos..pos))
                        };
                        buffer.push_str(string);
                    }
                    self.pos = pos + 1;
                    return Token::String(ArenaCow::Owned(buffer));
                } else {
                    // SAFETY: beg..pos is valid UTF-8 because we only process valid string content
                    let s = unsafe { str::from_utf8_unchecked(self.src.get_unchecked(beg..pos)) };
                    self.pos = pos + 1;
                    return Token::String(ArenaCow::Borrowed(s));
                }
            } else if c == b'\\' {
                has_escape = true;
                if buffer.is_empty() {
                    buffer.reserve_exact(bytes.len());
                    // SAFETY: beg..pos is valid UTF-8 because we only process valid string content
                    let string =
                        unsafe { str::from_utf8_unchecked(self.src.get_unchecked(beg..pos)) };
                    buffer.push_str(string);
                } else if self.pos < pos {
                    // SAFETY: self.pos..pos is valid UTF-8 because we only process valid string content
                    let string =
                        unsafe { str::from_utf8_unchecked(self.src.get_unchecked(self.pos..pos)) };
                    buffer.push_str(string);
                }
                // We don't want an incomplete escape sequence
                if pos + 1 >= self.len {
                    self.emit_error(
                        Range::from(start..self.pos),
                        LexError::UnterminatedString,
                        vec![Label {
                            span: Range::from(start..self.pos),
                            message: ArenaCow::Owned(arena_format!(
                                self.arena,
                                "Dis string no get ending quote `{}`",
                                quote as char
                            )),
                        }],
                    );
                    return Token::String(ArenaCow::Owned(buffer));
                }

                // SAFETY: pos + 1 < self.len, so this is safe
                let esc = unsafe { *self.src.get_unchecked(pos + 1) };
                match esc {
                    b'"' if quote == b'"' => buffer.push('"'),
                    b'\'' if quote == b'\'' => buffer.push('\''),
                    b'\\' => buffer.push('\\'),
                    b'n' => buffer.push('\n'),
                    b't' => buffer.push('\t'),
                    _ => {
                        self.emit_error(
                            Range::from(pos..pos + 2),
                            LexError::InvalidStringEscape,
                            vec![Label {
                                span: Range::from(pos..pos + 2),
                                message: ArenaCow::Borrowed("I no sabi dis escape character"),
                            }],
                        );
                        // Append the invalid escape character
                        buffer.push(esc as char);
                    }
                }
                self.pos = pos + 2;
            }
        }

        // It's EOF, no closing quote found ?
        self.emit_error(
            Range::from(start..self.pos),
            LexError::UnterminatedString,
            vec![Label {
                span: Range::from(start..self.pos),
                message: ArenaCow::Owned(arena_format!(
                    self.arena,
                    "Dis string no get ending quote `{}`",
                    quote as char
                )),
            }],
        );
        if has_escape {
            Token::String(ArenaCow::Owned(buffer))
        } else {
            // SAFETY: beg..self.pos is valid UTF-8 because we only process valid string content
            let s = unsafe { str::from_utf8_unchecked(self.src.get_unchecked(beg..self.pos)) };
            Token::String(ArenaCow::Borrowed(s))
        }
    }

    fn scan_punctuation(&mut self, b: u8) -> Option<Token<'arena, 'input>> {
        match b {
            b'(' => {
                self.pos += 1;
                Some(Token::LParen)
            }
            b')' => {
                self.pos += 1;
                Some(Token::RParen)
            }
            b'[' => {
                self.pos += 1;
                Some(Token::LBracket)
            }
            b']' => {
                self.pos += 1;
                Some(Token::RBracket)
            }
            b',' => {
                self.pos += 1;
                Some(Token::Comma)
            }
            _ => None,
        }
    }

    fn scan_number(&mut self, start: usize) -> Token<'arena, 'input> {
        let mut saw_dot = false;
        let len = self.len;

        // Try consume the integer part first
        while self.pos < len &&
        // SAFETY: self.pos < len, so this is safe
        unsafe {
            self.src.get_unchecked(self.pos).is_ascii_digit()
        } {
            self.pos += 1;
        }

        // Then the decimal part ?
        if self.pos < len &&
        // SAFETY: self.pos < len, so this is safe
        unsafe {
            *self.src.get_unchecked(self.pos) == b'.'
        } {
            saw_dot = true;
            self.pos += 1;

            let after_dot = if self.pos < len {
                // SAFETY: self.pos < len, so this is safe
                unsafe { *self.src.get_unchecked(self.pos) }
            } else {
                0
            };
            if !after_dot.is_ascii_digit() {
                // No digit after the dot
                self.emit_error(
                    Range::from(start..self.pos),
                    LexError::InvalidNumber,
                    vec![Label {
                        span: Range::from(start..self.pos),
                        message: ArenaCow::Borrowed("Dis number no get digit after `.`"),
                    }],
                );
                self.pos += 1;
                return self.next_token().token;
            }
            while self.pos < len &&
            // SAFETY: self.pos < len, so this is safe
            unsafe { self.src.get_unchecked(self.pos).is_ascii_digit() }
            {
                self.pos += 1;
            }
        }

        // Multiple dots in the number, like "1.2.3" ?
        if saw_dot && self.pos < len &&
        // SAFETY: self.pos < len, so this is safe
        unsafe { *self.src.get_unchecked(self.pos) == b'.'  }
        {
            self.pos += 1;
            self.emit_error(
                Range::from(start..self.pos),
                LexError::InvalidNumber,
                vec![Label {
                    span: Range::from(start..self.pos),
                    message: ArenaCow::Borrowed("Dis number get extra `.`"),
                }],
            );
            return self.next_token().token;
        }

        // Is it a letter or underscore (like "1foo" or "1_bar") ?
        let next_char = if self.pos < len {
            // SAFETY: self.pos < len, so this is safe
            unsafe { *self.src.get_unchecked(self.pos) }
        } else {
            0
        };
        if Self::is_alpha_or_underscore(next_char) {
            let id_start = self.pos;
            while self.pos < len {
                // SAFETY: self.pos < len, so this is safe
                let ch = unsafe { *self.src.get_unchecked(self.pos) };
                if !Self::is_alpha_or_underscore(ch) && !ch.is_ascii_digit() {
                    break;
                }
                self.pos += 1;
            }
            self.emit_error(
                Range::from(start..self.pos),
                LexError::InvalidIdentifier,
                vec![Label {
                    span: Range::from(start..self.pos),
                    message: ArenaCow::Borrowed("Identifier must start with letter or underscore"),
                }],
            );
            // SAFETY: start..id_start is valid UTF-8 because we only process valid number characters
            let num = unsafe { str::from_utf8_unchecked(self.src.get_unchecked(start..id_start)) };
            return Token::Number(num);
        }

        // SAFETY: start..self.pos is valid UTF-8 because we only process valid number characters
        let num = unsafe { str::from_utf8_unchecked(self.src.get_unchecked(start..self.pos)) };
        Token::Number(num)
    }

    fn scan_identifier_or_keyword(&mut self, start: usize) -> Token<'arena, 'input> {
        let word = self.read_word();

        // "if to say" (if-statement) or "if not so" (else-statement)
        if word == "if" {
            // Save position so we can rollback if needed
            let save = self.pos;
            // Try "if to say" = if statement
            if self.try_consume_word("to") && self.try_consume_word("say") {
                return Token::IfToSay;
            }
            // Try "if not so" = else statement
            if self.try_consume_word("not") && self.try_consume_word("so") {
                return Token::IfNotSo;
            }
            // It's neither, then it's just the identifier "if"
            self.pos = save;
            return Token::Identifier("if");
        }
        // "small pass" (less than)
        if word == "small" {
            let save = self.pos;
            if self.try_consume_word("pass") {
                return Token::SmallPass;
            }
            self.pos = save;
            return Token::Identifier("small");
        }
        match word {
            "make" => Token::Make,
            "get" => Token::Get,
            "add" => Token::Add,
            "minus" => Token::Minus,
            "times" => Token::Times,
            "divide" => Token::Divide,
            "mod" => Token::Mod,
            "and" => Token::And,
            "or" => Token::Or,
            "not" => Token::Not,
            "jasi" => Token::Jasi,
            "start" => Token::Start,
            "end" => Token::End,
            "na" => Token::Na,
            "pass" => Token::Pass,
            "true" => Token::True,
            "false" => Token::False,
            "do" => Token::Do,
            "return" => Token::Return,
            // Is it an identifier ?
            other => {
                // Identifier is valid if:
                // - The first character must be a letter or underscore.
                // - The rest can be letters, digits, or underscores.
                let mut bytes = other.bytes();
                if let Some(first) = bytes.next() {
                    if !Self::is_alpha_or_underscore(first) {
                        self.emit_error(
                            Range::from(start..self.pos),
                            LexError::InvalidIdentifier,
                            vec![Label {
                                span: Range::from(start..self.pos),
                                message: ArenaCow::Borrowed(
                                    "Identifier must start with letter or underscore",
                                ),
                            }],
                        );
                    } else {
                        // Invalid chars in the rest of the identifier ?
                        for b in bytes {
                            if !b.is_ascii_alphanumeric() && b != b'_' {
                                self.emit_error(
                                    Range::from(start..self.pos),
                                    LexError::InvalidIdentifier,
                                    vec![Label {
                                        span: Range::from(start..self.pos),
                                        message: ArenaCow::Borrowed(
                                            "Dis identifier get invalid character",
                                        ),
                                    }],
                                );
                                break;
                            }
                        }
                    }
                }
                Token::Identifier(other)
            }
        }
    }
}

impl<'arena, 'input> Iterator for Lexer<'arena, 'input> {
    type Item = SpannedToken<'arena, 'input>;
    fn next(&mut self) -> Option<Self::Item> {
        let st = self.next_token();
        // Yield exactly one EOF token at the end of the input.
        if matches!(st.token, Token::EOF) && self.pos >= self.len {
            return None;
        }
        Some(st)
    }
}
