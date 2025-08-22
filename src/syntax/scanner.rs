//! The lexer (or scanner) for NaijaScript.

use std::borrow::Cow;

use crate::arena::{Arena, ArenaCow, ArenaString};
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
    // The original source text as bytes
    src: &'input [u8],
    // Current position in the source text
    pos: usize,
    /// Collection of errors encountered during scanning
    pub errors: Diagnostics,
    arena: &'arena Arena,
}

impl<'arena, 'input> Lexer<'arena, 'input> {
    /// Creates a new lexer from source text
    #[inline(always)]
    pub fn new(src: &'input str, arena: &'arena Arena) -> Self {
        Lexer { src: src.as_bytes(), pos: 0, errors: Diagnostics::default(), arena }
    }

    // Returns the next token and its span from the input.
    #[inline(always)]
    fn next_token(&mut self) -> SpannedToken<'arena, 'input> {
        let len = self.src.len();

        loop {
            // Skip over any whitespace before the next token
            self.skip_whitespace();

            // Record starting position for spans and slicing
            let start = self.pos;

            // Check for EOF first
            if self.pos >= len {
                return SpannedToken { token: Token::EOF, span: start..start };
            }

            // SAFETY: We just checked that self.pos < len
            let b = unsafe { *self.src.get_unchecked(self.pos) };

            // If we see a '#' character, that means the start of a comment.
            if b == b'#' {
                self.skip_comment();
                continue;
            }

            // String literals
            if b == b'"' || b == b'\'' {
                let token = self.scan_string(start, b);
                return SpannedToken { token, span: start..self.pos };
            }
            if let Some(token) = self.scan_punctuation(b) {
                return SpannedToken { token, span: start..self.pos };
            }
            if b.is_ascii_digit() {
                let token = self.scan_number(start);
                return SpannedToken { token, span: start..self.pos };
            }
            if Self::is_alpha_or_underscore(b) {
                let token = self.scan_identifier_or_keyword(start);
                return SpannedToken { token, span: start..self.pos };
            }
            // Unicode characters beyond ASCII require special handling since our lexer operates on bytes
            if !b.is_ascii() {
                // We need the complete Unicode character to properly skip it and report accurate spans
                // SAFETY: We know this is valid UTF-8 because the original string was valid UTF-8
                let rest = unsafe { str::from_utf8_unchecked(self.src.get_unchecked(start..)) };
                if let Some(c) = rest.chars().next() {
                    let len = c.len_utf8();
                    self.errors.emit(
                        start..self.pos + len,
                        Severity::Error,
                        "lexical",
                        LexError::UnexpectedChar.as_str(),
                        vec![Label {
                            span: start..self.pos + len,
                            message: Cow::Borrowed("I no sabi dis character"),
                        }],
                    );
                    // Skip the entire Unicode character to avoid splitting it across byte boundaries
                    self.pos += len;
                    continue;
                } else {
                    // Invalid UTF-8 sequence detected, so we advance by one byte to prevent infinite loops
                    self.pos += 1;
                    continue;
                }
            }
            // An unexpected character? We emit an error and advance by one byte
            self.errors.emit(
                start..self.pos,
                Severity::Error,
                "lexical",
                LexError::UnexpectedChar.as_str(),
                vec![Label {
                    span: start..self.pos,
                    message: Cow::Borrowed("I no sabi dis character"),
                }],
            );
            self.pos += 1;
        }
    }

    // Skips all whitespace characters
    //
    // Since NaijaScript doesn't have significant whitespace (like Python),
    // we can simply skip over all spaces, tabs, newlines, etc.
    #[inline(always)]
    fn skip_whitespace(&mut self) {
        let len = self.src.len();
        while self.pos < len && unsafe { self.src.get_unchecked(self.pos) }.is_ascii_whitespace() {
            self.pos += 1;
        }
    }

    #[inline(always)]
    fn skip_comment(&mut self) {
        let len = self.src.len();

        // SAFETY: self.pos..len is guaranteed to be a valid slice
        // and the performance gain is significant in our benchmarks
        let pos = unsafe {
            let hs = self.src.get_unchecked(self.pos..len);
            memchr2(b'\n', b'\r', hs, 0)
        };

        // Advance position to the end of the comment
        self.pos += pos;

        // After skipping to end of line, we might be sitting on the newline or carriage return character
        // We need to consume it too so the next token scan starts fresh on the following line
        if self.pos < len {
            let ch = unsafe { *self.src.get_unchecked(self.pos) };
            if ch == b'\n' || ch == b'\r' {
                self.pos += 1;
            }
        }
    }

    // Checks if a byte is a letter (A-Z, a-z) or underscore (_)
    //
    // Used for identifier and keyword detection
    #[inline(always)]
    const fn is_alpha_or_underscore(b: u8) -> bool {
        b.is_ascii_alphabetic() || b == b'_'
    }

    // Reads an entire word (identifier or keyword)
    //
    // We can use from_utf8_unchecked safely here because:
    // 1. We only match ASCII alphabetic/numeric/underscore characters, which are valid UTF-8
    // 2. The original source was already valid UTF-8
    // 3. The performance gain is significant in our benchmarks
    #[inline(always)]
    fn read_word(&mut self) -> &'input str {
        let beg = self.pos;
        let len = self.src.len();

        while self.pos < len {
            let ch = unsafe { *self.src.get_unchecked(self.pos) };
            if !Self::is_alpha_or_underscore(ch) && !ch.is_ascii_digit() {
                break;
            }
            self.pos += 1;
        }

        unsafe { str::from_utf8_unchecked(self.src.get_unchecked(beg..self.pos)) }
    }

    // Try to consume a specific word at current position
    //
    // This is crucial for multi-word keywords like "if to say" and "small pass".
    // The function checks if the next word matches the expected one,
    // and only consumes it if:
    // 1. It's followed by a non-alphabetic character (prevents partial matches)
    // 2. We have enough input left
    #[inline(always)]
    fn try_consume_word(&mut self, word: &str) -> bool {
        let len = self.src.len();
        let mut beg = self.pos;

        while beg < len && unsafe { *self.src.get_unchecked(beg) }.is_ascii_whitespace() {
            beg += 1;
        }

        let end = beg + word.len();
        if end <= len {
            // SAFETY: beg..end is valid
            let slice = unsafe { self.src.get_unchecked(beg..end) };
            if slice == word.as_bytes()
                && (end == len
                    || !Self::is_alpha_or_underscore(unsafe { *self.src.get_unchecked(end) }))
            {
                self.pos = end;
                return true;
            }
        }
        false
    }

    #[inline(always)]
    fn scan_string(&mut self, start: usize, quote: u8) -> Token<'arena, 'input> {
        self.pos += 1; // Skip the opening quote
        let beg = self.pos;

        let mut has_escape = false;
        let mut buffer = ArenaString::new_in(self.arena);

        loop {
            // Find the next quote or escape sequence
            // SAFETY: self.pos is always <= self.src.len() due to lexer invariants
            let bytes = unsafe { self.src.get_unchecked(self.pos..) };
            let pos = memchr2(quote, b'\\', bytes, 0);
            // We don't have a closing quote or escape sequence
            if pos == bytes.len() {
                break;
            }

            let pos = self.pos + pos;
            // SAFETY: pos is guaranteed to be valid since it comes from memchr2
            let c = unsafe { *self.src.get_unchecked(pos) };
            if c == quote {
                // Closing quote
                if has_escape {
                    // Copy any remaining content before the quote
                    if self.pos < pos {
                        // SAFETY: We know this is valid UTF-8 because we only process valid string content
                        let slice = unsafe {
                            str::from_utf8_unchecked(self.src.get_unchecked(self.pos..pos))
                        };
                        buffer.push_str(slice);
                    }
                    self.pos = pos + 1;
                    return Token::String(ArenaCow::Owned(buffer));
                } else {
                    // SAFETY: We know this is valid UTF-8 because we only process valid string content
                    let s = unsafe { str::from_utf8_unchecked(self.src.get_unchecked(beg..pos)) };
                    self.pos = pos + 1;
                    return Token::String(ArenaCow::Borrowed(s));
                }
            } else if c == b'\\' {
                has_escape = true;
                if buffer.is_empty() {
                    buffer.reserve_exact(bytes.len());
                    // SAFETY: We know this is valid UTF-8 because we only process valid string content
                    let slice =
                        unsafe { str::from_utf8_unchecked(self.src.get_unchecked(beg..pos)) };
                    buffer.push_str(slice);
                } else if self.pos < pos {
                    // SAFETY: We know this is valid UTF-8 because we only process valid string content
                    let slice =
                        unsafe { str::from_utf8_unchecked(self.src.get_unchecked(self.pos..pos)) };
                    buffer.push_str(slice);
                }
                // We don't want an incomplete escape sequence
                if pos + 1 >= self.src.len() {
                    self.errors.emit(
                        start..self.src.len(),
                        Severity::Error,
                        "lexical",
                        LexError::UnterminatedString.as_str(),
                        vec![Label {
                            span: start..self.src.len(),
                            message: Cow::Owned(format!(
                                "Dis string no get ending quote `{}`",
                                quote as char
                            )),
                        }],
                    );
                    self.pos = self.src.len();
                    return Token::String(ArenaCow::Owned(buffer));
                }

                // SAFETY: We just checked that pos + 1 < self.src.len()
                let esc = unsafe { *self.src.get_unchecked(pos + 1) };
                match esc {
                    b'"' if quote == b'"' => buffer.push('"'),
                    b'\'' if quote == b'\'' => buffer.push('\''),
                    b'\\' => buffer.push('\\'),
                    b'n' => buffer.push('\n'),
                    b't' => buffer.push('\t'),
                    _ => {
                        self.errors.emit(
                            pos..pos + 2,
                            Severity::Error,
                            "lexical",
                            LexError::InvalidStringEscape.as_str(),
                            vec![Label {
                                span: pos..pos + 2,
                                message: Cow::Borrowed("Dis escape character no valid"),
                            }],
                        );
                        // Append the invalid escape character
                        buffer.push(esc as char);
                    }
                }
                self.pos = pos + 2;
            }
        }

        // We don't want an unterminated string
        let message = Cow::Borrowed(if quote == b'"' {
            r#"Dis string no get ending quote `"`"#
        } else {
            r#"Dis string no get ending quote `'`"#
        });
        self.errors.emit(
            start..self.src.len(),
            Severity::Error,
            "lexical",
            LexError::UnterminatedString.as_str(),
            vec![Label { span: start..self.src.len(), message }],
        );
        self.pos = self.src.len();
        if has_escape {
            Token::String(ArenaCow::Owned(buffer))
        } else {
            // SAFETY: We know this is valid UTF-8 because we only process valid string content
            let s =
                unsafe { str::from_utf8_unchecked(self.src.get_unchecked(beg..self.src.len())) };
            Token::String(ArenaCow::Borrowed(s))
        }
    }

    #[inline(always)]
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
            b',' => {
                self.pos += 1;
                Some(Token::Comma)
            }
            _ => None,
        }
    }

    #[inline(always)]
    fn scan_number(&mut self, start: usize) -> Token<'arena, 'input> {
        let mut saw_dot = false;
        let len = self.src.len();

        // Let's consume the integer part of the number first.
        // We'll keep bumping as long as we see digits.
        while self.pos < len && unsafe { *self.src.get_unchecked(self.pos) }.is_ascii_digit() {
            self.pos += 1;
        }

        // Consume decimal part if present
        if self.pos < len && unsafe { *self.src.get_unchecked(self.pos) } == b'.' {
            saw_dot = true;
            let dot_pos = self.pos;
            self.pos += 1;

            let after_dot =
                if self.pos < len { unsafe { *self.src.get_unchecked(self.pos) } } else { 0 };

            if !after_dot.is_ascii_digit() {
                // If there's no digit after the dot, that's not a valid number in NaijaScript.
                // We'll emit an error here so the user knows what's wrong,
                // and then skip the dot so we don't get stuck in an infinite loop.
                self.errors.emit(
                    start..self.pos,
                    Severity::Error,
                    "lexical",
                    LexError::InvalidNumber.as_str(),
                    vec![Label {
                        span: dot_pos..self.pos,
                        message: Cow::Borrowed("Dis number no get digit after `.`"),
                    }],
                );
                self.pos += 1;
                return self.next_token().token;
            }
            while self.pos < len && unsafe { *self.src.get_unchecked(self.pos) }.is_ascii_digit() {
                self.pos += 1;
            }
        }

        // Let's check if there are multiple dots in the number, like "1.2.3".
        // That's not valid in NaijaScript, so we'll catch it here and report an error.
        if saw_dot && self.pos < len && unsafe { *self.src.get_unchecked(self.pos) } == b'.' {
            let extra_dot = self.pos;
            self.pos += 1;
            self.errors.emit(
                start..self.pos,
                Severity::Error,
                "lexical",
                LexError::InvalidNumber.as_str(),
                vec![Label {
                    span: extra_dot..self.pos,
                    message: Cow::Borrowed("Dis number get extra `.`"),
                }],
            );
            return self.next_token().token;
        }

        // If the next character is a letter or underscore (like "1foo" or "1_bar"),
        // that's not a valid number or identifier in NaijaScript, so let's catch it here.
        let next_char =
            if self.pos < len { unsafe { *self.src.get_unchecked(self.pos) } } else { 0 };

        if Self::is_alpha_or_underscore(next_char) {
            let id_start = self.pos;
            while self.pos < len {
                let ch = unsafe { *self.src.get_unchecked(self.pos) };
                if !Self::is_alpha_or_underscore(ch) && !ch.is_ascii_digit() {
                    break;
                }
                self.pos += 1;
            }
            self.errors.emit(
                start..self.pos,
                Severity::Error,
                "lexical",
                LexError::InvalidIdentifier.as_str(),
                vec![Label {
                    span: start..id_start,
                    message: Cow::Borrowed("Identifier must start with letter or underscore"),
                }],
            );
            // SAFETY: We know this is valid UTF-8 because we only process ASCII digits
            let num = unsafe { str::from_utf8_unchecked(self.src.get_unchecked(start..id_start)) };
            return Token::Number(num);
        }

        // Grab the whole number as a string slice.
        // SAFETY: We know this is valid UTF-8 because we only process ASCII digits and dots
        let num = unsafe { str::from_utf8_unchecked(self.src.get_unchecked(start..self.pos)) };
        Token::Number(num)
    }

    #[inline(always)]
    fn scan_identifier_or_keyword(&mut self, start: usize) -> Token<'arena, 'input> {
        let word = self.read_word();
        // Handle multi-word constructs
        // This is a bit tricky because we need to look ahead without committing
        // to consuming more tokens until we know what we have

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
            // If neither worked, it's just the identifier "if"
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
            // If not a keyword, it's an identifier
            other => {
                // Let's make sure the identifier is valid:
                // - The first character must be a letter or underscore.
                // - The rest can be letters, digits, or underscores.
                let mut bytes = other.bytes();
                if let Some(first) = bytes.next() {
                    if !Self::is_alpha_or_underscore(first) {
                        self.errors.emit(
                            start..self.pos,
                            Severity::Error,
                            "lexical",
                            LexError::InvalidIdentifier.as_str(),
                            vec![Label {
                                span: start..start + 1,
                                message: Cow::Borrowed(
                                    "Identifier must start with letter or underscore",
                                ),
                            }],
                        );
                    } else {
                        // Let's check if there are any invalid characters in the rest of the identifier.
                        // We'll walk through each character, and if we find something that's not a letter, digit, or underscore,
                        // we'll emit an error so the user knows exactly where the problem is.
                        let mut offset = 1;
                        for b in bytes {
                            if !b.is_ascii_alphanumeric() && b != b'_' {
                                let bad_pos = start + offset;
                                self.errors.emit(
                                    start..self.pos,
                                    Severity::Error,
                                    "lexical",
                                    LexError::InvalidIdentifier.as_str(),
                                    vec![Label {
                                        span: bad_pos..bad_pos + 1,
                                        message: Cow::Borrowed(
                                            "Dis identifier get invalid character",
                                        ),
                                    }],
                                );
                                break;
                            }
                            offset += 1;
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
        // We want to yield exactly one EOF token at the end of the input.
        // After that, the iterator should return None on all subsequent calls.
        // This check ensures we don't emit multiple EOF tokens if .next() is called repeatedly.
        if matches!(st.token, Token::EOF) && self.pos >= self.src.len() {
            // We're already at the end, so let's stop iterating.
            return None;
        }
        Some(st)
    }
}
