//! The lexical analyzer (lexer) for NaijaScript.

use crate::diagnostics::{Diagnostic, DiagnosticHandler};

/// Represents a lexical token in NaijaScript.
///
/// Each variant corresponds to a distinct syntactic element, such as a keyword, operator,
/// identifier, or literal. The lifetime parameter `'a` allows tokens to reference slices of the
/// original input.
#[derive(Debug, Clone, PartialEq)]
pub struct Token<'a> {
    /// The kind of the token.
    pub kind: TokenKind<'a>,
    /// The span of the token in the source input.
    pub span: Span,
}

/// Represents the different kinds of tokens that can be produced by the lexer.
#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind<'a> {
    /// The `make` keyword.
    Make,
    /// The `get` keyword.
    Get,
    /// The `shout` keyword.
    Shout,
    /// The multi-word keyword `if to say`.
    IfToSay,
    /// The multi-word keyword `if not so`.
    IfNotSo,
    /// The `jasi` keyword.
    Jasi,
    /// The `start` keyword.
    Start,
    /// The `end` keyword.
    End,
    /// The `add` keyword.
    Add,
    /// The `minus` keyword.
    Minus,
    /// The `times` keyword.
    Times,
    /// The `divide` keyword.
    Divide,
    /// The `na` keyword.
    Na,
    /// The `pass` keyword.
    Pass,
    /// The multi-word keyword `small pass`.
    SmallPass,
    /// An identifier (variable or function name).
    Identifier(&'a str),
    /// A numeric literal.
    Number(f64),
    /// The left parenthesis `(`.
    LeftParen,
    /// The right parenthesis `)`.
    RightParen,
    /// A newline character (line break).
    Newline,
    /// End of file/input.
    Eof,
}

/// Represents a span in the source input (byte offsets).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Span {
    /// The starting byte offset of the span.
    pub start: usize,
    /// The ending byte offset of the span (exclusive).
    pub end: usize,
}

/// Enumerates possible lexical error kinds encountered during tokenization.
#[derive(Debug, Clone, PartialEq)]
pub enum LexErrorKind<'a> {
    /// An unexpected character was encountered.
    UnexpectedCharacter(char),
    /// An invalid number literal was found.
    InvalidNumber(&'a str),
    /// A string literal was not properly terminated.
    UnterminatedString,
    /// An invalid escape sequence was found in a string.
    InvalidEscapeSequence(char),
}

/// Represents a lexical error, including its kind and position in the source.
#[derive(Debug, Clone, PartialEq)]
pub struct LexError<'a> {
    /// The specific kind of lexical error.
    pub kind: LexErrorKind<'a>,
    /// The span of the error in the source input.
    pub span: Span,
    /// The line number where the error occurred.
    pub line: usize,
    /// The column number where the error occurred.
    pub column: usize,
}

impl<'a> LexError<'a> {
    /// Constructs a new `LexError` with the given kind, span, line, and column.
    #[cold]
    fn new(kind: LexErrorKind<'a>, span: Span, line: usize, column: usize) -> Self {
        Self { kind, span, line, column }
    }

    /// Converts this error into a `Diagnostic` for user-facing error reporting.
    ///
    /// The diagnostic includes a message, the relevant source span, and the line/column.
    pub fn to_diagnostic(&self, source: &'a str) -> Diagnostic<'a> {
        let message = match &self.kind {
            LexErrorKind::UnexpectedCharacter(_) => "Wetin be dis character",
            LexErrorKind::InvalidNumber(_) => "Wetin be dis number",
            LexErrorKind::UnterminatedString => "You no close dis string",
            LexErrorKind::InvalidEscapeSequence(_) => "Wetin be dis escape",
        };
        Diagnostic::error(
            "lexical error",
            message,
            source,
            (self.span.start, self.span.end),
            self.line,
            self.column,
        )
    }
}

/// The result type for lexing operations.
pub type LexResult<'a, T> = Result<T, LexError<'a>>;

/// The main lexer struct for NaijaScript.
///
/// The lexer processes the input string and produces a stream of tokens. It is optimized for
/// performance and supports both ASCII and Unicode input. The lexer maintains its current position
/// and tracks when the end of input is reached.
pub struct Lexer<'a> {
    /// The source input being tokenized.
    input: &'a str,
    /// The current byte position in the input.
    position: usize,
    /// Indicates whether the lexer has reached the end of input.
    done: bool,
    /// The current line number.
    line: usize,
    /// The current column number.
    column: usize,
}

impl<'a> Lexer<'a> {
    /// Creates a new lexer for the given input string.
    pub fn new(input: &'a str) -> Self {
        Lexer { input, position: 0, done: false, line: 1, column: 1 }
    }

    /// Returns the next token, optionally reporting errors via a diagnostic handler.
    ///
    /// This method is intended for use cases where error reporting should not stop lexing.
    pub fn next_with_handler(
        &mut self,
        handler: Option<&mut dyn DiagnosticHandler>,
        source: &'a str,
    ) -> Option<LexResult<'a, Token<'a>>> {
        let res = self.next_token_jump();
        if let Some(Err(ref e)) = res
            && let Some(h) = handler
        {
            h.report(&e.to_diagnostic(source));
        }
        res
    }

    /// Returns the current byte at the lexer's position, if any.
    #[inline(always)]
    fn current_byte(&self) -> Option<u8> {
        self.input.as_bytes().get(self.position).copied()
    }

    /// Returns the current character at the lexer's position, if any.
    #[inline(always)]
    fn current_char(&self) -> Option<char> {
        let bytes = self.input.as_bytes();
        if self.position >= bytes.len() {
            return None;
        }
        let b = bytes[self.position];
        if b < 0x80 { Some(b as char) } else { self.input[self.position..].chars().next() }
    }

    /// Returns the current position as a Span of length 1 (for single-char tokens/errors).
    #[inline(always)]
    fn single_char_span(&self) -> Span {
        Span { start: self.position, end: self.position + 1 }
    }

    /// Returns a span from a start position to the current position.
    #[inline(always)]
    fn span_from(&self, start: usize) -> Span {
        Span { start, end: self.position }
    }

    /// Advances the lexer by one byte, updating line and column counters.
    #[inline(always)]
    fn advance_byte_update_linecol(&mut self) {
        let bytes = self.input.as_bytes();
        if self.position >= bytes.len() {
            return;
        }
        let b = bytes[self.position];
        self.position += 1;
        if b == b'\n' {
            self.line += 1;
            self.column = 1;
        } else {
            self.column += 1;
        }
    }

    /// Advances the lexer's position by `n` bytes.
    #[inline(always)]
    fn advance(&mut self, n: usize) {
        for _ in 0..n {
            self.advance_byte_update_linecol();
        }
    }

    /// Advances the lexer's position by one character (handling UTF-8).
    #[inline(always)]
    fn advance_char(&mut self) {
        let bytes = self.input.as_bytes();
        if self.position >= bytes.len() {
            return;
        }
        let b = bytes[self.position];
        let char_len = if b < 0x80 {
            1
        } else {
            match b {
                0xC0..=0xDF => 2,
                0xE0..=0xEF => 3,
                0xF0..=0xF7 => 4,
                _ => 1,
            }
        };
        for _ in 0..char_len {
            self.advance_byte_update_linecol();
        }
    }

    /// Lookup table for fast ASCII alphanumeric and underscore checks.
    ///
    /// Used to quickly determine if a byte is valid in an identifier.
    const IS_ALNUM: [bool; 256] = {
        let mut arr = [false; 256];
        let mut i = b'0';
        while i <= b'9' {
            arr[i as usize] = true;
            i += 1;
        }
        let mut i = b'A';
        while i <= b'Z' {
            arr[i as usize] = true;
            i += 1;
        }
        let mut i = b'a';
        while i <= b'z' {
            arr[i as usize] = true;
            i += 1;
        }
        arr[b'_' as usize] = true;
        arr
    };

    /// Precomputed powers of 10 for fast decimal conversion.
    const POWERS_OF_10: [f64; 19] = [
        1e0, 1e1, 1e2, 1e3, 1e4, 1e5, 1e6, 1e7, 1e8, 1e9, 1e10, 1e11, 1e12, 1e13, 1e14, 1e15, 1e16,
        1e17, 1e18,
    ];

    /// Returns true if the given byte is an ASCII digit ('0'..='9').
    #[inline(always)]
    const fn is_digit(b: u8) -> bool {
        b.wrapping_sub(b'0') < 10
    }

    /// Advances the lexer's position up to `max` times while the predicate is true.
    #[inline(always)]
    fn advance_while_limit<F>(&mut self, pred: F, max: usize)
    where
        F: Fn(u8) -> bool,
    {
        let bytes = self.input.as_bytes();
        let len = bytes.len();
        for _ in 0..max {
            if self.position < len {
                let b = bytes[self.position];
                if pred(b) {
                    self.advance(1);
                } else {
                    break;
                }
            }
        }
    }

    /// Skips whitespace characters (space, tab, carriage return) in the input.
    ///
    /// This method is optimized to skip up to four consecutive whitespace characters per loop
    /// iteration for performance.
    #[inline(always)]
    fn skip_whitespace(&mut self) {
        let bytes = self.input.as_bytes();
        let len = bytes.len();
        while self.position < len {
            let b = bytes[self.position];
            if b == b' ' || b == b'\t' || b == b'\r' {
                self.advance(1);
                self.advance_while_limit(|b2| b2 == b' ' || b2 == b'\t' || b2 == b'\r', 3);
            } else {
                break;
            }
        }
    }

    /// Reads a number literal from the input, supporting both integer and floating-point formats.
    ///
    /// Uses a fast path for common cases and falls back to a slower method if overflow is possible.
    #[inline(always)]
    fn read_number(&mut self) -> LexResult<'a, f64> {
        let bytes = self.input.as_bytes();
        let len = bytes.len();
        let start = self.position;

        let mut integer_part = 0u64;

        while self.position < len {
            let b = bytes[self.position];
            if Self::is_digit(b) {
                let digit = (b - b'0') as u64;

                // Check for integer overflow.
                if integer_part > (u64::MAX - digit) / 10 {
                    self.position = start;
                    return self.read_number_fallback();
                }

                integer_part = integer_part * 10 + digit;
                self.advance(1);
            } else {
                break;
            }
        }

        // Check for decimal point.
        if self.position >= len || bytes[self.position] != b'.' {
            return Ok(integer_part as f64);
        }

        self.advance(1);
        let mut decimal_part = 0u64;
        let mut decimal_places = 0u32;

        while self.position < len && decimal_places < 17 {
            let b = bytes[self.position];
            if Self::is_digit(b) {
                decimal_part = decimal_part * 10 + (b - b'0') as u64;
                decimal_places += 1;
                self.advance(1);
            } else {
                break;
            }
        }

        let mut result = integer_part as f64;
        if decimal_places > 0 && decimal_places < Self::POWERS_OF_10.len() as u32 {
            let decimal_value = decimal_part as f64;
            result += decimal_value / Self::POWERS_OF_10[decimal_places as usize];
        }

        Ok(result)
    }

    /// Fallback method for reading a number literal as a string and parsing it.
    ///
    /// This is used when the fast path in `read_number` cannot be used due to possible overflow or
    /// unusual formats.
    #[inline(never)]
    fn read_number_fallback(&mut self) -> LexResult<'a, f64> {
        let bytes = self.input.as_bytes();
        let len = bytes.len();
        let start = self.position;
        let mut seen_dot = false;

        while self.position < len {
            let b = bytes[self.position];
            if b.is_ascii_digit() {
                self.advance(1);
            } else if b == b'.' && !seen_dot {
                seen_dot = true;
                self.advance(1);
            } else {
                break;
            }
        }
        let num_str = &self.input[start..self.position];
        num_str.parse().map_err(|_| {
            self.error_with_span(LexErrorKind::InvalidNumber(num_str), self.span_from(start))
        })
    }

    /// Reads an identifier or keyword from the input as a string slice.
    ///
    /// This method uses a fast path for ASCII alphanumeric and underscore characters.
    #[inline(always)]
    fn read_identifier_slice(&mut self) -> &str {
        let bytes = self.input.as_bytes();
        let len = bytes.len();
        let start = self.position;
        while self.position < len {
            let b = bytes[self.position];
            if Self::IS_ALNUM[b as usize] {
                self.advance(1);
                self.advance_while_limit(|b2| Self::IS_ALNUM[b2 as usize], 3);
            } else {
                break;
            }
        }
        // SAFETY: The slice is guaranteed to be valid UTF-8 as it is a substring of the input.
        unsafe { self.input.get_unchecked(start..self.position) }
    }

    /// Attempts to match a multi-word keyword at the current position.
    ///
    /// This function skips whitespace in the input as needed to match the keyword pattern.
    fn match_multiword(&mut self, kw: &'static [u8]) -> bool {
        let input_bytes = self.input.as_bytes();
        let mut pos = self.position;
        let mut kw_idx = 0;
        while kw_idx < kw.len() && pos < input_bytes.len() {
            if kw[kw_idx] == b' ' {
                while pos < input_bytes.len()
                    && (input_bytes[pos] == b' ' || input_bytes[pos] == b'\t')
                {
                    pos += 1;
                }
                kw_idx += 1;
                continue;
            }
            if input_bytes[pos] != kw[kw_idx] {
                return false;
            }
            pos += 1;
            kw_idx += 1;
        }
        if kw_idx == kw.len() {
            self.position = pos;
            return true;
        }
        false
    }

    /// Reads a keyword or identifier from the input.
    ///
    /// This method first checks for multi-word keywords, then single-word keywords, and finally
    /// returns an identifier if no keyword matches.
    #[inline(always)]
    fn read_keyword_or_identifier(&mut self) -> TokenKind<'a> {
        const IF_TO_SAY: &[u8] = b"if to say";
        const IF_NOT_SO: &[u8] = b"if not so";
        const SMALL_PASS: &[u8] = b"small pass";
        if self.match_multiword(IF_TO_SAY) {
            return TokenKind::IfToSay;
        }
        if self.match_multiword(IF_NOT_SO) {
            return TokenKind::IfNotSo;
        }
        if self.match_multiword(SMALL_PASS) {
            return TokenKind::SmallPass;
        }
        let ident = self.read_identifier_slice();
        // SAFETY: The identifier slice is valid for the input lifetime.
        let ident: &'a str = unsafe { std::mem::transmute::<&str, &'a str>(ident) };
        match ident {
            "make" => TokenKind::Make,
            "get" => TokenKind::Get,
            "shout" => TokenKind::Shout,
            "jasi" => TokenKind::Jasi,
            "start" => TokenKind::Start,
            "end" => TokenKind::End,
            "add" => TokenKind::Add,
            "minus" => TokenKind::Minus,
            "times" => TokenKind::Times,
            "divide" => TokenKind::Divide,
            "na" => TokenKind::Na,
            "pass" => TokenKind::Pass,
            _ => TokenKind::Identifier(ident),
        }
    }

    /// Handles whitespace by skipping it and returning the next token.
    #[inline(always)]
    fn handle_whitespace(&mut self) -> LexResult<'a, Token<'a>> {
        self.skip_whitespace();
        match self.next_token_jump() {
            Some(res) => res,
            None => Ok(Token { kind: TokenKind::Eof, span: self.single_char_span() }),
        }
    }
    /// Handles alphabetic input by reading a keyword or identifier.
    #[inline(always)]
    fn handle_alpha(&mut self) -> LexResult<'a, Token<'a>> {
        let start = self.position;
        let kind = self.read_keyword_or_identifier();
        Ok(Token { kind, span: self.span_from(start) })
    }
    /// Handles digit input by reading a number literal.
    #[inline(always)]
    fn handle_digit(&mut self) -> LexResult<'a, Token<'a>> {
        let start = self.position;
        let number = self.read_number()?;
        Ok(Token { kind: TokenKind::Number(number), span: self.span_from(start) })
    }
    /// Handles newline characters.
    #[inline(always)]
    fn handle_newline(&mut self) -> LexResult<'a, Token<'a>> {
        let start = self.position;
        self.advance_char();
        Ok(Token { kind: TokenKind::Newline, span: self.span_from(start) })
    }
    /// Handles left parenthesis.
    #[inline(always)]
    fn handle_left_paren(&mut self) -> LexResult<'a, Token<'a>> {
        let start = self.position;
        self.advance_char();
        Ok(Token { kind: TokenKind::LeftParen, span: self.span_from(start) })
    }
    /// Handles right parenthesis.
    #[inline(always)]
    fn handle_right_paren(&mut self) -> LexResult<'a, Token<'a>> {
        let start = self.position;
        self.advance_char();
        Ok(Token { kind: TokenKind::RightParen, span: self.span_from(start) })
    }
    /// Handles end-of-file by marking the lexer as done.
    #[inline(always)]
    fn handle_eof(&mut self) -> LexResult<'a, Token<'a>> {
        self.done = true;
        Ok(Token { kind: TokenKind::Eof, span: self.single_char_span() })
    }
    /// Handles unexpected or invalid input by producing a lexical error.
    #[inline(always)]
    fn handle_error(&mut self) -> LexResult<'a, Token<'a>> {
        let ch = self.current_char().unwrap();
        let err =
            self.error_with_span(LexErrorKind::UnexpectedCharacter(ch), self.single_char_span());
        self.advance_char();
        Err(err)
    }
    /// Constructs a lexical error at the current position with a given span.
    #[inline(always)]
    fn error_with_span(&self, kind: LexErrorKind<'a>, span: Span) -> LexError<'a> {
        LexError::new(kind, span, self.line, self.column)
    }

    /// The main lexing loop: dispatches to the appropriate handler based on the current input.
    ///
    /// This function is optimized for ASCII input but also supports Unicode. It returns `None` when
    /// the end of input is reached.
    #[allow(clippy::needless_return)]
    fn next_token_jump(&mut self) -> Option<LexResult<'a, Token<'a>>> {
        if self.done {
            return None;
        }
        let b = self.current_byte();
        if let Some(b) = b {
            if b < 0x80 {
                match b {
                    b' ' | b'\t' | b'\r' => Some(self.handle_whitespace()),
                    b'\n' => Some(self.handle_newline()),
                    b'(' => Some(self.handle_left_paren()),
                    b')' => Some(self.handle_right_paren()),
                    b'a'..=b'z' | b'A'..=b'Z' | b'_' => Some(self.handle_alpha()),
                    b'0'..=b'9' => Some(self.handle_digit()),
                    _ => Some(self.handle_error()),
                }
            } else {
                // Unicode-aware path for non-ASCII input.
                let ch = self.current_char().unwrap();
                if ch.is_whitespace() {
                    self.advance_char();
                    Some(self.next_token_jump().unwrap_or(Ok(Token {
                        kind: TokenKind::Eof,
                        span: Span { start: self.position, end: self.position },
                    })))
                } else if ch.is_alphabetic() || ch == '_' {
                    return Some(self.handle_alpha());
                } else if ch.is_numeric() {
                    return Some(self.handle_digit());
                } else {
                    return Some(self.handle_error());
                }
            }
        } else {
            Some(self.handle_eof())
        }
    }
}

/// Implements the `Iterator` trait for the lexer, producing a stream of tokens or errors.
impl<'a> Iterator for Lexer<'a> {
    type Item = LexResult<'a, Token<'a>>;
    fn next(&mut self) -> Option<Self::Item> {
        self.next_token_jump()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn kind_vec(tokens: Vec<Token>) -> Vec<TokenKind> {
        tokens.into_iter().map(|t| t.kind).collect()
    }

    #[test]
    fn test_basic_tokenization() {
        let lexer = Lexer::new("make x get 5");
        let tokens: Vec<_> = lexer.collect::<Result<Vec<_>, _>>().unwrap();
        assert_eq!(
            kind_vec(tokens),
            vec![
                TokenKind::Make,
                TokenKind::Identifier("x"),
                TokenKind::Get,
                TokenKind::Number(5.0),
                TokenKind::Eof
            ]
        );
    }

    #[test]
    fn test_expression_tokenization() {
        let lexer = Lexer::new("x add 3 times 2");
        let tokens: Vec<_> = lexer.collect::<Result<Vec<_>, _>>().unwrap();
        assert_eq!(
            kind_vec(tokens),
            vec![
                TokenKind::Identifier("x"),
                TokenKind::Add,
                TokenKind::Number(3.0),
                TokenKind::Times,
                TokenKind::Number(2.0),
                TokenKind::Eof
            ]
        );
    }

    #[test]
    fn test_conditional_tokenization() {
        let lexer = Lexer::new("if to say (x na 5)");
        let tokens: Vec<_> = lexer.collect::<Result<Vec<_>, _>>().unwrap();
        assert_eq!(
            kind_vec(tokens),
            vec![
                TokenKind::IfToSay,
                TokenKind::LeftParen,
                TokenKind::Identifier("x"),
                TokenKind::Na,
                TokenKind::Number(5.0),
                TokenKind::RightParen,
                TokenKind::Eof
            ]
        );
    }

    #[test]
    fn test_lexical_errors() {
        let lexer = Lexer::new("make x get @");
        let result: Result<Vec<_>, _> = lexer.collect();
        assert!(result.is_err());
        let error = result.unwrap_err();
        assert_eq!(error.kind, LexErrorKind::UnexpectedCharacter('@'));
        assert_eq!(error.line, 1);
        assert_eq!(error.column, 12);
    }

    #[test]
    fn test_invalid_number() {
        let mut lexer = Lexer::new("123.456.789");
        let token = lexer.next().unwrap();
        assert!(token.is_ok());
        assert_eq!(token.unwrap().kind, TokenKind::Number(123.456));
    }

    #[test]
    fn test_multi_word_keywords() {
        let lexer = Lexer::new("small pass");
        let tokens: Vec<_> = lexer.collect::<Result<Vec<_>, _>>().unwrap();
        assert_eq!(kind_vec(tokens), vec![TokenKind::SmallPass, TokenKind::Eof]);
    }

    #[test]
    fn bench_latency() {
        let mut input = String::with_capacity(2_000_000);
        for i in 0..100_000 {
            input.push_str("make x get ");
            input.push_str(&i.to_string());
            input.push('\n');
        }
        let start = std::time::Instant::now();
        let lexer = Lexer::new(&input);
        let tokens: Result<Vec<_>, _> = lexer.collect();
        let elapsed = start.elapsed();
        assert!(tokens.is_ok());
        println!("Lexed 100,000 lines in {elapsed:?}")
    }
}
