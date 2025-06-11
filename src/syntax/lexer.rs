use std::fmt;

/// All possible token types in NaijaScript.
/// Each variant represents a keyword, operator, literal, or punctuation.
#[derive(Debug, Clone, PartialEq)]
pub enum Token<'a> {
    Make,
    Get,
    Shout,
    IfToSay,
    IfNotSo,
    Jasi,
    Start,
    End,
    Add,
    Minus,
    Times,
    Divide,
    Na,
    Pass,
    SmallPass,
    Identifier(&'a str),
    Number(f64),
    LeftParen,
    RightParen,
    Newline,
    Eof,
}

impl<'a> fmt::Display for Token<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Token::Make => write!(f, "make"),
            Token::Get => write!(f, "get"),
            Token::Shout => write!(f, "shout"),
            Token::IfToSay => write!(f, "if to say"),
            Token::IfNotSo => write!(f, "if not so"),
            Token::Jasi => write!(f, "jasi"),
            Token::Start => write!(f, "start"),
            Token::End => write!(f, "end"),
            Token::Add => write!(f, "add"),
            Token::Minus => write!(f, "minus"),
            Token::Times => write!(f, "times"),
            Token::Divide => write!(f, "divide"),
            Token::Na => write!(f, "na"),
            Token::Pass => write!(f, "pass"),
            Token::SmallPass => write!(f, "small pass"),
            Token::Identifier(ident) => write!(f, "identifier({ident})"),
            Token::Number(n) => write!(f, "number({n})"),
            Token::LeftParen => write!(f, "("),
            Token::RightParen => write!(f, ")"),
            Token::Newline => write!(f, "newline"),
            Token::Eof => write!(f, "eof"),
        }
    }
}

/// All possible error kinds that can occur during lexing.
#[derive(Debug, Clone, PartialEq)]
pub enum LexErrorKind<'a> {
    UnexpectedCharacter(char),
    InvalidNumber(&'a str),
    UnterminatedString,
    InvalidEscapeSequence(char),
}

/// Error type for lexer errors, including line and column for diagnostics.
#[derive(Debug, Clone, PartialEq)]
pub struct LexError<'a> {
    pub kind: LexErrorKind<'a>,
    pub line: usize,
    pub column: usize,
}

impl<'a> LexError<'a> {
    #[cold]
    fn new(kind: LexErrorKind<'a>, line: usize, column: usize) -> Self {
        Self { kind, line, column }
    }
}

impl<'a> fmt::Display for LexError<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let message = match &self.kind {
            LexErrorKind::UnexpectedCharacter(ch) => format!("Wetin be dis character: '{ch}'"),
            LexErrorKind::InvalidNumber(num) => format!("Wetin be dis number: '{num}'"),
            LexErrorKind::UnterminatedString => "You no close dis string".to_string(),
            LexErrorKind::InvalidEscapeSequence(ch) => format!("Wetin be dis escape: '\\{ch}'"),
        };
        write!(f, "{} (line {}, column {})", message, self.line, self.column)
    }
}

impl<'a> std::error::Error for LexError<'a> {}

pub type LexResult<'a, T> = Result<T, LexError<'a>>;

/// The main lexer struct for NaijaScript.
/// Implements Iterator to produce tokens from source code.
pub struct Lexer<'a> {
    input: &'a str,
    position: usize,
    line: usize,
    column: usize,
    done: bool,
}

impl<'a> Lexer<'a> {
    /// Create a new lexer from an input string.
    pub fn new(input: &'a str) -> Self {
        Lexer { input, position: 0, line: 1, column: 1, done: false }
    }

    /// Get the current byte at position, or None if at end.
    #[inline(always)]
    fn current_byte(&self) -> Option<u8> {
        self.input.as_bytes().get(self.position).copied()
    }

    /// Get the current character at position, None if at end.
    #[inline(always)]
    fn current_char(&self) -> Option<char> {
        self.input[self.position..].chars().next()
    }

    /// Advance by n bytes, updating line/column.
    #[inline(always)]
    fn advance(&mut self, n: usize) {
        for _ in 0..n {
            if let Some(b) = self.current_byte() {
                if b == b'\n' {
                    self.line += 1;
                    self.column = 1;
                } else {
                    self.column += 1;
                }
                self.position += 1;
            }
        }
    }

    /// Advance by one character (may be multiple bytes).
    #[inline(always)]
    fn advance_char(&mut self) {
        if let Some(ch) = self.current_char() {
            let len = ch.len_utf8();
            self.advance(len);
        }
    }

    /// Static lookup table for ASCII alphanumeric and underscore
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

    /// Skip whitespace except for newlines.
    #[inline(always)]
    fn skip_whitespace(&mut self) {
        let bytes = self.input.as_bytes();
        let len = bytes.len();
        while self.position < len {
            let b = bytes[self.position];
            if b == b' ' || b == b'\t' || b == b'\r' {
                self.advance(1);
                // Try to skip up to 3 more bytes
                for _ in 0..3 {
                    if self.position < len {
                        let b2 = bytes[self.position];
                        if b2 == b' ' || b2 == b'\t' || b2 == b'\r' {
                            self.advance(1);
                        } else {
                            break;
                        }
                    }
                }
            } else {
                break;
            }
        }
    }

    /// Parse a number literal, supporting floating-point values.
    #[inline(always)]
    fn read_number(&mut self) -> LexResult<'a, f64> {
        let bytes = self.input.as_bytes();
        let len = bytes.len();
        let start = self.position;
        let mut seen_dot = false;
        while self.position < len {
            let b = bytes[self.position];
            if b.is_ascii_digit() {
                self.advance(1);
                // Try to skip up to 3 more digits
                for _ in 0..3 {
                    if self.position < len {
                        let b2 = bytes[self.position];
                        if b2.is_ascii_digit() {
                            self.advance(1);
                        } else {
                            break;
                        }
                    }
                }
            } else if b == b'.' && !seen_dot {
                seen_dot = true;
                self.advance(1);
            } else {
                break;
            }
        }
        let num_str = &self.input[start..self.position];
        num_str.parse().map_err(|_| self.error(LexErrorKind::InvalidNumber(num_str)))
    }

    /// Parse an identifier as a slice of the input.
    #[inline(always)]
    fn read_identifier_slice(&mut self) -> &str {
        let bytes = self.input.as_bytes();
        let len = bytes.len();
        let start = self.position;
        while self.position < len {
            let b = bytes[self.position];
            if Self::IS_ALNUM[b as usize] {
                self.advance(1);
                // Try to skip up to 3 more bytes
                for _ in 0..3 {
                    if self.position < len {
                        let b2 = bytes[self.position];
                        if Self::IS_ALNUM[b2 as usize] {
                            self.advance(1);
                        } else {
                            break;
                        }
                    }
                }
            } else {
                break;
            }
        }
        // SAFETY: start and self.position are always valid UTF-8 boundaries as we only advance by char boundaries.
        unsafe { self.input.get_unchecked(start..self.position) }
    }

    /// Fast byte-based check for multi-word keywords at the current position.
    fn match_multiword(&mut self, kw: &'static [u8]) -> bool {
        let input_bytes = self.input.as_bytes();
        let mut pos = self.position;
        let mut kw_idx = 0;
        let mut line = self.line;
        let mut column = self.column;
        while kw_idx < kw.len() && pos < input_bytes.len() {
            // Skip whitespace in input if present in keyword as space
            if kw[kw_idx] == b' ' {
                // Accept one or more spaces/tabs in input
                while pos < input_bytes.len()
                    && (input_bytes[pos] == b' ' || input_bytes[pos] == b'\t')
                {
                    pos += 1;
                    column += 1;
                }
                kw_idx += 1;
                continue;
            }
            if input_bytes[pos] != kw[kw_idx] {
                return false;
            }
            if input_bytes[pos] == b'\n' {
                line += 1;
                column = 1;
            } else {
                column += 1;
            }
            pos += 1;
            kw_idx += 1;
        }
        if kw_idx == kw.len() {
            while self.position < pos {
                self.advance(1);
            }
            self.line = line;
            self.column = column;
            return true;
        }
        false
    }

    /// Recognize multi-word keywords first, then fall back to single-word keywords or identifiers.
    #[inline(always)]
    fn read_keyword_or_identifier(&mut self) -> Token<'a> {
        // Multi-word keyword byte patterns
        const IF_TO_SAY: &[u8] = b"if to say";
        const IF_NOT_SO: &[u8] = b"if not so";
        const SMALL_PASS: &[u8] = b"small pass";
        if self.match_multiword(IF_TO_SAY) {
            return Token::IfToSay;
        }
        if self.match_multiword(IF_NOT_SO) {
            return Token::IfNotSo;
        }
        if self.match_multiword(SMALL_PASS) {
            return Token::SmallPass;
        }
        // Single-word: extract identifier, then match against static byte arrays
        let ident = self.read_identifier_slice();
        // SAFETY: ident is a slice of self.input, which lives at least as long as 'a.
        let ident: &'a str = unsafe { std::mem::transmute::<&str, &'a str>(ident) };
        match ident {
            "make" => Token::Make,
            "get" => Token::Get,
            "shout" => Token::Shout,
            "jasi" => Token::Jasi,
            "start" => Token::Start,
            "end" => Token::End,
            "add" => Token::Add,
            "minus" => Token::Minus,
            "times" => Token::Times,
            "divide" => Token::Divide,
            "na" => Token::Na,
            "pass" => Token::Pass,
            _ => Token::Identifier(ident),
        }
    }

    #[inline(always)]
    fn handle_whitespace(&mut self) -> LexResult<'a, Token<'a>> {
        self.skip_whitespace();
        // After skipping, dispatch again (tail call):
        // But must return a Result, so call next_token_jump and unwrap Option
        match self.next_token_jump() {
            Some(res) => res,
            None => Ok(Token::Eof),
        }
    }
    #[inline(always)]
    fn handle_alpha(&mut self) -> LexResult<'a, Token<'a>> {
        Ok(self.read_keyword_or_identifier())
    }
    #[inline(always)]
    fn handle_digit(&mut self) -> LexResult<'a, Token<'a>> {
        self.read_number().map(Token::Number)
    }
    #[inline(always)]
    fn handle_newline(&mut self) -> LexResult<'a, Token<'a>> {
        self.advance_char();
        Ok(Token::Newline)
    }
    #[inline(always)]
    fn handle_left_paren(&mut self) -> LexResult<'a, Token<'a>> {
        self.advance_char();
        Ok(Token::LeftParen)
    }
    #[inline(always)]
    fn handle_right_paren(&mut self) -> LexResult<'a, Token<'a>> {
        self.advance_char();
        Ok(Token::RightParen)
    }
    #[inline(always)]
    fn handle_eof(&mut self) -> LexResult<'a, Token<'a>> {
        self.done = true;
        Ok(Token::Eof)
    }
    #[inline(always)]
    fn handle_error(&mut self) -> LexResult<'a, Token<'a>> {
        let ch = self.current_char().unwrap();
        let err = self.error(LexErrorKind::UnexpectedCharacter(ch));
        self.advance_char();
        Err(err)
    }
    #[inline(always)]
    fn error(&self, kind: LexErrorKind<'a>) -> LexError<'a> {
        LexError::new(kind, self.line, self.column)
    }

    fn next_token_jump(&mut self) -> Option<LexResult<'a, Token<'a>>> {
        if self.done {
            return None;
        }
        let b = self.current_byte();
        match b {
            Some(b' ' | b'\t' | b'\r') => Some(self.handle_whitespace()),
            Some(b'\n') => Some(self.handle_newline()),
            Some(b'(') => Some(self.handle_left_paren()),
            Some(b')') => Some(self.handle_right_paren()),
            Some(b) if (b as char).is_ascii_alphabetic() || b == b'_' => Some(self.handle_alpha()),
            Some(b) if (b as char).is_ascii_digit() => Some(self.handle_digit()),
            None => Some(self.handle_eof()),
            Some(_) => Some(self.handle_error()),
        }
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = LexResult<'a, Token<'a>>;
    fn next(&mut self) -> Option<Self::Item> {
        self.next_token_jump()
    }
}

#[cfg(test)]
mod tests {
    use std::time::Instant;

    use super::*;

    #[test]
    fn test_basic_tokenization() {
        let lexer = Lexer::new("make x get 5");
        let tokens: Vec<_> = lexer.collect::<Result<Vec<_>, _>>().unwrap();
        assert_eq!(
            tokens,
            vec![Token::Make, Token::Identifier("x"), Token::Get, Token::Number(5.0), Token::Eof]
        );
    }

    #[test]
    fn test_expression_tokenization() {
        let lexer = Lexer::new("x add 3 times 2");
        let tokens: Vec<_> = lexer.collect::<Result<Vec<_>, _>>().unwrap();
        assert_eq!(
            tokens,
            vec![
                Token::Identifier("x"),
                Token::Add,
                Token::Number(3.0),
                Token::Times,
                Token::Number(2.0),
                Token::Eof
            ]
        );
    }

    #[test]
    fn test_conditional_tokenization() {
        let lexer = Lexer::new("if to say (x na 5)");
        let tokens: Vec<_> = lexer.collect::<Result<Vec<_>, _>>().unwrap();
        assert_eq!(
            tokens,
            vec![
                Token::IfToSay,
                Token::LeftParen,
                Token::Identifier("x"),
                Token::Na,
                Token::Number(5.0),
                Token::RightParen,
                Token::Eof
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
        assert_eq!(token.unwrap(), Token::Number(123.456));
    }

    #[test]
    fn test_multi_word_keywords() {
        let lexer = Lexer::new("small pass");
        let tokens: Vec<_> = lexer.collect::<Result<Vec<_>, _>>().unwrap();
        assert_eq!(tokens, vec![Token::SmallPass, Token::Eof]);
    }

    #[test]
    fn bench_large_input_lexing() {
        let mut input = String::with_capacity(2_000_000);
        for i in 0..100_000 {
            input.push_str("make x get ");
            input.push_str(&i.to_string());
            input.push('\n');
        }
        let start = Instant::now();
        let lexer = Lexer::new(&input);
        let tokens: Result<Vec<_>, _> = lexer.collect();
        let elapsed = start.elapsed();
        assert!(tokens.is_ok());
        println!("Lexed 100,000 lines in {elapsed:?}")
    }
}
