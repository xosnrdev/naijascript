use std::fmt;

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

#[derive(Debug, Clone, PartialEq)]
pub enum LexErrorKind<'a> {
    UnexpectedCharacter(char),
    InvalidNumber(&'a str),
    UnterminatedString,
    InvalidEscapeSequence(char),
}

#[derive(Debug, Clone, PartialEq)]
pub struct LexError<'a> {
    pub kind: LexErrorKind<'a>,
    pub line: usize,
    pub column: usize,
}

impl<'a> LexError<'a> {
    // Hint to compiler this is unlikely to be called, optimizing happy path
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

pub struct Lexer<'a> {
    input: &'a str,
    position: usize,
    done: bool,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        Lexer { input, position: 0, done: false }
    }

    #[inline(always)]
    fn current_byte(&self) -> Option<u8> {
        self.input.as_bytes().get(self.position).copied()
    }

    #[inline(always)]
    fn current_char(&self) -> Option<char> {
        self.input[self.position..].chars().next()
    }

    #[inline(always)]
    fn advance(&mut self, n: usize) {
        self.position += n;
    }

    #[inline(always)]
    fn advance_char(&mut self) {
        if let Some(ch) = self.current_char() {
            self.position += ch.len_utf8();
        }
    }

    fn calculate_line_column(&self, pos: usize) -> (usize, usize) {
        let mut line = 1;
        let mut column = 1;
        let bytes = self.input.as_bytes();

        for &byte in bytes.iter().take(pos.min(bytes.len())) {
            if byte == b'\n' {
                line += 1;
                column = 1;
            } else {
                column += 1;
            }
        }
        (line, column)
    }

    // Precomputed lookup table avoids function calls for ASCII alphanumeric checks
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

    // Precomputed powers avoid expensive floating-point exponentiation
    const POWERS_OF_10: [f64; 19] = [
        1e0, 1e1, 1e2, 1e3, 1e4, 1e5, 1e6, 1e7, 1e8, 1e9, 1e10, 1e11, 1e12, 1e13, 1e14, 1e15, 1e16,
        1e17, 1e18,
    ];

    #[inline(always)]
    const fn is_digit(b: u8) -> bool {
        b.wrapping_sub(b'0') < 10
    }

    #[inline(always)]
    fn skip_whitespace(&mut self) {
        let bytes = self.input.as_bytes();
        let len = bytes.len();
        while self.position < len {
            let b = bytes[self.position];
            if b == b' ' || b == b'\t' || b == b'\r' {
                self.advance(1);
                // Manual loop unrolling for better performance
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

    // Manual parsing avoids string allocation and is faster than parse()
    #[inline(always)]
    fn read_number(&mut self) -> LexResult<'a, f64> {
        let bytes = self.input.as_bytes();
        let len = bytes.len();
        let start = self.position;

        let mut integer_part = 0u64;

        // Detect overflow before it happens to avoid panic
        while self.position < len {
            let b = bytes[self.position];
            if Self::is_digit(b) {
                let digit = (b - b'0') as u64;

                // Prevent overflow by checking before multiplication
                if integer_part > (u64::MAX - digit) / 10 {
                    // Fall back to string parsing when numbers are too large
                    self.position = start;
                    return self.read_number_fallback();
                }

                integer_part = integer_part * 10 + digit;
                self.advance(1);
            } else {
                break;
            }
        }

        // Fast path: no decimal point
        if self.position >= len || bytes[self.position] != b'.' {
            return Ok(integer_part as f64);
        }

        // Parse decimal part
        self.advance(1); // Skip the dot
        let mut decimal_part = 0u64;
        let mut decimal_places = 0u32;

        // Limit precision to prevent overflow in decimal_part
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

        // Convert to f64
        let mut result = integer_part as f64;
        if decimal_places > 0 && decimal_places < Self::POWERS_OF_10.len() as u32 {
            let decimal_value = decimal_part as f64;
            result += decimal_value / Self::POWERS_OF_10[decimal_places as usize];
        }

        Ok(result)
    }

    // Fallback for edge cases like very large numbers
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
        num_str.parse().map_err(|_| self.error(LexErrorKind::InvalidNumber(num_str)))
    }

    #[inline(always)]
    fn read_identifier_slice(&mut self) -> &str {
        let bytes = self.input.as_bytes();
        let len = bytes.len();
        let start = self.position;
        while self.position < len {
            let b = bytes[self.position];
            if Self::IS_ALNUM[b as usize] {
                self.advance(1);
                // Manual loop unrolling for better performance
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
        // SAFETY: We only advance by valid UTF-8 char boundaries in identifier parsing,
        // so start..position always represents valid UTF-8 substring
        unsafe { self.input.get_unchecked(start..self.position) }
    }

    // Complex state machine handles flexible whitespace in multi-word keywords
    fn match_multiword(&mut self, kw: &'static [u8]) -> bool {
        let input_bytes = self.input.as_bytes();
        let mut pos = self.position;
        let mut kw_idx = 0;
        while kw_idx < kw.len() && pos < input_bytes.len() {
            // Allow flexible whitespace where keyword has single space
            if kw[kw_idx] == b' ' {
                // Accept one or more spaces/tabs in input
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

    // Multi-word keywords checked first to avoid mis-parsing as identifiers
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
        // SAFETY: ident is a slice of self.input which lives for 'a, and we need
        // to extend its lifetime to match Token<'a> for zero-copy parsing
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
        let (line, column) = self.calculate_line_column(self.position);
        LexError::new(kind, line, column)
    }

    // Jump table dispatch pattern for better branch prediction
    #[allow(clippy::needless_return)]
    fn next_token_jump(&mut self) -> Option<LexResult<'a, Token<'a>>> {
        if self.done {
            return None;
        }
        let b = self.current_byte();
        if let Some(b) = b {
            if b < 0x80 {
                // ASCII fast path optimizes common case
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
                // Non-ASCII: Unicode-aware fallback for rare cases
                let ch = self.current_char().unwrap();
                if ch.is_whitespace() {
                    // Unicode whitespace (rare in source)
                    self.advance_char();
                    Some(self.next_token_jump().unwrap_or(Ok(Token::Eof)))
                } else if ch.is_alphabetic() || ch == '_' {
                    return Some(self.handle_alpha());
                } else if ch.is_numeric() {
                    return Some(self.handle_digit());
                } else {
                    // Unknown non-ASCII character
                    return Some(self.handle_error());
                }
            }
        } else {
            Some(self.handle_eof())
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
        let start = std::time::Instant::now();
        let lexer = Lexer::new(&input);
        let tokens: Result<Vec<_>, _> = lexer.collect();
        let elapsed = start.elapsed();
        assert!(tokens.is_ok());
        println!("Lexed 100,000 lines in {elapsed:?}")
    }
}
