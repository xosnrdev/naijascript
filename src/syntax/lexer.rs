use std::fmt;

/// All possible token types in NaijaScript.
/// Each variant represents a keyword, operator, literal, or punctuation.
#[derive(Debug, Clone, PartialEq)]
pub enum Token {
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
    Identifier { start: usize, end: usize },
    Number(f64),
    LeftParen,
    RightParen,
    Newline,
    Eof,
}

impl Token {
    /// Get the identifier as a &str from the original input.
    pub fn identifier_str<'a>(&self, input: &'a str) -> Option<&'a str> {
        match self {
            Token::Identifier { start, end } => Some(&input[*start..*end]),
            _ => None,
        }
    }
}

impl<'a> From<(&'a str, usize, usize)> for Token {
    fn from((_ident, start, end): (&'a str, usize, usize)) -> Self {
        Token::Identifier { start, end }
    }
}

impl fmt::Display for Token {
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
            Token::Identifier { start, end } => write!(f, "identifier({start}..{end})"),
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

type LexResult<'a, T> = Result<T, LexError<'a>>;

/// The main lexer struct for NaijaScript.
/// Implements Iterator to produce tokens from source code.
pub struct Lexer<'a> {
    pub input: &'a str,
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

    /// Get the next character and its byte length from the current position.
    #[inline(always)]
    fn next_char_and_len(&self) -> Option<(char, usize)> {
        let bytes = self.input.as_bytes();
        if self.position >= bytes.len() {
            return None;
        }
        let b = bytes[self.position];
        // Fast ASCII path
        if b < 0x80 {
            return Some((b as char, 1));
        }
        // Manual UTF-8 decode for first char
        let remaining = &bytes[self.position..];
        let len = remaining.len();
        if len >= 2 && (b & 0b1110_0000) == 0b1100_0000 {
            // 2-byte UTF-8
            let c = ((b & 0x1F) as u32) << 6 | (remaining[1] & 0x3F) as u32;
            if let Some(ch) = std::char::from_u32(c) {
                return Some((ch, 2));
            }
        } else if len >= 3 && (b & 0b1111_0000) == 0b1110_0000 {
            // 3-byte UTF-8
            let c = ((b & 0x0F) as u32) << 12
                | ((remaining[1] & 0x3F) as u32) << 6
                | (remaining[2] & 0x3F) as u32;
            if let Some(ch) = std::char::from_u32(c) {
                return Some((ch, 3));
            }
        } else if len >= 4 && (b & 0b1111_1000) == 0b1111_0000 {
            // 4-byte UTF-8
            let c = ((b & 0x07) as u32) << 18
                | ((remaining[1] & 0x3F) as u32) << 12
                | ((remaining[2] & 0x3F) as u32) << 6
                | (remaining[3] & 0x3F) as u32;
            if let Some(ch) = std::char::from_u32(c) {
                return Some((ch, 4));
            }
        }
        // Fallback: use .chars() (should be rare)
        self.input[self.position..].chars().next().map(|ch| (ch, ch.len_utf8()))
    }

    /// Get the current character without advancing the iterator.
    #[inline(always)]
    fn current_char(&self) -> Option<char> {
        self.next_char_and_len().map(|(ch, _)| ch)
    }

    /// Advance the iterator and update position, line, and column counters.
    #[inline(always)]
    fn advance(&mut self) {
        if let Some((ch, char_len)) = self.next_char_and_len() {
            self.position += char_len;
            if ch == '\n' {
                self.line += 1;
                self.column = 1;
            } else {
                self.column += 1;
            }
        }
    }

    /// Skip whitespace except for newlines.
    #[inline(always)]
    fn skip_whitespace(&mut self) {
        let bytes = self.input.as_bytes();
        let len = bytes.len();
        while self.position < len {
            let start = self.position;
            // Batch process as many ASCII whitespace as possible
            while self.position < len {
                let b = bytes[self.position];
                if b < 0x80 && CHAR_CLASS[b as usize] == CharClass::Whitespace as u8 {
                    self.position += 1;
                } else {
                    break;
                }
            }
            let skipped = self.position - start;
            if skipped > 0 {
                self.column += skipped;
                continue;
            }
            break;
        }
    }

    /// Parse an identifier as a range of the input.
    #[inline(always)]
    fn read_identifier_range(&mut self) -> (usize, usize) {
        let start = self.position;
        let bytes = self.input.as_bytes();
        let len = bytes.len();
        while self.position < len {
            let begin = self.position;
            // Batch process as many ASCII alpha as possible
            while self.position < len {
                let b = bytes[self.position];
                if b < 0x80 && CHAR_CLASS[b as usize] == CharClass::Alpha as u8 {
                    self.position += 1;
                } else {
                    break;
                }
            }
            let advanced = self.position - begin;
            if advanced > 0 {
                self.column += advanced;
                continue;
            }
            break;
        }
        (start, self.position)
    }

    /// Parse a number literal, supporting floating-point values.
    #[inline(always)]
    fn read_number(&mut self) -> LexResult<'a, f64> {
        let start = self.position;
        let mut seen_dot = false;
        while let Some((ch, _)) = self.next_char_and_len() {
            if ch.is_ascii_digit() {
                self.advance();
            } else if ch == '.' && !seen_dot {
                seen_dot = true;
                self.advance();
            } else {
                break;
            }
        }
        let num_str: &'a str = &self.input[start..self.position];
        num_str.parse().map_err(|_| self.error(LexErrorKind::InvalidNumber(num_str)))
    }

    /// Use iterator clone for lookahead to match multi-word keywords without mutating the main lexer state unless a match is found.
    #[inline(always)]
    fn match_phrase(&mut self, words: &[&str]) -> bool {
        let mut pos = self.position;
        let mut line = self.line;
        let mut column = self.column;
        let bytes = self.input.as_bytes();
        for (i, word) in words.iter().enumerate() {
            let wbytes = word.as_bytes();
            if pos + wbytes.len() > bytes.len() {
                return false;
            }
            // Fast ASCII path: compare bytes directly
            if wbytes.iter().all(|&b| b < 0x80) && bytes[pos..pos + wbytes.len()] == *wbytes {
                // Update pos, line, column
                for &b in &bytes[pos..pos + wbytes.len()] {
                    if b == b'\n' {
                        line += 1;
                        column = 1;
                    } else {
                        column += 1;
                    }
                }
                pos += wbytes.len();
            } else {
                // Fallback: use .starts_with and .chars()
                if !self.input[pos..].starts_with(word) {
                    return false;
                }
                for ch in word.chars() {
                    let char_len = ch.len_utf8();
                    if ch == '\n' {
                        line += 1;
                        column = 1;
                    } else {
                        column += 1;
                    }
                    pos += char_len;
                }
            }
            if i < words.len() - 1 {
                // Skip whitespace except newlines
                while pos < bytes.len() {
                    let b = bytes[pos];
                    if b < 0x80 && CHAR_CLASS[b as usize] == CharClass::Whitespace as u8 {
                        column += 1;
                        pos += 1;
                    } else if b == b'\n' {
                        break;
                    } else {
                        // Fallback for non-ASCII
                        let ch = self.input[pos..].chars().next().unwrap();
                        if ch.is_whitespace() && ch != '\n' {
                            if ch == '\n' {
                                break;
                            }
                            column += 1;
                            pos += ch.len_utf8();
                        } else {
                            break;
                        }
                    }
                }
            }
        }
        while self.position < pos {
            self.advance();
        }
        self.line = line;
        self.column = column;
        true
    }

    /// Recognize multi-word keywords first, then fall back to single-word keywords or identifiers.
    #[inline(always)]
    fn read_keyword_or_identifier(&mut self) -> Token {
        if self.match_phrase(&["if", "to", "say"]) {
            return Token::IfToSay;
        }
        if self.match_phrase(&["if", "not", "so"]) {
            return Token::IfNotSo;
        }
        if self.match_phrase(&["small", "pass"]) {
            return Token::SmallPass;
        }
        let (start, end) = self.read_identifier_range();
        let ident = &self.input[start..end];
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
            _ => Token::Identifier { start, end },
        }
    }

    /// Helper to create a lexer error at the current position.
    #[cold]
    fn error(&self, kind: LexErrorKind<'a>) -> LexError<'a> {
        LexError::new(kind, self.line, self.column)
    }
}

// Character class codes for ASCII bytes
#[repr(u8)]
enum CharClass {
    Other = 0,
    Whitespace = 1,
    Newline = 2,
    Digit = 3,
    Alpha = 4,
    LeftParen = 5,
    RightParen = 6,
}

// Static lookup table for ASCII character classification
const CHAR_CLASS: [u8; 256] = {
    let mut table = [0u8; 256];
    let mut i = 0;
    while i < 256 {
        let c = i as u8;
        table[i] = if c == b' ' || c == b'\t' {
            CharClass::Whitespace as u8
        } else if c == b'\n' {
            CharClass::Newline as u8
        } else if c >= b'0' && c <= b'9' {
            CharClass::Digit as u8
        } else if (c >= b'a' && c <= b'z') || (c >= b'A' && c <= b'Z') || c == b'_' {
            CharClass::Alpha as u8
        } else if c == b'(' {
            CharClass::LeftParen as u8
        } else if c == b')' {
            CharClass::RightParen as u8
        } else {
            CharClass::Other as u8
        };
        i += 1;
    }
    table
};

/// Implements Iterator to produce tokens from source code, handling all lexing logic and errors.
impl<'a> Iterator for Lexer<'a> {
    type Item = LexResult<'a, Token>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.done {
            return None;
        }
        let bytes = self.input.as_bytes();
        let len = bytes.len();
        loop {
            if self.position >= len {
                self.done = true;
                return Some(Ok(Token::Eof));
            }
            let b = bytes[self.position];
            if b < 0x80 {
                match CHAR_CLASS[b as usize] {
                    x if x == CharClass::Whitespace as u8 => {
                        self.skip_whitespace();
                        continue;
                    }
                    x if x == CharClass::Newline as u8 => {
                        self.advance();
                        return Some(Ok(Token::Newline));
                    }
                    x if x == CharClass::LeftParen as u8 => {
                        self.advance();
                        return Some(Ok(Token::LeftParen));
                    }
                    x if x == CharClass::RightParen as u8 => {
                        self.advance();
                        return Some(Ok(Token::RightParen));
                    }
                    x if x == CharClass::Digit as u8 => {
                        return Some(self.read_number().map(Token::Number));
                    }
                    x if x == CharClass::Alpha as u8 => {
                        return Some(Ok(self.read_keyword_or_identifier()));
                    }
                    _ => {
                        // Unknown ASCII character
                        let ch = self.current_char().unwrap();
                        return Some(Err(self.error(LexErrorKind::UnexpectedCharacter(ch))));
                    }
                }
            } else {
                // Batch skip non-ASCII whitespace (except newline)
                if let Some(ch) = self.current_char() {
                    if ch.is_whitespace() && ch != '\n' {
                        self.skip_whitespace();
                        continue;
                    }
                    if ch == '\n' {
                        self.advance();
                        return Some(Ok(Token::Newline));
                    }
                    if ch == '(' {
                        self.advance();
                        return Some(Ok(Token::LeftParen));
                    }
                    if ch == ')' {
                        self.advance();
                        return Some(Ok(Token::RightParen));
                    }
                    if ch.is_ascii_digit() {
                        return Some(self.read_number().map(Token::Number));
                    }
                    if ch.is_alphabetic() {
                        return Some(Ok(self.read_keyword_or_identifier()));
                    }
                    // Unknown non-ASCII character
                    return Some(Err(self.error(LexErrorKind::UnexpectedCharacter(ch))));
                } else {
                    self.done = true;
                    return Some(Ok(Token::Eof));
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use std::time::Instant;

    use super::*;

    #[test]
    fn test_basic_tokenization() {
        let input = "make x get 5";
        let lexer = Lexer::new(input);
        let tokens: Vec<_> = lexer.collect::<Result<Vec<_>, _>>().unwrap();
        assert_eq!(tokens[0], Token::Make);
        assert!(matches!(tokens[1], Token::Identifier { .. }));
        assert_eq!(tokens[1].identifier_str(input), Some("x"));
        assert_eq!(tokens[2], Token::Get);
        assert_eq!(tokens[3], Token::Number(5.0));
        assert_eq!(tokens[4], Token::Eof);
    }

    #[test]
    fn test_expression_tokenization() {
        let input = "x add 3 times 2";
        let lexer = Lexer::new(input);
        let tokens: Vec<_> = lexer.collect::<Result<Vec<_>, _>>().unwrap();
        assert!(matches!(tokens[0], Token::Identifier { .. }));
        assert_eq!(tokens[0].identifier_str(input), Some("x"));
        assert_eq!(tokens[1], Token::Add);
        assert_eq!(tokens[2], Token::Number(3.0));
        assert_eq!(tokens[3], Token::Times);
        assert_eq!(tokens[4], Token::Number(2.0));
        assert_eq!(tokens[5], Token::Eof);
    }

    #[test]
    fn test_conditional_tokenization() {
        let input = "if to say (x na 5)";
        let lexer = Lexer::new(input);
        let tokens: Vec<_> = lexer.collect::<Result<Vec<_>, _>>().unwrap();
        assert_eq!(tokens[0], Token::IfToSay);
        assert_eq!(tokens[1], Token::LeftParen);
        assert!(matches!(tokens[2], Token::Identifier { .. }));
        assert_eq!(tokens[2].identifier_str(input), Some("x"));
        assert_eq!(tokens[3], Token::Na);
        assert_eq!(tokens[4], Token::Number(5.0));
        assert_eq!(tokens[5], Token::RightParen);
        assert_eq!(tokens[6], Token::Eof);
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
        assert_eq!(tokens[0], Token::SmallPass);
        assert_eq!(tokens[1], Token::Eof);
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
