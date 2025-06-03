use std::borrow::Cow;
use std::fmt;
use std::iter::Peekable;
use std::str::CharIndices;

#[derive(Debug, PartialEq)]
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
    Identifier(Cow<'static, str>),
    Number(f64),
    LeftParen,
    RightParen,
    Newline,
    Eof,
}

#[derive(Debug, PartialEq)]
pub enum LexErrorKind {
    UnexpectedCharacter(char),
    InvalidNumber(String),
    UnterminatedString,
    InvalidEscapeSequence(char),
}

#[derive(Debug, PartialEq)]
pub struct LexError {
    pub kind: LexErrorKind,
    pub line: usize,
    pub column: usize,
}

impl LexError {
    fn new(kind: LexErrorKind, line: usize, column: usize) -> Self {
        Self { kind, line, column }
    }
}

impl fmt::Display for LexError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let message = match &self.kind {
            LexErrorKind::UnexpectedCharacter(ch) => format!("Wetin be dis character: '{ch}'"),
            LexErrorKind::InvalidNumber(num) => format!("Wetin be dis number: '{num}'"),
            LexErrorKind::UnterminatedString => "You no close dis string".to_string(),
            LexErrorKind::InvalidEscapeSequence(ch) => format!("Wetin be dis escape: '\\{ch}'"),
        };
        write!(
            f,
            "{} (line {}, column {})",
            message, self.line, self.column
        )
    }
}

impl std::error::Error for LexError {}

pub type LexResult<T> = Result<T, LexError>;

pub struct Lexer<'a> {
    chars: Peekable<CharIndices<'a>>,
    input: &'a str,
    position: usize,
    line: usize,
    column: usize,
    done: bool,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        Lexer {
            chars: input.char_indices().peekable(),
            input,
            position: 0,
            line: 1,
            column: 1,
            done: false,
        }
    }

    #[inline]
    fn current_char(&mut self) -> Option<char> {
        self.chars.peek().map(|&(_, ch)| ch)
    }

    #[inline]
    fn advance(&mut self) {
        if let Some((idx, ch)) = self.chars.next() {
            self.position = idx + ch.len_utf8();
            if ch == '\n' {
                self.line += 1;
                self.column = 1;
            } else {
                self.column += 1;
            }
        }
    }

    #[inline]
    fn skip_whitespace(&mut self) {
        while let Some(&(_, ch)) = self.chars.peek() {
            if ch.is_whitespace() && ch != '\n' {
                self.advance();
            } else {
                break;
            }
        }
    }

    fn error(&self, kind: LexErrorKind) -> LexError {
        LexError::new(kind, self.line, self.column)
    }

    fn read_number(&mut self) -> LexResult<f64> {
        let start = self.position;
        let mut seen_dot = false;

        while let Some(ch) = self.current_char() {
            if ch.is_ascii_digit() {
                self.advance();
            } else if ch == '.' && !seen_dot {
                seen_dot = true;
                self.advance();
            } else {
                break;
            }
        }

        let num_str = &self.input[start..self.position];
        num_str
            .parse()
            .map_err(|_| self.error(LexErrorKind::InvalidNumber(num_str.to_string())))
    }

    fn read_identifier_slice(&mut self) -> &str {
        let start = self.position;
        while let Some(ch) = self.current_char() {
            if matches!(ch, 'a'..='z' | 'A'..='Z' | '0'..='9' | '_') {
                self.advance();
            } else {
                break;
            }
        }
        // SAFETY: start and self.position are always valid UTF-8 boundaries as they are advanced by char indices.
        unsafe { self.input.get_unchecked(start..self.position) }
    }

    fn match_phrase(&mut self, words: &[&str]) -> bool {
        // Intent: Use iterator clone for lookahead to match multi-word keywords without mutating the main lexer state unless a match is found.
        let mut chars_clone = self.chars.clone();
        let mut pos = self.position;
        let mut line = self.line;
        let mut column = self.column;
        for (i, word) in words.iter().enumerate() {
            if !self.input[pos..].starts_with(word) {
                return false;
            }
            for _ in 0..word.len() {
                if let Some((idx, ch)) = chars_clone.next() {
                    pos = idx + ch.len_utf8();
                    if ch == '\n' {
                        line += 1;
                        column = 1;
                    } else {
                        column += 1;
                    }
                }
            }
            if i < words.len() - 1 {
                while let Some(&(_, ch)) = chars_clone.peek() {
                    if ch.is_whitespace() && ch != '\n' {
                        if let Some((idx, ch)) = chars_clone.next() {
                            pos = idx + ch.len_utf8();
                            if ch == '\n' {
                                line += 1;
                                column = 1;
                            } else {
                                column += 1;
                            }
                        }
                    } else {
                        break;
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
        let ident = self.read_identifier_slice();
        Token::from(ident)
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = LexResult<Token>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.done {
            return None;
        }
        loop {
            match self.current_char() {
                None => {
                    self.done = true;
                    return Some(Ok(Token::Eof));
                }
                Some(ch) if ch.is_whitespace() && ch != '\n' => {
                    self.skip_whitespace();
                    continue;
                }
                Some('\n') => {
                    self.advance();
                    return Some(Ok(Token::Newline));
                }
                Some('(') => {
                    self.advance();
                    return Some(Ok(Token::LeftParen));
                }
                Some(')') => {
                    self.advance();
                    return Some(Ok(Token::RightParen));
                }
                Some(ch) if ch.is_ascii_digit() => {
                    return Some(self.read_number().map(Token::Number));
                }
                Some(ch) if ch.is_alphabetic() => {
                    return Some(Ok(self.read_keyword_or_identifier()));
                }
                Some(ch) => {
                    return Some(Err(self.error(LexErrorKind::UnexpectedCharacter(ch))));
                }
            }
        }
    }
}

impl From<&str> for Token {
    fn from(ident: &str) -> Self {
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
            _ => Token::Identifier(Cow::Owned(ident.to_owned())),
        }
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
            vec![
                Token::Make,
                Token::Identifier("x".to_string().into()),
                Token::Get,
                Token::Number(5.0),
                Token::Eof
            ]
        );
    }

    #[test]
    fn test_expression_tokenization() {
        let lexer = Lexer::new("x add 3 times 2");
        let tokens: Vec<_> = lexer.collect::<Result<Vec<_>, _>>().unwrap();
        assert_eq!(
            tokens,
            vec![
                Token::Identifier("x".to_string().into()),
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
                Token::Identifier("x".to_string().into()),
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
}
