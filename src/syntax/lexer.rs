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
    Identifier(String),
    Number(f64),
    LeftParen,
    RightParen,
    Newline,
    Eof,
}

#[derive(Debug, Clone, PartialEq)]
pub enum LexErrorKind {
    UnexpectedCharacter(char),
    InvalidNumber(String),
    UnterminatedString,
    InvalidEscapeSequence(char),
}

#[derive(Debug, Clone, PartialEq)]
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

impl std::fmt::Display for LexError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let message = match &self.kind {
            LexErrorKind::UnexpectedCharacter(ch) => {
                format!("Wetin be dis character: '{ch}'")
            }
            LexErrorKind::InvalidNumber(num) => {
                format!("Wetin be dis number: '{num}'")
            }
            LexErrorKind::UnterminatedString => "You no close dis string".to_string(),
            LexErrorKind::InvalidEscapeSequence(ch) => {
                format!("Wetin be dis escape: '\\{ch}'")
            }
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
    input: &'a str,
    bytes: &'a [u8],
    position: usize,
    line: usize,
    column: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        Lexer {
            input,
            bytes: input.as_bytes(),
            position: 0,
            line: 1,
            column: 1,
        }
    }

    fn current_char(&self) -> Option<char> {
        if self.position >= self.bytes.len() {
            None
        } else {
            Some(self.bytes[self.position] as char)
        }
    }

    fn advance(&mut self) {
        if self.position < self.bytes.len() {
            if self.bytes[self.position] == b'\n' {
                self.line += 1;
                self.column = 1;
            } else {
                self.column += 1;
            }
            self.position += 1;
        }
    }

    fn peek(&self) -> Option<char> {
        if self.position + 1 >= self.bytes.len() {
            None
        } else {
            Some(self.bytes[self.position + 1] as char)
        }
    }

    fn skip_whitespace(&mut self) {
        while let Some(ch) = self.current_char() {
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

        while let Some(ch) = self.current_char() {
            if ch.is_ascii_digit() {
                self.advance();
            } else {
                break;
            }
        }

        if self.current_char() == Some('.') && self.peek().is_some_and(|c| c.is_ascii_digit()) {
            self.advance();

            while let Some(ch) = self.current_char() {
                if ch.is_ascii_digit() {
                    self.advance();
                } else {
                    break;
                }
            }
        }

        let num_str = &self.input[start..self.position];
        num_str
            .parse()
            .map_err(|_| self.error(LexErrorKind::InvalidNumber(num_str.to_string())))
    }

    fn read_identifier(&mut self) -> String {
        let start = self.position;

        while let Some(ch) = self.current_char() {
            if ch.is_alphanumeric() || ch == '_' {
                self.advance();
            } else {
                break;
            }
        }

        self.input[start..self.position].to_string()
    }

    fn match_keyword(&mut self, expected: &str) -> bool {
        let start = self.position;
        let expected_bytes = expected.as_bytes();

        if start + expected_bytes.len() > self.bytes.len() {
            return false;
        }

        for (i, &byte) in expected_bytes.iter().enumerate() {
            if self.bytes[start + i] != byte {
                return false;
            }
        }

        let next_pos = start + expected_bytes.len();
        if next_pos < self.bytes.len() {
            let next_char = self.bytes[next_pos] as char;
            if next_char.is_alphanumeric() || next_char == '_' {
                return false;
            }
        }

        for _ in 0..expected_bytes.len() {
            self.advance();
        }
        true
    }

    fn read_keyword_or_identifier(&mut self) -> Token {
        let saved_pos = self.position;
        let saved_line = self.line;
        let saved_col = self.column;

        if self.match_keyword("if") {
            self.skip_whitespace();
            if self.match_keyword("to") {
                self.skip_whitespace();
                if self.match_keyword("say") {
                    return Token::IfToSay;
                }
            }
        }

        self.position = saved_pos;
        self.line = saved_line;
        self.column = saved_col;

        if self.match_keyword("if") {
            self.skip_whitespace();
            if self.match_keyword("not") {
                self.skip_whitespace();
                if self.match_keyword("so") {
                    return Token::IfNotSo;
                }
            }
        }

        self.position = saved_pos;
        self.line = saved_line;
        self.column = saved_col;

        if self.match_keyword("small") {
            self.skip_whitespace();
            if self.match_keyword("pass") {
                return Token::SmallPass;
            }
        }

        self.position = saved_pos;
        self.line = saved_line;
        self.column = saved_col;

        let ident = self.read_identifier();

        match ident.as_str() {
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

    pub fn next_token(&mut self) -> LexResult<Token> {
        loop {
            match self.current_char() {
                None => return Ok(Token::Eof),

                Some(' ') | Some('\t') | Some('\r') => {
                    self.skip_whitespace();
                    continue;
                }

                Some('\n') => {
                    self.advance();
                    return Ok(Token::Newline);
                }

                Some('(') => {
                    self.advance();
                    return Ok(Token::LeftParen);
                }

                Some(')') => {
                    self.advance();
                    return Ok(Token::RightParen);
                }

                Some(ch) if ch.is_ascii_digit() => {
                    return self.read_number().map(Token::Number);
                }

                Some(ch) if ch.is_alphabetic() => {
                    return Ok(self.read_keyword_or_identifier());
                }

                Some(ch) => {
                    return Err(self.error(LexErrorKind::UnexpectedCharacter(ch)));
                }
            }
        }
    }

    pub fn tokenize(&mut self) -> LexResult<Vec<Token>> {
        let mut tokens = Vec::new();

        loop {
            match self.next_token()? {
                Token::Eof => {
                    tokens.push(Token::Eof);
                    break;
                }
                token => tokens.push(token),
            }
        }

        Ok(tokens)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_basic_tokenization() {
        let mut lexer = Lexer::new("make x get 5");
        let tokens = lexer.tokenize().unwrap();

        assert_eq!(
            tokens,
            vec![
                Token::Make,
                Token::Identifier("x".to_string()),
                Token::Get,
                Token::Number(5.0),
                Token::Eof
            ]
        );
    }

    #[test]
    fn test_expression_tokenization() {
        let mut lexer = Lexer::new("x add 3 times 2");
        let tokens = lexer.tokenize().unwrap();

        assert_eq!(
            tokens,
            vec![
                Token::Identifier("x".to_string()),
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
        let mut lexer = Lexer::new("if to say (x na 5)");
        let tokens = lexer.tokenize().unwrap();

        assert_eq!(
            tokens,
            vec![
                Token::IfToSay,
                Token::LeftParen,
                Token::Identifier("x".to_string()),
                Token::Na,
                Token::Number(5.0),
                Token::RightParen,
                Token::Eof
            ]
        );
    }

    #[test]
    fn test_lexical_errors() {
        let mut lexer = Lexer::new("make x get @");
        let result = lexer.tokenize();
        assert!(result.is_err());

        let error = result.unwrap_err();
        assert_eq!(error.kind, LexErrorKind::UnexpectedCharacter('@'));
        assert_eq!(error.line, 1);
        assert_eq!(error.column, 12);
    }

    #[test]
    fn test_invalid_number() {
        let mut lexer = Lexer::new("123.456.789");
        let result = lexer.next_token();
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Token::Number(123.456));
    }

    #[test]
    fn test_multi_word_keywords() {
        let mut lexer = Lexer::new("small pass");
        let tokens = lexer.tokenize().unwrap();

        assert_eq!(tokens, vec![Token::SmallPass, Token::Eof]);
    }
}
