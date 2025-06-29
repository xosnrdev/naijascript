//! The lexer (or scanner) for NaijaScript.

use std::borrow::Cow;

use crate::diagnostics::{AsStr, Diagnostics, Label, Severity, Span};

/// Represents the type of lexical errors that can occur during tokenization
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
            LexError::UnexpectedChar => "Wetin be dis?",
            LexError::InvalidNumber => "Dis number no correct",
            LexError::InvalidIdentifier => "Dis name no correct",
            LexError::InvalidStringEscape => "Dis escape no make sense",
            LexError::UnterminatedString => "String never close",
        }
    }
}

/// All possible token types in NaijaScript
///
/// The lifetime parameter 'input ties token references to the source text lifetime,
/// letting us avoid copying strings for identifiers and numbers
#[derive(Debug, PartialEq)]
pub enum Token<'input> {
    // Keywords for variable declaration and assignment
    Make, // "make" - variable declaration
    Get,  // "get" - assignment operator

    // Arithmetic operators
    Add,    // "add" - addition
    Minus,  // "minus" - subtraction
    Times,  // "times" - multiplication
    Divide, // "divide" - division

    // I/O and control flow
    Shout, // "shout" - print to console
    Jasi,  // "jasi" - while loop construct (Nigerian slang for "keep going")
    Start, // "start" - block beginning
    End,   // "end" - block ending

    // Comparison operators
    Na,        // "na" - equality (Nigerian slang for "is")
    Pass,      // "pass" - greater than (Nigerian slang for "exceeds")
    SmallPass, // "small pass" - greater than or equal

    // Conditional constructs
    IfToSay, // "if to say" - if statement
    IfNotSo, // "if not so" - else statement

    // Punctuation
    LParen, // "("
    RParen, // ")"

    // Variable length tokens
    Identifier(&'input str),  // Variable names
    Number(&'input str),      // Numeric literals
    String(Cow<'input, str>), // String literals

    // Special tokens
    EOF, // End of file
}

/// Represents a token along with its location in the original source text.
#[derive(Debug, PartialEq)]
pub struct SpannedToken<'input> {
    pub token: Token<'input>,
    pub span: Span,
}

/// Our lexical analyzer that breaks source text into tokens
///
/// We use a byte-based scanner for performance rather than working with Unicode
/// characters directly. This is significantly faster for ASCII-heavy code.
pub struct Lexer<'input> {
    pub src: &'input str,    // Original source text (kept for slicing)
    pub pos: usize,          // Current position in the source
    pub errors: Diagnostics, // Collection of encountered errors
}

impl<'input> Lexer<'input> {
    /// Creates a new lexer from source text
    ///
    /// Using #[inline(always)] because this is a trivial constructor
    /// that gets called a lot during testing and benchmarks
    #[inline(always)]
    pub fn new(src: &'input str) -> Self {
        Lexer { src, pos: 0, errors: Diagnostics::default() }
    }

    /// Returns the next token and its span from the input.
    ///
    /// This is the main loop of the lexer. We process the input byte by byte,
    /// skipping whitespace, and then matching on the next character to decide
    /// what kind of token to produce. If we reach the end of input, we return EOF.
    /// For each token type (string, paren, number, identifier/keyword, or unexpected),
    /// we delegate to a helper function that handles the details.
    /// If we encounter something we don't recognize, we emit an error and keep going.
    #[inline(always)]
    pub fn next_token(&mut self) -> SpannedToken<'input> {
        loop {
            // Skip over any whitespace before the next token
            self.skip_whitespace();

            // Record starting position for spans and slicing
            let start = self.pos;

            // Check for EOF first
            let b = self.peek();
            if b == 0 {
                return SpannedToken { token: Token::EOF, span: start..start };
            }

            // String literals
            if b == b'"' {
                let token = self.scan_string(start);
                return SpannedToken { token, span: start..self.pos };
            }
            if let Some(token) = self.scan_paren(b) {
                return SpannedToken { token, span: start..self.pos };
            }
            if Self::is_digit(b) {
                let token = self.scan_number(start);
                return SpannedToken { token, span: start..self.pos };
            }
            if Self::is_alpha(b) {
                let token = self.scan_identifier_or_keyword(start);
                return SpannedToken { token, span: start..self.pos };
            }
            if b != 0 {
                // We always advance the position here, even for unexpected characters,
                // to make sure we don't get stuck in an infinite loop and that the error span isn't empty.
                self.bump();
                let token = self.scan_unexpected(start);
                if let Some(token) = token {
                    return SpannedToken { token, span: start..self.pos };
                }
            } else {
                self.bump();
            }
        }
    }

    /// Looks at the current byte without advancing position
    ///
    /// Returns 0 (NUL byte) if we're at the end of input - this is safer
    /// than panicking and lets us simplify our main tokenization loop
    #[inline(always)]
    fn peek(&self) -> u8 {
        *self.src.as_bytes().get(self.pos).unwrap_or(&0)
    }

    /// Moves the scanner position forward by one byte
    ///
    /// Uses saturating_add as a safety measure to prevent overflow,
    /// though this should never happen in practice
    #[inline(always)]
    const fn bump(&mut self) {
        self.pos = self.pos.saturating_add(1);
    }

    /// Skips all whitespace characters
    ///
    /// Since NaijaScript doesn't have significant whitespace (like Python),
    /// we can simply skip over all spaces, tabs, newlines, etc.
    #[inline(always)]
    fn skip_whitespace(&mut self) {
        while self.peek().is_ascii_whitespace() {
            self.bump();
        }
    }

    /// Checks if a byte is a letter (A-Z, a-z)
    ///
    /// Used for identifier and keyword detection
    #[inline(always)]
    const fn is_alpha(b: u8) -> bool {
        b.is_ascii_alphabetic()
    }

    /// Checks if a byte is a digit (0-9)
    ///
    /// Used for number literal detection
    #[inline(always)]
    const fn is_digit(b: u8) -> bool {
        b.is_ascii_digit()
    }

    /// Reads an entire word (identifier or keyword)
    ///
    /// We can use from_utf8_unchecked safely here because:
    /// 1. We only match ASCII alphabetic/numeric characters, which are valid UTF-8
    /// 2. The original source was already valid UTF-8
    /// 3. The performance gain is significant in our benchmarks
    #[inline(always)]
    fn read_word(&mut self) -> &'input str {
        let start = self.pos;
        while Self::is_alpha(self.peek()) || Self::is_digit(self.peek()) {
            self.bump();
        }
        unsafe { std::str::from_utf8_unchecked(&self.src.as_bytes()[start..self.pos]) }
    }

    /// Try to consume a specific word at current position
    ///
    /// This is crucial for multi-word keywords like "if to say" and "small pass".
    /// The function checks if the next word matches the expected one,
    /// and only consumes it if:
    /// 1. It's followed by a non-alphabetic character (prevents partial matches)
    /// 2. We have enough input left
    #[inline(always)]
    fn try_consume_word(&mut self, word: &str) -> bool {
        // Skip any whitespace before the word
        let mut p = self.pos;
        let bytes = self.src.as_bytes();
        while bytes.get(p).copied().unwrap_or(0).is_ascii_whitespace() {
            p += 1;
        }

        let end = p + word.len();
        if end <= bytes.len()
            && &bytes[p..end] == word.as_bytes()
            && (end == bytes.len() || !Self::is_alpha(bytes[end]))
        {
            self.pos = end;
            return true;
        }
        false
    }

    #[inline(always)]
    fn scan_string(&mut self, start: usize) -> Token<'input> {
        self.bump(); // Skip the opening quote
        let content_start = self.pos;
        let mut end = self.pos;
        let mut has_escape = false;
        let mut owned = String::new();
        while end < self.src.len() {
            let c = self.src.as_bytes()[end];
            if c == b'"' {
                // Closing quote
                if has_escape {
                    self.pos = end + 1;
                    return Token::String(Cow::Owned(owned));
                } else {
                    let s = &self.src[content_start..end];
                    self.pos = end + 1;
                    return Token::String(Cow::Borrowed(s));
                }
            } else if c == b'\\' {
                has_escape = true;
                // Push everything up to this point if first escape
                if owned.is_empty() {
                    owned.push_str(&self.src[content_start..end]);
                }
                if end + 1 >= self.src.len() {
                    // Looks like we've hit the end of the input right after a backslash.
                    // This means the string ends with an incomplete escape sequence, which is an error.
                    self.errors.emit(
                        start..self.src.len(),
                        Severity::Error,
                        "lexical",
                        LexError::UnterminatedString.as_str(),
                        vec![Label {
                            span: start..self.src.len(),
                            message: "String no get ending quote",
                        }],
                    );
                    self.pos = self.src.len();
                    return Token::String(Cow::Owned(owned));
                }
                let esc = self.src.as_bytes()[end + 1];
                match esc {
                    b'"' => owned.push('"'),
                    b'\\' => owned.push('\\'),
                    b'n' => owned.push('\n'),
                    b't' => owned.push('\t'),
                    _ => {
                        self.errors.emit(
                            end..end + 2,
                            Severity::Error,
                            "lexical",
                            LexError::InvalidStringEscape.as_str(),
                            vec![Label { span: end..end + 2, message: "Escape wey no correct" }],
                        );
                        // Append the invalid escape character
                        owned.push(esc as char);
                    }
                }
                end += 2;
            } else {
                if has_escape {
                    owned.push(c as char);
                }
                end += 1;
            }
        }
        // Looks like we reached the end of the input without finding a closing quote.
        // We'll emit an error to let the user know their string is unterminated.
        self.errors.emit(
            start..self.src.len(),
            Severity::Error,
            "lexical",
            LexError::UnterminatedString.as_str(),
            vec![Label { span: start..self.src.len(), message: "String no get ending quote" }],
        );
        self.pos = self.src.len();
        if has_escape {
            Token::String(Cow::Owned(owned))
        } else {
            Token::String(Cow::Borrowed(&self.src[content_start..self.src.len()]))
        }
    }

    #[inline(always)]
    fn scan_paren(&mut self, b: u8) -> Option<Token<'input>> {
        match b {
            b'(' => {
                self.bump();
                Some(Token::LParen)
            }
            b')' => {
                self.bump();
                Some(Token::RParen)
            }
            _ => None,
        }
    }

    #[inline(always)]
    fn scan_number(&mut self, start: usize) -> Token<'input> {
        let mut saw_dot = false;
        // Let's consume the integer part of the number first.
        // We'll keep bumping as long as we see digits.
        while Self::is_digit(self.peek()) {
            self.bump();
        }
        // Consume decimal part if present
        if self.peek() == b'.' {
            saw_dot = true;
            let dot_pos = self.pos;
            self.bump();
            let after_dot = self.peek();
            if !Self::is_digit(after_dot) {
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
                        message: "Number no get digit after dot",
                    }],
                );
                self.bump();
                return self.next_token().token;
            }
            while Self::is_digit(self.peek()) {
                self.bump();
            }
        }
        // Let's check if there are multiple dots in the number, like "1.2.3".
        // That's not valid in NaijaScript, so we'll catch it here and report an error.
        if saw_dot && self.peek() == b'.' {
            let extra_dot = self.pos;
            self.bump();
            self.errors.emit(
                start..self.pos,
                Severity::Error,
                "lexical",
                LexError::InvalidNumber.as_str(),
                vec![Label { span: extra_dot..self.pos, message: "Number get extra dot" }],
            );
            return self.next_token().token;
        }

        // If the next character is a letter, that means the user wrote something like "1foo".
        // That's not a valid number or identifier in NaijaScript, so let's catch it here.
        // We'll emit an error to let the user know, but still return the number part as a token.
        if Self::is_alpha(self.peek()) {
            let id_start = self.pos;
            while Self::is_alpha(self.peek()) || Self::is_digit(self.peek()) {
                self.bump();
            }
            self.errors.emit(
                start..self.pos,
                Severity::Error,
                "lexical",
                LexError::InvalidIdentifier.as_str(),
                vec![Label { span: start..id_start, message: "Identifier must start with letter" }],
            );
            let num = &self.src[start..id_start];
            return Token::Number(num);
        }

        // Let's grab the whole number as a string slice.
        // We'll actually parse it into a numeric value later, during evaluation.
        // For now, we just want to capture exactly what the user typed, so we don't lose any information.
        let num = &self.src[start..self.pos];
        Token::Number(num)
    }

    #[inline(always)]
    fn scan_identifier_or_keyword(&mut self, start: usize) -> Token<'input> {
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
        // "small pass" (greater than or equal)
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
            "shout" => Token::Shout,
            "jasi" => Token::Jasi,
            "start" => Token::Start,
            "end" => Token::End,
            "na" => Token::Na,
            "pass" => Token::Pass,
            // If not a keyword, it's an identifier
            other => {
                // Let's make sure the identifier is valid:
                // - The first character must be a letter.
                // - The rest can be letters or digits.
                let mut chars = other.chars();
                if let Some(first) = chars.next() {
                    if !first.is_ascii_alphabetic() {
                        self.errors.emit(
                            start..self.pos,
                            Severity::Error,
                            "lexical",
                            LexError::InvalidIdentifier.as_str(),
                            vec![Label {
                                span: start..start + 1,
                                message: "Identifier must start with letter",
                            }],
                        );
                    } else {
                        // Let's check if there are any invalid characters in the rest of the identifier.
                        // We'll walk through each character, and if we find something that's not a letter or digit,
                        // we'll emit an error so the user knows exactly where the problem is.
                        let mut offset = 1;
                        for c in chars {
                            if !c.is_ascii_alphanumeric() {
                                let bad_pos = start + offset;
                                self.errors.emit(
                                    start..self.pos,
                                    Severity::Error,
                                    "lexical",
                                    LexError::InvalidIdentifier.as_str(),
                                    vec![Label {
                                        span: bad_pos..bad_pos + 1,
                                        message: "Identifier get invalid character",
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

    #[inline(always)]
    fn scan_unexpected(&mut self, start: usize) -> Option<Token<'input>> {
        self.errors.emit(
            start..self.pos,
            Severity::Error,
            "lexical",
            LexError::UnexpectedChar.as_str(),
            vec![Label { span: start..self.pos, message: "Dis character no dey grammar" }],
        );
        None
    }
}
