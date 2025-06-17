//! The lexer (or scanner) for NaijaScript.

use std::ops::Range;

/// Represents an error encountered during lexical analysis
///
/// We track both the location (span) and message so errors can be
/// highlighted directly in the source code
#[derive(Clone, Debug)]
pub struct LexError {
    pub span: Range<usize>,
    pub message: &'static str,
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
    Identifier(&'input str), // Variable names
    Number(&'input str),     // Numeric literals

    // Special tokens
    EOF,                                                 // End of file
    Error { span: Range<usize>, message: &'static str }, // Lexical errors
}

/// Our lexical analyzer that breaks source text into tokens
///
/// We use a byte-based scanner for performance rather than working with Unicode
/// characters directly. This is significantly faster for ASCII-heavy code.
pub struct Lexer<'input> {
    src: &'input str,      // Original source text (kept for slicing)
    bytes: &'input [u8],   // Bytes view for faster processing
    pos: usize,            // Current position in the source
    errors: Vec<LexError>, // Collection of encountered errors
}

impl<'input> Lexer<'input> {
    /// Creates a new lexer from source text
    ///
    /// Using #[inline(always)] because this is a trivial constructor
    /// that gets called a lot during testing and benchmarks
    #[inline(always)]
    pub fn new(src: &'input str) -> Self {
        Lexer { src, bytes: src.as_bytes(), pos: 0, errors: Vec::new() }
    }

    /// Consumes the lexer and returns any accumulated errors
    ///
    /// Useful when you want to check if lexing was successful after
    /// you've finished tokenizing the entire source
    #[inline(always)]
    pub fn into_errors(self) -> Vec<LexError> {
        self.errors
    }

    /// Looks at the current byte without advancing position
    ///
    /// Returns 0 (NUL byte) if we're at the end of input - this is safer
    /// than panicking and lets us simplify our main tokenization loop
    #[inline(always)]
    fn peek(&self) -> u8 {
        *self.bytes.get(self.pos).unwrap_or(&0)
    }

    /// Moves the scanner position forward by one byte
    ///
    /// Uses saturating_add as a safety measure to prevent overflow,
    /// though this should never happen in practice
    #[inline(always)]
    fn bump(&mut self) {
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
    fn is_alpha(b: u8) -> bool {
        b.is_ascii_alphabetic()
    }

    /// Checks if a byte is a digit (0-9)
    ///
    /// Used for number literal detection
    #[inline(always)]
    fn is_digit(b: u8) -> bool {
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
        unsafe { std::str::from_utf8_unchecked(&self.bytes[start..self.pos]) }
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
        while self.bytes.get(p).copied().unwrap_or(0).is_ascii_whitespace() {
            p += 1;
        }

        let end = p + word.len();
        if end <= self.bytes.len()
            && &self.bytes[p..end] == word.as_bytes()
            && (end == self.bytes.len() || !Self::is_alpha(self.bytes[end]))
        {
            self.pos = end;
            return true;
        }
        false
    }

    /// Returns the next token from the input stream
    ///
    /// This is the heart of our lexer. We analyze the input character by character
    /// and produce tokens based on NaijaScript's grammar rules. The function uses
    /// a simple approach - look at the current character and decide what to do:
    /// - Punctuation: recognize immediately
    /// - Numbers: read all digits (and possibly decimal point + more digits)
    /// - Words: could be keywords or identifiers
    #[inline(always)]
    pub fn next_token(&mut self) -> Token<'input> {
        // Skip over any whitespace before the next token
        self.skip_whitespace();

        // Record starting position for spans and slicing
        let start = self.pos;

        // Check for EOF first
        let b = self.peek();
        if b == 0 {
            return Token::EOF;
        }

        // Single-character tokens - parentheses
        match b {
            b'(' => {
                self.bump();
                return Token::LParen;
            }
            b')' => {
                self.bump();
                return Token::RParen;
            }
            _ => {}
        }

        // Number literals - both integers and decimals
        // NaijaScript follows standard number format: digits + optional decimal point + more digits
        if Self::is_digit(b) {
            // Consume integer part
            while Self::is_digit(self.peek()) {
                self.bump();
            }

            // Consume decimal part if present
            if self.peek() == b'.' {
                self.bump();
                while Self::is_digit(self.peek()) {
                    self.bump();
                }
            }

            // Get the full number as a string slice - we'll parse it later during evaluation
            let num = &self.src[start..self.pos];
            return Token::Number(num);
        }
        // Identifiers and keywords
        if Self::is_alpha(b) {
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

            // Single-word keywords
            // This is cleaner and faster than a big if/else chain
            return match word {
                // Variable declaration and assignment
                "make" => Token::Make,
                "get" => Token::Get,

                // Arithmetic operators
                "add" => Token::Add,
                "minus" => Token::Minus,
                "times" => Token::Times,
                "divide" => Token::Divide,

                // I/O and control flow
                "shout" => Token::Shout,
                "jasi" => Token::Jasi,
                "start" => Token::Start,
                "end" => Token::End,

                // Comparison operators
                "na" => Token::Na,
                "pass" => Token::Pass,

                // If not a keyword, it's an identifier
                other => Token::Identifier(other),
            };
        }
        // If we got here, we encountered an unexpected character
        // We'll consume it and report an error, but keep going
        // This gives better error recovery than just panicking
        self.bump();
        let span = start..self.pos;
        let err = LexError { span: span.clone(), message: "Unexpected character" };

        // Store error for later reporting
        self.errors.push(err.clone());

        // Return error token so parser can decide how to handle it
        Token::Error { span, message: err.message }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_assignment() {
        let src = "make x get 42";
        let mut lexer = Lexer::new(src);
        assert_eq!(lexer.next_token(), Token::Make);
        assert_eq!(lexer.next_token(), Token::Identifier("x"));
        assert_eq!(lexer.next_token(), Token::Get);
        assert_eq!(lexer.next_token(), Token::Number("42"));
        assert_eq!(lexer.next_token(), Token::EOF);
    }

    #[test]
    fn test_arithmetic_expression() {
        let src = "(3 add 4) times 2";
        let mut lexer = Lexer::new(src);
        assert_eq!(lexer.next_token(), Token::LParen);
        assert_eq!(lexer.next_token(), Token::Number("3"));
        assert_eq!(lexer.next_token(), Token::Add);
        assert_eq!(lexer.next_token(), Token::Number("4"));
        assert_eq!(lexer.next_token(), Token::RParen);
        assert_eq!(lexer.next_token(), Token::Times);
        assert_eq!(lexer.next_token(), Token::Number("2"));
        assert_eq!(lexer.next_token(), Token::EOF);
    }

    #[test]
    fn test_multiword_keyword() {
        let src = "if to say (x na 1) start end";
        let mut lexer = Lexer::new(src);
        assert_eq!(lexer.next_token(), Token::IfToSay);
        assert_eq!(lexer.next_token(), Token::LParen);
        assert_eq!(lexer.next_token(), Token::Identifier("x"));
        assert_eq!(lexer.next_token(), Token::Na);
        assert_eq!(lexer.next_token(), Token::Number("1"));
        assert_eq!(lexer.next_token(), Token::RParen);
        assert_eq!(lexer.next_token(), Token::Start);
        assert_eq!(lexer.next_token(), Token::End);
        assert_eq!(lexer.next_token(), Token::EOF);
    }

    #[test]
    fn test_error_token() {
        let src = "make $ get 1";
        let mut lexer = Lexer::new(src);
        assert_eq!(lexer.next_token(), Token::Make);
        let err = lexer.next_token();
        match err {
            Token::Error { message, .. } => assert_eq!(message, "Unexpected character"),
            _ => panic!("Expected error token"),
        }
    }

    #[test]
    fn debug_latency() {
        use std::time::Instant;
        const MAX: usize = 100_000;
        let line = "make x get 1\n";
        let src = line.repeat(MAX);
        let start = Instant::now();
        let mut lexer = Lexer::new(&src);
        loop {
            let tok = lexer.next_token();
            if tok == Token::EOF {
                break;
            }
        }
        let elapsed = start.elapsed();
        println!("Scanned {MAX} lines in {elapsed:?} ms");
    }
}
