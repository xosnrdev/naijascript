use std::borrow::Cow;

use crate::diagnostics::Span;

/// All possible token types in NaijaScript
///
/// The lifetime parameter 'input ties token references to the source text lifetime,
/// letting us avoid copying strings for identifiers and numbers
#[derive(Debug, Default, Clone, PartialEq)]
pub enum Token<'input> {
    // Keywords for variable declaration and assignment
    Make, // "make" - variable declaration
    Get,  // "get" - assignment operator

    // Arithmetic operators
    Add,    // "add" - addition
    Minus,  // "minus" - subtraction
    Times,  // "times" - multiplication
    Divide, // "divide" - division
    Mod,    // "mod" - modulus

    // Logical operators
    And, // "and" - logical and
    Or,  // "or" - logical or
    Not, // "not" - logical not

    // Control flow
    Jasi,  // "jasi" - while loop construct (Nigerian slang for "keep going")
    Start, // "start" - block beginning
    End,   // "end" - block ending

    // Comparison operators
    Na,        // "na" - equality (Nigerian slang for "is")
    Pass,      // "pass" - greater than (Nigerian slang for "exceeds")
    SmallPass, // "small pass" - less than (Nigerian slang for "smaller than")

    // Conditional constructs
    IfToSay, // "if to say" - if statement
    IfNotSo, // "if not so" - else statement

    // Function constructs
    Do,     // "do" - function definition
    Return, // "return" - return statement

    // Boolean literals
    True,  // "true" - truthy
    False, // "false" - falsy

    // Punctuation
    LParen, // "("
    RParen, // ")"
    Comma,  // ","

    // Variable length tokens
    Identifier(&'input str),  // Variable names
    Number(&'input str),      // Numeric literals
    String(Cow<'input, str>), // String literals

    // Special tokens
    #[default]
    EOF, // End of file
}

impl<'input> std::fmt::Display for Token<'input> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::Make => write!(f, "make"),
            Token::Get => write!(f, "get"),
            Token::Add => write!(f, "add"),
            Token::Minus => write!(f, "minus"),
            Token::Times => write!(f, "times"),
            Token::Divide => write!(f, "divide"),
            Token::Mod => write!(f, "mod"),
            Token::And => write!(f, "and"),
            Token::Or => write!(f, "or"),
            Token::Not => write!(f, "not"),
            Token::Jasi => write!(f, "jasi"),
            Token::Start => write!(f, "start"),
            Token::End => write!(f, "end"),
            Token::Na => write!(f, "na"),
            Token::Pass => write!(f, "pass"),
            Token::SmallPass => write!(f, "small pass"),
            Token::IfToSay => write!(f, "if to say"),
            Token::IfNotSo => write!(f, "if not so"),
            Token::True => write!(f, "true"),
            Token::False => write!(f, "false"),
            _ => write!(f, "{self:?}"),
        }
    }
}

impl<'input> Token<'input> {
    pub fn suggest_keyword(ident: &str) -> Option<&'static str> {
        Self::all_keywords()
            .iter()
            .find(|&&kw| Self::is_single_edit(ident, kw))
            .copied()
            .map(|v| v as _)
    }

    pub fn is_reserved_keyword(&self) -> bool {
        matches!(
            self,
            Token::Make
                | Token::Get
                | Token::Add
                | Token::Minus
                | Token::Times
                | Token::Divide
                | Token::Mod
                | Token::And
                | Token::Or
                | Token::Not
                | Token::Jasi
                | Token::Start
                | Token::End
                | Token::Na
                | Token::Pass
                | Token::SmallPass
                | Token::IfToSay
                | Token::IfNotSo
                | Token::True
                | Token::False
        )
    }

    // Returns a static list of all reserved keywords in NaijaScript.
    #[inline(always)]
    const fn all_keywords() -> &'static [&'static str] {
        &[
            "make",
            "get",
            "add",
            "minus",
            "times",
            "divide",
            "mod",
            "jasi",
            "start",
            "end",
            "na",
            "pass",
            "small pass",
            "if to say",
            "if not so",
            "do",
            "return",
            "true",
            "false",
            "and",
            "not",
            "or",
        ]
    }

    // Checks if two strings differ by exactly one simple change: either a single character replaced, added, or removed.
    #[inline]
    fn is_single_edit(a: &str, b: &str) -> bool {
        if a == b {
            return false;
        }
        let a_bytes = a.as_bytes();
        let b_bytes = b.as_bytes();
        let (alen, blen) = (a_bytes.len(), b_bytes.len());
        if alen == blen {
            // Handles the case where only one character is different between the two strings
            let mut diff = 0;
            for (x, y) in a_bytes.iter().zip(b_bytes.iter()) {
                if x != y {
                    diff += 1;
                    if diff > 1 {
                        return false;
                    }
                }
            }
            diff == 1
        } else if alen + 1 == blen {
            // Handles when the second string has one extra character compared to the first
            b.starts_with(a) || Self::is_one_insert(a_bytes, b_bytes)
        } else if alen == blen + 1 {
            // Handles when the first string has one extra character compared to the second
            a.starts_with(b) || Self::is_one_insert(b_bytes, a_bytes)
        } else {
            false
        }
    }

    #[inline]
    const fn is_one_insert(short: &[u8], long: &[u8]) -> bool {
        let mut i = 0;
        let mut j = 0;
        let mut found = false;
        while i < short.len() && j < long.len() {
            if short[i] != long[j] {
                if found {
                    return false;
                }
                found = true;
                j += 1;
            } else {
                i += 1;
                j += 1;
            }
        }
        true
    }
}

/// Represents a token along with its location in the original source text.
#[derive(Debug, Default, Clone, PartialEq)]
pub struct SpannedToken<'input> {
    pub token: Token<'input>,
    pub span: Span,
}
