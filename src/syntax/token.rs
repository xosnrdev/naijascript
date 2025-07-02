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

    // I/O and control flow
    Shout, // "shout" - print to console
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

    // Boolean literals
    True,  // "true" - truthy
    False, // "false" - falsy

    // Punctuation
    LParen, // "("
    RParen, // ")"

    // Variable length tokens
    Identifier(&'input str),  // Variable names
    Number(&'input str),      // Numeric literals
    String(Cow<'input, str>), // String literals

    // Special tokens
    #[default]
    EOF, // End of file
}

impl<'input> Token<'input> {
    pub fn suggest_keyword(ident: &str) -> Option<&'static str> {
        Self::all_keywords()
            .iter()
            .find(|&&kw| Self::is_single_edit(ident, kw))
            .copied()
            .map(|v| v as _)
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
            "shout",
            "jasi",
            "start",
            "end",
            "na",
            "pass",
            "small pass",
            "if to say",
            "if not so",
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
