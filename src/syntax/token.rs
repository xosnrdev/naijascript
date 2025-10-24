use crate::arena::ArenaCow;
use crate::diagnostics::Span;

/// Represents a token
#[derive(Debug, Default, Clone, PartialEq)]
pub struct SpannedToken<'arena, 'input> {
    pub token: Token<'arena, 'input>,
    pub span: Span,
}

/// All possible token types in NaijaScript
#[derive(Debug, Default, Clone, PartialEq)]
pub enum Token<'arena, 'input> {
    // Variable length tokens
    String(ArenaCow<'arena, 'input>), // String literals
    Identifier(&'input str),          // Variable names
    Number(&'input str),              // Numeric literals

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
    Jasi,  // "jasi" - while loop construct
    Start, // "start" - block beginning
    End,   // "end" - block ending
    Comot, // "comot" - break
    Next,  // "next" - continue

    // Comparison operators
    Na,        // "na" - equality
    Pass,      // "pass" - greater than
    SmallPass, // "small pass" - less than

    // Conditional constructs
    IfToSay, // "if to say" - if statement
    IfNotSo, // "if not so" - else statement

    // Function constructs
    Do,     // "do" - function definition
    Return, // "return" - return statement

    // Boolean literals
    True,  // "true" - truthy
    False, // "false" - falsy

    // Null literal
    Null, // "null" - absence of value

    // Punctuation
    LParen,   // "("
    RParen,   // ")"
    LBracket, // "["
    RBracket, // "]"
    Comma,    // ","
    Dot,      // "."

    // Special tokens
    #[default]
    EOF, // End of file
}

impl<'arena, 'input> std::fmt::Display for Token<'arena, 'input> {
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
            Token::Comot => write!(f, "comot"),
            Token::Next => write!(f, "next"),
            Token::Na => write!(f, "na"),
            Token::Pass => write!(f, "pass"),
            Token::SmallPass => write!(f, "small pass"),
            Token::IfToSay => write!(f, "if to say"),
            Token::IfNotSo => write!(f, "if not so"),
            Token::True => write!(f, "true"),
            Token::False => write!(f, "false"),
            Token::Null => write!(f, "null"),
            Token::LBracket => write!(f, "["),
            Token::RBracket => write!(f, "]"),
            Token::Dot => write!(f, "."),
            _ => write!(f, "{self:?}"),
        }
    }
}

impl<'arena, 'input> Token<'arena, 'input> {
    #[inline]
    pub const fn is_reserved_keyword(&self) -> bool {
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
                | Token::Comot
                | Token::Next
                | Token::Na
                | Token::Pass
                | Token::SmallPass
                | Token::IfToSay
                | Token::IfNotSo
                | Token::True
                | Token::False
                | Token::Null
                | Token::Do
                | Token::Return
        )
    }
}
