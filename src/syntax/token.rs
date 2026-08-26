use crate::arena::ArenaCow;
use crate::diagnostics::Span;

/// Represents a token
#[derive(Debug, Default, Clone, PartialEq)]
pub struct SpannedToken<'a> {
    pub token: Token<'a>,
    pub span: Span,
}

/// All possible token types in `NaijaScript`
#[derive(Debug, Default, Clone, PartialEq)]
pub enum Token<'a> {
    // Variable length tokens
    String(ArenaCow<'a>), // String literals
    Identifier(&'a str),  // Variable names
    Number(&'a str),      // Numeric literals

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
    Jaara,   // "jaara" - while loop construct
    ForEach, // "for each" - collection loop
    Inside,  // "inside" - foreach separator
    Start,   // "start" - block beginning
    End,     // "end" - block ending
    Comot,   // "comot" - break
    Next,    // "next" - continue

    // Comparison operators
    Na,          // "na" - equality
    Pass,        // "pass" - greater than
    SmallPass,   // "small pass" - less than
    NoBe,        // "no be" - not equal
    NoPass,      // "no pass" - less than or equal
    NoSmallPass, // "no small pass" - greater than or equal

    // Conditional constructs
    IfNaSo,   // "if na so" - if statement
    IfNoBeSo, // "if no be so" - else statement

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
    LBrace,   // "{"
    RBrace,   // "}"
    Colon,    // ":"

    // Special tokens
    #[default]
    EOF, // End of file
}

impl std::fmt::Display for Token<'_> {
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
            Token::Jaara => write!(f, "jaara"),
            Token::ForEach => write!(f, "for each"),
            Token::Inside => write!(f, "inside"),
            Token::Start => write!(f, "start"),
            Token::End => write!(f, "end"),
            Token::Comot => write!(f, "comot"),
            Token::Next => write!(f, "next"),
            Token::Na => write!(f, "na"),
            Token::Pass => write!(f, "pass"),
            Token::SmallPass => write!(f, "small pass"),
            Token::NoBe => write!(f, "no be"),
            Token::NoPass => write!(f, "no pass"),
            Token::NoSmallPass => write!(f, "no small pass"),
            Token::IfNaSo => write!(f, "if na so"),
            Token::IfNoBeSo => write!(f, "if no be so"),
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

impl Token<'_> {
    #[inline]
    #[must_use]
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
                | Token::Jaara
                | Token::ForEach
                | Token::Inside
                | Token::Start
                | Token::End
                | Token::Comot
                | Token::Next
                | Token::Na
                | Token::Pass
                | Token::SmallPass
                | Token::NoBe
                | Token::NoPass
                | Token::NoSmallPass
                | Token::IfNaSo
                | Token::IfNoBeSo
                | Token::True
                | Token::False
                | Token::Null
                | Token::Do
                | Token::Return
        )
    }
}
