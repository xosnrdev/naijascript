//! The abstract syntax tree (AST) representation for NaijaScript.

use crate::syntax::Span;

/// The root node of a NaijaScript program.
///
/// A program consists of a sequence of statements, which may include assignments, outputs,
/// conditionals, and loops.
#[derive(Debug, Clone, PartialEq)]
pub struct Program<'input> {
    /// The list of statements in the program.
    pub statements: Vec<Statement<'input>>,
}

impl<'input> Program<'input> {
    /// Constructs a new `Program` from a vector of statements.
    #[inline]
    pub fn new(statements: Vec<Statement<'input>>) -> Self {
        Self { statements }
    }
}

/// Represents a top-level statement in NaijaScript.
///
/// Each variant corresponds to a distinct statement type in the language.
#[derive(Debug, Clone, PartialEq)]
pub enum Statement<'input> {
    /// Variable assignment, e.g., `make x get 5`.
    Assignment { variable: &'input str, value: Expression<'input>, span: Span },
    /// Output statement, e.g., `shout (x add 2)`.
    Output(Expression<'input>, Span),
    /// Conditional statement with optional else block.
    If {
        condition: Condition<'input>,
        then_block: Block<'input>,
        else_block: Option<Block<'input>>,
        span: Span,
    },
    /// Loop statement, executes the body while the condition is true.
    Loop { condition: Condition<'input>, body: Block<'input>, span: Span },
}

/// Represents a block of statements, used for the body of conditionals and loops.
#[derive(Debug, Clone, PartialEq)]
pub struct Block<'input> {
    /// The statements contained in the block.
    pub statements: Vec<Statement<'input>>,
    pub span: Span,
}

impl<'input> Block<'input> {
    /// Constructs a new `Block` from a vector of statements.
    #[inline]
    pub fn new(statements: Vec<Statement<'input>>) -> Self {
        Self { statements, span: Span::default() }
    }
}

/// Represents an expression in NaijaScript.
///
/// Expressions can be literals, variable references, binary operations, or grouped expressions.
#[derive(Debug, Clone, PartialEq)]
pub enum Expression<'input> {
    /// Numeric literal, e.g., `42`.
    Number(f64, Span),
    /// Variable reference, e.g., `x`.
    Variable(&'input str, Span),
    /// Binary operation, e.g., `x add 2`.
    Binary {
        left: Box<Expression<'input>>,
        op: BinaryOp,
        right: Box<Expression<'input>>,
        span: Span,
    },
    /// Parenthesized (grouped) expression, e.g., `(x add 2)`.
    Grouping(Box<Expression<'input>>, Span),
}

impl<'input> Expression<'input> {
    /// Constructs a numeric literal expression.
    #[inline]
    pub fn number(n: f64) -> Self {
        Expression::Number(n, Span::default())
    }
    /// Constructs a variable reference expression.
    #[inline]
    pub fn variable(name: &'input str) -> Self {
        Expression::Variable(name, Span::default())
    }
    /// Constructs a binary operation expression.
    #[inline]
    pub fn binary(left: Expression<'input>, op: BinaryOp, right: Expression<'input>) -> Self {
        Expression::Binary {
            left: Box::new(left),
            op,
            right: Box::new(right),
            span: Span::default(),
        }
    }
    /// Constructs a grouped (parenthesized) expression.
    #[inline]
    pub fn grouping(expr: Expression<'input>) -> Self {
        Expression::Grouping(Box::new(expr), Span::default())
    }
}

/// Enumerates the supported binary operators in NaijaScript.
#[derive(Debug, Clone, PartialEq)]
pub enum BinaryOp {
    /// Addition operator (`add`).
    Add,
    /// Subtraction operator (`minus`).
    Minus,
    /// Multiplication operator (`times`).
    Times,
    /// Division operator (`divide`).
    Divide,
}

/// Represents a boolean condition for use in `if` and `loop` statements.
#[derive(Debug, Clone, PartialEq)]
pub enum Condition<'input> {
    /// Equality check (`na`), e.g., `x na 2`.
    Na(Expression<'input>, Expression<'input>, Span),
    /// Greater-than check (`pass`), e.g., `x pass 2`.
    Pass(Expression<'input>, Expression<'input>, Span),
    /// Less-than check (`small pass`), e.g., `x small pass 2`.
    SmallPass(Expression<'input>, Expression<'input>, Span),
}

/// Trait for types that can provide a span, which is a range of source code positions.
pub trait Spanned {
    /// Returns the span of the item, which indicates its position in the source code.
    fn span(&self) -> Span;
}

impl<'input> Spanned for Expression<'input> {
    fn span(&self) -> Span {
        match self {
            Expression::Number(_, span) => *span,
            Expression::Variable(_, span) => *span,
            Expression::Binary { span, .. } => *span,
            Expression::Grouping(_, span) => *span,
        }
    }
}
impl<'input> Spanned for Statement<'input> {
    fn span(&self) -> Span {
        match self {
            Statement::Assignment { span, .. } => *span,
            Statement::Output(_, span) => *span,
            Statement::If { span, .. } => *span,
            Statement::Loop { span, .. } => *span,
        }
    }
}
