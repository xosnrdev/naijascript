//! The abstract syntax tree (AST) representation for NaijaScript.

/// The root node of a NaijaScript program.
///
/// A program consists of a sequence of statements, which may include assignments, outputs,
/// conditionals, and loops.
#[derive(Debug, Clone, PartialEq)]
pub struct Program<'a> {
    /// The list of statements in the program.
    pub statements: Vec<Statement<'a>>,
}

impl<'a> Program<'a> {
    /// Constructs a new `Program` from a vector of statements.
    #[inline]
    pub fn new(statements: Vec<Statement<'a>>) -> Self {
        Self { statements }
    }
}

/// Represents a top-level statement in NaijaScript.
///
/// Each variant corresponds to a distinct statement type in the language.
#[derive(Debug, Clone, PartialEq)]
pub enum Statement<'a> {
    /// Variable assignment, e.g., `make x get 5`.
    Assignment { variable: &'a str, value: Expression<'a> },
    /// Output statement, e.g., `shout (x add 2)`.
    Output(Expression<'a>),
    /// Conditional statement with optional else block.
    If { condition: Condition<'a>, then_block: Block<'a>, else_block: Option<Block<'a>> },
    /// Loop statement, executes the body while the condition is true.
    Loop { condition: Condition<'a>, body: Block<'a> },
}

/// Represents a block of statements, used for the body of conditionals and loops.
#[derive(Debug, Clone, PartialEq)]
pub struct Block<'a> {
    /// The statements contained in the block.
    pub statements: Vec<Statement<'a>>,
}

impl<'a> Block<'a> {
    /// Constructs a new `Block` from a vector of statements.
    #[inline]
    pub fn new(statements: Vec<Statement<'a>>) -> Self {
        Self { statements }
    }
}

/// Represents an expression in NaijaScript.
///
/// Expressions can be literals, variable references, binary operations, or grouped expressions.
#[derive(Debug, Clone, PartialEq)]
pub enum Expression<'a> {
    /// Numeric literal, e.g., `42`.
    Number(f64),
    /// Variable reference, e.g., `x`.
    Variable(&'a str),
    /// Binary operation, e.g., `x add 2`.
    Binary { left: Box<Expression<'a>>, op: BinaryOp, right: Box<Expression<'a>> },
    /// Parenthesized (grouped) expression, e.g., `(x add 2)`.
    Grouping(Box<Expression<'a>>),
}

impl<'a> Expression<'a> {
    /// Constructs a numeric literal expression.
    #[inline]
    pub fn number(n: f64) -> Self {
        Expression::Number(n)
    }
    /// Constructs a variable reference expression.
    #[inline]
    pub fn variable(name: &'a str) -> Self {
        Expression::Variable(name)
    }
    /// Constructs a binary operation expression.
    #[inline]
    pub fn binary(left: Expression<'a>, op: BinaryOp, right: Expression<'a>) -> Self {
        Expression::Binary { left: Box::new(left), op, right: Box::new(right) }
    }
    /// Constructs a grouped (parenthesized) expression.
    #[inline]
    pub fn grouping(expr: Expression<'a>) -> Self {
        Expression::Grouping(Box::new(expr))
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
pub enum Condition<'a> {
    /// Equality check (`na`), e.g., `x na 2`.
    Na(Expression<'a>, Expression<'a>),
    /// Greater-than check (`pass`), e.g., `x pass 2`.
    Pass(Expression<'a>, Expression<'a>),
    /// Less-than check (`small pass`), e.g., `x small pass 2`.
    SmallPass(Expression<'a>, Expression<'a>),
}
