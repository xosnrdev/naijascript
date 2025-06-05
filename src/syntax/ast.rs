use std::fmt;

/// The root node of a parsed NaijaScript program.
/// Holds a list of statements to be executed in order.
#[derive(Debug, Clone, PartialEq)]
pub struct Program<'a> {
    pub statements: Vec<Statement<'a>>,
}

impl<'a> Program<'a> {
    #[inline]
    pub fn new(statements: Vec<Statement<'a>>) -> Self {
        Self { statements }
    }
}

/// All possible statement forms in NaijaScript.
/// Each variant represents a top-level action or control flow construct.
#[derive(Debug, Clone, PartialEq)]
pub enum Statement<'a> {
    /// Variable assignment, e.g. `make x get expr`
    Assignment { variable: &'a str, value: Expression<'a> },
    /// Output statement, e.g. `shout (expr)`
    Output(Expression<'a>),
    /// Conditional statement, e.g. `if to say (cond) start ... end [if not so start ... end]`
    If { condition: Condition<'a>, then_block: Block<'a>, else_block: Option<Block<'a>> },
    /// Loop statement, e.g. `jasi (cond) start ... end`
    Loop { condition: Condition<'a>, body: Block<'a> },
}

/// Sequence of statements, used for block bodies in if/loop.
#[derive(Debug, Clone, PartialEq)]
pub struct Block<'a> {
    pub statements: Vec<Statement<'a>>,
}

impl<'a> Block<'a> {
    #[inline]
    pub fn new(statements: Vec<Statement<'a>>) -> Self {
        Self { statements }
    }
}

/// All possible expressions in NaijaScript.
/// Expressions produce values and can be nested.
#[derive(Debug, Clone, PartialEq)]
pub enum Expression<'a> {
    /// Numeric literal
    Number(f64),
    /// Variable reference
    Variable(&'a str),
    /// Binary operation (e.g. add, minus, times, divide)
    Binary { left: Box<Expression<'a>>, op: BinaryOp, right: Box<Expression<'a>> },
    /// Parenthesized expression
    Grouping(Box<Expression<'a>>),
}

impl<'a> Expression<'a> {
    #[inline]
    pub fn number(n: f64) -> Self {
        Expression::Number(n)
    }
    #[inline]
    pub fn variable(name: &'a str) -> Self {
        Expression::Variable(name)
    }
    #[inline]
    pub fn binary(left: Expression<'a>, op: BinaryOp, right: Expression<'a>) -> Self {
        Expression::Binary { left: Box::new(left), op, right: Box::new(right) }
    }
    #[inline]
    pub fn grouping(expr: Expression<'a>) -> Self {
        Expression::Grouping(Box::new(expr))
    }
}

/// Supported binary operators for expressions.
#[derive(Debug, Clone, PartialEq)]
pub enum BinaryOp {
    Add,
    Minus,
    Times,
    Divide,
}

/// Supported condition forms for if/loop constructs.
/// Each variant represents a comparison between two expressions.
#[derive(Debug, Clone, PartialEq)]
pub enum Condition<'a> {
    /// Equality: `na` (==)
    Na(Expression<'a>, Expression<'a>),
    /// Greater than: `pass` (>)
    Pass(Expression<'a>, Expression<'a>),
    /// Less than: `small pass` (<)
    SmallPass(Expression<'a>, Expression<'a>),
}

impl<'a> fmt::Display for Program<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "Program [")?;
        for stmt in &self.statements {
            writeln!(f, "  {stmt}")?;
        }
        write!(f, "]")
    }
}

impl<'a> fmt::Display for Statement<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Statement::Assignment { variable, value } => {
                write!(f, "Assignment(variable: {variable:?}, value: {value})")
            }
            Statement::Output(expr) => {
                write!(f, "Output({expr})")
            }
            Statement::If { condition, then_block, else_block } => {
                write!(f, "If(condition: {condition}, then: {then_block}")?;
                if let Some(else_block) = else_block {
                    write!(f, ", else: {else_block}")?;
                }
                write!(f, ")")
            }
            Statement::Loop { condition, body } => {
                write!(f, "Loop(condition: {condition}, body: {body})")
            }
        }
    }
}

impl<'a> fmt::Display for Block<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Block [")?;
        for stmt in &self.statements {
            write!(f, " {stmt}")?;
        }
        write!(f, "]")
    }
}

impl<'a> fmt::Display for Expression<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Expression::Number(n) => write!(f, "Number({n})"),
            Expression::Variable(name) => write!(f, "Variable({name:?})"),
            Expression::Binary { left, op, right } => write!(f, "Binary({op}, {left}, {right})"),
            Expression::Grouping(expr) => write!(f, "Grouping({expr})"),
        }
    }
}

impl fmt::Display for BinaryOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let op_str = match self {
            BinaryOp::Add => "Add",
            BinaryOp::Minus => "Minus",
            BinaryOp::Times => "Times",
            BinaryOp::Divide => "Divide",
        };
        write!(f, "{op_str}")
    }
}

impl<'a> fmt::Display for Condition<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Condition::Na(l, r) => write!(f, "Na({l}, {r})"),
            Condition::Pass(l, r) => write!(f, "Pass({l}, {r})"),
            Condition::SmallPass(l, r) => write!(f, "SmallPass({l}, {r})"),
        }
    }
}
