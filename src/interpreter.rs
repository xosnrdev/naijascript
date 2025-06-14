//! Tree-Walk Interpreter for NaijaScript.

use std::collections::HashMap;

use crate::diagnostics::{Diagnostic, DiagnosticHandler};
use crate::syntax::ast::*;

/// Represents a runtime value in NaijaScript.
///
/// Currently, only numeric values are supported. This type can be extended to support more value
/// kinds in the future.
#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    /// A numeric value (floating-point).
    Number(f64),
}

impl std::fmt::Display for Value {
    /// Formats the value for display (used in output statements).
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Number(n) => write!(f, "{n}"),
        }
    }
}

/// Enumerates possible runtime errors encountered during interpretation.
#[derive(Debug, PartialEq)]
pub enum InterpreterError<'source> {
    /// Variable was referenced before being defined.
    UndefinedVariable(&'source str),
    /// Division by zero was attempted.
    DivisionByZero,
    /// A type mismatch occurred during evaluation.
    TypeError(&'static str),
    /// No environment scope was found (internal error).
    ScopeError,
    /// Other miscellaneous error.
    Other(String),
}

impl<'source> InterpreterError<'source> {
    /// Converts this error into a `Diagnostic` for user-facing error reporting.
    ///
    /// The diagnostic includes a message, the relevant source span, and the line/column.
    pub fn to_diagnostic(
        &self,
        source: &'source str,
        filename: Option<&'source str>,
    ) -> Diagnostic<'source> {
        // Helper: computes line and column from a byte offset.
        fn line_col_from_offset(source: &str, offset: usize) -> (usize, usize) {
            let mut line = 1;
            let mut col = 1;
            let mut count = 0;
            for ch in source.chars() {
                if count >= offset {
                    break;
                }
                if ch == '\n' {
                    line += 1;
                    col = 1;
                } else {
                    col += 1;
                }
                count += ch.len_utf8();
            }
            (line, col)
        }
        // Helper: finds the span of the first word in a message within the source.
        fn find_first_word_span(msg: &str, source: &str) -> (usize, usize) {
            let first_word = msg.split_whitespace().next().unwrap_or("");
            let span = source.find(first_word).map(|start| start..start + first_word.len());
            span.map(|r| (r.start, r.end)).unwrap_or((0, 0))
        }
        match self {
            InterpreterError::UndefinedVariable(name) => {
                let (start, end) = if let Some(start) = source.find(name) {
                    (start, start + name.len())
                } else {
                    (0, 0)
                };
                let (line, col) = line_col_from_offset(source, start);
                Diagnostic::error(
                    "runtime error",
                    "Abeg, variable no dey defined for here o",
                    source,
                    (start, end),
                    line,
                    col,
                    filename,
                )
            }
            InterpreterError::DivisionByZero => {
                let (start, end) = if let Some(start) = source.find("divide 0") {
                    (start, start + "divide 0".len())
                } else if let Some(start) = source.find("0") {
                    (start, start + 1)
                } else {
                    (0, 0)
                };
                let (line, col) = line_col_from_offset(source, start);
                Diagnostic::error(
                    "runtime error",
                    "Kai! You wan divide by zero? Wahala o!",
                    source,
                    (start, end),
                    line,
                    col,
                    filename,
                )
            }
            InterpreterError::TypeError(msg) => {
                let (start, end) = find_first_word_span(msg, source);
                let (line, col) = line_col_from_offset(source, start);
                Diagnostic::error("runtime error", msg, source, (start, end), line, col, filename)
            }
            InterpreterError::ScopeError => Diagnostic::error(
                "runtime error",
                "No environment scope found (this na bug)",
                source,
                (0, 0),
                0,
                0,
                filename,
            ),
            InterpreterError::Other(msg) => {
                let (start, end) = find_first_word_span(msg, source);
                let (line, col) = line_col_from_offset(source, start);
                Diagnostic::error(
                    "runtime error",
                    "Interpreter wahala",
                    source,
                    (start, end),
                    line,
                    col,
                    filename,
                )
            }
        }
    }
}

/// The result type for interpreter operations.
pub type InterpreterResult<'source, T> = Result<T, InterpreterError<'source>>;

/// The main interpreter struct for NaijaScript.
///
/// The interpreter maintains a stack of environments (scopes), each mapping variable names to
/// values. Scopes are pushed and popped as blocks are entered and exited, supporting lexical scoping.
#[derive(Default)]
pub struct Interpreter<'source> {
    /// Stack of environments (scopes). Each scope is a map from variable names to values.
    env: Vec<HashMap<&'source str, Value>>,
}

impl<'source> Interpreter<'source> {
    /// Creates a new interpreter with a single, empty global scope.
    pub fn new() -> Self {
        Interpreter { env: vec![HashMap::new()] }
    }

    /// Evaluates a complete program AST.
    ///
    /// Returns an error if any statement fails.
    pub fn eval_program(&mut self, program: &Program<'source>) -> InterpreterResult<'source, ()> {
        for stmt in &program.statements {
            self.eval_statement(stmt)?;
        }
        Ok(())
    }

    /// Evaluates a program AST, reporting errors via a diagnostic handler if provided.
    ///
    /// Stops at the first error.
    pub fn eval_program_with_handler(
        &mut self,
        program: &Program<'source>,
        handler: Option<&mut dyn DiagnosticHandler>,
        source: &'source str,
        filename: Option<&'source str>,
    ) -> InterpreterResult<'source, ()> {
        for stmt in &program.statements {
            if let Err(e) = self.eval_statement(stmt) {
                if let Some(h) = handler {
                    h.report(&e.to_diagnostic(source, filename));
                }
                return Err(e);
            }
        }
        Ok(())
    }

    /// Evaluates a single statement node.
    ///
    /// This function dispatches based on the statement type and may recursively evaluate
    /// expressions, blocks, or conditions.
    fn eval_statement(&mut self, stmt: &Statement<'source>) -> InterpreterResult<'source, ()> {
        match stmt {
            Statement::Assignment { variable, value } => {
                let val = self.eval_expression(value)?;
                self.assign_variable(variable, val)
            }
            Statement::Output(expr) => {
                let val = self.eval_expression(expr)?;
                println!("{val}");
                Ok(())
            }
            Statement::If { condition, then_block, else_block } => {
                if self.eval_condition(condition)? {
                    self.eval_block(then_block)
                } else if let Some(else_block) = else_block {
                    self.eval_block(else_block)
                } else {
                    Ok(())
                }
            }
            Statement::Loop { condition, body } => {
                while self.eval_condition(condition)? {
                    self.eval_block(body)?;
                }
                Ok(())
            }
        }
    }

    /// Evaluates a block of statements, introducing a new lexical scope.
    ///
    /// A new environment is pushed before the block and popped after, ensuring block-local variables
    /// do not leak.
    fn eval_block(&mut self, block: &Block<'source>) -> InterpreterResult<'source, ()> {
        self.env.push(HashMap::new());
        for stmt in &block.statements {
            self.eval_statement(stmt)?;
        }
        self.env.pop();
        Ok(())
    }

    /// Recursively evaluates an expression node and returns its value.
    fn eval_expression(&mut self, expr: &Expression<'source>) -> InterpreterResult<'source, Value> {
        match expr {
            Expression::Number(n) => Ok(Value::Number(*n)),
            Expression::Variable(name) => {
                let val = self.lookup_variable(name)?;
                Ok(val.clone())
            }
            Expression::Binary { left, op, right } => {
                let l = self.eval_expression(left)?;
                let r = self.eval_expression(right)?;
                self.eval_binary_op(op, &l, &r)
            }
            Expression::Grouping(inner) => self.eval_expression(inner),
        }
    }

    /// Evaluates a binary operation between two values.
    ///
    /// Only numeric operations are supported. Division by zero is checked and returns an error.
    fn eval_binary_op(
        &self,
        op: &BinaryOp,
        l: &Value,
        r: &Value,
    ) -> InterpreterResult<'source, Value> {
        match (op, l, r) {
            (BinaryOp::Add, Value::Number(a), Value::Number(b)) => Ok(Value::Number(a + b)),
            (BinaryOp::Minus, Value::Number(a), Value::Number(b)) => Ok(Value::Number(a - b)),
            (BinaryOp::Times, Value::Number(a), Value::Number(b)) => Ok(Value::Number(a * b)),
            (BinaryOp::Divide, Value::Number(_), Value::Number(b)) if *b == 0.0 => {
                Err(InterpreterError::DivisionByZero)
            }
            (BinaryOp::Divide, Value::Number(a), Value::Number(b)) => Ok(Value::Number(a / b)),
        }
    }

    /// Evaluates a condition node and returns its boolean result.
    ///
    /// Each condition recursively evaluates its operands and applies the appropriate comparison.
    fn eval_condition(&mut self, cond: &Condition<'source>) -> InterpreterResult<'source, bool> {
        match cond {
            Condition::Na(l, r) => {
                let lv = self.eval_expression(l)?;
                let rv = self.eval_expression(r)?;
                let (Value::Number(a), Value::Number(b)) = (&lv, &rv);
                Ok(a == b)
            }
            Condition::Pass(l, r) => {
                let lv = self.eval_expression(l)?;
                let rv = self.eval_expression(r)?;
                let (Value::Number(a), Value::Number(b)) = (&lv, &rv);
                Ok(a > b)
            }
            Condition::SmallPass(l, r) => {
                let lv = self.eval_expression(l)?;
                let rv = self.eval_expression(r)?;
                let (Value::Number(a), Value::Number(b)) = (&lv, &rv);
                Ok(a < b)
            }
        }
    }

    /// Assigns a value to a variable in the current or outermost scope.
    ///
    /// If the variable exists in any scope, it is updated. Otherwise, it is created in the global scope.
    fn assign_variable(
        &mut self,
        name: &'source str,
        value: Value,
    ) -> InterpreterResult<'source, ()> {
        for scope in self.env.iter_mut().rev() {
            if scope.contains_key(name) {
                scope.insert(name, value);
                return Ok(());
            }
        }
        if let Some(scope) = self.env.first_mut() {
            scope.insert(name, value);
            Ok(())
        } else {
            Err(InterpreterError::ScopeError)
        }
    }

    /// Looks up a variable by name, searching from innermost to outermost scope.
    ///
    /// Returns an error if the variable is not found.
    fn lookup_variable(&self, name: &'source str) -> InterpreterResult<'source, &Value> {
        for scope in self.env.iter().rev() {
            if let Some(val) = scope.get(name) {
                return Ok(val);
            }
        }
        Err(InterpreterError::UndefinedVariable(name))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::syntax::Lexer;
    use crate::syntax::parser::Parser;

    #[test]
    fn test_assignment_and_output() {
        let src = "make x get 5\nshout ( x add 2 )";
        let mut parser = Parser::new(Lexer::new(src));
        let program = parser.parse_program().unwrap();
        let mut interpreter = Interpreter::new();
        assert!(interpreter.eval_program(&program).is_ok());
        assert_eq!(interpreter.lookup_variable("x").unwrap(), &Value::Number(5.0));
    }

    #[test]
    fn test_if_and_loop() {
        let src = "make x get 0\njasi ( x small pass 3 ) start make x get x add 1 end\nshout ( x )";
        let mut parser = Parser::new(Lexer::new(src));
        let program = parser.parse_program().unwrap();
        let mut interpreter = Interpreter::new();
        assert!(interpreter.eval_program(&program).is_ok());
        assert_eq!(interpreter.lookup_variable("x").unwrap(), &Value::Number(3.0));
    }

    #[test]
    fn test_if_else() {
        let src = "make x get 2\nif to say ( x na 2 ) start make y get 1 end if not so start make y get 0 end";
        let mut parser = Parser::new(Lexer::new(src));
        let program = parser.parse_program().unwrap();
        let mut interpreter = Interpreter::new();
        assert!(interpreter.eval_program(&program).is_ok());
        assert_eq!(interpreter.lookup_variable("y").unwrap(), &Value::Number(1.0));
    }
}
