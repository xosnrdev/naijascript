//! Tree-Walk Interpreter for NaijaScript.

use std::collections::HashMap;

use crate::diagnostics::{Diagnostic, DiagnosticHandler};
use crate::syntax::ast::*;
use crate::syntax::offset_to_line_col;

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
    UndefinedVariable(&'source str, (usize, usize)),
    /// Division by zero was attempted.
    DivisionByZero((usize, usize)),
    /// A type mismatch occurred during evaluation.
    TypeError(&'static str, (usize, usize)),
    /// No environment scope was found (internal error).
    ScopeError,
    /// Other miscellaneous error.
    Other(String, (usize, usize)),
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
        let (message, start, end) = match self {
            InterpreterError::UndefinedVariable(_, (start, end)) => {
                ("Abeg, variable no dey defined for here o", *start, *end)
            }
            InterpreterError::DivisionByZero((start, end)) => {
                ("Kai! You wan divide by zero? Wahala o!", *start, *end)
            }
            InterpreterError::TypeError(msg, (start, end)) => (*msg, *start, *end),
            InterpreterError::ScopeError => ("No environment scope found (this na bug)", 0, 0),
            InterpreterError::Other(_, (start, end)) => ("Interpreter wahala", *start, *end),
        };

        let (line, col) = offset_to_line_col(source, start);

        Diagnostic::error("runtime error", message, source, (start, end), line, col, filename)
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
            Statement::Assignment { variable, value, .. } => {
                let val = self.eval_expression(value)?;
                self.assign_variable(variable, val)
            }
            Statement::Output(expr, _) => {
                let val = self.eval_expression(expr)?;
                println!("{val}");
                Ok(())
            }
            Statement::If { condition, then_block, else_block, .. } => {
                if self.eval_condition(condition)? {
                    self.eval_block(then_block)
                } else if let Some(else_block) = else_block {
                    self.eval_block(else_block)
                } else {
                    Ok(())
                }
            }
            Statement::Loop { condition, body, .. } => {
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
            Expression::Number(n, _) => Ok(Value::Number(*n)),
            Expression::Variable(name, span) => {
                let val = self.lookup_variable(name, (span.start, span.end))?;
                Ok(val.clone())
            }
            Expression::Binary { left, op, right, span } => {
                let l = self.eval_expression(left)?;
                let r = self.eval_expression(right)?;
                self.eval_binary_op(op, &l, &r, (span.start, span.end))
            }
            Expression::Grouping(inner, _) => self.eval_expression(inner),
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
        span: (usize, usize),
    ) -> InterpreterResult<'source, Value> {
        match (op, l, r) {
            (BinaryOp::Add, Value::Number(a), Value::Number(b)) => Ok(Value::Number(a + b)),
            (BinaryOp::Minus, Value::Number(a), Value::Number(b)) => Ok(Value::Number(a - b)),
            (BinaryOp::Times, Value::Number(a), Value::Number(b)) => Ok(Value::Number(a * b)),
            (BinaryOp::Divide, Value::Number(_), Value::Number(b)) if *b == 0.0 => {
                Err(InterpreterError::DivisionByZero(span))
            }
            (BinaryOp::Divide, Value::Number(a), Value::Number(b)) => Ok(Value::Number(a / b)),
        }
    }

    /// Evaluates a condition node and returns its boolean result.
    ///
    /// Each condition recursively evaluates its operands and applies the appropriate comparison.
    fn eval_condition(&mut self, cond: &Condition<'source>) -> InterpreterResult<'source, bool> {
        match cond {
            Condition::Na(l, r, _span) => {
                let lv = self.eval_expression(l)?;
                let rv = self.eval_expression(r)?;
                let (Value::Number(a), Value::Number(b)) = (&lv, &rv);
                Ok(a == b)
            }
            Condition::Pass(l, r, _span) => {
                let lv = self.eval_expression(l)?;
                let rv = self.eval_expression(r)?;
                let (Value::Number(a), Value::Number(b)) = (&lv, &rv);
                Ok(a > b)
            }
            Condition::SmallPass(l, r, _span) => {
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
    fn lookup_variable(
        &self,
        name: &'source str,
        span: (usize, usize),
    ) -> InterpreterResult<'source, &Value> {
        for scope in self.env.iter().rev() {
            if let Some(val) = scope.get(name) {
                return Ok(val);
            }
        }
        Err(InterpreterError::UndefinedVariable(name, span))
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
        assert_eq!(interpreter.lookup_variable("x", (0, 0)).unwrap(), &Value::Number(5.0));
    }

    #[test]
    fn test_if_and_loop() {
        let src = "make x get 0\njasi ( x small pass 3 ) start make x get x add 1 end\nshout ( x )";
        let mut parser = Parser::new(Lexer::new(src));
        let program = parser.parse_program().unwrap();
        let mut interpreter = Interpreter::new();
        assert!(interpreter.eval_program(&program).is_ok());
        assert_eq!(interpreter.lookup_variable("x", (0, 0)).unwrap(), &Value::Number(3.0));
    }

    #[test]
    fn test_if_else() {
        let src = "make x get 2\nif to say ( x na 2 ) start make y get 1 end if not so start make y get 0 end";
        let mut parser = Parser::new(Lexer::new(src));
        let program = parser.parse_program().unwrap();
        let mut interpreter = Interpreter::new();
        assert!(interpreter.eval_program(&program).is_ok());
        assert_eq!(interpreter.lookup_variable("y", (0, 0)).unwrap(), &Value::Number(1.0));
    }
}
