use std::collections::HashMap;

use crate::diagnostics::{Diagnostic, DiagnosticHandler};
use crate::syntax::ast::*;

/// Represents a runtime value in NaijaScript.
/// Currently only supports numbers, but can be extended for more types.
#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Number(f64),
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Number(n) => write!(f, "{n}"),
        }
    }
}

/// Interpreter error type, with Pidgin-style user messages.
#[derive(Debug, PartialEq)]
pub enum InterpreterError<'a> {
    UndefinedVariable(&'a str),
    DivisionByZero,
    TypeError(&'static str),
    ScopeError,
    Other(String),
}

impl<'a> InterpreterError<'a> {
    pub fn to_diagnostic(&self, source: &'a str) -> Diagnostic<'a> {
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
                )
            }
            InterpreterError::TypeError(msg) => {
                let (start, end) = find_first_word_span(msg, source);
                let (line, col) = line_col_from_offset(source, start);
                Diagnostic::error("runtime error", msg, source, (start, end), line, col)
            }
            InterpreterError::ScopeError => Diagnostic::error(
                "runtime error",
                "No environment scope found (this na bug)",
                source,
                (0, 0),
                0,
                0,
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
                )
            }
        }
    }
}

/// Type alias for interpreter results, using custom error type.
pub type InterpreterResult<'a, T> = Result<T, InterpreterError<'a>>;

/// The main interpreter struct, holding the environment stack.
/// Each environment is a stack of scopes (HashMaps), allowing for block scoping.
#[derive(Default)]
pub struct Interpreter<'a> {
    env: Vec<HashMap<&'a str, Value>>, // Stack of variable scopes
}

impl<'a> Interpreter<'a> {
    /// Creates a new interpreter with a single global scope.
    pub fn new() -> Self {
        Interpreter { env: vec![HashMap::new()] }
    }

    /// Evaluates a full program (list of statements).
    /// Returns an error if any statement fails.
    pub fn eval_program(&mut self, program: &Program<'a>) -> InterpreterResult<'a, ()> {
        for stmt in &program.statements {
            self.eval_statement(stmt)?;
        }
        Ok(())
    }

    /// Evaluates a full program with error reporting via a diagnostics handler.
    /// Returns an error if any statement fails.
    pub fn eval_program_with_handler(
        &mut self,
        program: &Program<'a>,
        handler: Option<&mut dyn DiagnosticHandler>,
        source: &'a str,
    ) -> InterpreterResult<'a, ()> {
        for stmt in &program.statements {
            if let Err(e) = self.eval_statement(stmt) {
                if let Some(h) = handler {
                    h.report(&e.to_diagnostic(source));
                }
                return Err(e);
            }
        }
        Ok(())
    }

    /// Evaluates a single statement.
    /// Handles assignment, output, if/else, and loop constructs.
    fn eval_statement(&mut self, stmt: &Statement<'a>) -> InterpreterResult<'a, ()> {
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
                // If/else logic: only one branch executes
                if self.eval_condition(condition)? {
                    self.eval_block(then_block)
                } else if let Some(else_block) = else_block {
                    self.eval_block(else_block)
                } else {
                    Ok(())
                }
            }
            Statement::Loop { condition, body } => {
                // NaijaScript 'jasi' loop: executes body while condition is true
                while self.eval_condition(condition)? {
                    self.eval_block(body)?;
                }
                Ok(())
            }
        }
    }

    /// Evaluates a block, pushing a new scope for local variables.
    /// All variables declared in the block are dropped after block ends.
    fn eval_block(&mut self, block: &Block<'a>) -> InterpreterResult<'a, ()> {
        self.env.push(HashMap::new());
        for stmt in &block.statements {
            self.eval_statement(stmt)?;
        }
        self.env.pop();
        Ok(())
    }

    /// Evaluates an expression and returns its value.
    /// Handles literals, variables, binary operations, and groupings.
    fn eval_expression(&mut self, expr: &Expression<'a>) -> InterpreterResult<'a, Value> {
        match expr {
            Expression::Number(n) => Ok(Value::Number(*n)),
            Expression::Variable(name) => {
                // Variable lookup returns a reference; clone to produce a value
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

    /// Evaluates a binary operation (add, minus, times, divide).
    /// Returns error for division by zero.
    fn eval_binary_op(&self, op: &BinaryOp, l: &Value, r: &Value) -> InterpreterResult<'a, Value> {
        match (op, l, r) {
            (BinaryOp::Add, Value::Number(a), Value::Number(b)) => Ok(Value::Number(a + b)),
            (BinaryOp::Minus, Value::Number(a), Value::Number(b)) => Ok(Value::Number(a - b)),
            (BinaryOp::Times, Value::Number(a), Value::Number(b)) => Ok(Value::Number(a * b)),
            (BinaryOp::Divide, Value::Number(_), Value::Number(b)) if *b == 0.0 => {
                // Workaround: Rust panics on division by zero, so we catch it here
                Err(InterpreterError::DivisionByZero)
            }
            (BinaryOp::Divide, Value::Number(a), Value::Number(b)) => Ok(Value::Number(a / b)),
        }
    }

    /// Evaluates a condition (==, >, <) for if/loop constructs.
    fn eval_condition(&mut self, cond: &Condition<'a>) -> InterpreterResult<'a, bool> {
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

    /// Assigns a variable in the nearest scope where it exists, or the global scope if new.
    /// This allows for shadowing and block-local variables.
    fn assign_variable(&mut self, name: &'a str, value: Value) -> InterpreterResult<'a, ()> {
        for scope in self.env.iter_mut().rev() {
            if scope.contains_key(name) {
                scope.insert(name, value);
                return Ok(());
            }
        }
        // If variable not found, insert into global scope
        if let Some(scope) = self.env.first_mut() {
            scope.insert(name, value);
            Ok(())
        } else {
            // Should never happen: env always has at least one scope
            Err(InterpreterError::ScopeError)
        }
    }

    /// Looks up a variable by name, searching from innermost to outermost scope.
    /// Returns a reference to the value, or an error if not found.
    fn lookup_variable(&self, name: &'a str) -> InterpreterResult<'a, &Value> {
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
