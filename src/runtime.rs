//! The runtime for NaijaScript.

use crate::diagnostics::{AsStr, Diagnostics, Label, Severity, Span};
use crate::syntax::parser::{
    Arena, BinOp, Block, BlockId, CmpOp, Cond, CondId, Expr, ExprId, Stmt, StmtId,
};

/// Runtime errors that can occur during NaijaScript execution.
///
/// These represent the "dynamic" errors that we can only catch at runtime,
/// as opposed to syntax or semantic errors caught during parsing/analysis.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RuntimeErrorKind {
    /// Mathematical division by zero - catches both literal `1 divide 0`
    /// and computed divisions that evaluate to zero at runtime
    DivisionByZero,
    /// Number parsing failed - this happens when the lexer accepts a malformed
    /// number that looks valid syntactically but can't be parsed as f64
    InvalidNumber,
}

impl AsStr for RuntimeErrorKind {
    fn as_str(&self) -> &'static str {
        match self {
            RuntimeErrorKind::DivisionByZero => "You no fit divide by zero",
            RuntimeErrorKind::InvalidNumber => "Dis number no correct",
        }
    }
}

#[derive(Debug, Clone)]
pub struct RuntimeError<'src> {
    /// The specific kind of runtime error that occurred.
    pub kind: RuntimeErrorKind,
    /// Reference to the source span.
    pub span: &'src Span,
}

/// The value types our interpreter can work with at runtime.
///
/// Right now we're keeping it simple with just numbers, but the architecture
/// is set up to easily add strings, booleans, functions, etc.
#[derive(Debug, Clone, PartialEq)]
pub enum Value<'src> {
    /// All numbers are f64 internally - keeps arithmetic simple and matches
    /// what most dynamic languages do (JavaScript, Lua, etc.)
    Number(f64),
    /// String literals that reference the original source
    Str(&'src str),
    /// Standard boolean values for future conditional expressions
    Bool(bool),
}

impl std::fmt::Display for Value<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Number(n) => write!(f, "{n}"),
            Value::Str(s) => write!(f, "{s}"),
            Value::Bool(b) => write!(f, "{b}"),
        }
    }
}

/// Tree-walking interpreter for NaijaScript.
///
/// This follows the classic "interpreter pattern" where we recursively evaluate
/// AST nodes by walking the tree structure. While not the fastest approach (compared
/// to bytecode VMs), it's simple to understand and debug, making it perfect for
/// an educational language.
///
/// The interpreter borrows all AST data from the parser rather than owning it,
/// which keeps memory usage low and makes the relationship between parsing and
/// execution crystal clear.
pub struct Interpreter<'src> {
    // Borrowed references to parser's AST arenas - we never modify these
    stmts: &'src Arena<Stmt<'src>>,
    exprs: &'src Arena<Expr<'src>>,
    conds: &'src Arena<Cond>,
    blocks: &'src Arena<Block>,

    /// Simple variable environment as a vector of (name, value) pairs.
    /// This gives us O(n) variable lookup, but for small programs it's fine
    /// and keeps the implementation dead simple. A HashMap would be overkill here.
    env: Vec<(&'src str, f64)>,

    /// Accumulated diagnostics - we collect these instead of panicking so the
    /// caller can decide how to handle runtime errors (continue, stop, etc.)
    errors: Diagnostics,

    /// Captured output from `shout()` statements. This is public so callers
    /// can access program output after execution completes.
    pub output: Vec<Value<'src>>,
}

impl<'src> Interpreter<'src> {
    /// Creates a new interpreter instance bound to the given AST arenas.
    pub fn new(
        stmts: &'src Arena<Stmt<'src>>,
        exprs: &'src Arena<Expr<'src>>,
        conds: &'src Arena<Cond>,
        blocks: &'src Arena<Block>,
    ) -> Self {
        Interpreter {
            stmts,
            exprs,
            conds,
            blocks,
            env: Vec::new(),
            errors: Diagnostics::default(),
            output: Vec::new(),
        }
    }

    /// Executes a NaijaScript program starting from the root block.
    ///
    /// This is the main entry point for program execution. We use a fail-fast
    /// approach where the first runtime error stops execution and gets reported.
    /// This matches user expectations for most scripting languages.
    pub fn run(&mut self, root: BlockId) -> &Diagnostics {
        let block = &self.blocks.nodes[root.0];
        for &sid in &block.stmts {
            if let Err(err) = self.exec_stmt(sid) {
                let labels = match err.kind {
                    RuntimeErrorKind::DivisionByZero => vec![Label {
                        span: err.span.clone(),
                        message: "You divide by zero for here",
                    }],
                    RuntimeErrorKind::InvalidNumber => {
                        vec![Label { span: err.span.clone(), message: "This number no correct" }]
                    }
                };
                self.errors.emit(
                    err.span.clone(),
                    Severity::Error,
                    "runtime",
                    err.kind.as_str(),
                    labels,
                );
                break;
            }
        }
        &self.errors
    }

    /// Executes a single statement - the core of our statement dispatch logic.
    ///
    /// Each statement type has its own execution semantics:
    /// - Variable assignment/reassignment updates the environment
    /// - Shout statements evaluate expressions and capture output
    /// - Control flow (if/loop) recursively execute blocks based on conditions
    fn exec_stmt(&mut self, sid: StmtId) -> Result<(), RuntimeError<'src>> {
        match &self.stmts.nodes[sid.0] {
            Stmt::Assign { var, expr, .. } => {
                let val = self.eval_expr(*expr)?;
                self.insert_or_update(var, val);
                Ok(())
            }
            Stmt::AssignExisting { var, expr, .. } => {
                let val = self.eval_expr(*expr)?;
                // The semantic analyzer guarantees this variable exists, so we can safely unwrap.
                let slot = self
                    .env
                    .iter_mut()
                    .find(|(name, _)| *name == *var)
                    .expect("Semantic analysis should guarantee variable exists");
                slot.1 = val;
                Ok(())
            }
            Stmt::Shout { expr, .. } => {
                let val = self.eval_expr(*expr)?;
                self.output.push(Value::Number(val));
                Ok(())
            }
            Stmt::If { cond, then_b, else_b, .. } => {
                // Check condition, execute appropriate branch
                if self.eval_cond(*cond)? {
                    self.exec_block(*then_b)
                } else if let Some(eb) = else_b {
                    self.exec_block(*eb)
                } else {
                    Ok(())
                }
            }
            Stmt::Loop { cond, body, .. } => {
                // Keep executing body while condition is true
                // We check the condition before each iteration, including the first
                while self.eval_cond(*cond)? {
                    self.exec_block(*body)?;
                }
                Ok(())
            }
        }
    }

    fn exec_block(&mut self, bid: BlockId) -> Result<(), RuntimeError<'src>> {
        let block = &self.blocks.nodes[bid.0];
        for &sid in &block.stmts {
            self.exec_stmt(sid)?;
        }
        Ok(())
    }

    /// Evaluates a condition expression and returns its boolean result.
    ///
    /// Conditions in NaijaScript are always binary comparisons between two numeric
    /// expressions. We evaluate both sides to numbers first, then apply the comparison
    /// operator. This is simpler than having a separate boolean type system.
    fn eval_cond(&self, cid: CondId) -> Result<bool, RuntimeError<'src>> {
        let c = &self.conds.nodes[cid.0];
        let lhs = self.eval_expr(c.lhs)?;
        let rhs = self.eval_expr(c.rhs)?;
        // Direct f64 comparison - we don't worry about floating point precision issues
        // for now since this is an educational language focused on simplicity
        let result = match c.op {
            CmpOp::Eq => lhs == rhs,
            CmpOp::Gt => lhs > rhs,
            CmpOp::Lt => lhs < rhs,
        };
        Ok(result)
    }

    fn eval_expr(&self, eid: ExprId) -> Result<f64, RuntimeError<'src>> {
        match &self.exprs.nodes[eid.0] {
            Expr::Number(n, span) => {
                // Parse the string representation into an f64. The lexer has already
                // validated basic number syntax, but we still need to handle edge cases
                // like numbers too large for f64 representation.
                n.parse::<f64>()
                    .map_err(|_| RuntimeError { kind: RuntimeErrorKind::InvalidNumber, span })
            }
            Expr::Var(v, _) => {
                // Variable lookup in our simple linear environment. The semantic analyzer
                // has guaranteed this variable exists, so we can safely unwrap here.
                // This is much faster than returning Result and checking everywhere.
                let val = self
                    .env
                    .iter()
                    .find(|(name, _)| *name == *v)
                    .map(|(_, val)| *val)
                    .expect("Semantic analysis should guarantee all variables are declared");
                Ok(val)
            }
            Expr::Binary { op, lhs, rhs, span } => {
                // Standard binary operator evaluation with left-to-right operand evaluation.
                // We evaluate both operands before applying the operator, which matches
                // most programming language semantics.
                let l = self.eval_expr(*lhs)?;
                let r = self.eval_expr(*rhs)?;
                match op {
                    BinOp::Add => Ok(l + r),
                    BinOp::Minus => Ok(l - r),
                    BinOp::Times => Ok(l * r),
                    BinOp::Divide => {
                        // Division by zero is the classic runtime error - we check for it
                        // explicitly rather than letting the FPU return infinity/NaN
                        if r == 0.0 {
                            Err(RuntimeError { kind: RuntimeErrorKind::DivisionByZero, span })
                        } else {
                            Ok(l / r)
                        }
                    }
                }
            }
        }
    }

    /// Helper for variable assignment - handles both new declarations and updates.
    ///
    /// This implements the "upsert" pattern: if the variable already exists in our
    /// environment, update its value; otherwise, add it as a new binding.
    /// This keeps the assignment logic simple and avoids duplicate code between
    /// initial declaration and reassignment.
    fn insert_or_update(&mut self, var: &'src str, val: f64) {
        if let Some((_, slot)) = self.env.iter_mut().find(|(name, _)| *name == var) {
            *slot = val;
        } else {
            self.env.push((var, val));
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::resolver::{SemAnalyzer, SemanticError};
    use crate::syntax::parser::Parser;

    #[test]
    fn test_assignment_and_shout() {
        let src = "make x get 5\nshout(x)";
        let mut parser = Parser::new(src);
        let (root, parse_errors) = parser.parse_program();
        assert!(parse_errors.diagnostics.is_empty());
        let mut interp = Interpreter::new(
            &parser.stmt_arena,
            &parser.expr_arena,
            &parser.cond_arena,
            &parser.block_arena,
        );
        interp.run(root);
        assert_eq!(interp.output, vec![Value::Number(5.0)]);
    }

    #[test]
    fn test_reassignment() {
        let src = "make x get 2\nx get 7\nshout(x)";
        let mut parser = Parser::new(src);
        let (root, parse_errors) = parser.parse_program();
        assert!(parse_errors.diagnostics.is_empty());
        let mut interp = Interpreter::new(
            &parser.stmt_arena,
            &parser.expr_arena,
            &parser.cond_arena,
            &parser.block_arena,
        );
        interp.run(root);
        assert_eq!(interp.output, vec![Value::Number(7.0)]);
    }

    #[test]
    fn test_expression_arithmetic() {
        let src = "make x get 2 add 3 times 4\nshout(x)"; // 2 + (3*4) = 14
        let mut parser = Parser::new(src);
        let (root, parse_errors) = parser.parse_program();
        assert!(parse_errors.diagnostics.is_empty());
        let mut interp = Interpreter::new(
            &parser.stmt_arena,
            &parser.expr_arena,
            &parser.cond_arena,
            &parser.block_arena,
        );
        interp.run(root);
        assert_eq!(interp.output, vec![Value::Number(14.0)]);
    }

    #[test]
    fn test_if_statement_then() {
        let src = "make x get 1\nif to say (x na 1) start shout(42) end";
        let mut parser = Parser::new(src);
        let (root, parse_errors) = parser.parse_program();
        assert!(parse_errors.diagnostics.is_empty());
        let mut interp = Interpreter::new(
            &parser.stmt_arena,
            &parser.expr_arena,
            &parser.cond_arena,
            &parser.block_arena,
        );
        interp.run(root);
        assert_eq!(interp.output, vec![Value::Number(42.0)]);
    }

    #[test]
    fn test_if_statement_else() {
        let src =
            "make x get 2\nif to say (x na 1) start shout(1) end if not so start shout(2) end";
        let mut parser = Parser::new(src);
        let (root, parse_errors) = parser.parse_program();
        assert!(parse_errors.diagnostics.is_empty());
        let mut interp = Interpreter::new(
            &parser.stmt_arena,
            &parser.expr_arena,
            &parser.cond_arena,
            &parser.block_arena,
        );
        interp.run(root);
        assert_eq!(interp.output, vec![Value::Number(2.0)]);
    }

    #[test]
    fn test_loop_statement() {
        let src = "make x get 1\njasi (x small pass 4) start shout(x) x get x add 1 end";
        let mut parser = Parser::new(src);
        let (root, parse_errors) = parser.parse_program();
        assert!(parse_errors.diagnostics.is_empty());
        let mut interp = Interpreter::new(
            &parser.stmt_arena,
            &parser.expr_arena,
            &parser.cond_arena,
            &parser.block_arena,
        );
        interp.run(root);
        assert_eq!(interp.output, vec![Value::Number(1.0), Value::Number(2.0), Value::Number(3.0)]);
    }

    #[test]
    fn test_division_by_zero_error() {
        let src = "make x get 1 divide 0\nshout(x)";
        let mut parser = Parser::new(src);
        let (root, parse_errors) = parser.parse_program();
        assert!(parse_errors.diagnostics.is_empty());
        let mut interp = Interpreter::new(
            &parser.stmt_arena,
            &parser.expr_arena,
            &parser.cond_arena,
            &parser.block_arena,
        );
        interp.run(root);
        assert!(
            interp
                .errors
                .diagnostics
                .iter()
                .any(|e| e.message == RuntimeErrorKind::DivisionByZero.as_str())
        );
    }

    #[test]
    fn test_assignment_to_undeclared_variable_error() {
        let src = "x get 5";
        let mut parser = Parser::new(src);
        let (root, parse_errors) = parser.parse_program();
        assert!(parse_errors.diagnostics.is_empty());
        let mut analyzer = SemAnalyzer::new(
            &parser.stmt_arena,
            &parser.expr_arena,
            &parser.cond_arena,
            &parser.block_arena,
        );
        analyzer.analyze(root);
        assert!(
            analyzer
                .errors
                .diagnostics
                .iter()
                .any(|e| e.message == SemanticError::AssignmentToUndeclared.as_str())
        );
    }

    #[test]
    fn test_undeclared_variable_error() {
        let src = "shout(y)";
        let mut parser = Parser::new(src);
        let (root, parse_errors) = parser.parse_program();
        assert!(parse_errors.diagnostics.is_empty());
        let mut analyzer = SemAnalyzer::new(
            &parser.stmt_arena,
            &parser.expr_arena,
            &parser.cond_arena,
            &parser.block_arena,
        );
        analyzer.analyze(root);
        assert!(
            analyzer
                .errors
                .diagnostics
                .iter()
                .any(|e| e.message == SemanticError::UseOfUndeclared.as_str())
        );
    }
}
