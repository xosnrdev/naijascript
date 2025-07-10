//! The runtime for NaijaScript.

use std::borrow::Cow;

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
/// is set up to easily add booleans, functions, etc.
#[derive(Debug, Clone, PartialEq)]
pub enum Value<'src> {
    /// All numbers are f64 internally - keeps arithmetic simple and matches
    /// what most dynamic languages do (JavaScript, Lua, etc.)
    Number(f64),
    /// String literals that reference the original source
    Str(Cow<'src, str>),
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

    /// Keeps track of all variables for each scope as we enter and exit blocks.
    /// Each inner vector is a scope, and each tuple holds a variable name and its value.
    env: Vec<Vec<(&'src str, Value<'src>)>>,

    /// Accumulated diagnostics - we collect these instead of panicking so the
    /// caller can decide how to handle runtime errors (continue, stop, etc.)
    pub errors: Diagnostics,

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
        self.env.push(Vec::new()); // enter global scope
        let block = &self.blocks.nodes[root.0];
        for &sid in &block.stmts {
            if let Err(err) = self.exec_stmt(sid) {
                let labels = match err.kind {
                    RuntimeErrorKind::DivisionByZero => vec![Label {
                        span: err.span.clone(),
                        message: Cow::Borrowed("You divide by zero for here"),
                    }],
                    RuntimeErrorKind::InvalidNumber => {
                        vec![Label {
                            span: err.span.clone(),
                            message: Cow::Borrowed("This number no correct"),
                        }]
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
        self.env.pop(); // exit global scope
        &self.errors
    }

    // Executes a single statement - the core of our statement dispatch logic.
    //
    // Each statement type has its own execution semantics:
    // - Variable assignment/reassignment updates the environment
    // - Shout statements evaluate expressions and capture output
    // - Control flow (if/loop) recursively execute blocks based on conditions
    fn exec_stmt(&mut self, sid: StmtId) -> Result<(), RuntimeError<'src>> {
        match &self.stmts.nodes[sid.0] {
            Stmt::Assign { var, expr, .. } => {
                let val = self.eval_expr(*expr)?;
                self.insert_or_update(var, val);
                Ok(())
            }
            Stmt::AssignExisting { var, expr, .. } => {
                let val = self.eval_expr(*expr)?;
                self.update_existing(var, val);
                Ok(())
            }
            Stmt::Shout { expr, .. } => {
                let val = self.eval_expr(*expr)?;
                self.output.push(val);
                Ok(())
            }
            Stmt::If { cond, then_b, else_b, .. } => {
                if self.eval_cond(*cond)? {
                    self.exec_block(*then_b)
                } else if let Some(eb) = else_b {
                    self.exec_block(*eb)
                } else {
                    Ok(())
                }
            }
            Stmt::Loop { cond, body, .. } => {
                while self.eval_cond(*cond)? {
                    self.exec_block(*body)?;
                }
                Ok(())
            }
            Stmt::Block { block, .. } => self.exec_block(*block),
            Stmt::FunctionDef { .. } | Stmt::Return { .. } => todo!(),
            Stmt::Expression { .. } => todo!(),
        }
    }

    fn exec_block(&mut self, bid: BlockId) -> Result<(), RuntimeError<'src>> {
        self.env.push(Vec::new()); // enter new block scope
        let block = &self.blocks.nodes[bid.0];
        for &sid in &block.stmts {
            self.exec_stmt(sid)?;
        }
        self.env.pop(); // exit block scope
        Ok(())
    }

    // Evaluates a condition expression and returns its boolean result.
    //
    // Conditions in NaijaScript are always binary comparisons between two numeric
    // expressions. We evaluate both sides to numbers first, then apply the comparison
    // operator. This is simpler than having a separate boolean type system.
    fn eval_cond(&self, cid: CondId) -> Result<bool, RuntimeError<'src>> {
        let c = &self.conds.nodes[cid.0];
        let lhs = self.eval_expr(c.lhs)?;
        let rhs = self.eval_expr(c.rhs)?;
        match (&lhs, &rhs) {
            (Value::Number(l), Value::Number(r)) => {
                let result = match c.op {
                    CmpOp::Eq => l == r,
                    CmpOp::Gt => l > r,
                    CmpOp::Lt => l < r,
                };
                Ok(result)
            }
            (Value::Str(l), Value::Str(r)) => {
                let result = match c.op {
                    CmpOp::Eq => l == r,
                    CmpOp::Gt => l > r,
                    CmpOp::Lt => l < r,
                };
                Ok(result)
            }
            (Value::Bool(l), Value::Bool(r)) => {
                let result = match c.op {
                    CmpOp::Eq => l == r,
                    // Boolean comparison follows Rust's standard where false equals 0 and true equals 1, so false comes before true in ordering
                    CmpOp::Gt => l > r,
                    CmpOp::Lt => l < r,
                };
                Ok(result)
            }
            _ => unreachable!("Semantic analysis should guarantee only valid comparisons"),
        }
    }

    fn eval_expr(&self, eid: ExprId) -> Result<Value<'src>, RuntimeError<'src>> {
        match &self.exprs.nodes[eid.0] {
            Expr::Number(n, span) => n
                .parse::<f64>()
                .map(Value::Number)
                .map_err(|_| RuntimeError { kind: RuntimeErrorKind::InvalidNumber, span }),
            Expr::String(s, _) => Ok(Value::Str(s.clone())),
            Expr::Bool(b, _) => Ok(Value::Bool(*b)),
            Expr::Var(v, _) => {
                let val = self
                    .lookup_var(v)
                    .expect("Semantic analysis should guarantee all variables are declared");
                Ok(val)
            }
            Expr::Binary { op, lhs, rhs, span } => {
                match op {
                    BinOp::And => {
                        let l = self.eval_expr(*lhs)?;
                        if let Value::Bool(false) = l {
                            return Ok(Value::Bool(false)); // short-circuit
                        }
                        let r = self.eval_expr(*rhs)?;
                        match r {
                            Value::Bool(b) => Ok(Value::Bool(b)),
                            _ => unreachable!(
                                "Semantic analysis should guarantee only valid boolean expressions"
                            ),
                        }
                    }
                    BinOp::Or => {
                        let l = self.eval_expr(*lhs)?;
                        if let Value::Bool(true) = l {
                            return Ok(Value::Bool(true)); // short-circuit
                        }
                        let r = self.eval_expr(*rhs)?;
                        match r {
                            Value::Bool(b) => Ok(Value::Bool(b)),
                            _ => unreachable!(
                                "Semantic analysis should guarantee only valid boolean expressions"
                            ),
                        }
                    }
                    _ => {
                        let l = self.eval_expr(*lhs)?;
                        let r = self.eval_expr(*rhs)?;
                        match (l, r) {
                            (Value::Number(lv), Value::Number(rv)) => match op {
                                BinOp::Add => Ok(Value::Number(lv + rv)),
                                BinOp::Minus => Ok(Value::Number(lv - rv)),
                                BinOp::Times => Ok(Value::Number(lv * rv)),
                                BinOp::Divide | BinOp::Mod => {
                                    if rv == 0.0 {
                                        Err(RuntimeError {
                                            kind: RuntimeErrorKind::DivisionByZero,
                                            span,
                                        })
                                    } else {
                                        Ok(Value::Number(match op {
                                            BinOp::Divide => lv / rv,
                                            BinOp::Mod => lv % rv,
                                            _ => unreachable!(
                                                "Unexpected binary operation for numbers"
                                            ),
                                        }))
                                    }
                                }
                                _ => unreachable!("Unexpected op for numbers"),
                            },
                            (Value::Str(ls), Value::Str(rs)) => match op {
                                BinOp::Add => {
                                    let mut s = String::with_capacity(ls.len() + rs.len());
                                    s.push_str(&ls);
                                    s.push_str(&rs);
                                    Ok(Value::Str(Cow::Owned(s)))
                                }
                                _ => unreachable!(
                                    "Semantic analysis should guarantee only valid string operations"
                                ),
                            },
                            (Value::Str(_), Value::Number(_))
                            | (Value::Number(_), Value::Str(_)) => {
                                unreachable!(
                                    "Semantic analysis should guarantee only valid type combinations"
                                )
                            }
                            _ => unreachable!(
                                "Semantic analysis should guarantee only valid expressions"
                            ),
                        }
                    }
                }
            }
            Expr::Not { expr, .. } => {
                let v = self.eval_expr(*expr)?;
                match v {
                    Value::Bool(b) => Ok(Value::Bool(!b)),
                    _ => unreachable!(
                        "Semantic analysis should guarantee only valid boolean expressions"
                    ),
                }
            }
            Expr::Call { .. } => todo!(),
        }
    }

    // Adds a new variable to the current scope, or updates its value if it already exists in this scope.
    fn insert_or_update(&mut self, var: &'src str, val: Value<'src>) {
        if let Some((_, slot)) =
            self.env.last_mut().unwrap().iter_mut().find(|(name, _)| *name == var)
        {
            *slot = val;
        } else {
            self.env.last_mut().unwrap().push((var, val));
        }
    }

    // Looks for the variable in all scopes starting from the innermost one,
    // and updates its value as soon as it is found.
    fn update_existing(&mut self, var: &'src str, val: Value<'src>) {
        for scope in self.env.iter_mut().rev() {
            if let Some((_, slot)) = scope.iter_mut().find(|(name, _)| *name == var) {
                *slot = val;
                return;
            }
        }
        unreachable!("Semantic analysis should guarantee variable exists");
    }

    // Looks up a variable by its name,
    // starting from the innermost scope and moving outward.
    fn lookup_var(&self, var: &str) -> Option<Value<'src>> {
        for scope in self.env.iter().rev() {
            if let Some((_, val)) = scope.iter().find(|(name, _)| *name == var) {
                return Some(val.clone());
            }
        }
        None
    }
}
