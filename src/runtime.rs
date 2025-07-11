//! The runtime for NaijaScript.

use std::borrow::Cow;

use crate::builtins::Builtin;
use crate::diagnostics::{AsStr, Diagnostics, Label, Severity, Span};
use crate::syntax::parser::{
    Arena, ArgList, ArgListId, BinOp, Block, BlockId, CmpOp, Cond, CondId, Expr, ExprId, ParamList,
    ParamListId, Stmt, StmtId,
};

/// Runtime errors that can occur during NaijaScript execution.
///
/// We separate runtime errors from syntax/semantic errors because these
/// can only be detected when expressions actually evaluate.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RuntimeErrorKind {
    /// Division by zero, which we can't catch until the divisor evaluates to zero
    DivisionByZero,
    /// Number parsing failed when lexer accepts malformed numbers that f64::parse rejects
    InvalidNumber,
    /// Call stack overflow to prevent infinite recursion from crashing the host
    StackOverflow,
}

impl AsStr for RuntimeErrorKind {
    fn as_str(&self) -> &'static str {
        match self {
            RuntimeErrorKind::DivisionByZero => "Division by zero",
            RuntimeErrorKind::InvalidNumber => "Invalid number",
            RuntimeErrorKind::StackOverflow => "Stack overflow",
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
/// Numbers are f64 to match JavaScript/Lua simplicity,
/// and strings use Cow<str> for zero-copy handling.
#[derive(Debug, Clone, PartialEq)]
pub enum Value<'src> {
    /// All numbers are f64 to keep arithmetic simple and avoid int/float distinction
    Number(f64),
    /// String literals reference original source when possible
    Str(Cow<'src, str>),
    /// Standard boolean values
    Bool(bool),
    /// Function definition for callable values
    Function(FunctionDef<'src>),
}

/// Function definition stored at runtime, leveraging semantic analysis guarantees
#[derive(Debug, Clone)]
pub struct FunctionDef<'src> {
    name: &'src str,
    params: ParamListId, // Arena reference, semantic analysis guarantees validity
    body: BlockId,       // Arena reference, semantic analysis guarantees validity
}

impl<'src> PartialEq for FunctionDef<'src> {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
    }
}

/// Activation record for function calls, represents a function call frame
#[derive(Debug, Clone)]
struct ActivationRecord<'src> {
    local_vars: Vec<(&'src str, Value<'src>)>, // Parameters become local variables
}

/// Controls how function execution should proceed after statements
#[derive(Debug, Clone, PartialEq)]
enum ExecFlow<'src> {
    Continue,
    Return(Value<'src>), // Early return with value
}

impl std::fmt::Display for Value<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Number(n) => write!(f, "{n}"),
            Value::Str(s) => write!(f, "{s}"),
            Value::Bool(b) => write!(f, "{b}"),
            Value::Function(func) => write!(f, "[Function {}]", func.name),
        }
    }
}

/// Tree-walking interpreter for NaijaScript.
///
/// Tree-walking is simple to debug and understand,
/// perfect for an educational language. The interpreter borrows AST
/// data rather than owning it to keep memory usage low.
pub struct Interpreter<'src> {
    // Borrowed arena references, we never modify the AST
    stmts: &'src Arena<Stmt<'src>>,
    exprs: &'src Arena<Expr<'src>>,
    conds: &'src Arena<Cond>,
    blocks: &'src Arena<Block>,
    params: &'src Arena<ParamList<'src>>,
    args: &'src Arena<ArgList>,

    /// Variable scopes, each Vec is a scope, inner Vec is variables in that scope
    env: Vec<Vec<(&'src str, Value<'src>)>>,

    /// Global function table, functions are first-class but stored separately
    functions: Vec<FunctionDef<'src>>,

    /// Call stack for function execution, prevents infinite recursion
    call_stack: Vec<ActivationRecord<'src>>,

    /// Stack depth limit, reasonable default to prevent host crashes
    max_call_depth: usize,

    /// Error accumulator, we collect rather than panic for better UX
    pub errors: Diagnostics,

    /// Output from `shout()` function calls, public for caller access
    pub output: Vec<Value<'src>>,
}

impl<'src> Interpreter<'src> {
    /// Creates a new interpreter instance bound to the given AST arenas.
    pub fn new(
        stmts: &'src Arena<Stmt<'src>>,
        exprs: &'src Arena<Expr<'src>>,
        conds: &'src Arena<Cond>,
        blocks: &'src Arena<Block>,
        params: &'src Arena<ParamList<'src>>,
        args: &'src Arena<ArgList>,
    ) -> Self {
        Interpreter {
            stmts,
            exprs,
            conds,
            blocks,
            params,
            args,
            env: Vec::new(),
            functions: Vec::new(),
            call_stack: Vec::new(),
            max_call_depth: 1000, // Reasonable default to prevent stack overflow
            errors: Diagnostics::default(),
            output: Vec::new(),
        }
    }

    /// Executes a NaijaScript program starting from the root block.
    ///
    /// Fail-fast execution where the first runtime error stops
    /// the program, matching user expectations for scripting languages.
    pub fn run(&mut self, root: BlockId) -> &Diagnostics {
        self.env.push(Vec::new()); // enter global scope

        // Execute the program and handle both regular flow and early returns
        match self.exec_block_with_flow(root) {
            Ok(..) => {} // Normal completion
            Err(err) => {
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
                    RuntimeErrorKind::StackOverflow => {
                        vec![Label {
                            span: err.span.clone(),
                            message: Cow::Borrowed("Function call too deep, stop the recursion"),
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
            }
        }
        self.env.pop(); // exit global scope
        &self.errors
    }

    // Statement execution dispatcher
    //
    // Each statement type updates the environment or controls flow.
    // Functions definitions populate the global function table.
    fn exec_stmt(&mut self, sid: StmtId) -> Result<ExecFlow<'src>, RuntimeError<'src>> {
        match &self.stmts.nodes[sid.0] {
            Stmt::Assign { var, expr, .. } => {
                let val = self.eval_expr(*expr)?;
                self.insert_or_update(var, val);
                Ok(ExecFlow::Continue)
            }
            Stmt::AssignExisting { var, expr, .. } => {
                let val = self.eval_expr(*expr)?;
                self.update_existing(var, val);
                Ok(ExecFlow::Continue)
            }
            Stmt::If { cond, then_b, else_b, .. } => {
                if self.eval_cond(*cond)? {
                    self.exec_block_with_flow(*then_b)
                } else if let Some(eb) = else_b {
                    self.exec_block_with_flow(*eb)
                } else {
                    Ok(ExecFlow::Continue)
                }
            }
            Stmt::Loop { cond, body, .. } => {
                while self.eval_cond(*cond)? {
                    match self.exec_block_with_flow(*body)? {
                        ExecFlow::Continue => continue,
                        flow @ ExecFlow::Return(..) => return Ok(flow), // Bubble up return
                    }
                }
                Ok(ExecFlow::Continue)
            }
            Stmt::Block { block, .. } => self.exec_block_with_flow(*block),
            Stmt::FunctionDef { name, params, body, .. } => {
                // Store function for later calls, semantic analysis ensures no duplicates
                let func_def = FunctionDef { name, params: *params, body: *body };
                self.functions.push(func_def);
                Ok(ExecFlow::Continue)
            }
            Stmt::Return { expr, .. } => {
                // Semantic analysis guarantees we're inside a function
                let return_val = if let Some(expr_id) = expr {
                    self.eval_expr(*expr_id)?
                } else {
                    Value::Number(0.0) // Default return value
                };
                Ok(ExecFlow::Return(return_val))
            }
            Stmt::Expression { expr, .. } => {
                // Execute expression for side effects (e.g., function calls)
                self.eval_expr(*expr)?;
                Ok(ExecFlow::Continue)
            }
        }
    }

    // Block execution with proper scope management and early return handling
    fn exec_block_with_flow(&mut self, bid: BlockId) -> Result<ExecFlow<'src>, RuntimeError<'src>> {
        self.env.push(Vec::new()); // enter new block scope
        let block = &self.blocks.nodes[bid.0];
        for &sid in &block.stmts {
            match self.exec_stmt(sid)? {
                ExecFlow::Continue => continue,
                flow @ ExecFlow::Return(..) => {
                    self.env.pop(); // exit block scope before returning
                    return Ok(flow);
                }
            }
        }

        self.env.pop(); // exit block scope
        Ok(ExecFlow::Continue)
    }

    // Condition evaluation, compares two expressions using the specified operator
    //
    // Number, string, and boolean comparisons are supported. Semantic analysis
    // guarantees both sides have compatible types.
    fn eval_cond(&mut self, cid: CondId) -> Result<bool, RuntimeError<'src>> {
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
                    // Boolean ordering: false < true
                    CmpOp::Gt => l > r,
                    CmpOp::Lt => l < r,
                };
                Ok(result)
            }
            _ => unreachable!("Semantic analysis should guarantee only valid comparisons"),
        }
    }

    fn eval_expr(&mut self, eid: ExprId) -> Result<Value<'src>, RuntimeError<'src>> {
        match &self.exprs.nodes[eid.0] {
            Expr::Number(n, span) => n
                .parse::<f64>()
                .map(Value::Number)
                .map_err(|_| RuntimeError { kind: RuntimeErrorKind::InvalidNumber, span }),
            Expr::String(s, ..) => Ok(Value::Str(s.clone())),
            Expr::Bool(b, ..) => Ok(Value::Bool(*b)),
            Expr::Var(v, ..) => {
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
                            return Ok(Value::Bool(false)); // Short-circuit evaluation
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
                            return Ok(Value::Bool(true)); // Short-circuit evaluation
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
                            (Value::Str(ls), Value::Number(n)) => match op {
                                BinOp::Add => {
                                    let num_str = n.to_string();
                                    let mut s = String::with_capacity(ls.len() + num_str.len());
                                    s.push_str(&ls);
                                    s.push_str(&num_str);
                                    Ok(Value::Str(Cow::Owned(s)))
                                }
                                _ => unreachable!(
                                    "Semantic analysis should guarantee only valid string-number operations"
                                ),
                            },
                            (Value::Number(n), Value::Str(rs)) => match op {
                                BinOp::Add => {
                                    let num_str = n.to_string();
                                    let mut s = String::with_capacity(num_str.len() + rs.len());
                                    s.push_str(&num_str);
                                    s.push_str(&rs);
                                    Ok(Value::Str(Cow::Owned(s)))
                                }
                                _ => unreachable!(
                                    "Semantic analysis should guarantee only valid number-string operations"
                                ),
                            },
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
            Expr::Call { callee, args, span } => {
                // Function calls, semantic analysis guarantees function exists
                self.eval_function_call(*callee, *args, span)
            }
        }
    }

    // Function call execution with proper call stack management
    fn eval_function_call(
        &mut self,
        callee: ExprId,
        args: ArgListId,
        span: &'src Span,
    ) -> Result<Value<'src>, RuntimeError<'src>> {
        // Extract function name, semantic analysis guarantees it's a variable
        let func_name = match &self.exprs.nodes[callee.0] {
            Expr::Var(name, ..) => *name,
            _ => unreachable!("Semantic analysis should guarantee callee is a variable"),
        };

        // Check if this is a built-in function first
        if let Some(builtin) = Builtin::from_name(func_name) {
            return self.eval_builtin_call(builtin, args);
        }

        // Find user-defined function, semantic analysis guarantees it exists
        let func_def = self
            .functions
            .iter()
            .find(|f| f.name == func_name)
            .expect("Semantic analysis should guarantee function exists")
            .clone(); // Clone to avoid borrow checker issues

        // Prevent stack overflow from infinite recursion
        if self.call_stack.len() >= self.max_call_depth {
            return Err(RuntimeError { kind: RuntimeErrorKind::StackOverflow, span });
        }

        // Evaluate arguments in order
        let arg_list = &self.args.nodes[args.0];
        let mut arg_values = Vec::new();
        for &arg_expr in &arg_list.args {
            arg_values.push(self.eval_expr(arg_expr)?);
        }

        // Parameter count validated by semantic analysis
        let param_list = &self.params.nodes[func_def.params.0];
        assert_eq!(
            arg_values.len(),
            param_list.params.len(),
            "Semantic analysis should guarantee parameter count"
        );

        // Bind arguments to parameters
        let mut local_vars = Vec::new();
        for (param_name, arg_value) in param_list.params.iter().zip(arg_values.iter()) {
            local_vars.push((*param_name, arg_value.clone()));
        }

        let activation = ActivationRecord { local_vars };

        // Push activation record onto call stack
        self.call_stack.push(activation);

        // Execute function body
        let result = match self.exec_block_with_flow(func_def.body)? {
            ExecFlow::Continue => Value::Number(0.0), // No explicit return
            ExecFlow::Return(val) => val,             // Explicit return value
        };

        // Pop activation record
        self.call_stack.pop();

        Ok(result)
    }

    // Built-in function execution
    fn eval_builtin_call(
        &mut self,
        builtin: Builtin,
        args: ArgListId,
    ) -> Result<Value<'src>, RuntimeError<'src>> {
        // Evaluate arguments in order
        let arg_list = &self.args.nodes[args.0];
        let mut arg_values = Vec::new();
        for &arg_expr in &arg_list.args {
            arg_values.push(self.eval_expr(arg_expr)?);
        }

        let expected_arity = builtin.arity();
        assert_eq!(
            arg_values.len(),
            expected_arity,
            "Semantic analysis should guarantee builtin parameter count"
        );

        // Execute the built-in function
        match builtin {
            Builtin::Shout => {
                let value = &arg_values[0];
                self.output.push(value.clone());
                Ok(Value::Number(0.0))
            }
        }
    }

    // Variable assignment, adds or updates in current scope
    fn insert_or_update(&mut self, var: &'src str, val: Value<'src>) {
        if let Some((_, slot)) =
            self.env.last_mut().unwrap().iter_mut().find(|(name, _)| *name == var)
        {
            *slot = val;
        } else {
            self.env.last_mut().unwrap().push((var, val));
        }
    }

    // Variable reassignment, updates existing variable in any scope
    //
    // Check function locals first, then outer scopes. Semantic analysis
    // guarantees the variable exists somewhere.
    fn update_existing(&mut self, var: &'src str, val: Value<'src>) {
        // First try function-local variables
        if let Some(activation) = self.call_stack.last_mut()
            && let Some((.., slot)) =
                activation.local_vars.iter_mut().find(|(name, ..)| *name == var)
        {
            *slot = val;
            return;
        }

        // Then try regular scope stack
        for scope in self.env.iter_mut().rev() {
            if let Some((.., slot)) = scope.iter_mut().find(|(name, ..)| *name == var) {
                *slot = val;
                return;
            }
        }
        unreachable!("Semantic analysis should guarantee variable exists");
    }

    // Variable lookup, searches function locals first, then outer scopes
    fn lookup_var(&self, var: &str) -> Option<Value<'src>> {
        // Function locals take precedence over outer scopes
        if let Some(activation) = self.call_stack.last()
            && let Some((.., val)) = activation.local_vars.iter().find(|(name, ..)| *name == var)
        {
            return Some(val.clone());
        }

        // Then check regular scope stack (global and block scopes)
        for scope in self.env.iter().rev() {
            if let Some((.., val)) = scope.iter().find(|(name, ..)| *name == var) {
                return Some(val.clone());
            }
        }
        None
    }
}
