//! The runtime for NaijaScript.

use std::fmt::{self, Write};
use std::io;

use crate::arena::{Arena, ArenaCow, ArenaString};
use crate::builtins::{self, Builtin};
use crate::diagnostics::{AsStr, Diagnostics, Label, Severity, Span};
use crate::syntax::parser::{
    ArgList, BinaryOp, BlockRef, Expr, ExprRef, ParamListRef, Stmt, StmtRef, StringParts,
    StringSegment, UnaryOp,
};
#[cfg(not(target_family = "wasm"))]
use crate::sys::{self, Stdin};

/// Runtime errors that can occur during NaijaScript execution.
///
/// We separate runtime errors from syntax/semantic errors because these
/// can only be detected when expressions actually evaluate.
#[derive(Debug, PartialEq, Eq)]
pub enum RuntimeErrorKind {
    /// Division by zero, which we can't catch until the divisor evaluates to zero
    DivisionByZero,
    /// Call stack overflow to prevent infinite recursion from crashing the host
    StackOverflow,
    /// I/O error that occurred
    Io(&'static str),
}

impl AsStr for RuntimeErrorKind {
    fn as_str(&self) -> &'static str {
        match &self {
            RuntimeErrorKind::DivisionByZero => "Division by zero",
            RuntimeErrorKind::StackOverflow => "Stack overflow",
            RuntimeErrorKind::Io(..) => "I/O error",
        }
    }
}

impl From<io::Error> for RuntimeErrorKind {
    fn from(err: io::Error) -> Self {
        // RuntimeErrorKind::Io takes a str with a static lifetime
        // We want to transmute the io::Error into a &'static str
        // TODO: make `AsStr` return `ArenaCow` so we don't have to do this.
        let msg: &'static str = Box::leak(err.to_string().into_boxed_str());
        RuntimeErrorKind::Io(msg)
    }
}

#[derive(Debug)]
struct RuntimeError {
    // The specific kind of runtime error that occurred.
    kind: RuntimeErrorKind,
    // Reference to the source span.
    span: Span,
}

impl From<RuntimeErrorKind> for RuntimeError {
    fn from(kind: RuntimeErrorKind) -> Self {
        RuntimeError { kind, span: Span::default() }
    }
}

/// Maximum recursion depth to prevent stack overflow
const MAX_CALL_DEPTH: usize = 1000;

/// The value types our runtime can work with at runtime.
#[derive(Debug, Clone, PartialEq)]
pub enum Value<'arena, 'src> {
    /// All numbers are f64 to keep arithmetic simple and avoid int/float distinction
    Number(f64),
    /// String literals reference original source when possible
    Str(ArenaCow<'arena, 'src>),
    /// Standard boolean values
    Bool(bool),
}

impl fmt::Display for Value<'_, '_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Number(n) => write!(f, "{n}"),
            Value::Str(s) => write!(f, "{s}"),
            Value::Bool(b) => write!(f, "{b}"),
        }
    }
}

#[derive(Debug, Clone)]
struct FunctionDef<'ast> {
    name: &'ast str,
    params: ParamListRef<'ast>,
    body: BlockRef<'ast>,
}

// Activation record for function calls, represents a function call frame
#[derive(Debug, Clone)]
struct ActivationRecord<'arena, 'src> {
    local_vars: Vec<(&'src str, Value<'arena, 'src>), &'arena Arena>,
}

// Controls how function execution should proceed after statements
#[derive(Debug, Clone, PartialEq)]
enum ExecFlow<'arena, 'src> {
    Continue,
    Return(Value<'arena, 'src>),
}

/// The runtime interface for NaijaScript using arena-allocated AST.
pub struct Runtime<'arena, 'src> {
    // Variable scopes, each Vec is a scope, inner Vec is variables in that scope
    env: Vec<Vec<(&'src str, Value<'arena, 'src>), &'arena Arena>, &'arena Arena>,

    // Global function table, functions are first-class but stored separately
    functions: Vec<FunctionDef<'src>, &'arena Arena>,

    // Call stack for function execution, prevents infinite recursion
    call_stack: Vec<ActivationRecord<'arena, 'src>, &'arena Arena>,

    /// Collection of runtime errors encountered during execution
    pub errors: Diagnostics<'arena>,

    /// Output from `shout()` function calls, public for caller access
    pub output: Vec<Value<'arena, 'src>, &'arena Arena>,

    // Reference to the arena for allocating runtime data structures
    arena: &'arena Arena,
}

impl<'arena, 'src> Runtime<'arena, 'src> {
    /// Creates a new [`Runtime`] instance.
    pub fn new(arena: &'arena Arena) -> Self {
        Runtime {
            env: Vec::new_in(arena),
            functions: Vec::new_in(arena),
            call_stack: Vec::new_in(arena),
            errors: Diagnostics::new(arena),
            output: Vec::new_in(arena),
            arena,
        }
    }

    /// Executes a NaijaScript program starting from the root block.
    pub fn run(&mut self, root: BlockRef<'src>) -> &Diagnostics<'arena> {
        self.env.push(Vec::new_in(self.arena)); // enter global scope

        // Execute the program and handle both regular flow and early returns
        match self.exec_block_with_flow(root) {
            Ok(..) => {} // Normal completion
            Err(err) => {
                let labels = match err.kind {
                    RuntimeErrorKind::DivisionByZero => vec![Label {
                        span: err.span,
                        message: ArenaCow::Borrowed("No divide by zero"),
                    }],
                    RuntimeErrorKind::StackOverflow => {
                        vec![Label {
                            span: err.span,
                            message: ArenaCow::Borrowed("Dis function call too deep"),
                        }]
                    }
                    RuntimeErrorKind::Io(msg) => {
                        vec![Label { span: err.span, message: ArenaCow::Borrowed(msg) }]
                    }
                };
                self.errors.emit(err.span, Severity::Error, "runtime", err.kind.as_str(), labels);
            }
        }
        self.env.pop(); // exit global scope
        &self.errors
    }

    // Statement execution dispatcher
    //
    // Each statement type updates the environment or controls flow.
    // Functions definitions populate the global function table.
    fn exec_stmt(&mut self, stmt: StmtRef<'src>) -> Result<ExecFlow<'arena, 'src>, RuntimeError> {
        match stmt {
            Stmt::Assign { var, expr, .. } => {
                let val = self.eval_expr(expr)?;
                self.insert_or_update(var, val);
                Ok(ExecFlow::Continue)
            }
            Stmt::AssignExisting { var, expr, .. } => {
                let val = self.eval_expr(expr)?;
                self.update_existing(var, val);
                Ok(ExecFlow::Continue)
            }
            Stmt::If { cond, then_b, else_b, .. } => {
                let condition_value = self.eval_expr(cond)?;
                let is_truthy = match condition_value {
                    Value::Bool(b) => b,
                    _ => unreachable!(
                        "Semantic analysis guarantees only boolean expressions in conditions"
                    ),
                };
                if is_truthy {
                    self.exec_block_with_flow(then_b)
                } else if let Some(eb) = else_b {
                    self.exec_block_with_flow(eb)
                } else {
                    Ok(ExecFlow::Continue)
                }
            }
            Stmt::Loop { cond, body, .. } => {
                loop {
                    let condition_value = self.eval_expr(cond)?;
                    let should_continue = match condition_value {
                        Value::Bool(b) => b,
                        _ => unreachable!(
                            "Semantic analysis guarantees only boolean expressions in loop conditions"
                        ),
                    };
                    if !should_continue {
                        break;
                    }
                    match self.exec_block_with_flow(body)? {
                        ExecFlow::Continue => continue,
                        flow @ ExecFlow::Return(..) => return Ok(flow), // Bubble up return
                    }
                }
                Ok(ExecFlow::Continue)
            }
            Stmt::Block { block, .. } => self.exec_block_with_flow(block),
            Stmt::FunctionDef { name, params, body, .. } => {
                // Store function for later calls, semantic analysis ensures no duplicates
                let func_def = FunctionDef { name, params, body };
                self.functions.push(func_def);
                Ok(ExecFlow::Continue)
            }
            Stmt::Return { expr, .. } => {
                // Semantic analysis guarantees we're inside a function
                let return_val = if let Some(expr_ref) = expr {
                    self.eval_expr(expr_ref)?
                } else {
                    Value::Number(0.0) // Default return value
                };
                Ok(ExecFlow::Return(return_val))
            }
            Stmt::Expression { expr, .. } => {
                // Execute expression for side effects (e.g., function calls)
                self.eval_expr(expr)?;
                Ok(ExecFlow::Continue)
            }
        }
    }

    // Block execution with proper scope management and early return handling
    fn exec_block_with_flow(
        &mut self,
        block: BlockRef<'src>,
    ) -> Result<ExecFlow<'arena, 'src>, RuntimeError> {
        self.env.push(Vec::new_in(self.arena)); // enter new block scope
        for &stmt in block.stmts {
            match self.exec_stmt(stmt)? {
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

    fn eval_expr(&mut self, expr: ExprRef<'src>) -> Result<Value<'arena, 'src>, RuntimeError> {
        match expr {
            Expr::Number(n, ..) => Ok(Value::Number(
                n.parse::<f64>().expect("Scanner should guarantee valid number format"),
            )),
            Expr::String { parts, .. } => self.eval_string_expr(parts),
            Expr::Bool(b, ..) => Ok(Value::Bool(*b)),
            Expr::Var(v, ..) => {
                let val = self
                    .lookup_var(v)
                    .expect("Semantic analysis should guarantee all variables are declared");
                Ok(val)
            }
            Expr::Binary { op, lhs, rhs, span } => {
                match op {
                    BinaryOp::And => {
                        let l = self.eval_expr(lhs)?;
                        if let Value::Bool(false) = l {
                            return Ok(Value::Bool(false)); // Short-circuit evaluation
                        }
                        let r = self.eval_expr(rhs)?;
                        match r {
                            Value::Bool(b) => Ok(Value::Bool(b)),
                            _ => unreachable!("Semantic analysis guarantees boolean expressions"),
                        }
                    }
                    BinaryOp::Or => {
                        let l = self.eval_expr(lhs)?;
                        if let Value::Bool(true) = l {
                            return Ok(Value::Bool(true)); // Short-circuit evaluation
                        }
                        let r = self.eval_expr(rhs)?;
                        match r {
                            Value::Bool(b) => Ok(Value::Bool(b)),
                            _ => unreachable!("Semantic analysis guarantees boolean expressions"),
                        }
                    }
                    _ => {
                        let l = self.eval_expr(lhs)?;
                        let r = self.eval_expr(rhs)?;
                        match (l, r) {
                            (Value::Number(lv), Value::Number(rv)) => match op {
                                BinaryOp::Add => Ok(Value::Number(lv + rv)),
                                BinaryOp::Minus => Ok(Value::Number(lv - rv)),
                                BinaryOp::Times => Ok(Value::Number(lv * rv)),
                                BinaryOp::Divide | BinaryOp::Mod => {
                                    if rv == 0.0 {
                                        Err(RuntimeError {
                                            kind: RuntimeErrorKind::DivisionByZero,
                                            span: *span,
                                        })
                                    } else {
                                        Ok(Value::Number(match op {
                                            BinaryOp::Divide => lv / rv,
                                            BinaryOp::Mod => lv % rv,
                                            _ => unreachable!(),
                                        }))
                                    }
                                }
                                BinaryOp::Eq => Ok(Value::Bool(lv == rv)),
                                BinaryOp::Gt => Ok(Value::Bool(lv > rv)),
                                BinaryOp::Lt => Ok(Value::Bool(lv < rv)),
                                _ => unreachable!(),
                            },
                            (Value::Str(ls), Value::Str(rs)) => match op {
                                BinaryOp::Add => {
                                    let mut s = ArenaString::with_capacity_in(
                                        ls.len() + rs.len(),
                                        self.arena,
                                    );
                                    s.push_str(&ls);
                                    s.push_str(&rs);
                                    Ok(Value::Str(ArenaCow::Owned(s)))
                                }
                                BinaryOp::Eq => Ok(Value::Bool(ls == rs)),
                                BinaryOp::Gt => Ok(Value::Bool(ls > rs)),
                                BinaryOp::Lt => Ok(Value::Bool(ls < rs)),
                                _ => unreachable!("Semantic analysis guarantees valid string ops"),
                            },
                            (Value::Str(ls), Value::Number(n)) => {
                                assert!(matches!(op, BinaryOp::Add));
                                let num_str = n.to_string();
                                let mut s = ArenaString::with_capacity_in(
                                    ls.len() + num_str.len(),
                                    self.arena,
                                );
                                s.push_str(&ls);
                                s.push_str(&num_str);
                                Ok(Value::Str(ArenaCow::Owned(s)))
                            }
                            (Value::Number(n), Value::Str(rs)) => {
                                assert!(matches!(op, BinaryOp::Add));
                                let num_str = n.to_string();
                                let mut s = ArenaString::with_capacity_in(
                                    num_str.len() + rs.len(),
                                    self.arena,
                                );
                                s.push_str(&num_str);
                                s.push_str(&rs);
                                Ok(Value::Str(ArenaCow::Owned(s)))
                            }
                            (Value::Bool(lv), Value::Bool(rv)) => match op {
                                BinaryOp::Eq => Ok(Value::Bool(lv == rv)),
                                BinaryOp::Gt => Ok(Value::Bool(lv & !rv)), // false < true
                                BinaryOp::Lt => Ok(Value::Bool(!lv & rv)),
                                _ => unreachable!("Semantic analysis guarantees valid bool ops"),
                            },
                            _ => {
                                unreachable!("Semantic analysis guarantees valid type combinations")
                            }
                        }
                    }
                }
            }
            Expr::Unary { op, expr, .. } => {
                let v = self.eval_expr(expr)?;
                match (op, v) {
                    (UnaryOp::Not, Value::Bool(b)) => Ok(Value::Bool(!b)),
                    (UnaryOp::Minus, Value::Number(n)) => Ok(Value::Number(-n)),
                    _ => unreachable!("Semantic analysis guarantees valid unary expressions"),
                }
            }
            Expr::Call { callee, args, span } => self.eval_function_call(callee, args, span),
        }
    }

    // Function call evaluation
    fn eval_function_call(
        &mut self,
        callee: ExprRef<'src>,
        args: &'src ArgList<'src>,
        span: &'src Span,
    ) -> Result<Value<'arena, 'src>, RuntimeError> {
        let func_name = match callee {
            Expr::Var(name, ..) => *name,
            _ => unreachable!("Semantic analysis guarantees callee is a variable"),
        };

        // We check for built-in functions first, as they are guaranteed to exist
        if let Some(builtin) = Builtin::from_name(func_name) {
            return self.eval_builtin_call(builtin, args);
        }

        // We check for user-defined functions next
        let func_def = self
            .functions
            .iter()
            .find(|f| f.name == func_name)
            .expect("Semantic analysis guarantees function exists")
            .clone();

        // We check for recursion depth
        if self.call_stack.len() >= MAX_CALL_DEPTH {
            return Err(RuntimeError { kind: RuntimeErrorKind::StackOverflow, span: *span });
        }

        // We evaluate all arguments eagerly (left-to-right evaluation order)
        let mut arg_values = Vec::new_in(self.arena);
        for &arg_expr in args.args {
            arg_values.push(self.eval_expr(arg_expr)?);
        }

        assert_eq!(arg_values.len(), func_def.params.params.len());

        // We create a new activation record for the function call
        let mut local_vars = Vec::new_in(self.arena);
        for (param, arg) in func_def.params.params.iter().zip(arg_values.iter()) {
            local_vars.push((*param, arg.clone()));
        }

        self.call_stack.push(ActivationRecord { local_vars });

        // We execute the function body with proper return value handling
        let result = match self.exec_block_with_flow(func_def.body)? {
            ExecFlow::Continue => Value::Number(0.0),
            ExecFlow::Return(val) => val,
        };

        // We remove the activation record from the call stack
        self.call_stack.pop();
        Ok(result)
    }

    // Built-in function evaluation
    fn eval_builtin_call(
        &mut self,
        builtin: Builtin,
        args: &'src ArgList<'src>,
    ) -> Result<Value<'arena, 'src>, RuntimeError> {
        // We evaluate all arguments eagerly (left-to-right evaluation order)
        let mut arg_values = Vec::new_in(self.arena);
        for &arg_expr in args.args {
            arg_values.push(self.eval_expr(arg_expr)?);
        }

        assert_eq!(arg_values.len(), builtin.arity());

        match builtin {
            Builtin::Shout => {
                self.output.push(arg_values[0].clone());
                Ok(Value::Number(0.0))
            }
            Builtin::Abs => {
                if let Value::Number(n) = arg_values[0] {
                    Ok(Value::Number(n.abs()))
                } else {
                    unreachable!("Semantic analysis guarantees number argument for abs")
                }
            }
            Builtin::Sqrt => {
                if let Value::Number(n) = arg_values[0] {
                    Ok(Value::Number(n.sqrt()))
                } else {
                    unreachable!("Semantic analysis guarantees number argument for sqrt")
                }
            }
            Builtin::Floor => {
                if let Value::Number(n) = arg_values[0] {
                    Ok(Value::Number(n.floor()))
                } else {
                    unreachable!("Semantic analysis guarantees number argument for floor")
                }
            }
            Builtin::Ceil => {
                if let Value::Number(n) = arg_values[0] {
                    Ok(Value::Number(n.ceil()))
                } else {
                    unreachable!("Semantic analysis guarantees number argument for ceil")
                }
            }
            Builtin::Round => {
                if let Value::Number(n) = arg_values[0] {
                    Ok(Value::Number(n.round()))
                } else {
                    unreachable!("Semantic analysis guarantees number argument for round")
                }
            }
            Builtin::Len => {
                if let Value::Str(s) = &arg_values[0] {
                    // Count Unicode characters, not bytes
                    Ok(Value::Number(s.chars().count() as f64))
                } else {
                    unreachable!("Semantic analysis guarantees string argument for len")
                }
            }
            Builtin::Slice => {
                if let (Value::Str(s), Value::Number(start), Value::Number(end)) =
                    (&arg_values[0], &arg_values[1], &arg_values[2])
                {
                    let s = builtins::string_slice(s, *start, *end, self.arena);
                    Ok(Value::Str(ArenaCow::owned(s)))
                } else {
                    unreachable!("Semantic analysis guarantees correct argument types for slice")
                }
            }
            Builtin::Upper => {
                if let Value::Str(s) = &arg_values[0] {
                    let s = builtins::string_upper(s, self.arena);
                    Ok(Value::Str(ArenaCow::owned(s)))
                } else {
                    unreachable!("Semantic analysis guarantees string argument for upper")
                }
            }
            Builtin::Lower => {
                if let Value::Str(s) = &arg_values[0] {
                    let s = builtins::string_lower(s, self.arena);
                    Ok(Value::Str(ArenaCow::owned(s)))
                } else {
                    unreachable!("Semantic analysis guarantees string argument for lower")
                }
            }
            Builtin::Find => {
                if let (Value::Str(haystack), Value::Str(needle)) = (&arg_values[0], &arg_values[1])
                {
                    let i = builtins::string_find(haystack, needle);
                    Ok(Value::Number(i))
                } else {
                    unreachable!("Semantic analysis guarantees string arguments for find")
                }
            }
            Builtin::Replace => {
                if let (Value::Str(s), Value::Str(old), Value::Str(new)) =
                    (&arg_values[0], &arg_values[1], &arg_values[2])
                {
                    let s = builtins::string_replace(s, old, new, self.arena);
                    Ok(Value::Str(ArenaCow::owned(s)))
                } else {
                    unreachable!("Semantic analysis guarantees string arguments for replace")
                }
            }
            Builtin::Trim => {
                if let Value::Str(s) = &arg_values[0] {
                    let s = builtins::string_trim(s, self.arena);
                    Ok(Value::Str(ArenaCow::owned(s)))
                } else {
                    unreachable!("Semantic analysis guarantees string argument for trim")
                }
            }
            Builtin::ToString => {
                let s = arg_values[0].to_string();
                let s = ArenaString::from_str(self.arena, &s);
                Ok(Value::Str(ArenaCow::owned(s)))
            }
            Builtin::ToNumber => {
                if let Value::Str(s) = &arg_values[0] {
                    let n = s.parse::<f64>().unwrap_or(f64::NAN);
                    Ok(Value::Number(n))
                } else {
                    unreachable!("Semantic analysis guarantees string argument for to_number")
                }
            }
            Builtin::TypeOf => {
                let t = match &arg_values[0] {
                    Value::Number(..) => "number",
                    Value::Str(..) => "string",
                    Value::Bool(..) => "boolean",
                };
                Ok(Value::Str(ArenaCow::borrowed(t)))
            }
            #[cfg(not(target_family = "wasm"))]
            Builtin::ReadLine => {
                let prompt = arg_values[0].to_string();
                let s =
                    sys::stdin::read_line(&prompt, self.arena).map_err(RuntimeErrorKind::from)?;
                Ok(Value::Str(ArenaCow::owned(s)))
            }
        }
    }

    // Variable assignment, adds or updates in current scope
    fn insert_or_update(&mut self, var: &'src str, val: Value<'arena, 'src>) {
        if let Some(scope) = self.env.last_mut() {
            if let Some((.., slot)) = scope.iter_mut().find(|(name, ..)| *name == var) {
                *slot = val;
            } else {
                scope.push((var, val));
            }
        }
    }

    fn update_existing(&mut self, var: &'src str, val: Value<'arena, 'src>) {
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
        unreachable!("Semantic analysis guarantees variable exists");
    }

    #[inline]
    fn lookup_var(&self, var: &str) -> Option<Value<'arena, 'src>> {
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

    #[inline]
    fn eval_string_expr(
        &mut self,
        parts: &StringParts<'src>,
    ) -> Result<Value<'arena, 'src>, RuntimeError> {
        match parts {
            StringParts::Static(content) => Ok(Value::Str(ArenaCow::borrowed(content))),
            StringParts::Interpolated(segments) => {
                let mut result = ArenaString::new_in(self.arena);
                for segment in *segments {
                    match segment {
                        StringSegment::Literal(s) => result.push_str(s),
                        StringSegment::Variable(var) => {
                            let value = self
                                .lookup_var(var)
                                .expect("Semantic analysis should guarantee variable exists");
                            write!(result, "{value}").unwrap();
                        }
                    }
                }
                Ok(Value::Str(ArenaCow::owned(result)))
            }
        }
    }
}
