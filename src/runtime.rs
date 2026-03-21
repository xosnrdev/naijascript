//! The runtime for `NaijaScript`.

use std::fmt::{self, Write};
use std::ptr::NonNull;
use std::{io, mem};

use crate::analysis::facts::ProgramFacts;
use crate::analysis::ids::FunctionId;
use crate::analysis::opt::OptimizationPlan;
use crate::arena::{Arena, ArenaCow, ArenaString, PoolSet};
use crate::arena_format;
use crate::builtins::{
    ArrayBuiltin, Builtin, GlobalBuiltin, MemberBuiltin, NumberBuiltin, StringBuiltin,
};
use crate::diagnostics::{AsStr, Diagnostics, Label, Severity, Span};
#[cfg(target_family = "wasm")]
use crate::helpers::KIBI;
use crate::helpers::LenWriter;
#[cfg(not(target_family = "wasm"))]
use crate::helpers::MEBI;
use crate::syntax::parser::{
    ArgList, BinaryOp, BlockRef, Expr, ExprRef, ParamListRef, Stmt, StmtRef, StringParts,
    StringSegment, UnaryOp,
};

/// Runtime errors that can occur during code execution.
#[derive(Debug, PartialEq, Eq)]
pub enum RuntimeErrorKind {
    Io(&'static str),
    DivisionByZero,
    StackOverflow,
    IndexOutOfBounds,
    /// Type mismatch error that can't be caught in semantic analysis
    TypeMismatch,
    InvalidIndex,
}

impl AsStr for RuntimeErrorKind {
    fn as_str(&self) -> &'static str {
        match self {
            RuntimeErrorKind::Io(..) => "I/O error",
            RuntimeErrorKind::DivisionByZero => "Division by zero",
            RuntimeErrorKind::StackOverflow => "Stack overflow",
            RuntimeErrorKind::IndexOutOfBounds => "Index out of bounds",
            RuntimeErrorKind::TypeMismatch => "Type mismatch",
            RuntimeErrorKind::InvalidIndex => "Invalid index",
        }
    }
}

impl From<io::Error> for RuntimeErrorKind {
    fn from(err: io::Error) -> Self {
        // Safety: It's unlikely for io error to emit so this is fine
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
    name: String,
    ty: &'static str,
}

impl RuntimeError {
    fn new(kind: RuntimeErrorKind, span: Span) -> Self {
        Self { kind, span, name: String::new(), ty: "" }
    }

    fn new_with_extras(
        kind: RuntimeErrorKind,
        span: Span,
        name: impl Into<String>,
        ty: &'static str,
    ) -> Self {
        Self { kind, span, name: name.into(), ty }
    }
}

/// Maximum native stack bytes the runtime is allowed to consume.
/// Adapts automatically to debug vs release frame sizes and platform
/// stack limits. Sized to fit within the default 8 MiB thread stack
/// with headroom for the parser, resolver, and error reporting above.
#[cfg(target_family = "wasm")]
const STACK_BUDGET: usize = 512 * KIBI;
#[cfg(not(target_family = "wasm"))]
const STACK_BUDGET: usize = 4 * MEBI;

// Epsilon used for approximate floating-point equality checks
const FLOAT_EQ_EPS: f64 = 1e-12;

/// The value types our runtime can work with at runtime.
#[derive(Debug, Clone, PartialEq)]
pub enum Value<'a> {
    /// String literals reference original source when possible
    Str(ArenaCow<'a>),
    /// All numbers are f64 to keep arithmetic simple and avoid int/float distinction
    Number(f64),
    /// Standard boolean values
    Bool(bool),
    /// Array literal values
    Array(Vec<Value<'a>, &'a Arena>),
    /// Represents the absence of a value
    Null,
}

impl<'a> Value<'a> {
    /// Clones the value, placing any array backing stores in the given arena.
    /// Strings use zero-cost clone. Numbers/bools/null are trivial copies.
    fn clone_into(&self, arena: &'a Arena) -> Self {
        match self {
            Value::Str(cow) => Value::Str(cow.clone()),
            Value::Number(n) => Value::Number(*n),
            Value::Bool(b) => Value::Bool(*b),
            Value::Null => Value::Null,
            Value::Array(items) => {
                let mut new = Vec::with_capacity_in(items.len(), arena);
                for item in items {
                    new.push(item.clone_into(arena));
                }
                Value::Array(new)
            }
        }
    }

    /// Returns this value's pool slot to the pool allocator.
    /// Strings > 256 bytes (arena-fallback) are not pool-managed and
    /// are silently skipped. Non-string values are no-ops.
    ///
    /// # Safety
    ///
    /// Caller must guarantee no live references (including Borrowed clones)
    /// exist into this value's backing memory.
    unsafe fn return_to_pool(&self, pool: &PoolSet<'_>) {
        if let Value::Str(ArenaCow::Owned(s)) = self {
            let ptr = s.as_bytes().as_ptr();
            if pool.contains(ptr) {
                unsafe {
                    pool.dealloc(
                        std::ptr::NonNull::new_unchecked(ptr.cast_mut()),
                        s.capacity() as u32,
                    );
                }
            }
        }
    }

    /// Copies frame-allocated data to the persistent pool/arena.
    /// Stack values (Number, Bool, Null) pass through unchanged.
    /// Values already on the target arena pass through (no double-promote).
    fn promote(self, pool: &PoolSet<'a>, frame: &Arena) -> Self {
        match self {
            Value::Number(n) => Value::Number(n),
            Value::Bool(b) => Value::Bool(b),
            Value::Null => Value::Null,
            Value::Str(cow) => Value::Str(cow.promote(pool, frame)),
            Value::Array(items) => {
                let mut promoted = Vec::with_capacity_in(items.len(), pool.arena());
                for item in items {
                    promoted.push(item.promote(pool, frame));
                }
                Value::Array(promoted)
            }
        }
    }
}

impl fmt::Display for Value<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Str(s) => write!(f, "{s}"),
            Value::Number(n) => write!(f, "{n}"),
            Value::Bool(b) => write!(f, "{b}"),
            Value::Array(items) => {
                write!(f, "[")?;
                for (idx, item) in items.iter().enumerate() {
                    if idx > 0 {
                        write!(f, ", ")?;
                    }
                    match item {
                        Value::Str(s) => write!(f, r#""{s}""#)?,
                        _ => write!(f, "{item}")?,
                    }
                }
                write!(f, "]")
            }
            Value::Null => write!(f, "null"),
        }
    }
}

#[derive(Debug, Clone, Copy)]
struct FunctionDef<'a> {
    id: Option<FunctionId>,
    name: &'a str,
    params: ParamListRef<'a>,
    body: BlockRef<'a>,
}

// Controls how function execution should proceed after statements
#[derive(Debug, Clone, PartialEq)]
enum ExecFlow<'a> {
    Continue,
    Return(Value<'a>),
    Break,
    LoopContinue,
}

/// The runtime interface for `NaijaScript` using arena-allocated AST.
pub struct Runtime<'a> {
    // Variable scopes, each Vec is a scope, inner Vec is variables in that scope
    env: Vec<Vec<(&'a str, Value<'a>), &'a Arena>, &'a Arena>,

    // Function scopes mirror lexical block scopes so lookup stays lexical.
    function_scopes: Vec<Vec<FunctionDef<'a>, &'a Arena>, &'a Arena>,

    pub output: Vec<Value<'a>, &'a Arena>,

    /// Collection of runtime errors encountered during execution
    pub errors: Diagnostics<'a>,

    // Persistent arena for data that must survive frame resets
    arena: &'a Arena,

    // Frame arena for per-iteration/per-call temporaries, reset at loop and function boundaries
    frame: &'a Arena,

    // Pool allocator for recyclable string storage during promote
    pool: PoolSet<'a>,

    // Native stack pointer recorded at run() entry. Used to detect
    // stack overflow by comparing against the current stack pointer.
    stack_base: usize,

    // Optional persistent analysis facts used for binding-safe runtime dispatch.
    facts: Option<NonNull<ProgramFacts<'a, 'a>>>,

    // Optional conservative optimization plan. Only function-def pruning is applied.
    optimization_plan: Option<NonNull<OptimizationPlan<'a>>>,
}

impl<'a> Runtime<'a> {
    /// Creates a new [`Runtime`] instance.
    /// If `frame` is `None`, the persistent arena is used for both roles
    /// (no frame resets, identical to pre-frame-arena behavior).
    pub fn new(arena: &'a Arena, frame: Option<&'a Arena>) -> Self {
        let frame = frame.unwrap_or(arena);
        let pool = PoolSet::new(arena);
        Self {
            env: Vec::new_in(arena),
            function_scopes: Vec::new_in(arena),
            output: Vec::new_in(arena),
            errors: Diagnostics::new(arena),
            arena,
            frame,
            pool,
            stack_base: 0,
            facts: None,
            optimization_plan: None,
        }
    }

    /// Executes a `NaijaScript` program with resolved binding facts attached.
    pub fn run_with_analysis<'facts>(
        &mut self,
        root: BlockRef<'a>,
        facts: &'facts ProgramFacts<'a, 'a>,
        optimization_plan: Option<&'facts OptimizationPlan<'a>>,
    ) -> &Diagnostics<'a> {
        self.facts = Some(NonNull::from(facts));
        self.optimization_plan = optimization_plan.map(NonNull::from);
        self.run_inner(root);
        self.facts = None;
        self.optimization_plan = None;
        &self.errors
    }

    /// Returns true when a separate frame arena is in use.
    fn has_frame_arena(&self) -> bool {
        !std::ptr::eq(self.frame, self.arena)
    }

    /// Checks whether the native stack has grown beyond `STACK_BUDGET`
    /// since `run()` was entered. Covers both function-call recursion and
    /// deeply nested expression evaluation in a single check.
    #[inline]
    fn check_stack(&self, span: Span) -> Result<(), RuntimeError> {
        let probe = 0u8;
        let current = &raw const probe as usize;
        if self.stack_base.wrapping_sub(current) > STACK_BUDGET {
            return Err(RuntimeError::new(RuntimeErrorKind::StackOverflow, span));
        }
        Ok(())
    }

    /// Executes a `NaijaScript` program starting from the root block.
    pub fn run(&mut self, root: BlockRef<'a>) -> &Diagnostics<'a> {
        self.run_inner(root);
        &self.errors
    }

    fn run_inner(&mut self, root: BlockRef<'a>) {
        let anchor = 0u8;
        self.stack_base = &raw const anchor as usize;
        self.push_scope_with_capacity(0, self.arena);

        match self.exec_block_with_flow(root) {
            Ok(..) => {}
            Err(err) => {
                let labels = match err.kind {
                    RuntimeErrorKind::DivisionByZero => vec![Label {
                        span: err.span,
                        message: ArenaCow::Borrowed("Zero no fit be divisor"),
                    }],
                    RuntimeErrorKind::StackOverflow => {
                        vec![Label {
                            span: err.span,
                            message: ArenaCow::Borrowed("Call stack don full"),
                        }]
                    }
                    RuntimeErrorKind::Io(msg) => {
                        vec![Label { span: err.span, message: ArenaCow::Borrowed(msg) }]
                    }
                    RuntimeErrorKind::IndexOutOfBounds => vec![Label {
                        span: err.span,
                        message: ArenaCow::Borrowed("Index value don pass array length"),
                    }],
                    RuntimeErrorKind::TypeMismatch => vec![Label {
                        span: err.span,
                        message: ArenaCow::Owned(arena_format!(
                            self.arena,
                            "Method `{}` no dey for `{}` type",
                            err.name,
                            err.ty
                        )),
                    }],
                    RuntimeErrorKind::InvalidIndex => vec![Label {
                        span: err.span,
                        message: ArenaCow::Borrowed("Index value no be whole number"),
                    }],
                };
                self.errors.emit(err.span, Severity::Error, "runtime", err.kind.as_str(), labels);
            }
        }
        self.pop_scope();
    }

    fn exec_stmt(&mut self, stmt: StmtRef<'a>) -> Result<ExecFlow<'a>, RuntimeError> {
        match stmt {
            Stmt::Assign { var, expr, .. } => {
                let val = self.eval_expr(expr)?;
                self.define_var(var, val);
                Ok(ExecFlow::Continue)
            }
            Stmt::AssignExisting { var, expr, .. } => {
                let val = self.eval_expr(expr)?;
                self.assign_var(var, val);
                Ok(ExecFlow::Continue)
            }
            Stmt::AssignIndex { target, expr, span } => {
                let val = self.eval_expr(expr)?;
                self.assign_index(target, val, *span)?;
                Ok(ExecFlow::Continue)
            }
            Stmt::If { cond, then_b, else_b, .. } => {
                let val = self.eval_expr(cond)?;
                let is_truthy = match val {
                    Value::Bool(b) => b,
                    Value::Null => false, // null is falsy
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
                    let val = self.eval_expr(cond)?;
                    let should_continue = match val {
                        Value::Bool(b) => b,
                        Value::Null => false,
                        _ => unreachable!(
                            "Semantic analysis guarantees only boolean expressions in loop conditions"
                        ),
                    };
                    if !should_continue {
                        break;
                    }

                    let frame_offset =
                        if self.has_frame_arena() { Some(self.frame.offset()) } else { None };

                    match self.exec_block_with_flow(body)? {
                        ExecFlow::Break => break,
                        ExecFlow::Continue | ExecFlow::LoopContinue => {}
                        flow @ ExecFlow::Return(..) => return Ok(flow),
                    }

                    if let Some(offset) = frame_offset {
                        unsafe { self.frame.reset(offset) };
                    }
                }
                Ok(ExecFlow::Continue)
            }
            Stmt::Block { block, .. } => self.exec_block_with_flow(block),
            Stmt::FunctionDef { name, params, body, .. } => {
                let bound_function = self.facts().and_then(|facts| {
                    facts.function_by_body(body).map(|id| (id, facts.function(id)))
                });
                let id = bound_function.map(|(function_id, _)| function_id);
                if let Some(function_id) = id
                    && self.function_is_pruned(function_id)
                {
                    return Ok(ExecFlow::Continue);
                }

                let func_def = if let Some((function_id, info)) = bound_function {
                    FunctionDef {
                        id: Some(function_id),
                        name: info.name,
                        params: info
                            .params
                            .expect("User-defined function metadata should include parameters"),
                        body: info.body,
                    }
                } else {
                    FunctionDef { id: None, name, params, body }
                };
                self.function_scopes
                    .last_mut()
                    .expect("Runtime should always execute inside a function scope")
                    .push(func_def);
                Ok(ExecFlow::Continue)
            }
            Stmt::Return { expr, .. } => {
                let val =
                    if let Some(expr_ref) = expr { self.eval_expr(expr_ref)? } else { Value::Null };
                Ok(ExecFlow::Return(val))
            }
            Stmt::Break { .. } => Ok(ExecFlow::Break),
            Stmt::Continue { .. } => Ok(ExecFlow::LoopContinue),
            Stmt::Expression { expr, .. } => {
                self.eval_expr(expr)?;
                Ok(ExecFlow::Continue)
            }
        }
    }

    #[inline]
    fn exec_block_with_flow(&mut self, block: BlockRef<'a>) -> Result<ExecFlow<'a>, RuntimeError> {
        self.push_scope_with_capacity(0, self.frame);
        for stmt in block.stmts {
            match self.exec_stmt(stmt)? {
                ExecFlow::Continue => {}
                flow @ (ExecFlow::Return(..) | ExecFlow::Break | ExecFlow::LoopContinue) => {
                    self.pop_scope();
                    return Ok(flow);
                }
            }
        }
        self.pop_scope();
        Ok(ExecFlow::Continue)
    }

    /// Pops the innermost scope and returns pool slots for any
    /// pool-managed strings in its variables.
    fn pop_scope(&mut self) {
        self.function_scopes.pop();
        if let Some(scope) = self.env.pop() {
            for (_, val) in &scope {
                unsafe { val.return_to_pool(&self.pool) };
            }
        }
    }

    #[inline]
    fn eval_expr(&mut self, expr: ExprRef<'a>) -> Result<Value<'a>, RuntimeError> {
        self.check_stack(expr.span())?;
        match expr {
            Expr::Number(n, ..) => Ok(Value::Number(
                n.parse::<f64>().expect("Scanner should guarantee valid number format"),
            )),
            Expr::String { parts, .. } => Ok(self.eval_string_expr(parts)),
            Expr::Bool(b, ..) => Ok(Value::Bool(*b)),
            Expr::Null(..) => Ok(Value::Null),
            Expr::Var(v, ..) => {
                let frame = self.frame;
                let val = self
                    .lookup_var(v, frame)
                    .expect("Semantic analysis should guarantee all variables are declared");
                Ok(val)
            }
            Expr::Binary { op, lhs, rhs, span } => match op {
                BinaryOp::And => {
                    let l = self.eval_expr(lhs)?;
                    if matches!(l, Value::Bool(false) | Value::Null) {
                        return Ok(Value::Bool(false)); // Short-circuit evaluation
                    }
                    let r = self.eval_expr(rhs)?;
                    match r {
                        Value::Bool(b) => Ok(Value::Bool(b)),
                        Value::Null => Ok(Value::Bool(false)),
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
                        Value::Null => Ok(Value::Bool(false)),
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
                                    Err(RuntimeError::new(RuntimeErrorKind::DivisionByZero, *span))
                                } else {
                                    let n = if *op == BinaryOp::Divide { lv / rv } else { lv % rv };
                                    Ok(Value::Number(n))
                                }
                            }
                            BinaryOp::Eq => Ok(Value::Bool((lv - rv).abs() <= FLOAT_EQ_EPS)),
                            BinaryOp::Gt => Ok(Value::Bool(lv > rv)),
                            BinaryOp::Lt => Ok(Value::Bool(lv < rv)),
                            _ => unreachable!("Semantic analysis guarantees valid number ops"),
                        },
                        (Value::Str(ls), Value::Str(rs)) => match op {
                            BinaryOp::Add => {
                                let mut s =
                                    ArenaString::with_capacity_in(ls.len() + rs.len(), self.frame);
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
                            let mut writer = LenWriter(0);
                            write!(writer, "{n}").unwrap();
                            let mut s =
                                ArenaString::with_capacity_in(ls.len() + writer.0, self.frame);
                            s.push_str(&ls);
                            write!(s, "{n}").unwrap();
                            Ok(Value::Str(ArenaCow::Owned(s)))
                        }
                        (Value::Number(n), Value::Str(rs)) => {
                            assert!(matches!(op, BinaryOp::Add));
                            let mut writer = LenWriter(0);
                            write!(writer, "{n}").unwrap();
                            let mut s =
                                ArenaString::with_capacity_in(writer.0 + rs.len(), self.frame);
                            write!(s, "{n}").unwrap();
                            s.push_str(&rs);
                            Ok(Value::Str(ArenaCow::Owned(s)))
                        }
                        (Value::Bool(lv), Value::Bool(rv)) => match op {
                            BinaryOp::Eq => Ok(Value::Bool(lv == rv)),
                            BinaryOp::Gt => Ok(Value::Bool(lv && !rv)), // false < true
                            BinaryOp::Lt => Ok(Value::Bool(!lv & rv)),
                            _ => unreachable!("Semantic analysis guarantees valid bool ops"),
                        },
                        (Value::Null, Value::Null) => match op {
                            BinaryOp::Eq => Ok(Value::Bool(true)),
                            BinaryOp::Gt | BinaryOp::Lt => Ok(Value::Bool(false)),
                            _ => unreachable!("Semantic analysis guarantees valid null ops"),
                        },
                        (Value::Null, ..) | (.., Value::Null) => match op {
                            BinaryOp::Eq | BinaryOp::Gt | BinaryOp::Lt => Ok(Value::Bool(false)),
                            _ => unreachable!("Semantic analysis guarantees valid null ops"),
                        },
                        _ => {
                            unreachable!("Semantic analysis guarantees matching operand types")
                        }
                    }
                }
            },

            Expr::Unary { op, expr, .. } => {
                let v = self.eval_expr(expr)?;
                match (op, v) {
                    (UnaryOp::Not, Value::Bool(b)) => Ok(Value::Bool(!b)),
                    (UnaryOp::Not, Value::Null) => Ok(Value::Bool(true)),
                    (UnaryOp::Minus, Value::Number(n)) => Ok(Value::Number(-n)),
                    _ => unreachable!("Semantic analysis guarantees valid unary expressions"),
                }
            }
            Expr::Array { elements, .. } => {
                let mut values = Vec::with_capacity_in(elements.len(), self.frame);
                for element in *elements {
                    let val = self.eval_expr(element)?;
                    values.push(val);
                }
                Ok(Value::Array(values))
            }
            Expr::Index { array, index, index_span, .. } => {
                let array_value = self.eval_expr(array)?;
                let index_value = self.eval_expr(index)?;
                let Value::Array(mut items) = array_value else {
                    unreachable!("Semantic analysis guarantees only arrays can be indexed")
                };

                let Value::Number(index_number) = index_value else {
                    return Err(RuntimeError::new(RuntimeErrorKind::InvalidIndex, *index_span));
                };

                if !index_number.is_finite() || index_number.fract() != 0.0 {
                    return Err(RuntimeError::new(RuntimeErrorKind::InvalidIndex, *index_span));
                }

                #[allow(clippy::cast_possible_truncation)]
                let idx = index_number as isize;
                if idx < 0 || idx >= items.len().cast_signed() {
                    return Err(RuntimeError::new(RuntimeErrorKind::IndexOutOfBounds, *index_span));
                }

                // SAFETY: `idx` is >= 0 and < items.len()
                let slot = unsafe { items.get_unchecked_mut(idx.cast_unsigned()) };
                let slot = mem::replace(slot, Value::Null);
                Ok(slot)
            }
            Expr::Member { .. } => {
                unreachable!("Semantic analysis guarantees member access is always a function call")
            }
            Expr::Call { .. } => self.eval_function_call(expr),
        }
    }

    fn eval_function_call(&mut self, call: ExprRef<'a>) -> Result<Value<'a>, RuntimeError> {
        let Expr::Call { callee, args, span } = call else {
            unreachable!("Runtime function call evaluation requires a call expression")
        };

        // Handle member method calls
        if let Expr::Member { object, field, .. } = callee {
            return self.eval_member_call(object, field, args, *span);
        }

        let func_name = match callee {
            Expr::Var(name, ..) => *name,
            _ => unreachable!("Semantic analysis guarantees callee is variable or member"),
        };

        if let Some(builtin) = GlobalBuiltin::from_name(func_name) {
            return self.eval_builtin_call(builtin, args, *span);
        }

        let func_def =
            if let Some(callee_id) = self.facts().and_then(|facts| facts.user_call_callee(call)) {
                self.lookup_func_by_id(callee_id)
            } else {
                self.lookup_func_by_name(func_name)
            };

        let frame_offset = if self.has_frame_arena() { Some(self.frame.offset()) } else { None };

        // We evaluate all arguments eagerly (left-to-right evaluation order)
        let mut arg_values = Vec::with_capacity_in(args.args.len(), self.frame);
        for arg_expr in args.args {
            arg_values.push(self.eval_expr(arg_expr)?);
        }

        assert_eq!(arg_values.len(), func_def.params.params.len());

        // Parameters live in their own lexical scope so block locals can shadow them.
        self.push_scope_with_capacity(func_def.params.params.len(), self.frame);
        let param_scope =
            self.env.last_mut().expect("Parameter scope should exist immediately after push");
        for (param, arg) in func_def.params.params.iter().zip(arg_values) {
            let arg = match arg {
                Value::Str(ArenaCow::Borrowed(s)) if self.pool.contains(s.as_ptr()) => {
                    Value::Str(ArenaCow::Owned(self.pool.alloc_str(s)))
                }
                other => other,
            };
            param_scope.push((*param, arg));
        }

        // We execute the function body with proper return value handling
        let flow = self.exec_block_with_flow(func_def.body);
        self.pop_scope();

        let val = match flow? {
            ExecFlow::Continue => Value::Null,
            ExecFlow::Return(val) => val,
            ExecFlow::Break | ExecFlow::LoopContinue => {
                unreachable!(
                    "Break/Continue should be caught by loop, not escape to function boundary"
                )
            }
        };

        if let Some(offset) = frame_offset {
            return Ok(self.relocate_return_value(val, offset));
        }

        Ok(val)
    }

    fn eval_builtin_call(
        &mut self,
        builtin: GlobalBuiltin,
        args: &'a ArgList<'a>,
        span: Span,
    ) -> Result<Value<'a>, RuntimeError> {
        // We evaluate all arguments eagerly (left-to-right evaluation order)
        let mut arg_values = Vec::with_capacity_in(args.args.len(), self.frame);
        for arg_expr in args.args {
            arg_values.push(self.eval_expr(arg_expr)?);
        }
        assert_eq!(arg_values.len(), builtin.arity());

        match builtin {
            GlobalBuiltin::Shout => {
                let argv = mem::replace(&mut arg_values[0], Value::Null);
                GlobalBuiltin::shout(&argv);
                let argv = if self.has_frame_arena() {
                    argv.promote(&self.pool, self.frame)
                } else {
                    argv
                };
                self.output.push(argv);
                Ok(Value::Null)
            }

            GlobalBuiltin::TypeOf => {
                let t = GlobalBuiltin::type_of(&arg_values[0]);
                Ok(Value::Str(ArenaCow::borrowed(t)))
            }
            GlobalBuiltin::ReadLine => {
                let s = GlobalBuiltin::read_line(&arg_values[0], self.frame)
                    .map_err(|err| RuntimeError::new(err.into(), span))?;
                Ok(Value::Str(ArenaCow::owned(s)))
            }
            GlobalBuiltin::ToString => {
                let s = GlobalBuiltin::to_string(self.frame, &arg_values[0]);
                Ok(Value::Str(ArenaCow::owned(s)))
            }
        }
    }

    fn eval_member_call(
        &mut self,
        object: ExprRef<'a>,
        field: &'a str,
        args: &'a ArgList<'a>,
        span: Span,
    ) -> Result<Value<'a>, RuntimeError> {
        let Some(builtin) = MemberBuiltin::from_name(field) else {
            let receiver = self.eval_expr(object)?;
            return Err(RuntimeError::new_with_extras(
                RuntimeErrorKind::TypeMismatch,
                span,
                field,
                GlobalBuiltin::type_of(&receiver),
            ));
        };

        // Mutable methods go directly to the mutable path without cloning the receiver.
        if builtin.requires_mut_receiver() {
            match builtin {
                MemberBuiltin::Array(array_builtin) => {
                    return self.eval_array_member_call_mut(
                        object,
                        array_builtin,
                        field,
                        args,
                        span,
                    );
                }
                _ => unreachable!("Only array methods require mutable receiver"),
            }
        }

        let receiver = self.eval_expr(object)?;
        match receiver {
            Value::Str(s) => self.eval_string_member_call(&s, field, args),
            Value::Number(n) => Ok(Self::eval_number_member_call(n, field)),
            Value::Array(arr) => self.eval_array_member_call(&arr, field, args),
            Value::Bool(..) => unimplemented!("Boolean methods not implemented yet"),
            Value::Null => Err(RuntimeError::new_with_extras(
                RuntimeErrorKind::TypeMismatch,
                span,
                field,
                GlobalBuiltin::type_of(&receiver),
            )),
        }
    }

    fn eval_array_member_call_mut(
        &mut self,
        receiver: ExprRef<'a>,
        builtin: ArrayBuiltin,
        field: &'a str,
        args: &'a ArgList<'a>,
        span: Span,
    ) -> Result<Value<'a>, RuntimeError> {
        match builtin {
            ArrayBuiltin::Push => {
                let value = self.eval_expr(args.args[0])?;
                // Promote before pushing, the target array lives on persistent,
                // but the value may reference frame-arena memory.
                let value = if self.has_frame_arena() {
                    value.promote(&self.pool, self.frame)
                } else {
                    value
                };
                let array = self.get_mutable_array(receiver, span, field)?;
                ArrayBuiltin::push(array, value);
                Ok(Value::Null)
            }
            ArrayBuiltin::Pop => {
                let array = self.get_mutable_array(receiver, span, field)?;
                Ok(ArrayBuiltin::pop(array).unwrap_or(Value::Null))
            }
            ArrayBuiltin::Reverse => {
                let array = self.get_mutable_array(receiver, span, field)?;
                ArrayBuiltin::reverse(array);
                Ok(Value::Null)
            }
            ArrayBuiltin::Len | ArrayBuiltin::Join => {
                unreachable!("Len and Join do not require mutable receiver")
            }
        }
    }

    fn eval_array_member_call(
        &mut self,
        array: &Vec<Value<'a>, &'a Arena>,
        field: &'a str,
        args: &'a ArgList<'a>,
    ) -> Result<Value<'a>, RuntimeError> {
        let array_builtin = ArrayBuiltin::from_name(field)
            .expect("Semantic analysis guarantees valid array method");
        match array_builtin {
            ArrayBuiltin::Len => Ok(Value::Number(ArrayBuiltin::len(array))),
            ArrayBuiltin::Join => {
                let sep = self.eval_expr(args.args[0])?;
                let Value::Str(sep) = sep else {
                    unreachable!("Semantic analysis guarantees string arg")
                };
                let result = ArrayBuiltin::join(array, &sep, self.frame);
                Ok(Value::Str(ArenaCow::Owned(result)))
            }
            ArrayBuiltin::Push | ArrayBuiltin::Pop | ArrayBuiltin::Reverse => {
                unreachable!("Push, Pop, and Reverse require mutable receiver")
            }
        }
    }

    fn eval_string_member_call(
        &mut self,
        s: &ArenaCow<'a>,
        field: &'a str,
        args: &'a ArgList<'a>,
    ) -> Result<Value<'a>, RuntimeError> {
        let string_builtin = StringBuiltin::from_name(field)
            .expect("Semantic analysis guarantees valid string method");
        match string_builtin {
            StringBuiltin::Len => Ok(Value::Number(StringBuiltin::len(s))),
            StringBuiltin::Slice => {
                let start = self.eval_expr(args.args[0])?;
                let end = self.eval_expr(args.args[1])?;
                match (start, end) {
                    (Value::Number(start), Value::Number(end)) => {
                        let s = StringBuiltin::slice(s, start, end, self.frame);
                        Ok(Value::Str(ArenaCow::Owned(s)))
                    }
                    _ => unreachable!("Semantic analysis guarantees number args"),
                }
            }
            StringBuiltin::ToUppercase => {
                let s = StringBuiltin::to_uppercase(s, self.frame);
                Ok(Value::Str(ArenaCow::Owned(s)))
            }
            StringBuiltin::ToLowercase => {
                let s = StringBuiltin::to_lowercase(s, self.frame);
                Ok(Value::Str(ArenaCow::Owned(s)))
            }
            StringBuiltin::Trim => {
                let s = StringBuiltin::trim(s, self.frame);
                Ok(Value::Str(ArenaCow::Owned(s)))
            }
            StringBuiltin::Find => {
                let needle = self.eval_expr(args.args[0])?;
                match needle {
                    Value::Str(n) => Ok(Value::Number(StringBuiltin::find(s, &n))),
                    _ => unreachable!("Semantic analysis guarantees string arg"),
                }
            }
            StringBuiltin::Replace => {
                let old = self.eval_expr(args.args[0])?;
                let new = self.eval_expr(args.args[1])?;
                match (old, new) {
                    (Value::Str(o), Value::Str(n)) => {
                        let result = StringBuiltin::replace(s, &o, &n, self.frame);
                        Ok(Value::Str(ArenaCow::Owned(result)))
                    }
                    _ => unreachable!("Semantic analysis guarantees string args"),
                }
            }
            StringBuiltin::ToNumber => Ok(Value::Number(StringBuiltin::to_number(s))),
            StringBuiltin::Split => {
                let pattern = self.eval_expr(args.args[0])?;
                match pattern {
                    Value::Str(pat) => {
                        let mut collection =
                            Vec::with_capacity_in(s.len() / pat.len().max(1) + 1, self.frame);
                        StringBuiltin::split(s, &pat, self.frame)
                            .for_each(|s| collection.push(Value::Str(ArenaCow::Owned(s))));
                        Ok(Value::Array(collection))
                    }
                    _ => unreachable!("Semantic analysis guarantees string arg"),
                }
            }
        }
    }

    fn eval_number_member_call(n: f64, field: &'a str) -> Value<'a> {
        let number_builtin = NumberBuiltin::from_name(field)
            .expect("Semantic analysis guarantees valid number method");
        match number_builtin {
            NumberBuiltin::Abs => Value::Number(NumberBuiltin::abs(n)),
            NumberBuiltin::Sqrt => Value::Number(NumberBuiltin::sqrt(n)),
            NumberBuiltin::Floor => Value::Number(NumberBuiltin::floor(n)),
            NumberBuiltin::Ceil => Value::Number(NumberBuiltin::ceil(n)),
            NumberBuiltin::Round => Value::Number(NumberBuiltin::round(n)),
        }
    }

    fn get_mutable_array(
        &mut self,
        object: ExprRef<'a>,
        span: Span,
        field: impl Into<String>,
    ) -> Result<&mut Vec<Value<'a>, &'a Arena>, RuntimeError> {
        match object {
            Expr::Var(name, ..) => {
                let var = self
                    .lookup_var_mut(name)
                    .expect("Semantic analysis guarantees variable exists");
                match var {
                    Value::Array(arr) => Ok(arr),
                    _ => Err(RuntimeError::new_with_extras(
                        RuntimeErrorKind::TypeMismatch,
                        span,
                        field,
                        GlobalBuiltin::type_of(var),
                    )),
                }
            }
            Expr::Index { .. } => {
                let (base_var, index_exprs) = self.flatten_index_target(object);

                let mut evaluated_indices = Vec::with_capacity_in(index_exprs.len(), self.frame);
                for (index_expr, index_span) in &index_exprs {
                    let idx = self.eval_index_value(index_expr, *index_span)?;
                    evaluated_indices.push((idx, *index_span));
                }

                let mut slot = self
                    .lookup_var_mut(base_var)
                    .expect("Semantic analysis guarantees variable exists");

                for (idx, index_span) in &evaluated_indices {
                    match slot {
                        Value::Array(items) => {
                            if *idx >= items.len() {
                                return Err(RuntimeError::new(
                                    RuntimeErrorKind::IndexOutOfBounds,
                                    *index_span,
                                ));
                            }
                            // SAFETY: idx >= 0 and < items.len()
                            slot = unsafe { items.get_unchecked_mut(*idx) };
                        }
                        _ => {
                            return Err(RuntimeError::new(
                                RuntimeErrorKind::InvalidIndex,
                                *index_span,
                            ));
                        }
                    }
                }

                match slot {
                    Value::Array(arr) => Ok(arr),
                    _ => Err(RuntimeError::new_with_extras(
                        RuntimeErrorKind::TypeMismatch,
                        span,
                        field,
                        GlobalBuiltin::type_of(slot),
                    )),
                }
            }
            _ => Err(RuntimeError::new(RuntimeErrorKind::TypeMismatch, span)),
        }
    }

    fn eval_string_expr(&mut self, parts: &StringParts<'a>) -> Value<'a> {
        match parts {
            StringParts::Static(content) => Value::Str(ArenaCow::borrowed(content)),
            StringParts::Interpolated(segments) => {
                let mut result = ArenaString::with_capacity_in(segments.len(), self.frame);
                for segment in *segments {
                    match segment {
                        StringSegment::Literal(s) => result.push_str(s),
                        StringSegment::Variable(var) => {
                            let value = self
                                .lookup_var_ref(var)
                                .expect("Semantic analysis should guarantee variable exists");
                            write!(result, "{value}").unwrap();
                        }
                    }
                }
                Value::Str(ArenaCow::owned(result))
            }
        }
    }

    fn define_var(&mut self, name: &'a str, val: Value<'a>) {
        let has_frame = self.has_frame_arena();
        if let Some(scope) = self.env.last_mut() {
            if let Some((.., slot)) = scope.iter_mut().rev().find(|(var, ..)| *var == name) {
                Self::overwrite_slot(slot, val, has_frame, &self.pool, self.frame);
            } else {
                let val = if has_frame { val.promote(&self.pool, self.frame) } else { val };
                scope.push((name, val));
            }
        }
    }

    fn assign_var(&mut self, name: &'a str, val: Value<'a>) {
        let has_frame = self.has_frame_arena();
        let pool = &self.pool;
        let frame = self.frame;

        for scope in self.env.iter_mut().rev() {
            if let Some((.., slot)) = scope.iter_mut().rev().find(|(var, ..)| *var == name) {
                Self::overwrite_slot(slot, val, has_frame, pool, frame);
                return;
            }
        }
        unreachable!("Semantic analysis guarantees variable exists");
    }

    /// Moves a function return value across a frame reset boundary.
    /// Frame-allocated owned strings are staged through persistent arena
    /// then reconstructed on the caller's frame level, so the staging is
    /// reclaimed and the return value arrives on frame.
    fn relocate_return_value(&self, val: Value<'a>, frame_offset: usize) -> Value<'a> {
        let is_frame_string = match &val {
            Value::Str(ArenaCow::Owned(s)) => !std::ptr::eq(s.arena(), self.arena),
            _ => false,
        };

        if is_frame_string {
            let Value::Str(ArenaCow::Owned(s)) = val else { unreachable!() };
            // Stage string bytes on persistent (at the current tail).
            let stage_mark = self.arena.offset();
            let staged = ArenaString::from_str(self.arena, s.as_str());
            // Drop frame string before reset (deallocate is a no-op).
            drop(s);
            unsafe { self.frame.reset(frame_offset) };
            // Reconstruct on the caller's frame from staged bytes.
            let result = ArenaString::from_str(self.frame, staged.as_str());
            // Reclaim staging!!! it is the only allocation above stage_mark.
            drop(staged);
            unsafe { self.arena.reset(stage_mark) };
            return Value::Str(ArenaCow::Owned(result));
        }

        if matches!(val, Value::Array(_)) {
            // Arrays promoted to persistent via pool.
            let promoted = val.promote(&self.pool, self.frame);
            unsafe { self.frame.reset(frame_offset) };
            return promoted;
        }

        // Numbers, bools, null, borrowed strings, persistent-owned strings
        // all survive frame reset without staging.
        unsafe { self.frame.reset(frame_offset) };
        val
    }

    /// Overwrites a variable slot, returning the old value's pool slot
    /// before promoting the new value.
    fn overwrite_slot(
        slot: &mut Value<'a>,
        val: Value<'a>,
        has_frame: bool,
        pool: &PoolSet<'a>,
        frame: &Arena,
    ) {
        if has_frame {
            let old = mem::replace(slot, Value::Null);
            unsafe { old.return_to_pool(pool) };
            *slot = val.promote(pool, frame);
        } else {
            *slot = val;
        }
    }

    fn assign_index(
        &mut self,
        target: ExprRef<'a>,
        value: Value<'a>,
        span: Span,
    ) -> Result<(), RuntimeError> {
        let (base_var, index_exprs) = self.flatten_index_target(target);

        let mut evaluated_indices = Vec::with_capacity_in(index_exprs.len(), self.frame);
        for (index_expr, index_span) in &index_exprs {
            let idx = self.eval_index_value(index_expr, *index_span)?;
            evaluated_indices.push((idx, *index_span));
        }

        // Promote before taking the mutable borrow on the variable.
        let value =
            if self.has_frame_arena() { value.promote(&self.pool, self.frame) } else { value };

        let mut slot =
            self.lookup_var_mut(base_var).expect("Semantic analysis guarantees variable exists");

        for (i, (idx, index_span)) in evaluated_indices.iter().enumerate() {
            let is_last = i + 1 == evaluated_indices.len();
            match slot {
                Value::Array(items) => {
                    if *idx >= items.len() {
                        return Err(RuntimeError::new(
                            RuntimeErrorKind::IndexOutOfBounds,
                            *index_span,
                        ));
                    }
                    if is_last {
                        let old = mem::replace(&mut items[*idx], value);
                        unsafe { old.return_to_pool(&self.pool) };
                        return Ok(());
                    }
                    // SAFETY: idx >= 0 and < items.len()
                    slot = unsafe { items.get_unchecked_mut(*idx) };
                }
                _ => {
                    return Err(RuntimeError::new(RuntimeErrorKind::InvalidIndex, span));
                }
            }
        }

        unreachable!("Index assignment should return inside loop");
    }

    fn flatten_index_target(
        &self,
        mut target: ExprRef<'a>,
    ) -> (&'a str, Vec<(ExprRef<'a>, Span), &'a Arena>) {
        let mut indices = Vec::new_in(self.frame);
        loop {
            match target {
                Expr::Index { array, index, index_span, .. } => {
                    indices.push((*index, *index_span));
                    target = array;
                }
                Expr::Var(name, ..) => {
                    indices.reverse();
                    return (*name, indices);
                }
                _ => unreachable!("Semantic analysis guarantees valid index assignment target",),
            }
        }
    }

    fn eval_index_value(
        &mut self,
        index_expr: ExprRef<'a>,
        index_span: Span,
    ) -> Result<usize, RuntimeError> {
        let value = self.eval_expr(index_expr)?;
        let Value::Number(number) = value else {
            return Err(RuntimeError::new(RuntimeErrorKind::InvalidIndex, index_span));
        };

        if !number.is_finite() || number.fract() != 0.0 {
            return Err(RuntimeError::new(RuntimeErrorKind::InvalidIndex, index_span));
        }

        if number < 0.0 {
            return Err(RuntimeError::new(RuntimeErrorKind::IndexOutOfBounds, index_span));
        }

        #[allow(clippy::cast_possible_truncation, clippy::cast_sign_loss)]
        Ok(number as usize)
    }

    fn push_scope_with_capacity(&mut self, var_capacity: usize, arena: &'a Arena) {
        self.env.push(Vec::with_capacity_in(var_capacity, arena));
        self.function_scopes.push(Vec::new_in(arena));
    }

    #[inline]
    fn lookup_var(&self, name: &str, clone_arena: &'a Arena) -> Option<Value<'a>> {
        self.lookup_env(name).map(|v| v.clone_into(clone_arena))
    }

    #[inline]
    fn lookup_var_ref(&self, name: &str) -> Option<&Value<'a>> {
        self.lookup_env(name)
    }

    fn lookup_var_mut(&mut self, name: &str) -> Option<&mut Value<'a>> {
        for scope in self.env.iter_mut().rev() {
            for (var, val) in scope.iter_mut().rev() {
                if *var == name {
                    return Some(val);
                }
            }
        }
        None
    }

    fn lookup_func_by_name(&self, name: &str) -> FunctionDef<'a> {
        self.function_scopes
            .iter()
            .rev()
            .find_map(|scope| scope.iter().rev().find(|f| f.name == name).copied())
            .expect("Semantic analysis guarantees function exists")
    }

    fn lookup_func_by_id(&self, id: FunctionId) -> FunctionDef<'a> {
        self.function_scopes
            .iter()
            .rev()
            .find_map(|scope| scope.iter().rev().find(|f| f.id == Some(id)).copied())
            .expect("Semantic analysis guarantees function exists in runtime scope")
    }

    fn function_is_pruned(&self, id: FunctionId) -> bool {
        self.optimization_plan().is_some_and(|plan| plan.removable_function_defs.contains(&id))
    }

    fn facts(&self) -> Option<&ProgramFacts<'a, 'a>> {
        // Safety: `run_with_analysis` only installs these pointers for the duration
        // of one synchronous execution and clears them before returning.
        self.facts.map(|facts| unsafe { facts.as_ref() })
    }

    fn optimization_plan(&self) -> Option<&OptimizationPlan<'a>> {
        // Safety: `run_with_analysis` only installs these pointers for the duration
        // of one synchronous execution and clears them before returning.
        self.optimization_plan.map(|plan| unsafe { plan.as_ref() })
    }

    fn lookup_env(&self, name: &str) -> Option<&Value<'a>> {
        self.env.iter().rev().find_map(|scope| {
            scope.iter().rev().find_map(|(var, val)| if *var == name { Some(val) } else { None })
        })
    }
}
