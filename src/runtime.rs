//! The runtime for NaijaScript.

use std::fmt::{self, Write};
use std::{io, mem};

use crate::arena::{Arena, ArenaCow, ArenaString};
use crate::arena_format;
use crate::builtins::{
    ArrayBuiltin, Builtin, GlobalBuiltin, MemberBuiltin, NumberBuiltin, StringBuiltin,
};
use crate::diagnostics::{AsStr, Diagnostics, Label, Severity, Span};
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

// Maximum recursion depth to prevent stack overflow
const MAX_CALL_DEPTH: usize = 512;

/// The value types our runtime can work with at runtime.
#[derive(Debug, Clone, PartialEq)]
pub enum Value<'arena, 'src> {
    /// String literals reference original source when possible
    Str(ArenaCow<'arena, 'src>),
    /// All numbers are f64 to keep arithmetic simple and avoid int/float distinction
    Number(f64),
    /// Standard boolean values
    Bool(bool),
    /// Array literal values
    Array(Vec<Value<'arena, 'src>, &'arena Arena>),
    /// Represents the absence of a value
    Null,
}

impl fmt::Display for Value<'_, '_> {
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
    Break,
    LoopContinue,
}

/// The runtime interface for NaijaScript using arena-allocated AST.
pub struct Runtime<'arena, 'src> {
    // Variable scopes, each Vec is a scope, inner Vec is variables in that scope
    env: Vec<Vec<(&'src str, Value<'arena, 'src>), &'arena Arena>, &'arena Arena>,

    // Global function table, functions are first-class but stored separately
    functions: Vec<FunctionDef<'src>, &'arena Arena>,

    // Call stack for function execution, prevents infinite recursion
    call_stack: Vec<ActivationRecord<'arena, 'src>, &'arena Arena>,

    pub output: Vec<Value<'arena, 'src>, &'arena Arena>,

    /// Collection of runtime errors encountered during execution
    pub errors: Diagnostics<'arena>,

    // Reference to the arena for allocating runtime data structures
    arena: &'arena Arena,
}

impl<'arena, 'src> Runtime<'arena, 'src> {
    /// Creates a new [`Runtime`] instance.
    pub fn new(arena: &'arena Arena) -> Self {
        Self {
            env: Vec::new_in(arena),
            functions: Vec::new_in(arena),
            call_stack: Vec::new_in(arena),
            output: Vec::new_in(arena),
            errors: Diagnostics::new(arena),
            arena,
        }
    }

    /// Executes a NaijaScript program starting from the root block.
    pub fn run(&mut self, root: BlockRef<'src>) -> &Diagnostics<'arena> {
        self.env.push(Vec::new_in(self.arena)); // enter global scope

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
        self.env.pop(); // exit global scope
        &self.errors
    }

    fn exec_stmt(&mut self, stmt: StmtRef<'src>) -> Result<ExecFlow<'arena, 'src>, RuntimeError> {
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
                    match self.exec_block_with_flow(body)? {
                        ExecFlow::Continue => continue,
                        ExecFlow::Break => break,
                        ExecFlow::LoopContinue => continue,
                        flow @ ExecFlow::Return(..) => return Ok(flow),
                    }
                }
                Ok(ExecFlow::Continue)
            }
            Stmt::Block { block, .. } => self.exec_block_with_flow(block),
            Stmt::FunctionDef { name, params, body, .. } => {
                let func_def = FunctionDef { name, params, body };
                self.functions.push(func_def);
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
    fn exec_block_with_flow(
        &mut self,
        block: BlockRef<'src>,
    ) -> Result<ExecFlow<'arena, 'src>, RuntimeError> {
        self.env.push(Vec::new_in(self.arena)); // enter new block scope
        for stmt in block.stmts {
            match self.exec_stmt(stmt)? {
                ExecFlow::Continue => continue,
                flow @ (ExecFlow::Return(..) | ExecFlow::Break | ExecFlow::LoopContinue) => {
                    self.env.pop(); // exit block scope before returning
                    return Ok(flow);
                }
            }
        }
        self.env.pop(); // exit block scope
        Ok(ExecFlow::Continue)
    }

    #[inline]
    fn eval_expr(&mut self, expr: ExprRef<'src>) -> Result<Value<'arena, 'src>, RuntimeError> {
        match expr {
            Expr::Number(n, ..) => Ok(Value::Number(
                n.parse::<f64>().expect("Scanner should guarantee valid number format"),
            )),
            Expr::String { parts, .. } => self.eval_string_expr(parts),
            Expr::Bool(b, ..) => Ok(Value::Bool(*b)),
            Expr::Null(..) => Ok(Value::Null),
            Expr::Var(v, ..) => {
                let val = self
                    .lookup_var(v)
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
                            BinaryOp::Eq => Ok(Value::Bool(lv == rv)),
                            BinaryOp::Gt => Ok(Value::Bool(lv > rv)),
                            BinaryOp::Lt => Ok(Value::Bool(lv < rv)),
                            _ => unreachable!("Semantic analysis guarantees valid number ops"),
                        },
                        (Value::Str(ls), Value::Str(rs)) => match op {
                            BinaryOp::Add => {
                                let mut s =
                                    ArenaString::with_capacity_in(ls.len() + rs.len(), self.arena);
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
                            let mut s =
                                ArenaString::with_capacity_in(ls.len() + num_str.len(), self.arena);
                            s.push_str(&ls);
                            s.push_str(&num_str);
                            Ok(Value::Str(ArenaCow::Owned(s)))
                        }
                        (Value::Number(n), Value::Str(rs)) => {
                            assert!(matches!(op, BinaryOp::Add));
                            let num_str = n.to_string();
                            let mut s =
                                ArenaString::with_capacity_in(num_str.len() + rs.len(), self.arena);
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
                        (Value::Null, Value::Null) => match op {
                            BinaryOp::Eq => Ok(Value::Bool(true)),
                            BinaryOp::Gt | BinaryOp::Lt => Ok(Value::Bool(false)),
                            _ => unreachable!("Semantic analysis guarantees valid null ops"),
                        },
                        (Value::Null, ..) | (.., Value::Null) => match op {
                            BinaryOp::Eq => Ok(Value::Bool(false)),
                            BinaryOp::Gt | BinaryOp::Lt => Ok(Value::Bool(false)),
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
                let mut values = Vec::with_capacity_in(elements.len(), self.arena);
                for element in *elements {
                    let val = self.eval_expr(element)?;
                    values.push(val);
                }
                Ok(Value::Array(values))
            }
            Expr::Index { array, index, index_span, .. } => {
                let array_value = self.eval_expr(array)?;
                let index_value = self.eval_expr(index)?;
                let mut items = match array_value {
                    Value::Array(items) => items,
                    _ => unreachable!("Semantic analysis guarantees only arrays can be indexed"),
                };

                let index_number = match index_value {
                    Value::Number(n) => n,
                    _ => {
                        return Err(RuntimeError::new(RuntimeErrorKind::InvalidIndex, *index_span));
                    }
                };

                if !index_number.is_finite() || index_number.fract() != 0.0 {
                    return Err(RuntimeError::new(RuntimeErrorKind::InvalidIndex, *index_span));
                }

                let idx = index_number as isize;
                if idx < 0 || idx >= items.len() as isize {
                    return Err(RuntimeError::new(RuntimeErrorKind::IndexOutOfBounds, *index_span));
                }

                // SAFETY: `idx` is >= 0 and < items.len()
                let slot = unsafe { items.get_unchecked_mut(idx as usize) };
                let slot = mem::replace(slot, Value::Null);
                Ok(slot)
            }
            Expr::Member { .. } => {
                unreachable!("Semantic analysis guarantees member access is always a function call")
            }
            Expr::Call { callee, args, span } => self.eval_function_call(callee, args, span),
        }
    }

    fn eval_function_call(
        &mut self,
        callee: ExprRef<'src>,
        args: &'src ArgList<'src>,
        span: &'src Span,
    ) -> Result<Value<'arena, 'src>, RuntimeError> {
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

        // We check for user-defined functions next
        let func_idx = self.lookup_func(func_name);

        // We check for recursion depth
        if self.call_stack.len() >= MAX_CALL_DEPTH {
            return Err(RuntimeError::new(RuntimeErrorKind::StackOverflow, *span));
        }

        // We evaluate all arguments eagerly (left-to-right evaluation order)
        let mut arg_values = Vec::with_capacity_in(args.args.len(), self.arena);
        for arg_expr in args.args {
            arg_values.push(self.eval_expr(arg_expr)?);
        }

        let func_def = &self.functions[func_idx];
        assert_eq!(arg_values.len(), func_def.params.params.len());

        // We create a new activation record for the function call
        let mut local_vars = Vec::with_capacity_in(func_def.params.params.len(), self.arena);
        for (param, arg) in func_def.params.params.iter().zip(arg_values.into_iter()) {
            local_vars.push((*param, arg));
        }
        self.call_stack.push(ActivationRecord { local_vars });

        // We execute the function body with proper return value handling
        let val = match self.exec_block_with_flow(func_def.body)? {
            ExecFlow::Continue => Value::Null,
            ExecFlow::Return(val) => val,
            ExecFlow::Break | ExecFlow::LoopContinue => {
                unreachable!(
                    "Break/Continue should be caught by loop, not escape to function boundary"
                )
            }
        };

        // We remove the activation record from the call stack
        self.call_stack.pop();
        Ok(val)
    }

    fn eval_builtin_call(
        &mut self,
        builtin: GlobalBuiltin,
        args: &'src ArgList<'src>,
        span: Span,
    ) -> Result<Value<'arena, 'src>, RuntimeError> {
        // We evaluate all arguments eagerly (left-to-right evaluation order)
        let mut arg_values = Vec::with_capacity_in(args.args.len(), self.arena);
        for arg_expr in args.args {
            arg_values.push(self.eval_expr(arg_expr)?);
        }
        assert_eq!(arg_values.len(), builtin.arity());

        match builtin {
            GlobalBuiltin::Shout => {
                let argv = mem::replace(&mut arg_values[0], Value::Null);
                GlobalBuiltin::shout(&argv);
                self.output.push(argv);
                Ok(Value::Null)
            }

            GlobalBuiltin::TypeOf => {
                let t = GlobalBuiltin::type_of(&arg_values[0]);
                Ok(Value::Str(ArenaCow::borrowed(t)))
            }
            GlobalBuiltin::ReadLine => {
                let prompt = arg_values[0].to_string();
                let s = GlobalBuiltin::read_line(&prompt, self.arena)
                    .map_err(|err| RuntimeError::new(err.into(), span))?;
                Ok(Value::Str(ArenaCow::owned(s)))
            }
            GlobalBuiltin::ToString => {
                let s = GlobalBuiltin::to_string(self.arena, &arg_values[0]);
                Ok(Value::Str(ArenaCow::owned(s)))
            }
        }
    }

    fn eval_member_call(
        &mut self,
        object: ExprRef<'src>,
        field: &'src str,
        args: &'src ArgList<'src>,
        span: Span,
    ) -> Result<Value<'arena, 'src>, RuntimeError> {
        let receiver = self.eval_expr(object)?;
        let builtin = match MemberBuiltin::from_name(field) {
            Some(b) => b,
            None => {
                return Err(RuntimeError::new_with_extras(
                    RuntimeErrorKind::TypeMismatch,
                    span,
                    field,
                    GlobalBuiltin::type_of(&receiver),
                ));
            }
        };

        if builtin.requires_mut_receiver() {
            match builtin {
                MemberBuiltin::Array(..) => {
                    return self.eval_array_member_call_mut(object, field, args, span);
                }
                _ => unreachable!("Only array methods require mutable receiver"),
            }
        }

        match receiver {
            Value::Str(s) => self.eval_string_member_call(s, field, args),
            Value::Number(n) => self.eval_number_member_call(n, field),
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
        receiver: ExprRef<'src>,
        field: &'src str,
        args: &'src ArgList<'src>,
        span: Span,
    ) -> Result<Value<'arena, 'src>, RuntimeError> {
        let receiver_value = self.eval_expr(receiver)?;

        if !matches!(receiver, Expr::Var(..) | Expr::Index { .. }) {
            return Err(RuntimeError::new_with_extras(
                RuntimeErrorKind::TypeMismatch,
                span,
                field,
                GlobalBuiltin::type_of(&receiver_value),
            ));
        }

        let array_builtin = match ArrayBuiltin::from_name(field) {
            Some(b) => b,
            None => {
                return Err(RuntimeError::new_with_extras(
                    RuntimeErrorKind::TypeMismatch,
                    span,
                    field,
                    GlobalBuiltin::type_of(&receiver_value),
                ));
            }
        };

        match array_builtin {
            ArrayBuiltin::Push => {
                let value = self.eval_expr(args.args[0])?;
                let array = self.get_mutable_array(receiver, span, field)?;
                ArrayBuiltin::push(array, value);
                Ok(Value::Null)
            }
            ArrayBuiltin::Pop => {
                let array = self.get_mutable_array(receiver, span, field)?;
                let popped = ArrayBuiltin::pop(array);
                Ok(popped.unwrap_or(Value::Null))
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
        array: &Vec<Value<'arena, 'src>, &'arena Arena>,
        field: &'src str,
        args: &'src ArgList<'src>,
    ) -> Result<Value<'arena, 'src>, RuntimeError> {
        let array_builtin = ArrayBuiltin::from_name(field)
            .expect("Semantic analysis guarantees valid array method");
        match array_builtin {
            ArrayBuiltin::Len => Ok(Value::Number(ArrayBuiltin::len(array))),
            ArrayBuiltin::Join => {
                let sep = self.eval_expr(args.args[0])?;
                let sep = match sep {
                    Value::Str(s) => s,
                    _ => unreachable!("Semantic analysis guarantees string arg"),
                };
                let result = ArrayBuiltin::join(array, &sep, self.arena);
                Ok(Value::Str(ArenaCow::Owned(result)))
            }
            ArrayBuiltin::Push | ArrayBuiltin::Pop | ArrayBuiltin::Reverse => {
                unreachable!("Push, Pop, and Reverse require mutable receiver")
            }
        }
    }

    fn eval_string_member_call(
        &mut self,
        s: ArenaCow<'arena, 'src>,
        field: &'src str,
        args: &'src ArgList<'src>,
    ) -> Result<Value<'arena, 'src>, RuntimeError> {
        let string_builtin = StringBuiltin::from_name(field)
            .expect("Semantic analysis guarantees valid string method");
        match string_builtin {
            StringBuiltin::Len => Ok(Value::Number(StringBuiltin::len(&s))),
            StringBuiltin::Slice => {
                let start = self.eval_expr(args.args[0])?;
                let end = self.eval_expr(args.args[1])?;
                match (start, end) {
                    (Value::Number(start), Value::Number(end)) => {
                        let s = StringBuiltin::slice(&s, start, end, self.arena);
                        Ok(Value::Str(ArenaCow::Owned(s)))
                    }
                    _ => unreachable!("Semantic analysis guarantees number args"),
                }
            }
            StringBuiltin::ToUppercase => {
                let s = StringBuiltin::to_uppercase(&s, self.arena);
                Ok(Value::Str(ArenaCow::Owned(s)))
            }
            StringBuiltin::ToLowercase => {
                let s = StringBuiltin::to_lowercase(&s, self.arena);
                Ok(Value::Str(ArenaCow::Owned(s)))
            }
            StringBuiltin::Trim => {
                let s = StringBuiltin::trim(&s, self.arena);
                Ok(Value::Str(ArenaCow::Owned(s)))
            }
            StringBuiltin::Find => {
                let needle = self.eval_expr(args.args[0])?;
                match needle {
                    Value::Str(n) => Ok(Value::Number(StringBuiltin::find(&s, &n))),
                    _ => unreachable!("Semantic analysis guarantees string arg"),
                }
            }
            StringBuiltin::Replace => {
                let old = self.eval_expr(args.args[0])?;
                let new = self.eval_expr(args.args[1])?;
                match (old, new) {
                    (Value::Str(o), Value::Str(n)) => {
                        let result = StringBuiltin::replace(&s, &o, &n, self.arena);
                        Ok(Value::Str(ArenaCow::Owned(result)))
                    }
                    _ => unreachable!("Semantic analysis guarantees string args"),
                }
            }
            StringBuiltin::ToNumber => Ok(Value::Number(StringBuiltin::to_number(&s))),
            StringBuiltin::Split => {
                let pattern = self.eval_expr(args.args[0])?;
                match pattern {
                    Value::Str(pat) => {
                        let mut collection =
                            Vec::with_capacity_in(s.len() / pat.len().max(1) + 1, self.arena);
                        StringBuiltin::split(&s, &pat, self.arena)
                            .for_each(|s| collection.push(Value::Str(ArenaCow::Owned(s))));
                        Ok(Value::Array(collection))
                    }
                    _ => unreachable!("Semantic analysis guarantees string arg"),
                }
            }
        }
    }

    fn eval_number_member_call(
        &mut self,
        n: f64,
        field: &'src str,
    ) -> Result<Value<'arena, 'src>, RuntimeError> {
        let number_builtin = NumberBuiltin::from_name(field)
            .expect("Semantic analysis guarantees valid number method");
        match number_builtin {
            NumberBuiltin::Abs => Ok(Value::Number(NumberBuiltin::abs(n))),
            NumberBuiltin::Sqrt => Ok(Value::Number(NumberBuiltin::sqrt(n))),
            NumberBuiltin::Floor => Ok(Value::Number(NumberBuiltin::floor(n))),
            NumberBuiltin::Ceil => Ok(Value::Number(NumberBuiltin::ceil(n))),
            NumberBuiltin::Round => Ok(Value::Number(NumberBuiltin::round(n))),
        }
    }

    fn get_mutable_array(
        &mut self,
        object: ExprRef<'src>,
        span: Span,
        field: impl Into<String>,
    ) -> Result<&mut Vec<Value<'arena, 'src>, &'arena Arena>, RuntimeError> {
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

                let mut evaluated_indices = Vec::with_capacity_in(index_exprs.len(), self.arena);
                for (index_expr, index_span) in &index_exprs {
                    let idx = self.eval_index_value(index_expr, *index_span)?;
                    evaluated_indices.push((idx, *index_span));
                }

                let mut slot = self
                    .lookup_var_mut(base_var)
                    .expect("Semantic analysis guarantees variable exists");

                for (idx, index_span) in evaluated_indices.iter() {
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

    fn eval_string_expr(
        &mut self,
        parts: &StringParts<'src>,
    ) -> Result<Value<'arena, 'src>, RuntimeError> {
        match parts {
            StringParts::Static(content) => Ok(Value::Str(ArenaCow::borrowed(content))),
            StringParts::Interpolated(segments) => {
                let mut result = ArenaString::with_capacity_in(segments.len(), self.arena);
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

    fn define_var(&mut self, name: &'src str, val: Value<'arena, 'src>) {
        if let Some(scope) = self.env.last_mut() {
            if let Some((.., slot)) = scope.iter_mut().find(|(var, ..)| *var == name) {
                *slot = val;
            } else {
                scope.push((name, val));
            }
        }
    }

    fn assign_var(&mut self, name: &'src str, val: Value<'arena, 'src>) {
        // First try function-local variables
        if let Some(slot) = self.lookup_call_stack_mut(name) {
            *slot = val;
            return;
        }

        // Then try regular scope stack
        for scope in self.env.iter_mut().rev() {
            if let Some((.., slot)) = scope.iter_mut().find(|(var, ..)| *var == name) {
                *slot = val;
                return;
            }
        }
        unreachable!("Semantic analysis guarantees variable exists");
    }

    fn assign_index(
        &mut self,
        target: ExprRef<'src>,
        value: Value<'arena, 'src>,
        span: Span,
    ) -> Result<(), RuntimeError> {
        let (base_var, index_exprs) = self.flatten_index_target(target);

        let mut evaluated_indices = Vec::with_capacity_in(index_exprs.len(), self.arena);
        for (index_expr, index_span) in &index_exprs {
            let idx = self.eval_index_value(index_expr, *index_span)?;
            evaluated_indices.push((idx, *index_span));
        }

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
                        items[*idx] = value;
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
        mut target: ExprRef<'src>,
    ) -> (&'src str, Vec<(ExprRef<'src>, Span), &'arena Arena>) {
        let mut indices = Vec::new_in(self.arena);
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
        index_expr: ExprRef<'src>,
        index_span: Span,
    ) -> Result<usize, RuntimeError> {
        let value = self.eval_expr(index_expr)?;
        let number = match value {
            Value::Number(n) => n,
            _ => {
                return Err(RuntimeError::new(RuntimeErrorKind::InvalidIndex, index_span));
            }
        };

        if !number.is_finite() || number.fract() != 0.0 {
            return Err(RuntimeError::new(RuntimeErrorKind::InvalidIndex, index_span));
        }

        if number < 0.0 {
            return Err(RuntimeError::new(RuntimeErrorKind::IndexOutOfBounds, index_span));
        }

        Ok(number as usize)
    }

    #[inline]
    fn lookup_var(&self, name: &str) -> Option<Value<'arena, 'src>> {
        self.lookup_call_stack(name).cloned().or_else(|| self.lookup_env(name).cloned())
    }

    fn lookup_var_mut(&mut self, name: &str) -> Option<&mut Value<'arena, 'src>> {
        if let Some(activation) = self.call_stack.last_mut() {
            for (var, val) in activation.local_vars.iter_mut() {
                if *var == name {
                    return Some(val);
                }
            }
        }

        for scope in self.env.iter_mut().rev() {
            for (var, val) in scope.iter_mut() {
                if *var == name {
                    return Some(val);
                }
            }
        }
        None
    }

    fn lookup_func(&self, name: &str) -> usize {
        self.functions
            .iter()
            .position(|f| f.name == name)
            .expect("Semantic analysis guarantees function exists")
    }

    fn lookup_env(&self, name: &str) -> Option<&Value<'arena, 'src>> {
        self.env.iter().rev().find_map(|scope| {
            scope.iter().find_map(|(var, val)| if *var == name { Some(val) } else { None })
        })
    }

    fn lookup_call_stack(&self, name: &str) -> Option<&Value<'arena, 'src>> {
        self.call_stack.last().and_then(|activation| {
            activation
                .local_vars
                .iter()
                .find_map(|(var, val)| if *var == name { Some(val) } else { None })
        })
    }

    fn lookup_call_stack_mut(&mut self, name: &str) -> Option<&mut Value<'arena, 'src>> {
        self.call_stack.last_mut().and_then(|activation| {
            activation
                .local_vars
                .iter_mut()
                .find_map(|(var, val)| if *var == name { Some(val) } else { None })
        })
    }
}
