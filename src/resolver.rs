//! The resolver (semantic analyzer) for NaijaScript.

use std::collections::HashSet;

use crate::arena::{Arena, ArenaCow};
use crate::arena_format;
use crate::builtins::{Builtin, BuiltinReturnType};
use crate::diagnostics::{AsStr, Diagnostics, Label, Severity, Span};
use crate::syntax::parser::{
    BinaryOp, BlockRef, Expr, ExprRef, ParamListRef, Stmt, StmtRef, StringParts, StringSegment,
    UnaryOp,
};

/// Represents semantic errors that can occur during analysis.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SemanticError {
    DuplicateIdentifier,
    AssignmentToUndeclared,
    TypeMismatch,
    UndeclaredIdentifier,
    FunctionCallArity,
    UnreachableCode,
    ReservedKeyword,
}

impl AsStr for SemanticError {
    fn as_str(&self) -> &'static str {
        match self {
            SemanticError::DuplicateIdentifier => "Duplicate identifier",
            SemanticError::AssignmentToUndeclared => "Assignment to undeclared variable",
            SemanticError::TypeMismatch => "Type mismatch",
            SemanticError::UndeclaredIdentifier => "Undeclared identifier",
            SemanticError::FunctionCallArity => "Invalid parameter count",
            SemanticError::UnreachableCode => "Unreachable code",
            SemanticError::ReservedKeyword => "Use of reserved keyword",
        }
    }
}

// Represents the value types in NaijaScript
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
enum ValueType {
    Number,
    String,
    Bool,
    Array,
    Dynamic,
}

impl From<BuiltinReturnType> for ValueType {
    fn from(builtin_type: BuiltinReturnType) -> Self {
        match builtin_type {
            BuiltinReturnType::Number => ValueType::Number,
            BuiltinReturnType::String => ValueType::String,
            BuiltinReturnType::Bool => ValueType::Bool,
        }
    }
}

// Represents the blueprint of a function signature in NaijaScript
#[derive(Debug)]
struct FunctionSig<'ast> {
    name: &'ast str,
    param_names: &'ast [&'ast str],
    name_span: &'ast Span,
}

/// An arena-backed semantic analyzer.
///
/// FWIW, this is a work in progress and may change as the language evolves.
pub struct Resolver<'ast> {
    // Stack of variable symbol tables for block-scoped variables
    variable_scopes: Vec<Vec<(&'ast str, ValueType, &'ast Span), &'ast Arena>, &'ast Arena>,

    // Stack of function symbol tables for block-scoped functions
    function_scopes: Vec<Vec<FunctionSig<'ast>, &'ast Arena>, &'ast Arena>,

    // Track current function context for return statement validation
    current_function: Option<&'ast str>,

    /// Collection of semantic errors found during analysis
    pub errors: Diagnostics<'ast>,

    // Reference to the arena for allocating scope vectors
    arena: &'ast Arena,
}

impl<'ast> Resolver<'ast> {
    /// Creates a new [`Resolver`] instance.
    pub fn new(arena: &'ast Arena) -> Self {
        Self {
            variable_scopes: Vec::new_in(arena),
            function_scopes: Vec::new_in(arena),
            current_function: None,
            errors: Diagnostics::new(arena),
            arena,
        }
    }

    /// Resolves the given AST root node.
    pub fn resolve(&mut self, root: BlockRef<'ast>) {
        // Enter the first (global) scope
        self.variable_scopes.push(Vec::new_in(self.arena));
        self.function_scopes.push(Vec::new_in(self.arena));
        self.check_block(root);
        // Leave the global scope
        self.variable_scopes.pop();
        self.function_scopes.pop();
    }

    fn emit_error(&mut self, span: Span, error: SemanticError, labels: Vec<Label<'ast>>) {
        self.errors.emit(span, Severity::Error, "semantic", error.as_str(), labels);
    }

    #[inline]
    fn check_block(&mut self, block: BlockRef<'ast>) {
        // Enter new block scope
        self.variable_scopes.push(Vec::new_in(self.arena));
        self.function_scopes.push(Vec::new_in(self.arena));

        let mut has_return = false;
        for &stmt in block.stmts {
            // Check for dead code after return statement
            if has_return {
                let span = match stmt {
                    Stmt::Assign { span, .. } => span,
                    Stmt::AssignExisting { span, .. } => span,
                    Stmt::AssignIndex { span, .. } => span,
                    Stmt::If { span, .. } => span,
                    Stmt::Loop { span, .. } => span,
                    Stmt::Block { span, .. } => span,
                    Stmt::FunctionDef { span, .. } => span,
                    Stmt::Return { span, .. } => span,
                    Stmt::Expression { span, .. } => span,
                };

                self.emit_error(
                    *span,
                    SemanticError::UnreachableCode,
                    vec![Label {
                        span: *span,
                        message: ArenaCow::Borrowed("Dead code after `return` statement"),
                    }],
                );
            }

            if matches!(stmt, Stmt::Return { .. }) {
                has_return = true;
            }

            self.check_stmt(stmt);
        }

        // Leave this block scope
        self.variable_scopes.pop();
        self.function_scopes.pop();
    }

    fn check_stmt(&mut self, stmt: StmtRef<'ast>) {
        match stmt {
            // Handle "make variable get expression" statements
            Stmt::Assign { var, var_span, expr, .. } => {
                if Builtin::from_name(var).is_some() {
                    self.emit_error(
                        *var_span,
                        SemanticError::ReservedKeyword,
                        vec![Label {
                            span: *var_span,
                            message: ArenaCow::Owned(arena_format!(
                                self.arena,
                                "`{var}` na reserved keyword",
                            )),
                        }],
                    );
                }

                self.check_expr(expr);
                let var_type = self.infer_expr_type(expr).unwrap_or(ValueType::Dynamic);
                self.variable_scopes
                    .last_mut()
                    .expect("scope stack should never be empty")
                    .push((var, var_type, var_span));
            }
            // Handle variable reassignment: <variable> get <expression>
            Stmt::AssignExisting { var, var_span, expr, .. } => {
                if !self.lookup_var(var) {
                    self.emit_error(
                        *var_span,
                        SemanticError::AssignmentToUndeclared,
                        vec![Label {
                            span: *var_span,
                            message: ArenaCow::Owned(arena_format!(
                                self.arena,
                                "Variable `{var}` no dey scope",
                            )),
                        }],
                    );
                }
                self.check_expr(expr);
            }
            Stmt::AssignIndex { target, expr, .. } => {
                self.check_assign_index(target, expr);
            }
            // Handle "if to say(condition) start...end" with optional "if not so"
            Stmt::If { cond, then_b, else_b, .. } => {
                self.check_expr(cond);
                self.check_boolean_expr(cond);
                self.check_block(then_b);
                // Else block is optional in the grammar
                if let Some(eb) = else_b {
                    self.check_block(eb);
                }
            }
            // Handle "jasi(condition) start...end" loop statements
            Stmt::Loop { cond, body, .. } => {
                self.check_expr(cond);
                self.check_boolean_expr(cond);
                self.check_block(body);
            }
            // Handle nested blocks
            Stmt::Block { block, .. } => {
                self.check_block(block);
            }
            Stmt::FunctionDef { name, name_span, params, body, span } => {
                if Builtin::from_name(name).is_some() {
                    self.emit_error(
                        *name_span,
                        SemanticError::ReservedKeyword,
                        vec![Label {
                            span: *name_span,
                            message: ArenaCow::Owned(arena_format!(
                                self.arena,
                                "`{name}` na reserved keyword",
                            )),
                        }],
                    );
                }
                self.check_function_def(name, name_span, params, body, span);
            }
            Stmt::Return { expr, span } => {
                self.check_return_stmt(*expr, span);
            }
            Stmt::Expression { expr, .. } => {
                self.check_expr(expr);
            }
        }
    }

    fn check_assign_index(&mut self, target: ExprRef<'ast>, expr: ExprRef<'ast>) {
        self.check_expr(target);
        self.check_expr(expr);
    }

    #[inline]
    fn lookup_var(&self, var: &str) -> bool {
        self.variable_scopes.iter().rev().any(|scope| scope.iter().any(|(name, ..)| *name == var))
    }

    #[inline]
    fn lookup_func(&self, func: &str) -> Option<&FunctionSig<'ast>> {
        for scope in self.function_scopes.iter().rev() {
            if let Some(sig) = scope.iter().find(|sig| sig.name == func) {
                return Some(sig);
            }
        }
        None
    }

    fn check_function_def(
        &mut self,
        name: &'ast str,
        name_span: &'ast Span,
        params: ParamListRef<'ast>,
        body: BlockRef<'ast>,
        span: &'ast Span,
    ) {
        // Check for duplicate function declaration in current scope
        if let Some(current_scope) = self.function_scopes.last()
            && let Some(existing_func) = current_scope.iter().find(|sig| sig.name == name)
        {
            self.emit_error(
                *name_span,
                SemanticError::DuplicateIdentifier,
                vec![
                    Label {
                        span: *existing_func.name_span,
                        message: ArenaCow::Owned(arena_format!(
                            self.arena,
                            "Function `{name}` dey scope already",
                        )),
                    },
                    Label { span: *name_span, message: ArenaCow::Borrowed("Duplicate here") },
                ],
            );
            return;
        }

        // Check for duplicate parameter names
        let mut set = HashSet::with_capacity(params.params.len());
        for (param_name, param_span) in params.params.iter().zip(params.param_spans.iter()) {
            if Builtin::from_name(param_name).is_some() {
                self.emit_error(
                    *param_span,
                    SemanticError::ReservedKeyword,
                    vec![Label {
                        span: *param_span,
                        message: ArenaCow::Owned(arena_format!(
                            self.arena,
                            "`{param_name}` na reserved keyword",
                        )),
                    }],
                );
            }
            if !set.insert(param_name) {
                self.emit_error(
                    *param_span,
                    SemanticError::DuplicateIdentifier,
                    vec![Label {
                        span: *param_span,
                        message: ArenaCow::Owned(arena_format!(
                            self.arena,
                            "Parameter `{param_name}` na duplicate",
                        )),
                    }],
                );
            }
        }

        // Add function to current function scope
        self.function_scopes
            .last_mut()
            .expect("function scope stack should never be empty")
            .push(FunctionSig { name, name_span, param_names: params.params });

        // Set current function context for return validation
        let prev_function = self.current_function;
        self.current_function = Some(name);

        // Create new scope for function parameters
        self.variable_scopes.push(Vec::new_in(self.arena));

        // Add parameters as variables with dynamic typing
        // Parameters can accept any type and their usage will be validated at runtime
        for param_name in params.params {
            self.variable_scopes.last_mut().unwrap().push((param_name, ValueType::Dynamic, span));
        }

        // Check function body using check_block to get dead code detection
        // Note: check_block will create its own scope, but that's fine for function bodies
        self.check_block(body);

        // Clean up parameter scope
        self.variable_scopes.pop();

        // Restore previous function context
        self.current_function = prev_function;
    }

    fn check_return_stmt(&mut self, expr: Option<ExprRef<'ast>>, span: &'ast Span) {
        // Check if we're inside a function
        if self.current_function.is_none() {
            self.emit_error(
                *span,
                SemanticError::UnreachableCode,
                vec![Label {
                    span: *span,
                    message: ArenaCow::Borrowed(
                        "You no fit use `return` statement outside function body",
                    ),
                }],
            );
        }

        // Check return expression if present
        if let Some(expr_id) = expr {
            self.check_expr(expr_id);
        }
    }

    #[inline]
    fn check_boolean_expr(&mut self, expr: ExprRef<'ast>) {
        let expr_type = self.infer_expr_type(expr);
        if let Some(t) = expr_type
            && !matches!(t, ValueType::Bool | ValueType::Dynamic)
        {
            let span = match expr {
                Expr::Number(.., span) => *span,
                Expr::String { span, .. } => *span,
                Expr::Bool(.., span) => *span,
                Expr::Var(.., span) => *span,
                Expr::Binary { span, .. } => *span,
                Expr::Unary { span, .. } => *span,
                Expr::Call { span, .. } => *span,
                Expr::Index { span, .. } => *span,
                Expr::Array { span, .. } => *span,
            };
            self.emit_error(
                span,
                SemanticError::TypeMismatch,
                vec![Label { span, message: ArenaCow::Borrowed("Dis expression no be boolean") }],
            );
        }
    }

    #[inline]
    fn check_expr(&mut self, expr: ExprRef<'ast>) {
        match expr {
            // Literals are always valid since they represent concrete values
            Expr::Number(..) | Expr::Bool(..) => {}
            Expr::String { parts, span } => {
                if let StringParts::Interpolated(segments) = parts {
                    for segment in *segments {
                        if let StringSegment::Variable(var) = segment
                            && !self.lookup_var(var)
                        {
                            self.emit_error(
                                *span,
                                SemanticError::UndeclaredIdentifier,
                                vec![Label {
                                    span: *span,
                                    message: ArenaCow::Owned(arena_format!(
                                        self.arena,
                                        "Variable `{var}` no dey scope"
                                    )),
                                }],
                            );
                        }
                    }
                }
            }
            Expr::Array { elements, .. } => {
                for element in *elements {
                    self.check_expr(element);
                }
            }
            Expr::Index { array, index, index_span, span } => {
                self.check_expr(array);
                self.check_expr(index);

                let array_ty = self.infer_expr_type(array);
                if array_ty != Some(ValueType::Array) && array_ty != Some(ValueType::Dynamic) {
                    self.emit_error(
                        *span,
                        SemanticError::TypeMismatch,
                        vec![Label {
                            span: *span,
                            message: ArenaCow::Borrowed("Dis expression no be array"),
                        }],
                    );
                }

                let index_ty = self.infer_expr_type(index);
                if index_ty != Some(ValueType::Number) && index_ty != Some(ValueType::Dynamic) {
                    self.emit_error(
                        *index_span,
                        SemanticError::TypeMismatch,
                        vec![Label {
                            span: *index_span,
                            message: ArenaCow::Borrowed("Array index no be number"),
                        }],
                    );
                }
            }
            // Variables need to exist in our symbol table before we can use them
            Expr::Var(v, span) => {
                if !self.lookup_var(v) {
                    self.emit_error(
                        *span,
                        SemanticError::UndeclaredIdentifier,
                        vec![Label {
                            span: *span,
                            message: ArenaCow::Owned(arena_format!(
                                self.arena,
                                "Variable {v} no dey scope"
                            )),
                        }],
                    );
                }
            }
            // Binary operations need type compatibility between operands
            Expr::Binary { op, lhs, rhs, span } => {
                self.check_expr(lhs);
                self.check_expr(rhs);
                let l = self.infer_expr_type(lhs);
                let r = self.infer_expr_type(rhs);
                match op {
                    BinaryOp::Add => match (l, r) {
                        (Some(ValueType::String), ..)
                        | (.., Some(ValueType::String))
                        | (Some(ValueType::Dynamic), ..)
                        | (.., Some(ValueType::Dynamic))
                        | (Some(ValueType::Number), Some(ValueType::Number)) => {}
                        _ => {
                            self.emit_error(
                                *span,
                                SemanticError::TypeMismatch,
                                vec![Label {
                                    span: *span,
                                    message: ArenaCow::Borrowed(
                                        "Dis expression no be number or string",
                                    ),
                                }],
                            );
                        }
                    },
                    BinaryOp::Minus | BinaryOp::Times | BinaryOp::Divide | BinaryOp::Mod => {
                        match (l, r) {
                            (Some(ValueType::Number), Some(ValueType::Number))
                            | (Some(ValueType::Dynamic), Some(ValueType::Number))
                            | (Some(ValueType::Number), Some(ValueType::Dynamic))
                            | (Some(ValueType::Dynamic), Some(ValueType::Dynamic)) => {}
                            _ => {
                                self.emit_error(
                                    *span,
                                    SemanticError::TypeMismatch,
                                    vec![Label {
                                        span: *span,
                                        message: ArenaCow::Borrowed("Dis expression no be number"),
                                    }],
                                );
                            }
                        }
                    }
                    BinaryOp::Eq | BinaryOp::Gt | BinaryOp::Lt => match (l, r) {
                        (Some(ValueType::Number), Some(ValueType::Number))
                        | (Some(ValueType::String), Some(ValueType::String))
                        | (Some(ValueType::Bool), Some(ValueType::Bool))
                        | (Some(ValueType::Dynamic), ..)
                        | (.., Some(ValueType::Dynamic)) => {}
                        _ => self.emit_error(
                            *span,
                            SemanticError::TypeMismatch,
                            vec![Label {
                                span: *span,
                                message: ArenaCow::Borrowed(
                                    "Dis expression no be number, string, or boolean",
                                ),
                            }],
                        ),
                    },
                    BinaryOp::And | BinaryOp::Or => match (l, r) {
                        (Some(ValueType::Bool), Some(ValueType::Bool))
                        | (Some(ValueType::Dynamic), ..)
                        | (.., Some(ValueType::Dynamic)) => {}
                        _ => {
                            self.emit_error(
                                *span,
                                SemanticError::TypeMismatch,
                                vec![Label {
                                    span: *span,
                                    message: ArenaCow::Borrowed("Dis expression no be boolean"),
                                }],
                            );
                        }
                    },
                }
            }
            Expr::Unary { op, expr, span } => {
                self.check_expr(expr);
                let t = self.infer_expr_type(expr);
                match op {
                    // Unary not requires a boolean operand or dynamic type
                    UnaryOp::Not => {
                        if t != Some(ValueType::Bool) && t != Some(ValueType::Dynamic) {
                            self.emit_error(
                                *span,
                                SemanticError::TypeMismatch,
                                vec![Label {
                                    span: *span,
                                    message: ArenaCow::Borrowed("Dis expression no be boolean"),
                                }],
                            )
                        }
                    }
                    // Unary minus requires a numeric operand or dynamic type
                    UnaryOp::Minus => {
                        if t != Some(ValueType::Number) && t != Some(ValueType::Dynamic) {
                            self.emit_error(
                                *span,
                                SemanticError::TypeMismatch,
                                vec![Label {
                                    span: *span,
                                    message: ArenaCow::Borrowed("Dis expression no be number"),
                                }],
                            );
                        }
                    }
                }
            }
            Expr::Call { callee, args, span } => {
                // Check the callee expression
                match callee {
                    Expr::Var(func_name, ..) => {
                        // Check if this is a built-in function first
                        if let Some(builtin) = Builtin::from_name(func_name) {
                            // Validate arity for built-in functions
                            if args.args.len() != builtin.arity() {
                                self.emit_error(
                                    *span,
                                    SemanticError::FunctionCallArity,
                                    vec![Label {
                                        span: *span,
                                        message: ArenaCow::Owned(arena_format!(
                                            self.arena,
                                            "Function `{}` dey expect {} arguments but you pass {}",
                                            func_name,
                                            builtin.arity(),
                                            args.args.len()
                                        )),
                                    }],
                                );
                            }
                        } else if let Some(func_sig) = self.lookup_func(func_name) {
                            // Check parameter count for user-defined function
                            if args.args.len() != func_sig.param_names.len() {
                                self.emit_error(
                                    *span,
                                    SemanticError::FunctionCallArity,
                                    vec![Label {
                                        span: *span,
                                        message: ArenaCow::Owned(arena_format!(
                                            self.arena,
                                            "Function `{}` dey expect {} arguments but you pass {}",
                                            func_name,
                                            func_sig.param_names.len(),
                                            args.args.len()
                                        )),
                                    }],
                                );
                            }
                        } else {
                            self.emit_error(
                                *span,
                                SemanticError::UndeclaredIdentifier,
                                vec![Label {
                                    span: *span,
                                    message: ArenaCow::Owned(arena_format!(
                                        self.arena,
                                        "Function `{func_name}` no dey scope"
                                    )),
                                }],
                            );
                        }
                    }
                    _ => self.check_expr(callee),
                }

                // Check all arguments
                for &arg in args.args {
                    self.check_expr(arg);
                }
            }
        }
    }

    #[inline]
    fn infer_expr_type(&self, expr: ExprRef<'ast>) -> Option<ValueType> {
        match expr {
            Expr::Number(..) => Some(ValueType::Number),
            Expr::String { .. } => Some(ValueType::String),
            Expr::Bool(..) => Some(ValueType::Bool),
            Expr::Array { .. } => Some(ValueType::Array),
            Expr::Index { .. } => Some(ValueType::Dynamic),
            Expr::Var(v, ..) => {
                for scope in self.variable_scopes.iter().rev() {
                    if let Some((_, t, ..)) = scope.iter().rev().find(|(name, ..)| *name == *v) {
                        return Some(*t);
                    }
                }
                None
            }
            Expr::Binary { op, lhs, rhs, .. } => {
                let l = self.infer_expr_type(lhs)?;
                let r = self.infer_expr_type(rhs)?;
                match op {
                    BinaryOp::Add => match (l, r) {
                        (ValueType::String, ..) | (.., ValueType::String) => {
                            Some(ValueType::String)
                        }
                        (ValueType::Number, ValueType::Number) => Some(ValueType::Number),
                        (ValueType::Dynamic, ..) | (.., ValueType::Dynamic) => {
                            if l == ValueType::Number || r == ValueType::Number {
                                Some(ValueType::Number)
                            } else {
                                Some(ValueType::String)
                            }
                        }
                        _ => None,
                    },
                    BinaryOp::Minus | BinaryOp::Times | BinaryOp::Divide | BinaryOp::Mod => {
                        match (l, r) {
                            (ValueType::Number, ValueType::Number)
                            | (ValueType::Dynamic, ..)
                            | (.., ValueType::Dynamic) => Some(ValueType::Number),
                            _ => None,
                        }
                    }
                    BinaryOp::Eq | BinaryOp::Gt | BinaryOp::Lt => match (l, r) {
                        (ValueType::Number, ValueType::Number)
                        | (ValueType::String, ValueType::String)
                        | (ValueType::Bool, ValueType::Bool)
                        | (ValueType::Dynamic, ..)
                        | (.., ValueType::Dynamic) => Some(ValueType::Bool),
                        _ => None,
                    },
                    BinaryOp::And | BinaryOp::Or => match (l, r) {
                        (ValueType::Bool, ValueType::Bool)
                        | (ValueType::Dynamic, ..)
                        | (.., ValueType::Dynamic) => Some(ValueType::Bool),
                        _ => None,
                    },
                }
            }
            Expr::Unary { op, expr, .. } => {
                let t = self.infer_expr_type(expr)?;
                match op {
                    UnaryOp::Not => {
                        if t == ValueType::Bool {
                            Some(ValueType::Bool)
                        } else {
                            None
                        }
                    }
                    UnaryOp::Minus => {
                        if t == ValueType::Number {
                            Some(ValueType::Number)
                        } else {
                            None
                        }
                    }
                }
            }
            Expr::Call { callee, .. } => match callee {
                Expr::Var(func_name, ..) => {
                    if let Some(builtin) = Builtin::from_name(func_name) {
                        Some(ValueType::from(builtin.return_type()))
                    } else if self.lookup_func(func_name).is_some() {
                        Some(ValueType::Dynamic)
                    } else {
                        None
                    }
                }
                _ => None,
            },
        }
    }
}
