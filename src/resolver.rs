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
    InvalidStringOperation,
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
            SemanticError::InvalidStringOperation => "Invalid string operation",
            SemanticError::UndeclaredIdentifier => "Undeclared identifier",
            SemanticError::FunctionCallArity => "Invalid parameter count",
            SemanticError::UnreachableCode => "Unreachable code",
            SemanticError::ReservedKeyword => "Use of reserved keyword",
        }
    }
}

// Represents the variable types in NaijaScript
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
enum VarType {
    Number,
    String,
    Bool,
    Dynamic,
}

impl From<BuiltinReturnType> for VarType {
    fn from(builtin_type: BuiltinReturnType) -> Self {
        match builtin_type {
            BuiltinReturnType::Number => VarType::Number,
            BuiltinReturnType::String => VarType::String,
            BuiltinReturnType::Bool => VarType::Bool,
        }
    }
}

// Represents the blueprint of a function signature in NaijaScript
#[derive(Debug)]
struct FunctionSig<'ast> {
    name: &'ast str,
    name_span: &'ast Span,
    param_names: &'ast [&'ast str],
    has_return: bool,
    return_type: Option<VarType>,
}

/// An arena-backed semantic analyzer.
///
/// FWIW, this is a work in progress and may change as the language evolves.
pub struct Resolver<'ast> {
    // Stack of variable symbol tables for block-scoped variables
    variable_scopes: Vec<Vec<(&'ast str, VarType, &'ast Span), &'ast Arena>, &'ast Arena>,

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
    #[inline]
    pub fn new(arena: &'ast Arena) -> Self {
        Resolver {
            variable_scopes: Vec::new_in(arena),
            function_scopes: Vec::new_in(arena),
            current_function: None,
            errors: Diagnostics::new(arena),
            arena,
        }
    }

    /// Resolves the given AST root node.
    #[inline]
    pub fn resolve(&mut self, root: BlockRef<'ast>) {
        // Enter the first (global) scope
        self.variable_scopes.push(Vec::new_in(self.arena));
        self.function_scopes.push(Vec::new_in(self.arena));
        self.check_block(root);
        // Leave the global scope
        self.variable_scopes.pop();
        self.function_scopes.pop();
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
                    Stmt::If { span, .. } => span,
                    Stmt::Loop { span, .. } => span,
                    Stmt::Block { span, .. } => span,
                    Stmt::FunctionDef { span, .. } => span,
                    Stmt::Return { span, .. } => span,
                    Stmt::Expression { span, .. } => span,
                };

                self.errors.emit(
                    *span,
                    Severity::Warning,
                    "semantic",
                    SemanticError::UnreachableCode.as_str(),
                    vec![Label {
                        span: *span,
                        message: ArenaCow::Borrowed("Unreachable code after return statement"),
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

    #[inline]
    fn check_stmt(&mut self, stmt: StmtRef<'ast>) {
        match stmt {
            // Handle "make variable get expression" statements
            Stmt::Assign { var, expr, span } => {
                if Builtin::from_name(var).is_some() {
                    self.errors.emit(
                        *span,
                        Severity::Error,
                        "semantic",
                        SemanticError::ReservedKeyword.as_str(),
                        vec![Label {
                            span: *span,
                            message: ArenaCow::Owned(arena_format!(
                                &self.arena,
                                "`{var}` na reserved keyword",
                            )),
                        }],
                    );
                }
                // We only want to prevent redeclaring a variable in the same block,
                // so we check just the current (innermost) scope for duplicates
                if let Some((.., orig_span)) = self
                    .variable_scopes
                    .last()
                    .expect("scope stack should never be empty")
                    .iter()
                    .find(|(name, ..)| name == var)
                {
                    self.errors.emit(
                        *span,
                        Severity::Error,
                        "semantic",
                        SemanticError::DuplicateIdentifier.as_str(),
                        vec![
                            Label {
                                span: **orig_span,
                                message: ArenaCow::Owned(arena_format!(
                                    &self.arena,
                                    "You don already declare `{var}` for here",
                                )),
                            },
                            Label {
                                span: *span,
                                message: ArenaCow::Borrowed("You try declare am again for here"),
                            },
                        ],
                    );
                } else {
                    // Let's figure out the type of this variable from the expression,
                    // then add it to our symbol table so future code knows it's declared.
                    let var_type = self.infer_expr_type(expr).unwrap_or(VarType::Dynamic);
                    self.variable_scopes
                        .last_mut()
                        .expect("scope stack should never be empty")
                        .push((var, var_type, span));
                }
                // Always check the expression, even if variable was duplicate
                // This catches more errors in one pass
                self.check_expr(expr);
            }
            // Handle variable reassignment: <variable> get <expression>
            Stmt::AssignExisting { var, expr, span } => {
                if !self.lookup_var(var) {
                    self.errors.emit(
                        *span,
                        Severity::Error,
                        "semantic",
                        SemanticError::AssignmentToUndeclared.as_str(),
                        vec![Label {
                            span: *span,
                            message: ArenaCow::Borrowed("Dis variable no dey scope"),
                        }],
                    );
                }
                self.check_expr(expr);
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
                    self.errors.emit(
                        *name_span,
                        Severity::Error,
                        "semantic",
                        SemanticError::ReservedKeyword.as_str(),
                        vec![Label {
                            span: *name_span,
                            message: ArenaCow::Owned(arena_format!(
                                &self.arena,
                                "`{name}` dey reserved, you no fit use am as function name",
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

    #[inline]
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
            self.errors.emit(
                *name_span,
                Severity::Error,
                "semantic",
                SemanticError::DuplicateIdentifier.as_str(),
                vec![
                    Label {
                        span: *existing_func.name_span,
                        message: ArenaCow::Owned(arena_format!(
                            &self.arena,
                            "You don already define `{name}` for here"
                        )),
                    },
                    Label {
                        span: *name_span,
                        message: ArenaCow::Borrowed("You try define am again for here"),
                    },
                ],
            );
            return;
        }

        // Check for duplicate parameter names
        let mut set = HashSet::with_capacity(params.params.len());
        for (param_name, param_span) in params.params.iter().zip(params.param_spans.iter()) {
            if Builtin::from_name(param_name).is_some() {
                self.errors.emit(
                    *param_span,
                    Severity::Error,
                    "semantic",
                    SemanticError::ReservedKeyword.as_str(),
                    vec![Label {
                        span: *param_span,
                        message: ArenaCow::Owned(arena_format!(
                            &self.arena,
                            "`{param_name}` dey reserved, you no fit use am as parameter name",
                        )),
                    }],
                );
            }
            if !set.insert(param_name) {
                self.errors.emit(
                    *param_span,
                    Severity::Error,
                    "semantic",
                    SemanticError::DuplicateIdentifier.as_str(),
                    vec![Label {
                        span: *param_span,
                        message: ArenaCow::Owned(arena_format!(
                            &self.arena,
                            "Dis parameter `{param_name}` na duplicate"
                        )),
                    }],
                );
            }
        }

        // Add function to current function scope
        self.function_scopes.last_mut().expect("function scope stack should never be empty").push(
            FunctionSig {
                name,
                name_span,
                param_names: params.params,
                has_return: false,
                return_type: None,
            },
        );

        // Set current function context for return validation
        let prev_function = self.current_function;
        self.current_function = Some(name);

        // Create new scope for function parameters
        self.variable_scopes.push(Vec::new_in(self.arena));

        // Add parameters as variables with dynamic typing
        // Parameters can accept any type and their usage will be validated at runtime
        for param_name in params.params {
            self.variable_scopes.last_mut().unwrap().push((param_name, VarType::Dynamic, span));
        }

        // We collect return types and spans from the function body, skipping nested functions.
        let mut return_types = Vec::new_in(self.arena);
        self.collect_return_types(body, &mut return_types);

        let unique_types: HashSet<VarType> = return_types.iter().map(|(t, ..)| *t).collect();
        if unique_types.len() > 1 && !unique_types.contains(&VarType::Dynamic) {
            for (return_type, return_span) in &return_types {
                if *return_type != VarType::Dynamic {
                    self.errors.emit(
                        **return_span,
                        Severity::Warning,
                        "semantic",
                        SemanticError::TypeMismatch.as_str(),
                        vec![Label {
                            span: **return_span,
                            message: ArenaCow::Owned(arena_format!(
                                &self.arena,
                                "Function `{name}` return types no match",
                            )),
                        }],
                    );
                }
            }
        }

        // We want to set function return type to Dynamic if inconsistent, else to the single type
        let func_scope = self.function_scopes.last_mut().unwrap();
        if let Some(func_sig) = func_scope.iter_mut().find(|f| f.name == name) {
            func_sig.has_return = !return_types.is_empty();
            func_sig.return_type = if unique_types.len() == 1 {
                Some(*unique_types.iter().next().unwrap())
            } else {
                Some(VarType::Dynamic)
            };
        }

        // Check function body using check_block to get dead code detection
        // Note: check_block will create its own scope, but that's fine for function bodies
        self.check_block(body);

        // Clean up parameter scope
        self.variable_scopes.pop();

        // Restore previous function context
        self.current_function = prev_function;
    }

    // FWIW, this function recursively collects return types and spans from a block
    // which [`check_return_stmt`] uses to validate return statements.
    #[inline]
    fn collect_return_types(
        &self,
        block: BlockRef<'ast>,
        types: &mut Vec<(VarType, &'ast Span), &'ast Arena>,
    ) {
        for &stmt in block.stmts {
            match stmt {
                Stmt::Return { expr, span } => {
                    let var_type = if let Some(expr) = expr {
                        self.infer_expr_type(expr).unwrap_or(VarType::Dynamic)
                    } else {
                        VarType::Dynamic
                    };
                    types.push((var_type, span));
                }
                Stmt::If { then_b, else_b, .. } => {
                    self.collect_return_types(then_b, types);
                    if let Some(else_b) = else_b {
                        self.collect_return_types(else_b, types);
                    }
                }
                Stmt::Loop { body, .. } => {
                    self.collect_return_types(body, types);
                }
                Stmt::Block { block, .. } => {
                    self.collect_return_types(block, types);
                }
                Stmt::FunctionDef { .. } => {}
                _ => {}
            }
        }
    }

    #[inline]
    fn check_return_stmt(&mut self, expr: Option<ExprRef<'ast>>, span: &'ast Span) {
        // Check if we're inside a function
        if self.current_function.is_none() {
            self.errors.emit(
                *span,
                Severity::Error,
                "semantic",
                SemanticError::UnreachableCode.as_str(),
                vec![Label {
                    span: *span,
                    message: ArenaCow::Borrowed("You no fit use `return` outside function"),
                }],
            );
        } else {
            let return_type = if let Some(expr_id) = expr {
                self.infer_expr_type(expr_id)
            } else {
                Some(VarType::Dynamic)
            };

            if let Some(current_func_name) = self.current_function {
                // Find the function signature in the innermost function scope for mutation
                if let Some(scope) = self.function_scopes.last_mut()
                    && let Some(func_sig) = scope.iter_mut().find(|f| f.name == current_func_name)
                {
                    func_sig.has_return = true;

                    // Update function return type if we don't have one yet
                    if func_sig.return_type.is_none() {
                        func_sig.return_type = return_type;
                    } else if let (Some(existing_type), Some(new_type)) =
                        (func_sig.return_type, return_type)
                    {
                        // Check if return types are consistent
                        if existing_type != new_type
                            && existing_type != VarType::Dynamic
                            && new_type != VarType::Dynamic
                        {
                            self.errors.emit(
                                *span,
                                Severity::Warning,
                                "semantic",
                                SemanticError::TypeMismatch.as_str(),
                                vec![Label {
                                    span: *span,
                                    message: ArenaCow::Borrowed(
                                        "Return types for dis function no match",
                                    ),
                                }],
                            );
                            func_sig.return_type = Some(VarType::Dynamic);
                        }
                    }
                }
            }
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
            && !matches!(t, VarType::Bool | VarType::Dynamic)
        {
            let span = match expr {
                Expr::Number(.., span) => *span,
                Expr::String { span, .. } => *span,
                Expr::Bool(.., span) => *span,
                Expr::Var(.., span) => *span,
                Expr::Binary { span, .. } => *span,
                Expr::Unary { span, .. } => *span,
                Expr::Call { span, .. } => *span,
            };
            self.errors.emit(
                span,
                Severity::Error,
                "semantic",
                SemanticError::TypeMismatch.as_str(),
                vec![Label {
                    span,
                    message: ArenaCow::Borrowed("Condition for here suppose be true or false"),
                }],
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
                            self.errors.emit(
                                *span,
                                Severity::Error,
                                "semantic",
                                SemanticError::UndeclaredIdentifier.as_str(),
                                vec![Label {
                                    span: *span,
                                    message: ArenaCow::Owned(arena_format!(
                                        &self.arena,
                                        "Variable `{var}` no dey scope"
                                    )),
                                }],
                            );
                        }
                    }
                }
            }
            // Variables need to exist in our symbol table before we can use them
            Expr::Var(v, span) => {
                if !self.lookup_var(v) {
                    self.errors.emit(
                        *span,
                        Severity::Error,
                        "semantic",
                        SemanticError::UndeclaredIdentifier.as_str(),
                        vec![Label {
                            span: *span,
                            message: ArenaCow::Borrowed("Dis variable no dey scope"),
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
                        (Some(VarType::Bool), ..) | (.., Some(VarType::Bool)) => {
                            self.errors.emit(
                                *span,
                                Severity::Error,
                                "semantic",
                                SemanticError::TypeMismatch.as_str(),
                                vec![Label {
                                    span: *span,
                                    message: ArenaCow::Borrowed("You no fit add boolean values"),
                                }],
                            );
                        }
                        (Some(VarType::String), ..)
                        | (.., Some(VarType::String))
                        | (Some(VarType::Dynamic), ..)
                        | (.., Some(VarType::Dynamic))
                        | (Some(VarType::Number), Some(VarType::Number)) => {}
                        _ => {}
                    },
                    BinaryOp::Minus | BinaryOp::Times | BinaryOp::Divide | BinaryOp::Mod => {
                        match (l, r) {
                            (Some(VarType::Number), Some(VarType::Number)) => {}
                            (Some(VarType::Dynamic), Some(VarType::Number))
                            | (Some(VarType::Number), Some(VarType::Dynamic))
                            | (Some(VarType::Dynamic), Some(VarType::Dynamic)) => {}
                            (Some(VarType::Dynamic), Some(VarType::String))
                            | (Some(VarType::String), Some(VarType::Dynamic))
                            | (Some(VarType::Dynamic), Some(VarType::Bool))
                            | (Some(VarType::Bool), Some(VarType::Dynamic)) => {
                                self.errors.emit(
                                    *span,
                                    Severity::Error,
                                    "semantic",
                                    SemanticError::TypeMismatch.as_str(),
                                    vec![Label {
                                        span: *span,
                                        message: ArenaCow::Borrowed(
                                            "You fit only use arithmetic operators with numbers",
                                        ),
                                    }],
                                );
                            }
                            (Some(VarType::String), Some(VarType::String)) => {
                                self.errors.emit(
                                    *span,
                                    Severity::Error,
                                    "semantic",
                                    SemanticError::InvalidStringOperation.as_str(),
                                    vec![Label {
                                        span: *span,
                                        message: ArenaCow::Borrowed(
                                            "You no fit do arithmetic with strings",
                                        ),
                                    }],
                                );
                            }
                            (Some(VarType::String), Some(VarType::Number))
                            | (Some(VarType::Number), Some(VarType::String)) => {
                                self.errors.emit(
                                    *span,
                                    Severity::Error,
                                    "semantic",
                                    SemanticError::TypeMismatch.as_str(),
                                    vec![Label {
                                        span: *span,
                                        message: ArenaCow::Borrowed(
                                            "You no fit do arithmetic with string and number",
                                        ),
                                    }],
                                );
                            }
                            (Some(VarType::Bool), ..) | (.., Some(VarType::Bool)) => {
                                self.errors.emit(
                                    *span,
                                    Severity::Error,
                                    "semantic",
                                    SemanticError::TypeMismatch.as_str(),
                                    vec![Label {
                                        span: *span,
                                        message: ArenaCow::Borrowed(
                                            "You no fit do arithmetic with boolean values",
                                        ),
                                    }],
                                );
                            }
                            _ => {}
                        }
                    }
                    BinaryOp::Eq | BinaryOp::Gt | BinaryOp::Lt => match (l, r) {
                        (Some(VarType::Number), Some(VarType::Number)) => {}
                        (Some(VarType::String), Some(VarType::String)) => {}
                        (Some(VarType::Bool), Some(VarType::Bool)) => {}
                        (Some(VarType::Dynamic), ..) | (.., Some(VarType::Dynamic)) => {}
                        _ => {
                            self.errors.emit(
                                *span,
                                Severity::Error,
                                "semantic",
                                SemanticError::TypeMismatch.as_str(),
                                vec![Label {
                                    span: *span,
                                    message: ArenaCow::Borrowed(
                                        "You fit only compare numbers, strings, or booleans",
                                    ),
                                }],
                            );
                        }
                    },
                    BinaryOp::And | BinaryOp::Or => match (l, r) {
                        (Some(VarType::Bool), Some(VarType::Bool)) => {}
                        (Some(VarType::Dynamic), ..) | (.., Some(VarType::Dynamic)) => {}
                        _ => {
                            self.errors.emit(
                                *span,
                                Severity::Error,
                                "semantic",
                                SemanticError::TypeMismatch.as_str(),
                                vec![Label {
                                    span: *span,
                                    message: ArenaCow::Borrowed(
                                        "Logical operators fit only work with boolean values",
                                    ),
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
                        if t != Some(VarType::Bool) && t != Some(VarType::Dynamic) {
                            self.errors.emit(
                                *span,
                                Severity::Error,
                                "semantic",
                                SemanticError::TypeMismatch.as_str(),
                                vec![Label {
                                    span: *span,
                                    message: ArenaCow::Borrowed(
                                        "You fit only use `not` with boolean",
                                    ),
                                }],
                            );
                        }
                    }
                    // Unary minus requires a numeric operand or dynamic type
                    UnaryOp::Minus => {
                        if t != Some(VarType::Number) && t != Some(VarType::Dynamic) {
                            self.errors.emit(
                                *span,
                                Severity::Error,
                                "semantic",
                                SemanticError::TypeMismatch.as_str(),
                                vec![Label {
                                    span: *span,
                                    message: ArenaCow::Borrowed(
                                        "You fit only use `minus` with number",
                                    ),
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
                                self.errors.emit(
                                    *span,
                                    Severity::Error,
                                    "semantic",
                                    SemanticError::FunctionCallArity.as_str(),
                                    vec![Label {
                                        span: *span,
                                        message: ArenaCow::Owned(arena_format!(
                                            &self.arena,
                                            "Function `{}` expect {} arguments but you pass {}",
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
                                self.errors.emit(
                                    *span,
                                    Severity::Error,
                                    "semantic",
                                    SemanticError::FunctionCallArity.as_str(),
                                    vec![Label {
                                        span: *span,
                                        message: ArenaCow::Owned(arena_format!(
                                            &self.arena,
                                            "Function `{}` expect {} arguments but you pass {}",
                                            func_name,
                                            func_sig.param_names.len(),
                                            args.args.len()
                                        )),
                                    }],
                                );
                            }
                        } else {
                            self.errors.emit(
                                *span,
                                Severity::Error,
                                "semantic",
                                SemanticError::UndeclaredIdentifier.as_str(),
                                vec![Label {
                                    span: *span,
                                    message: ArenaCow::Borrowed("Dis function no dey scope"),
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
    fn infer_expr_type(&self, expr: ExprRef<'ast>) -> Option<VarType> {
        match expr {
            Expr::Number(..) => Some(VarType::Number),
            Expr::String { .. } => Some(VarType::String),
            Expr::Bool(..) => Some(VarType::Bool),
            Expr::Var(v, ..) => {
                for scope in self.variable_scopes.iter().rev() {
                    if let Some((_, t, ..)) = scope.iter().find(|(name, ..)| *name == *v) {
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
                        (VarType::Bool, ..) | (.., VarType::Bool) => None,
                        (VarType::String, ..) | (.., VarType::String) => Some(VarType::String),
                        (VarType::Dynamic, ..) | (.., VarType::Dynamic) => {
                            if l == VarType::Number || r == VarType::Number {
                                Some(VarType::Number)
                            } else {
                                Some(VarType::String)
                            }
                        }
                        (VarType::Number, VarType::Number) => Some(VarType::Number),
                    },
                    BinaryOp::Minus | BinaryOp::Times | BinaryOp::Divide | BinaryOp::Mod => {
                        match (l, r) {
                            (VarType::Number, VarType::Number) => Some(VarType::Number),
                            (VarType::Dynamic, ..) | (.., VarType::Dynamic) => {
                                Some(VarType::Number)
                            }
                            _ => None,
                        }
                    }
                    BinaryOp::Eq | BinaryOp::Gt | BinaryOp::Lt => match (l, r) {
                        (VarType::Number, VarType::Number) => Some(VarType::Bool),
                        (VarType::String, VarType::String) => Some(VarType::Bool),
                        (VarType::Bool, VarType::Bool) => Some(VarType::Bool),
                        (VarType::Dynamic, ..) | (.., VarType::Dynamic) => Some(VarType::Bool),
                        _ => None,
                    },
                    BinaryOp::And | BinaryOp::Or => match (l, r) {
                        (VarType::Bool, VarType::Bool) => Some(VarType::Bool),
                        (VarType::Dynamic, ..) | (.., VarType::Dynamic) => Some(VarType::Bool),
                        _ => None,
                    },
                }
            }
            Expr::Unary { op, expr, .. } => {
                let t = self.infer_expr_type(expr)?;
                match op {
                    UnaryOp::Not => {
                        if t == VarType::Bool {
                            Some(VarType::Bool)
                        } else {
                            None
                        }
                    }
                    UnaryOp::Minus => {
                        if t == VarType::Number {
                            Some(VarType::Number)
                        } else {
                            None
                        }
                    }
                }
            }
            Expr::Call { callee, .. } => match callee {
                Expr::Var(func_name, ..) => {
                    if let Some(builtin) = Builtin::from_name(func_name) {
                        Some(VarType::from(builtin.return_type()))
                    } else if let Some(func_sig) = self.lookup_func(func_name) {
                        func_sig.return_type.or(Some(VarType::Dynamic))
                    } else {
                        None
                    }
                }
                _ => None,
            },
        }
    }
}
