//! The semantic analyzer (or resolver) for NaijaScript.

use std::borrow::Cow;
use std::collections::HashSet;

use crate::builtins::{Builtin, BuiltinReturnType};
use crate::diagnostics::{AsStr, Diagnostics, Label, Severity, Span};
use crate::syntax::parser::{
    self, Arena, ArgList, BinOp, Block, BlockId, Expr, ExprId, ParamList, ParamListId, Stmt, StmtId,
};

/// Represents the type of semantic errors that can occur
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SemanticError {
    DuplicateIdentifier,
    AssignmentToUndeclared,
    TypeMismatch,
    InvalidStringOperation,
    UndeclaredIdentifier,
    FunctionCallArity,
    ReturnOutsideFunction,
    DeadCodeAfterReturn,
    ReservedKeywordAsIdentifier,
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
            SemanticError::ReturnOutsideFunction => "Return statement outside function",
            SemanticError::DeadCodeAfterReturn => "Dead code after return statement",
            SemanticError::ReservedKeywordAsIdentifier => "Use of reserved keyword as identifier",
        }
    }
}

/// Represents the type of a variable in NaijaScript
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
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

// Represents a function signature for semantic analysis
#[derive(Debug)]
struct FunctionSig<'src> {
    name: &'src str,
    param_names: Vec<&'src str>,
    span: &'src Span,
    has_return: bool,
    return_type: Option<VarType>,
}

/// The interface for the NaijaScript semantic analyzer.
pub struct SemAnalyzer<'src> {
    // These are borrowed references to the parser's AST arenas
    // We don't own this data, just analyze what the parser built
    stmts: &'src Arena<Stmt<'src>>,
    exprs: &'src Arena<Expr<'src>>,
    blocks: &'src Arena<Block>,
    params: &'src Arena<ParamList<'src>>,
    args: &'src Arena<ArgList>,

    // Stack of variable symbol tables for block-scoped variables
    variable_scopes: Vec<Vec<(&'src str, VarType, &'src Span)>>,

    // Stack of function symbol tables for block-scoped functions
    function_scopes: Vec<Vec<FunctionSig<'src>>>,

    // Track current function context for return statement validation
    current_function: Option<&'src str>,

    /// Collection of semantic errors found during analysis
    pub errors: Diagnostics,
}

impl<'src> SemAnalyzer<'src> {
    /// Sets up a new analyzer with the AST arenas from parsing.
    /// We start with empty symbol table and error list - fresh slate for analysis.
    pub fn new(
        stmts: &'src Arena<Stmt<'src>>,
        exprs: &'src Arena<Expr<'src>>,
        blocks: &'src Arena<Block>,
        params: &'src Arena<ParamList<'src>>,
        args: &'src Arena<ArgList>,
    ) -> Self {
        SemAnalyzer {
            stmts,
            exprs,
            blocks,
            params,
            args,
            variable_scopes: Vec::new(),
            function_scopes: Vec::new(),
            current_function: None,
            errors: Diagnostics::default(),
        }
    }

    /// Start semantic checking from the root block (the whole program).
    /// Sets up the first scope, checks everything, and cleans up after.
    pub fn analyze(&mut self, root: BlockId) {
        // Enter the first (global) scope
        self.variable_scopes.push(Vec::new());
        self.function_scopes.push(Vec::new());
        self.check_block(root);
        // Leave the global scope
        self.variable_scopes.pop();
        self.function_scopes.pop();
    }

    // Check every statement inside a block.
    // Each "start ... end" creates a new scope for variables and functions.
    fn check_block(&mut self, bid: BlockId) {
        // Enter new block scope
        self.variable_scopes.push(Vec::new());
        self.function_scopes.push(Vec::new());
        let block = &self.blocks.nodes[bid.0];

        let mut has_return = false;
        for &sid in &block.stmts {
            // Check for dead code after return statement
            if has_return {
                let stmt = &self.stmts.nodes[sid.0];
                let span = match stmt {
                    parser::Stmt::Assign { span, .. } => span,
                    parser::Stmt::AssignExisting { span, .. } => span,
                    parser::Stmt::If { span, .. } => span,
                    parser::Stmt::Loop { span, .. } => span,
                    parser::Stmt::Block { span, .. } => span,
                    parser::Stmt::FunctionDef { span, .. } => span,
                    parser::Stmt::Return { span, .. } => span,
                    parser::Stmt::Expression { span, .. } => span,
                };

                self.errors.emit(
                    span.clone(),
                    Severity::Warning,
                    "semantic",
                    SemanticError::DeadCodeAfterReturn.as_str(),
                    vec![Label {
                        span: span.clone(),
                        message: Cow::Borrowed("Code after return statement dey unreachable"),
                    }],
                );
            }

            if matches!(&self.stmts.nodes[sid.0], parser::Stmt::Return { .. }) {
                has_return = true;
            }

            self.check_stmt(sid);
        }
        // Leave this block scope
        self.variable_scopes.pop();
        self.function_scopes.pop();
    }

    // The main semantic validation logic - handles each type of statement.
    // This is where we enforce the key rules of NaijaScript:
    // 1. No redeclaring variables (each "make" creates a new variable)
    // 2. Variables must be declared before use
    // 3. All expressions and conditions must be semantically valid
    fn check_stmt(&mut self, sid: StmtId) {
        match &self.stmts.nodes[sid.0] {
            // Handle "make variable get expression" statements
            Stmt::Assign { var, expr, span } => {
                if Builtin::from_name(var).is_some() {
                    self.errors.emit(
                        span.clone(),
                        Severity::Error,
                        "semantic",
                        SemanticError::ReservedKeywordAsIdentifier.as_str(),
                        vec![Label {
                            span: span.clone(),
                            message: Cow::Owned(format!(
                                "`{var}` dey reserved, you no fit use am as variable name",
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
                        span.clone(),
                        Severity::Error,
                        "semantic",
                        SemanticError::DuplicateIdentifier.as_str(),
                        vec![
                            Label {
                                span: (*orig_span).clone(),
                                message: Cow::Owned(format!(
                                    "You don already declare `{var}` for here",
                                )),
                            },
                            Label {
                                span: span.clone(),
                                message: Cow::Borrowed("You try declare am again for here"),
                            },
                        ],
                    );
                } else {
                    // Let's figure out the type of this variable from the expression,
                    // then add it to our symbol table so future code knows it's declared.
                    let typ = self.infer_expr_type(*expr).unwrap_or(VarType::Dynamic);
                    self.variable_scopes
                        .last_mut()
                        .expect("scope stack should never be empty")
                        .push((var, typ, span));
                }
                // Always check the expression, even if variable was duplicate
                // This catches more errors in one pass
                self.check_expr(*expr);
            }
            // Handle variable reassignment: <variable> get <expression>
            Stmt::AssignExisting { var, expr, span } => {
                if !self.lookup_var(var) {
                    self.errors.emit(
                        span.clone(),
                        Severity::Error,
                        "semantic",
                        SemanticError::AssignmentToUndeclared.as_str(),
                        vec![Label {
                            span: span.clone(),
                            message: Cow::Borrowed("You never declare dis variable before"),
                        }],
                    );
                }
                self.check_expr(*expr);
            }
            // Handle "if to say(condition) start...end" with optional "if not so"
            Stmt::If { cond, then_b, else_b, .. } => {
                self.check_expr(*cond);
                self.require_boolean_expr(*cond);
                self.check_block(*then_b);
                // Else block is optional in the grammar
                if let Some(eb) = else_b {
                    self.check_block(*eb);
                }
            }
            // Handle "jasi(condition) start...end" loop statements
            Stmt::Loop { cond, body, .. } => {
                self.check_expr(*cond);
                self.require_boolean_expr(*cond);
                self.check_block(*body);
            }
            // Handle nested blocks
            Stmt::Block { block, .. } => {
                self.check_block(*block);
            }
            Stmt::FunctionDef { name, params, body, span } => {
                if Builtin::from_name(name).is_some() {
                    self.errors.emit(
                        span.clone(),
                        Severity::Error,
                        "semantic",
                        SemanticError::ReservedKeywordAsIdentifier.as_str(),
                        vec![Label {
                            span: span.clone(),
                            message: Cow::Owned(format!(
                                "`{name}` dey reserved, you no fit use am as function name",
                            )),
                        }],
                    );
                }
                self.check_function_def(name, *params, *body, span);
            }
            Stmt::Return { expr, span } => {
                self.check_return_stmt(*expr, span);
            }
            Stmt::Expression { expr, .. } => {
                self.check_expr(*expr);
            }
        }
    }

    // Check if a variable has already been declared in any scope,
    // starting from the innermost and moving outward
    #[inline]
    fn lookup_var(&self, var: &&'src str) -> bool {
        self.variable_scopes.iter().rev().any(|scope| scope.iter().any(|(name, ..)| name == var))
    }

    // Find function signature by name
    #[inline]
    fn find_function(&self, func_name: &str) -> Option<&FunctionSig<'src>> {
        for scope in self.function_scopes.iter().rev() {
            if let Some(sig) = scope.iter().find(|sig| sig.name == func_name) {
                return Some(sig);
            }
        }
        None
    }

    // Validates function definition
    fn check_function_def(
        &mut self,
        name: &'src str,
        params: ParamListId,
        body: BlockId,
        span: &'src Span,
    ) {
        // Check for duplicate function declaration in current scope
        if let Some(current_scope) = self.function_scopes.last()
            && let Some(existing_func) = current_scope.iter().find(|sig| sig.name == name)
        {
            self.errors.emit(
                span.clone(),
                Severity::Error,
                "semantic",
                SemanticError::DuplicateIdentifier.as_str(),
                vec![
                    Label {
                        span: existing_func.span.clone(),
                        message: Cow::Owned(format!("You don already define `{name}` for here",)),
                    },
                    Label {
                        span: span.clone(),
                        message: Cow::Borrowed("You try define am again for here"),
                    },
                ],
            );
            return;
        }

        let param_list = &self.params.nodes[params.0];

        // Check for duplicate parameter names
        let mut seen_params = HashSet::new();
        for param_name in &param_list.params {
            if Builtin::from_name(param_name).is_some() {
                self.errors.emit(
                    span.clone(),
                    Severity::Error,
                    "semantic",
                    SemanticError::ReservedKeywordAsIdentifier.as_str(),
                    vec![Label {
                        span: span.clone(),
                        message: Cow::Owned(format!(
                            "`{param_name}` dey reserved, you no fit use am as parameter name",
                        )),
                    }],
                );
            }
            if !seen_params.insert(param_name) {
                self.errors.emit(
                    span.clone(),
                    Severity::Error,
                    "semantic",
                    SemanticError::DuplicateIdentifier.as_str(),
                    vec![Label {
                        span: span.clone(),
                        message: Cow::Owned(format!(
                            "Parameter `{param_name}` dey appear more than once for dis function"
                        )),
                    }],
                );
            }
        }

        // Store parameter names in function signature
        let param_names = param_list.params.clone();

        // Add function to current function scope
        self.function_scopes
            .last_mut()
            .expect("function scope stack should never be empty")
            .push(FunctionSig { name, param_names, span, has_return: false, return_type: None });

        // Set current function context for return validation
        let prev_function = self.current_function;
        self.current_function = Some(name);

        // Create new scope for function parameters
        self.variable_scopes.push(Vec::new());

        // Add parameters as variables with dynamic typing
        // Parameters can accept any type and their usage will be validated at runtime
        for param_name in &param_list.params {
            self.variable_scopes.last_mut().unwrap().push((param_name, VarType::Dynamic, span));
        }

        // Check function body using check_block to get dead code detection
        // Note: check_block will create its own scope, but that's fine for function bodies
        self.check_block(body);

        // Clean up parameter scope
        self.variable_scopes.pop();

        // Restore previous function context
        self.current_function = prev_function;
    }

    // Validates return statements
    fn check_return_stmt(&mut self, expr: Option<ExprId>, span: &'src Span) {
        // Check if we're inside a function
        if self.current_function.is_none() {
            self.errors.emit(
                span.clone(),
                Severity::Error,
                "semantic",
                SemanticError::ReturnOutsideFunction.as_str(),
                vec![Label {
                    span: span.clone(),
                    message: Cow::Borrowed("You fit only use `return` inside function"),
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
                                span.clone(),
                                Severity::Warning,
                                "semantic",
                                SemanticError::TypeMismatch.as_str(),
                                vec![Label {
                                    span: span.clone(),
                                    message: Cow::Borrowed(
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

    // Checks that an expression used in a condition evaluates to a boolean type.
    fn require_boolean_expr(&mut self, eid: ExprId) {
        let expr_type = self.infer_expr_type(eid);
        if let Some(t) = expr_type
            && !matches!(t, VarType::Bool | VarType::Dynamic)
        {
            let span = match &self.exprs.nodes[eid.0] {
                Expr::Number(.., span) => span.clone(),
                Expr::String(.., span) => span.clone(),
                Expr::Bool(.., span) => span.clone(),
                Expr::Var(.., span) => span.clone(),
                Expr::Binary { span, .. } => span.clone(),
                Expr::Not { span, .. } => span.clone(),
                Expr::Call { span, .. } => span.clone(),
            };
            self.errors.emit(
                span.clone(),
                Severity::Error,
                "semantic",
                SemanticError::TypeMismatch.as_str(),
                vec![Label {
                    span,
                    message: Cow::Borrowed("Condition for here suppose be true or false"),
                }],
            );
        }
    }

    // Recursively validates expressions - the core of our semantic checking.
    // This is where we catch the most common programming error: using variables
    // before declaring them. Numbers are always valid, but variables need to
    // exist in our symbol table.
    fn check_expr(&mut self, eid: ExprId) {
        match &self.exprs.nodes[eid.0] {
            // Literals are always valid since they represent concrete values
            Expr::Number(..) | Expr::String(..) | Expr::Bool(..) => {}
            // Variables need to exist in our symbol table before we can use them
            Expr::Var(v, span) => {
                if !self.lookup_var(v) {
                    self.errors.emit(
                        span.clone(),
                        Severity::Error,
                        "semantic",
                        SemanticError::UndeclaredIdentifier.as_str(),
                        vec![Label {
                            span: span.clone(),
                            message: Cow::Borrowed("You never declare dis variable before"),
                        }],
                    );
                }
            }
            // Binary operations need type compatibility between operands
            Expr::Binary { op, lhs, rhs, span } => {
                self.check_expr(*lhs);
                self.check_expr(*rhs);
                let l = self.infer_expr_type(*lhs);
                let r = self.infer_expr_type(*rhs);
                match op {
                    BinOp::Add => match (l, r) {
                        (Some(VarType::Number), Some(VarType::Number))
                        | (Some(VarType::String), Some(VarType::String)) => {}
                        (Some(VarType::Dynamic), ..) | (.., Some(VarType::Dynamic)) => {}
                        (Some(VarType::String), Some(VarType::Number))
                        | (Some(VarType::Number), Some(VarType::String)) => {
                            self.errors.emit(
                                span.clone(),
                                Severity::Error,
                                "semantic",
                                SemanticError::TypeMismatch.as_str(),
                                vec![Label {
                                    span: span.clone(),
                                    message: Cow::Borrowed(
                                        "You no fit add string and number together",
                                    ),
                                }],
                            );
                        }
                        (Some(VarType::Bool), ..) | (.., Some(VarType::Bool)) => {
                            self.errors.emit(
                                span.clone(),
                                Severity::Error,
                                "semantic",
                                SemanticError::TypeMismatch.as_str(),
                                vec![Label {
                                    span: span.clone(),
                                    message: Cow::Borrowed(
                                        "You no fit add boolean values together",
                                    ),
                                }],
                            );
                        }
                        _ => {}
                    },
                    BinOp::Minus | BinOp::Times | BinOp::Divide | BinOp::Mod => match (l, r) {
                        (Some(VarType::Number), Some(VarType::Number)) => {}
                        (Some(VarType::Dynamic), ..) | (.., Some(VarType::Dynamic)) => {}
                        (Some(VarType::String), Some(VarType::String)) => {
                            self.errors.emit(
                                span.clone(),
                                Severity::Error,
                                "semantic",
                                SemanticError::InvalidStringOperation.as_str(),
                                vec![Label {
                                    span: span.clone(),
                                    message: Cow::Borrowed("You no fit do arithmetic with strings"),
                                }],
                            );
                        }
                        (Some(VarType::String), Some(VarType::Number))
                        | (Some(VarType::Number), Some(VarType::String)) => {
                            self.errors.emit(
                                span.clone(),
                                Severity::Error,
                                "semantic",
                                SemanticError::TypeMismatch.as_str(),
                                vec![Label {
                                    span: span.clone(),
                                    message: Cow::Borrowed(
                                        "You no fit do arithmetic with string and number together",
                                    ),
                                }],
                            );
                        }
                        (Some(VarType::Bool), ..) | (.., Some(VarType::Bool)) => {
                            self.errors.emit(
                                span.clone(),
                                Severity::Error,
                                "semantic",
                                SemanticError::TypeMismatch.as_str(),
                                vec![Label {
                                    span: span.clone(),
                                    message: Cow::Borrowed(
                                        "You no fit do arithmetic with boolean values",
                                    ),
                                }],
                            );
                        }
                        _ => {}
                    },
                    BinOp::Eq | BinOp::Gt | BinOp::Lt => match (l, r) {
                        (Some(VarType::Number), Some(VarType::Number)) => {}
                        (Some(VarType::String), Some(VarType::String)) => {}
                        (Some(VarType::Bool), Some(VarType::Bool)) => {}
                        (Some(VarType::Dynamic), ..) | (.., Some(VarType::Dynamic)) => {}
                        _ => {
                            self.errors.emit(
                                span.clone(),
                                Severity::Error,
                                "semantic",
                                SemanticError::TypeMismatch.as_str(),
                                vec![Label {
                                    span: span.clone(),
                                    message: Cow::Borrowed(
                                        "You fit only compare numbers, strings, or booleans",
                                    ),
                                }],
                            );
                        }
                    },
                    BinOp::And | BinOp::Or => match (l, r) {
                        (Some(VarType::Bool), Some(VarType::Bool)) => {}
                        (Some(VarType::Dynamic), ..) | (.., Some(VarType::Dynamic)) => {}
                        _ => {
                            self.errors.emit(
                                span.clone(),
                                Severity::Error,
                                "semantic",
                                SemanticError::TypeMismatch.as_str(),
                                vec![Label {
                                    span: span.clone(),
                                    message: Cow::Borrowed(
                                        "Logical operators fit only work with boolean values",
                                    ),
                                }],
                            );
                        }
                    },
                }
            }
            // Unary not requires a boolean operand or dynamic type
            Expr::Not { expr, span } => {
                self.check_expr(*expr);
                let t = self.infer_expr_type(*expr);
                if t != Some(VarType::Bool) && t != Some(VarType::Dynamic) {
                    self.errors.emit(
                        span.clone(),
                        Severity::Error,
                        "semantic",
                        SemanticError::TypeMismatch.as_str(),
                        vec![Label {
                            span: span.clone(),
                            message: Cow::Borrowed("You fit only use `not` with boolean"),
                        }],
                    );
                }
            }
            Expr::Call { callee, args, span } => {
                // Access arguments through the arena
                let arg_list = &self.args.nodes[args.0];

                // Check the callee expression
                match &self.exprs.nodes[callee.0] {
                    Expr::Var(func_name, ..) => {
                        // Check if this is a built-in function first
                        if let Some(builtin) = Builtin::from_name(func_name) {
                            // Check parameter count for built-in
                            if arg_list.args.len() != builtin.arity() {
                                self.errors.emit(
                                    span.clone(),
                                    Severity::Error,
                                    "semantic",
                                    SemanticError::FunctionCallArity.as_str(),
                                    vec![Label {
                                        span: span.clone(),
                                        message: Cow::Owned(format!(
                                            "Function `{}` expect {} arguments but you pass {}",
                                            func_name,
                                            builtin.arity(),
                                            arg_list.args.len()
                                        )),
                                    }],
                                );
                            }
                        } else if let Some(func_sig) = self.find_function(func_name) {
                            // Check parameter count for user-defined function
                            if arg_list.args.len() != func_sig.param_names.len() {
                                self.errors.emit(
                                    span.clone(),
                                    Severity::Error,
                                    "semantic",
                                    SemanticError::FunctionCallArity.as_str(),
                                    vec![Label {
                                        span: span.clone(),
                                        message: Cow::Owned(format!(
                                            "Function `{}` expect {} arguments but you pass {}",
                                            func_name,
                                            func_sig.param_names.len(),
                                            arg_list.args.len()
                                        )),
                                    }],
                                );
                            }
                        } else {
                            self.errors.emit(
                                span.clone(),
                                Severity::Error,
                                "semantic",
                                SemanticError::UndeclaredIdentifier.as_str(),
                                vec![Label {
                                    span: span.clone(),
                                    message: Cow::Borrowed("You never define dis function before"),
                                }],
                            );
                        }
                    }
                    _ => self.check_expr(*callee),
                }

                // Check all arguments
                for &arg in &arg_list.args {
                    self.check_expr(arg);
                }
            }
        }
    }

    // Infers the type of an expression based on its structure and context.
    fn infer_expr_type(&self, eid: ExprId) -> Option<VarType> {
        match &self.exprs.nodes[eid.0] {
            Expr::Number(..) => Some(VarType::Number),
            Expr::String(..) => Some(VarType::String),
            Expr::Bool(..) => Some(VarType::Bool),
            Expr::Var(v, ..) => {
                for scope in self.variable_scopes.iter().rev() {
                    if let Some((_, t, ..)) = scope.iter().find(|(name, ..)| name == v) {
                        return Some(*t);
                    }
                }
                None
            }
            Expr::Binary { op, lhs, rhs, .. } => {
                let l = self.infer_expr_type(*lhs)?;
                let r = self.infer_expr_type(*rhs)?;
                match op {
                    BinOp::Add => match (l, r) {
                        (VarType::Number, VarType::Number) => Some(VarType::Number),
                        (VarType::String, VarType::String) => Some(VarType::String),
                        (VarType::Dynamic, VarType::Dynamic)
                        | (VarType::Dynamic, VarType::Number)
                        | (VarType::Number, VarType::Dynamic) => Some(VarType::Number),
                        (VarType::Dynamic, VarType::String)
                        | (VarType::String, VarType::Dynamic) => Some(VarType::String),
                        _ => None,
                    },
                    BinOp::Minus | BinOp::Times | BinOp::Divide | BinOp::Mod => match (l, r) {
                        (VarType::Number, VarType::Number) => Some(VarType::Number),
                        (VarType::Dynamic, ..) | (.., VarType::Dynamic) => Some(VarType::Number),
                        _ => None,
                    },
                    BinOp::Eq | BinOp::Gt | BinOp::Lt => match (l, r) {
                        (VarType::Number, VarType::Number) => Some(VarType::Bool),
                        (VarType::String, VarType::String) => Some(VarType::Bool),
                        (VarType::Bool, VarType::Bool) => Some(VarType::Bool),
                        (VarType::Dynamic, ..) | (.., VarType::Dynamic) => Some(VarType::Bool),
                        _ => None,
                    },
                    BinOp::And | BinOp::Or => match (l, r) {
                        (VarType::Bool, VarType::Bool) => Some(VarType::Bool),
                        (VarType::Dynamic, ..) | (.., VarType::Dynamic) => Some(VarType::Bool),
                        _ => None,
                    },
                }
            }
            Expr::Not { expr, .. } => {
                let t = self.infer_expr_type(*expr)?;
                if t == VarType::Bool { Some(VarType::Bool) } else { None }
            }
            Expr::Call { callee, .. } => match &self.exprs.nodes[callee.0] {
                Expr::Var(func_name, ..) => {
                    if let Some(builtin) = Builtin::from_name(func_name) {
                        Some(VarType::from(builtin.return_type()))
                    } else if let Some(func_sig) = self.find_function(func_name) {
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
