//! The semantic analyzer (or resolver) for NaijaScript.

use std::borrow::Cow;
use std::collections::HashSet;

use crate::diagnostics::{AsStr, Diagnostics, Label, Severity, Span};
use crate::syntax::parser::{
    self, Arena, ArgList, BinOp, Block, BlockId, Cond, CondId, Expr, ExprId, ParamList,
    ParamListId, Stmt, StmtId,
};

/// Identifies any node in our AST for precise error reporting.
/// We use this instead of raw pointers because arena-based storage gives us
/// memory safety while still allowing efficient lookups by index.
#[derive(Copy, Clone, Debug)]
pub enum NodeId {
    Stmt(usize),
    Expr(usize),
    Cond(usize),
    Block(usize),
}

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
    FunctionCallInCondition,
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
            SemanticError::FunctionCallInCondition => "Function call in condition",
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

// Represents a function signature for semantic analysis
#[derive(Debug)]
struct FunctionSig<'src> {
    name: &'src str,
    param_names: Vec<&'src str>,
    span: &'src Span,
    has_return: bool,
}

/// The heart of our semantic analysis - this walks through NaijaScript code
/// and catches logical errors that the parser can't detect.
///
/// Right now we're keeping things simple with a flat symbol table, meaning
/// variables declared anywhere are visible everywhere. This matches how the
/// grammar is structured but we could extend this later for block scoping.
pub struct SemAnalyzer<'src> {
    // These are borrowed references to the parser's AST arenas
    // We don't own this data, just analyze what the parser built
    stmts: &'src Arena<Stmt<'src>>,
    exprs: &'src Arena<Expr<'src>>,
    conds: &'src Arena<Cond>,
    blocks: &'src Arena<Block>,
    params: &'src Arena<ParamList<'src>>,
    args: &'src Arena<ArgList>,

    // We use a stack of symbol tables so each block gets its own list of variables
    scopes: Vec<Vec<(&'src str, VarType, &'src Span)>>,

    // Function symbol table for tracking function definitions
    functions: Vec<FunctionSig<'src>>,

    // Track current function context for return statement validation
    current_function: Option<&'src str>,

    // Collect all errors instead of failing fast - gives better user experience
    pub errors: Diagnostics,
}

impl<'src> SemAnalyzer<'src> {
    /// Sets up a new analyzer with the AST arenas from parsing.
    /// We start with empty symbol table and error list - fresh slate for analysis.
    pub fn new(
        stmts: &'src Arena<Stmt<'src>>,
        exprs: &'src Arena<Expr<'src>>,
        conds: &'src Arena<Cond>,
        blocks: &'src Arena<Block>,
        params: &'src Arena<ParamList<'src>>,
        args: &'src Arena<ArgList>,
    ) -> Self {
        SemAnalyzer {
            stmts,
            exprs,
            conds,
            blocks,
            params,
            args,
            scopes: Vec::new(),
            functions: Vec::new(),
            current_function: None,
            errors: Diagnostics::default(),
        }
    }

    /// Main entry point for semantic checking.
    /// Takes the root block (representing the whole program) and recursively
    /// validates everything inside it.
    pub fn analyze(&mut self, root: BlockId) {
        self.scopes.push(Vec::new()); // enter global scope
        self.check_block(root);
        self.scopes.pop(); // exit global scope
    }

    // Validates all statements within a block.
    // Blocks in NaijaScript are wrapped with "start" and "end" keywords,
    // but here we just process the list of statements inside.
    fn check_block(&mut self, bid: BlockId) {
        self.scopes.push(Vec::new()); // enter new block scope
        let block = &self.blocks.nodes[bid.0];

        let mut has_return = false;
        for &sid in &block.stmts {
            // Check for dead code after return statement
            if has_return {
                let stmt = &self.stmts.nodes[sid.0];
                let span = match stmt {
                    parser::Stmt::Assign { span, .. } => span,
                    parser::Stmt::AssignExisting { span, .. } => span,
                    parser::Stmt::Shout { span, .. } => span,
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
                        message: Cow::Borrowed("This code no go run because return dey before am"),
                    }],
                );
            }

            if matches!(&self.stmts.nodes[sid.0], parser::Stmt::Return { .. }) {
                has_return = true;
            }

            self.check_stmt(sid);
        }
        self.scopes.pop(); // exit block scope
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
                // We only want to prevent redeclaring a variable in the same block,
                // so we check just the current (innermost) scope for duplicates
                if self.scopes.last().unwrap().iter().any(|(name, ..)| name == var) {
                    let orig_span = self
                        .scopes
                        .last()
                        .unwrap()
                        .iter()
                        .find(|(name, ..)| name == var)
                        .unwrap()
                        .2;
                    self.errors.emit(
                        span.clone(),
                        Severity::Error,
                        "semantic",
                        SemanticError::DuplicateIdentifier.as_str(),
                        vec![
                            Label {
                                span: (*orig_span).clone(),
                                message: Cow::Borrowed("First time you declare am here"),
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
                    let typ = self.infer_expr_type(*expr).unwrap_or(VarType::Number); // fallback to Number if unknown
                    self.scopes.last_mut().unwrap().push((var, typ, span));
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
                            message: Cow::Borrowed("This variable never dey before"),
                        }],
                    );
                }
                self.check_expr(*expr);
            }
            // Handle "shout(expression)" statements - just validate the expression
            Stmt::Shout { expr, .. } => {
                self.check_expr(*expr);
            }
            // Handle "if to say(condition) start...end" with optional "if not so"
            Stmt::If { cond, then_b, else_b, .. } => {
                self.check_cond(*cond);
                self.check_block(*then_b);
                // Else block is optional in the grammar
                if let Some(eb) = else_b {
                    self.check_block(*eb);
                }
            }
            // Handle "jasi(condition) start...end" loop statements
            Stmt::Loop { cond, body, .. } => {
                self.check_cond(*cond);
                self.check_block(*body);
            }
            // Handle nested blocks
            Stmt::Block { block, .. } => {
                self.check_block(*block);
            }
            Stmt::FunctionDef { name, params, body, span } => {
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
        self.scopes.iter().rev().any(|scope| scope.iter().any(|(name, ..)| name == var))
    }

    // Check if a function has already been declared
    #[inline]
    fn lookup_function(&self, func_name: &str) -> bool {
        self.functions.iter().any(|sig| sig.name == func_name)
    }

    // Find function signature by name
    #[inline]
    fn find_function(&self, func_name: &str) -> Option<&FunctionSig<'src>> {
        self.functions.iter().find(|sig| sig.name == func_name)
    }

    // Validates function definition
    fn check_function_def(
        &mut self,
        name: &'src str,
        params: ParamListId,
        body: BlockId,
        span: &'src Span,
    ) {
        // Check for duplicate function declaration
        if self.lookup_function(name) {
            let orig_span = self.find_function(name).unwrap().span;
            self.errors.emit(
                span.clone(),
                Severity::Error,
                "semantic",
                SemanticError::DuplicateIdentifier.as_str(),
                vec![
                    Label {
                        span: orig_span.clone(),
                        message: Cow::Borrowed("First time you define am here"),
                    },
                    Label {
                        span: span.clone(),
                        message: Cow::Borrowed("You try define am again for here"),
                    },
                ],
            );
        } else {
            let param_list = &self.params.nodes[params.0];

            // Check for duplicate parameter names
            let mut seen_params = HashSet::new();
            for param_name in &param_list.params {
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

            // Add function to symbol table
            self.functions.push(FunctionSig { name, param_names, span, has_return: false });
        }

        // Set current function context for return validation
        let prev_function = self.current_function;
        self.current_function = Some(name);

        // Create new scope for function parameters
        self.scopes.push(Vec::new());

        // Add parameters as variables with dynamic typing
        // Parameters can accept any type and their usage will be validated at runtime
        let param_list = &self.params.nodes[params.0];
        for param_name in &param_list.params {
            self.scopes.last_mut().unwrap().push((param_name, VarType::Dynamic, span));
        }

        // Check function body using check_block to get dead code detection
        // Note: check_block will create its own scope, but that's fine for function bodies
        self.check_block(body);

        // Clean up parameter scope
        self.scopes.pop();

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
                    message: Cow::Borrowed("Return statement must be inside function body"),
                }],
            );
        } else {
            // Mark current function as having a return statement
            if let Some(current_func_name) = self.current_function
                && let Some(func_sig) =
                    self.functions.iter_mut().find(|f| f.name == current_func_name)
            {
                func_sig.has_return = true;
            }
        }

        // Check return expression if present
        if let Some(expr_id) = expr {
            self.check_expr(expr_id);
        }
    }

    // Validates condition expressions used in if statements and loops.
    // NaijaScript conditions are binary comparisons: "na" (equals),
    // "pass" (greater), "small pass" (less than).
    // Both sides must be valid expressions.
    fn check_cond(&mut self, cid: CondId) {
        let cond = &self.conds.nodes[cid.0];

        // Check for function calls in conditions
        self.check_expr_for_function_calls(cond.lhs, "condition");
        self.check_expr_for_function_calls(cond.rhs, "condition");

        self.check_expr(cond.lhs);
        self.check_expr(cond.rhs);
        let lhs_type = self.infer_expr_type(cond.lhs);
        let rhs_type = self.infer_expr_type(cond.rhs);
        match (lhs_type, rhs_type) {
            (Some(VarType::String), Some(VarType::String))
            | (Some(VarType::Number), Some(VarType::Number))
            | (Some(VarType::Bool), Some(VarType::Bool)) => {}
            (Some(VarType::Dynamic), ..) | (.., Some(VarType::Dynamic)) => {}
            (Some(VarType::String), Some(VarType::Number))
            | (Some(VarType::Number), Some(VarType::String))
            | (Some(VarType::Bool), Some(VarType::Number))
            | (Some(VarType::Number), Some(VarType::Bool))
            | (Some(VarType::Bool), Some(VarType::String))
            | (Some(VarType::String), Some(VarType::Bool)) => {
                self.errors.emit(
                    cond.span.clone(),
                    Severity::Error,
                    "semantic",
                    SemanticError::TypeMismatch.as_str(),
                    vec![Label {
                        span: cond.span.clone(),
                        message: Cow::Borrowed("You no fit compare different types together"),
                    }],
                );
            }
            _ => {}
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
                            message: Cow::Borrowed("This variable never dey before"),
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
                                        "You no fit do arithmetic with boolean values",
                                    ),
                                }],
                            );
                        }
                        _ => {}
                    },
                    BinOp::Minus | BinOp::Times | BinOp::Divide | BinOp::Mod => {
                        match (l, r) {
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
                                        message: Cow::Borrowed(
                                            "You no fit minus/times/divide/mod string for here",
                                        ),
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
                                    message: Cow::Borrowed("You no fit do arithmetic with string and number together"),
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
                        }
                    }
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
                                        "Logical operators dey only work with boolean values",
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
                            message: Cow::Borrowed("You fit only use `not` for boolean values"),
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
                        // Check if function exists
                        if let Some(func_sig) = self.find_function(func_name) {
                            // Check parameter count
                            if arg_list.args.len() != func_sig.param_names.len() {
                                self.errors.emit(
                                    span.clone(),
                                    Severity::Error,
                                    "semantic",
                                    SemanticError::FunctionCallArity.as_str(),
                                    vec![Label {
                                        span: span.clone(),
                                        message: Cow::Owned(format!(
                                            "Function `{}` expect {} parameters but you give {}",
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
                                    message: Cow::Owned(format!(
                                        "Function `{func_name}` never dey before"
                                    )),
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

    // Infer the type of an expression, using the symbol table for variables
    fn infer_expr_type(&self, eid: ExprId) -> Option<VarType> {
        match &self.exprs.nodes[eid.0] {
            Expr::Number(..) => Some(VarType::Number),
            Expr::String(..) => Some(VarType::String),
            Expr::Bool(..) => Some(VarType::Bool),
            Expr::Var(v, ..) => {
                for scope in self.scopes.iter().rev() {
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
                    BinOp::Add | BinOp::Minus | BinOp::Times | BinOp::Divide | BinOp::Mod => {
                        if l == VarType::Number && r == VarType::Number {
                            Some(VarType::Number)
                        } else if l == VarType::String
                            && r == VarType::String
                            && matches!(op, BinOp::Add)
                        {
                            Some(VarType::String)
                        } else {
                            None
                        }
                    }
                    BinOp::And | BinOp::Or => {
                        if l == VarType::Bool && r == VarType::Bool {
                            Some(VarType::Bool)
                        } else {
                            None
                        }
                    }
                }
            }
            Expr::Not { expr, .. } => {
                let t = self.infer_expr_type(*expr)?;
                if t == VarType::Bool { Some(VarType::Bool) } else { None }
            }
            Expr::Call { callee, .. } => match &self.exprs.nodes[callee.0] {
                Expr::Var(func_name, ..) => {
                    if self.lookup_function(func_name) {
                        Some(VarType::Number)
                    } else {
                        None
                    }
                }
                _ => None,
            },
        }
    }

    fn check_expr_for_function_calls(&mut self, eid: ExprId, context: &str) {
        match &self.exprs.nodes[eid.0] {
            Expr::Call { span, .. } => {
                self.errors.emit(
                    span.clone(),
                    Severity::Warning,
                    "semantic",
                    SemanticError::FunctionCallInCondition.as_str(),
                    vec![Label {
                        span: span.clone(),
                        message: Cow::Owned(format!(
                            "Function call for {context} fit slow down your code if e dey run plenty times"
                        )),
                    }],
                );
            }
            Expr::Binary { lhs, rhs, .. } => {
                self.check_expr_for_function_calls(*lhs, context);
                self.check_expr_for_function_calls(*rhs, context);
            }
            Expr::Not { expr, .. } => {
                self.check_expr_for_function_calls(*expr, context);
            }
            Expr::Number(..) | Expr::String(..) | Expr::Bool(..) | Expr::Var(..) => {}
        }
    }
}
