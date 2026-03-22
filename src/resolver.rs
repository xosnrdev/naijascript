//! The resolver (semantic analyzer) for `NaijaScript`.

use std::collections::HashSet;

use crate::analysis::effects::{self, ExprClass};
use crate::analysis::facts::ProgramFacts;
use crate::analysis::ids::{FunctionId, LocalId, ScopeId, StmtId};
use crate::analysis::opt::{OptimizationInputs, OptimizationPlan};
use crate::analysis::{
    cfg, diagnostics as analysis_diagnostics, limits, liveness, opt, reachability, summary,
};
use crate::arena::{Arena, ArenaCow};
use crate::arena_format;
use crate::builtins::{
    ArrayBuiltin, Builtin, GlobalBuiltin, MemberBuiltin, NumberBuiltin, StringBuiltin,
};
use crate::diagnostics::{AsStr, Diagnostics, Label, Severity, Span};
use crate::helpers::ValueType;
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
    UnusedAssignment,
    UnusedVariable,
    UnusedFunction,
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
            SemanticError::UnusedAssignment => "Unused assignment",
            SemanticError::UnusedVariable => "Unused variable",
            SemanticError::UnusedFunction => "Unused function",
            SemanticError::ReservedKeyword => "Use of reserved keyword",
        }
    }
}

// Represents the blueprint of a function signature in NaijaScript
#[derive(Debug)]
struct FunctionSig<'ast> {
    name: &'ast str,
    id: FunctionId,
    param_names: &'ast [&'ast str],
    name_span: &'ast Span,
    return_type: ValueType,
}

type VariableScopeEntry<'ast> = (&'ast str, ValueType, &'ast Span, LocalId);
type VariableScope<'ast, 'res> = Vec<VariableScopeEntry<'ast>, &'res Arena>;

#[derive(Clone)]
struct WarningRecord<'a> {
    stmt_id: StmtId,
    span: Span,
    error: SemanticError,
    message: ArenaCow<'a>,
}

/// An arena-allocated semantic analyzer.
///
/// Uses two lifetime parameters to separate AST data (`'ast`) from the resolver's
/// own working memory (`'res`). This allows the resolver's scope tables and diagnostics
/// to be freed independently of the AST e.g., by using a scratch arena for `'res`.
pub struct Resolver<'ast, 'res> {
    // Stack of variable symbol tables for block-scoped variables
    variable_scopes: Vec<VariableScope<'ast, 'res>, &'res Arena>,

    // Stack of function symbol tables for block-scoped functions
    function_scopes: Vec<Vec<FunctionSig<'ast>, &'res Arena>, &'res Arena>,

    // Track current function context for return statement validation
    current_function: Option<FunctionId>,

    // Track the function that owns declarations in the current lexical context
    current_owner: FunctionId,

    // Track loop nesting depth (0 = not in loop)
    in_loop: usize,

    // Track lexical scope identity for later analyses
    scope_stack: Vec<ScopeId, &'res Arena>,

    // Track the statement currently being analyzed so local use facts can be attached once.
    current_stmt: Option<StmtId>,

    /// Collection of semantic errors found during analysis
    pub errors: Diagnostics<'res>,

    /// Binding facts collected during resolution.
    pub facts: ProgramFacts<'ast, 'ast>,

    /// Conservative optimization plan built from the current analysis facts.
    pub optimization_plan: Option<OptimizationPlan<'ast>>,

    // Reference to the arena for allocating scope vectors
    arena: &'res Arena,

    // Reference to the arena that owns persistent analysis artifacts.
    facts_arena: &'ast Arena,
}

impl<'ast, 'res> Resolver<'ast, 'res> {
    /// Creates a new [`Resolver`] instance whose persistent facts live in `facts_arena`.
    pub fn with_facts_arena(arena: &'res Arena, facts_arena: &'ast Arena) -> Self {
        Self {
            variable_scopes: Vec::new_in(arena),
            function_scopes: Vec::new_in(arena),
            current_function: None,
            current_owner: FunctionId(0),
            in_loop: 0,
            scope_stack: Vec::new_in(arena),
            current_stmt: None,
            errors: Diagnostics::new(arena),
            facts: ProgramFacts::new(facts_arena),
            optimization_plan: None,
            arena,
            facts_arena,
        }
    }

    /// Consumes the resolver and returns the persistent artifacts needed after resolution.
    #[must_use]
    pub fn into_artifacts(self) -> (ProgramFacts<'ast, 'ast>, Option<OptimizationPlan<'ast>>) {
        (self.facts, self.optimization_plan)
    }

    /// Creates a new [`Resolver`] instance.
    pub fn new(arena: &'ast Arena) -> Resolver<'ast, 'ast> {
        Resolver::with_facts_arena(arena, arena)
    }

    /// Resolves the given AST root node.
    pub fn resolve(&mut self, root: BlockRef<'ast>) {
        let root_function = self.facts.push_root_function(root);
        self.current_owner = root_function;
        self.current_function = None;
        self.check_block(root);
        self.facts.finalize_pointer_bindings();
        self.emit_analysis_warnings();
    }

    fn emit_error(&mut self, span: Span, error: SemanticError, labels: Vec<Label<'res>>) {
        self.errors.emit(span, Severity::Error, "semantic", error.as_str(), labels);
    }

    fn emit_warning(&mut self, span: Span, warning: SemanticError, labels: Vec<Label<'res>>) {
        self.errors.emit(span, Severity::Warning, "semantic", warning.as_str(), labels);
    }

    #[inline]
    fn check_block(&mut self, block: BlockRef<'ast>) {
        let scope_id =
            self.facts.push_scope(self.scope_stack.last().copied(), self.current_owner, block.span);
        self.facts.record_block_scope(block, scope_id);
        if self.scope_stack.is_empty() && self.current_owner == self.facts.root_function {
            self.facts.set_root_scope(scope_id);
        }

        // Enter new block scope
        self.scope_stack.push(scope_id);
        self.variable_scopes.push(Vec::new_in(self.arena));
        self.function_scopes.push(Vec::new_in(self.arena));

        for &stmt in block.stmts {
            self.check_stmt(stmt);
        }

        // Leave this block scope
        self.variable_scopes.pop();
        self.function_scopes.pop();
        self.scope_stack.pop();
    }

    fn check_stmt(&mut self, stmt: StmtRef<'ast>) {
        let stmt_id = self.facts.push_stmt_effect(stmt, self.current_owner, self.current_scope());
        let previous_stmt = self.current_stmt.replace(stmt_id);

        match stmt {
            // Handle "make variable get expression" statements
            Stmt::Assign { var, var_span, expr, .. } => {
                if GlobalBuiltin::from_name(var).is_some() {
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
                self.set_stmt_expr_class(self.classify_expr(expr));
                let var_type = self.infer_expr_type(expr).unwrap_or(ValueType::Dynamic);
                let current_scope = self
                    .variable_scopes
                    .last()
                    .expect("scope stack should never be empty")
                    .iter()
                    .rposition(|(name, ..)| name == var);
                if let Some(slot) = current_scope {
                    let local_id =
                        self.variable_scopes.last().expect("scope stack should never be empty")
                            [slot]
                            .3;
                    self.variable_scopes.last_mut().expect("scope stack should never be empty")
                        [slot]
                        .1 = var_type;
                    self.facts.record_stmt_local(stmt, local_id);
                    self.record_stmt_write(local_id);
                } else {
                    let scope_id = self.current_scope();
                    let local_id = self.facts.push_local_decl(
                        var,
                        self.current_owner,
                        scope_id,
                        *var_span,
                        stmt_id,
                    );
                    self.variable_scopes
                        .last_mut()
                        .expect("scope stack should never be empty")
                        .push((var, var_type, var_span, local_id));
                    self.facts.record_stmt_local(stmt, local_id);
                    self.record_stmt_write(local_id);
                }
            }
            // Handle variable reassignment: <variable> get <expression>
            Stmt::AssignExisting { var, var_span, expr, .. } => {
                if let Some((_, local_id)) = self.lookup_var_info(var) {
                    self.facts.record_stmt_local(stmt, local_id);
                    self.record_stmt_write(local_id);
                    self.record_capture_write(local_id);
                } else {
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
                self.set_stmt_expr_class(self.classify_expr(expr));
            }
            Stmt::AssignIndex { target, expr, .. } => {
                self.check_assign_index(target, expr);
                self.set_stmt_expr_class(ExprClass::Impure);
            }
            // Handle "if to say(condition) start...end" with optional "if not so"
            Stmt::If { cond, then_b, else_b, .. } => {
                self.check_expr(cond);
                self.check_boolean_expr(cond);
                self.set_stmt_expr_class(self.classify_expr(cond));
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
                self.set_stmt_expr_class(self.classify_expr(cond));
                self.in_loop += 1;
                self.check_block(body);
                self.in_loop -= 1;
            }
            // Handle nested blocks
            Stmt::Block { block, .. } => {
                self.check_block(block);
            }
            Stmt::FunctionDef { name, name_span, params, body, span } => {
                self.set_stmt_expr_class(ExprClass::Impure);
                if GlobalBuiltin::from_name(name).is_some() {
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
                self.set_stmt_expr_class(
                    expr.map_or(ExprClass::PureNoTrap, |expr| self.classify_expr(expr)),
                );
            }
            Stmt::Break { span } => {
                if self.in_loop == 0 {
                    self.emit_error(
                        *span,
                        SemanticError::UnreachableCode,
                        vec![Label {
                            span: *span,
                            message: ArenaCow::Borrowed("`comot` statement outside loop body"),
                        }],
                    );
                }
            }
            Stmt::Continue { span } => {
                if self.in_loop == 0 {
                    self.emit_error(
                        *span,
                        SemanticError::UnreachableCode,
                        vec![Label {
                            span: *span,
                            message: ArenaCow::Borrowed("`next` statement outside loop body"),
                        }],
                    );
                }
            }
            Stmt::Expression { expr, .. } => {
                self.check_expr(expr);
                self.set_stmt_expr_class(self.classify_expr(expr));
            }
        }

        self.current_stmt = previous_stmt;
    }

    fn check_assign_index(&mut self, target: ExprRef<'ast>, expr: ExprRef<'ast>) {
        self.check_expr(target);
        self.check_expr(expr);
        if let Some(local_id) = self.expr_root_local(target) {
            self.record_stmt_read(local_id);
            self.record_stmt_write(local_id);
            self.record_capture_read(local_id);
            self.record_capture_write(local_id);
        }
    }

    #[inline]
    fn lookup_var_info(&self, var: &str) -> Option<(ValueType, LocalId)> {
        self.variable_scopes.iter().rev().find_map(|scope| {
            scope.iter().rev().find(|(name, ..)| *name == var).map(|(_, t, _, id)| (*t, *id))
        })
    }

    #[inline]
    fn record_capture_read(&mut self, local: LocalId) {
        if self.facts.locals[local.0 as usize].owner != self.current_owner {
            self.facts.record_capture_read(self.current_owner, local);
        }
    }

    #[inline]
    fn record_capture_write(&mut self, local: LocalId) {
        if self.facts.locals[local.0 as usize].owner != self.current_owner {
            self.facts.record_capture_write(self.current_owner, local);
        }
    }

    #[inline]
    fn record_stmt_read(&mut self, local: LocalId) {
        if self.facts.locals[local.0 as usize].owner == self.current_owner
            && let Some(stmt) = self.current_stmt
        {
            self.facts.record_stmt_read(stmt, local);
        }
    }

    #[inline]
    fn record_stmt_write(&mut self, local: LocalId) {
        if self.facts.locals[local.0 as usize].owner == self.current_owner
            && let Some(stmt) = self.current_stmt
        {
            self.facts.record_stmt_write(stmt, local);
        }
    }

    #[inline]
    fn record_stmt_callee(&mut self, callee: FunctionId) {
        if let Some(stmt) = self.current_stmt {
            self.facts.record_stmt_callee(stmt, callee);
        }
    }

    #[inline]
    fn set_stmt_expr_class(&mut self, class: ExprClass) {
        if let Some(stmt) = self.current_stmt {
            self.facts.join_stmt_expr_class(stmt, class);
        }
    }

    fn expr_root_local(&self, mut expr: ExprRef<'ast>) -> Option<LocalId> {
        loop {
            match expr {
                Expr::Var(name, ..) => {
                    return self.lookup_var_info(name).map(|(_, local_id)| local_id);
                }
                Expr::Index { array, .. } | Expr::Member { object: array, .. } => expr = array,
                _ => return None,
            }
        }
    }

    #[inline]
    fn current_scope(&self) -> ScopeId {
        *self.scope_stack.last().expect("scope stack should never be empty while resolving")
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
        _span: &'ast Span,
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
                    Label {
                        span: *name_span,
                        message: ArenaCow::Owned(arena_format!(
                            self.arena,
                            "Another function `{name}` for here"
                        )),
                    },
                ],
            );
            return;
        }

        // Check for duplicate parameter names
        let mut set = HashSet::with_capacity(params.params.len());
        for (param_name, param_span) in params.params.iter().zip(params.param_spans.iter()) {
            if GlobalBuiltin::from_name(param_name).is_some() {
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
                            "Parameter `{param_name}` used more than once",
                        )),
                    }],
                );
            }
        }

        let return_type = self.infer_function_return_type(body);
        let function_id = self.facts.push_function(
            name,
            params,
            Some(self.current_owner),
            self.current_scope(),
            *name_span,
            body,
        );
        if let Some(stmt) = self.current_stmt {
            self.facts.set_function_def_stmt(function_id, stmt);
        }

        // Add function to current function scope
        self.function_scopes.last_mut().expect("function scope stack should never be empty").push(
            FunctionSig {
                name,
                id: function_id,
                name_span,
                param_names: params.params,
                return_type,
            },
        );

        // Set current function context for return validation
        let prev_owner = self.current_owner;
        let prev_function = self.current_function;
        self.current_owner = function_id;
        self.current_function = Some(function_id);

        let param_scope =
            self.facts.push_scope(Some(self.current_scope()), self.current_owner, body.span);
        self.scope_stack.push(param_scope);

        // Create new scope for function parameters
        self.variable_scopes.push(Vec::new_in(self.arena));

        // Add parameters as variables with dynamic typing
        // Parameters can accept any type and their usage will be validated at runtime
        for (param_name, param_span) in params.params.iter().zip(params.param_spans.iter()) {
            let local_id = self.facts.push_param(
                param_name,
                self.current_owner,
                self.current_scope(),
                *param_span,
            );
            self.variable_scopes.last_mut().unwrap().push((
                param_name,
                ValueType::Dynamic,
                param_span,
                local_id,
            ));
        }

        // Check function body using check_block to get dead code detection
        // Note: check_block will create its own scope, but that's fine for function bodies
        self.check_block(body);

        // Clean up parameter scope
        self.variable_scopes.pop();
        self.scope_stack.pop();

        // Restore previous function context
        self.current_owner = prev_owner;
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
                    message: ArenaCow::Borrowed("`return` statement outside function body"),
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
            && !matches!(t, ValueType::Bool | ValueType::Null | ValueType::Dynamic)
        {
            let span = match expr {
                Expr::Number(.., span)
                | Expr::Null(span)
                | Expr::String { span, .. }
                | Expr::Bool(.., span)
                | Expr::Var(.., span)
                | Expr::Binary { span, .. }
                | Expr::Unary { span, .. }
                | Expr::Call { span, .. }
                | Expr::Index { span, .. }
                | Expr::Array { span, .. }
                | Expr::Member { span, .. } => *span,
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
            Expr::Number(..) | Expr::Bool(..) | Expr::Null(..) => {}
            Expr::String { parts, span } => {
                if let StringParts::Interpolated(segments) = parts {
                    for (segment_idx, segment) in segments.iter().enumerate() {
                        if let StringSegment::Variable(var) = segment {
                            if let Some((_, local_id)) = self.lookup_var_info(var) {
                                self.facts.record_string_segment_local(
                                    expr,
                                    u32::try_from(segment_idx)
                                        .expect("string segment index should fit in u32"),
                                    local_id,
                                );
                                self.record_stmt_read(local_id);
                                self.record_capture_read(local_id);
                            } else {
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
                            message: ArenaCow::Borrowed("Dis expression type no be array"),
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
                            message: ArenaCow::Borrowed("Dis index type no be number"),
                        }],
                    );
                }
            }
            // Variables need to exist in our symbol table before we can use them
            Expr::Var(v, span) => {
                if let Some((_, local_id)) = self.lookup_var_info(v) {
                    self.facts.record_expr_local(expr, local_id);
                    self.record_stmt_read(local_id);
                    self.record_capture_read(local_id);
                } else {
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
                        (Some(ValueType::String | ValueType::Dynamic), ..)
                        | (.., Some(ValueType::String | ValueType::Dynamic))
                        | (Some(ValueType::Number), Some(ValueType::Number)) => {}
                        _ => {
                            self.emit_error(
                                *span,
                                SemanticError::TypeMismatch,
                                vec![Label {
                                    span: *span,
                                    message: ArenaCow::Borrowed(
                                        "Dis expression type no be number or string",
                                    ),
                                }],
                            );
                        }
                    },
                    BinaryOp::Minus | BinaryOp::Times | BinaryOp::Divide | BinaryOp::Mod => {
                        match (l, r) {
                            (
                                Some(ValueType::Number | ValueType::Dynamic),
                                Some(ValueType::Number | ValueType::Dynamic),
                            ) => {}
                            _ => {
                                self.emit_error(
                                    *span,
                                    SemanticError::TypeMismatch,
                                    vec![Label {
                                        span: *span,
                                        message: ArenaCow::Borrowed(
                                            "Dis expression type no be number",
                                        ),
                                    }],
                                );
                            }
                        }
                    }
                    BinaryOp::Eq | BinaryOp::Gt | BinaryOp::Lt => match (l, r) {
                        (Some(ValueType::Number), Some(ValueType::Number))
                        | (Some(ValueType::String), Some(ValueType::String))
                        | (Some(ValueType::Bool), Some(ValueType::Bool))
                        | (Some(ValueType::Null | ValueType::Dynamic), ..)
                        | (.., Some(ValueType::Null | ValueType::Dynamic)) => {}
                        _ => self.emit_error(
                            *span,
                            SemanticError::TypeMismatch,
                            vec![Label {
                                span: *span,
                                message: ArenaCow::Borrowed(
                                    "Dis expression type no be number, string, or boolean",
                                ),
                            }],
                        ),
                    },
                    BinaryOp::And | BinaryOp::Or => match (l, r) {
                        (Some(ValueType::Bool), Some(ValueType::Bool))
                        | (Some(ValueType::Null | ValueType::Dynamic), ..)
                        | (.., Some(ValueType::Null | ValueType::Dynamic)) => {}
                        _ => {
                            self.emit_error(
                                *span,
                                SemanticError::TypeMismatch,
                                vec![Label {
                                    span: *span,
                                    message: ArenaCow::Borrowed(
                                        "Dis expression type no be boolean",
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
                        if t != Some(ValueType::Bool)
                            && t != Some(ValueType::Null)
                            && t != Some(ValueType::Dynamic)
                        {
                            self.emit_error(
                                *span,
                                SemanticError::TypeMismatch,
                                vec![Label {
                                    span: *span,
                                    message: ArenaCow::Borrowed(
                                        "Dis expression type no be boolean",
                                    ),
                                }],
                            );
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
                                    message: ArenaCow::Borrowed("Dis expression type no be number"),
                                }],
                            );
                        }
                    }
                }
            }
            Expr::Member { object, .. } => {
                // We don't track precise receiver types, so validation is deferred to runtime
                self.check_expr(object);
            }
            Expr::Call { callee, args, span } => {
                // Check the callee expression
                match callee {
                    Expr::Var(func_name, ..) => {
                        // Check if this is a built-in function first
                        if let Some(builtin) = GlobalBuiltin::from_name(func_name) {
                            // Validate arity for built-in functions
                            if args.args.len() != builtin.arity() {
                                self.emit_error(
                                    *span,
                                    SemanticError::FunctionCallArity,
                                    vec![Label {
                                        span: *span,
                                        message: ArenaCow::Owned(arena_format!(
                                            self.arena,
                                            "Function `{}` dey expect {} argument{} but na {} dey here",
                                            func_name,
                                            builtin.arity(),
                                            if builtin.arity() == 1 { "" } else { "s" },
                                            args.args.len()
                                        )),
                                    }],
                                );
                            }
                        } else if let Some((callee_id, arity)) =
                            self.lookup_func(func_name).map(|sig| (sig.id, sig.param_names.len()))
                        {
                            self.facts.record_direct_callee(self.current_owner, callee_id);
                            self.facts.record_user_call(expr, self.current_owner, callee_id);
                            self.record_stmt_callee(callee_id);
                            // Check parameter count for user-defined function
                            if args.args.len() != arity {
                                self.emit_error(
                                    *span,
                                    SemanticError::FunctionCallArity,
                                    vec![Label {
                                        span: *span,
                                        message: ArenaCow::Owned(arena_format!(
                                            self.arena,
                                            "Function `{}` dey expect {} argument{} but na {} dey here",
                                            func_name,
                                            arity,
                                            if arity == 1 { "" } else { "s" },
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
                    Expr::Member { object, field, span, .. } => {
                        self.check_expr(object);

                        if let Some(receiver_type) = self.infer_expr_type(object) {
                            let builtin = match receiver_type {
                                ValueType::String => {
                                    StringBuiltin::from_name(field).map(MemberBuiltin::String)
                                }
                                ValueType::Array => {
                                    ArrayBuiltin::from_name(field).map(MemberBuiltin::Array)
                                }
                                ValueType::Number => {
                                    NumberBuiltin::from_name(field).map(MemberBuiltin::Number)
                                }
                                ValueType::Bool | ValueType::Null | ValueType::Dynamic => None,
                            };

                            if let Some(builtin) = builtin {
                                if builtin.requires_mut_receiver()
                                    && let Some(local_id) = self.expr_root_local(object)
                                {
                                    self.record_stmt_read(local_id);
                                    self.record_stmt_write(local_id);
                                    self.record_capture_read(local_id);
                                    self.record_capture_write(local_id);
                                }
                                if args.args.len() != builtin.arity() {
                                    self.emit_error(
                                        *span,
                                        SemanticError::FunctionCallArity,
                                        vec![Label {
                                            span: *span,
                                            message: ArenaCow::Owned(arena_format!(
                                                self.arena,
                                                "Method `{}` dey expect {} argument{} but na {} dey here",
                                                field,
                                                builtin.arity(),
                                                if builtin.arity() == 1 { "" } else { "s" },
                                                args.args.len()
                                            )),
                                        }],
                                    );
                                }

                                // TODO: Extend Builtin trait with arg_types() method for generic argument type validation
                                if matches!(builtin, MemberBuiltin::Array(ArrayBuiltin::Join))
                                    && !args.args.is_empty()
                                    && let Some(arg_ty) = self.infer_expr_type(args.args[0])
                                    && arg_ty != ValueType::String
                                    && arg_ty != ValueType::Dynamic
                                {
                                    self.emit_error(
                                                    *span,
                                                    SemanticError::TypeMismatch,
                                                    vec![Label {
                                                        span: *span,
                                                        message: ArenaCow::Owned(arena_format!(
                                                            self.arena,
                                                            "Method `{field}` dey expect string but na {arg_ty} dey here",
                                                        )),
                                                    }],
                                                );
                                }
                            } else {
                                // We defer method validation at runtime for dynamic receivers
                                if receiver_type != ValueType::Dynamic {
                                    self.emit_error(
                                        *span,
                                        SemanticError::UndeclaredIdentifier,
                                        vec![Label {
                                            span: *span,
                                            message: ArenaCow::Owned(arena_format!(
                                                self.arena,
                                                "Method `{field}` no dey for {receiver_type} type",
                                            )),
                                        }],
                                    );
                                }
                            }
                        }
                    }
                    _ => self.check_expr(callee),
                }

                // Check all arguments
                for arg in args.args {
                    self.check_expr(arg);
                }
            }
        }
    }

    fn classify_expr(&self, expr: ExprRef<'ast>) -> ExprClass {
        match expr {
            Expr::Number(..) | Expr::Bool(..) | Expr::Null(..) | Expr::Var(..) => {
                ExprClass::PureNoTrap
            }
            Expr::String { .. } => ExprClass::PureNoTrap,
            Expr::Array { elements, .. } => {
                elements.iter().fold(ExprClass::PureNoTrap, |class, element| {
                    class.join(self.classify_expr(element))
                })
            }
            Expr::Index { array, index, .. } => self
                .classify_expr(array)
                .join(self.classify_expr(index))
                .join(ExprClass::PureMayTrap),
            Expr::Binary { op, lhs, rhs, .. } => {
                let class = self.classify_expr(lhs).join(self.classify_expr(rhs));
                if matches!(op, BinaryOp::Divide | BinaryOp::Mod) {
                    class.join(ExprClass::PureMayTrap)
                } else {
                    class
                }
            }
            Expr::Unary { expr, .. } => self.classify_expr(expr),
            Expr::Member { object, .. } => self.classify_expr(object),
            Expr::Call { callee, args, .. } => {
                let mut class = args
                    .args
                    .iter()
                    .fold(ExprClass::PureNoTrap, |class, arg| class.join(self.classify_expr(arg)));

                match callee {
                    Expr::Var(func_name, ..) => {
                        if let Some(builtin) = GlobalBuiltin::from_name(func_name) {
                            class = class.join(effects::global_builtin_class(builtin));
                        } else if self.lookup_func(func_name).is_none() {
                            class = class.join(ExprClass::Impure);
                        }
                    }
                    Expr::Member { object, field, .. } => {
                        class = class.join(self.classify_expr(object));
                        if let Some(builtin) = MemberBuiltin::from_name(field) {
                            class = class.join(effects::member_builtin_class(builtin));
                        } else {
                            class = class.join(ExprClass::Impure);
                        }
                    }
                    _ => class = class.join(ExprClass::Impure),
                }

                class
            }
        }
    }

    #[inline]
    fn infer_expr_type(&self, expr: ExprRef<'ast>) -> Option<ValueType> {
        match expr {
            Expr::Number(..) => Some(ValueType::Number),
            Expr::Null(..) => Some(ValueType::Null),
            Expr::String { .. } => Some(ValueType::String),
            Expr::Bool(..) => Some(ValueType::Bool),
            Expr::Array { .. } => Some(ValueType::Array),
            Expr::Index { .. } => Some(ValueType::Dynamic),
            Expr::Var(v, ..) => self.lookup_var_info(v).map(|(t, _)| t),
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
                        | (ValueType::Null | ValueType::Dynamic, ..)
                        | (.., ValueType::Null | ValueType::Dynamic) => Some(ValueType::Bool),
                        _ => None,
                    },
                    BinaryOp::And | BinaryOp::Or => match (l, r) {
                        (ValueType::Bool, ValueType::Bool)
                        | (ValueType::Null | ValueType::Dynamic, ..)
                        | (.., ValueType::Null | ValueType::Dynamic) => Some(ValueType::Bool),
                        _ => None,
                    },
                }
            }
            Expr::Unary { op, expr, .. } => {
                let t = self.infer_expr_type(expr)?;
                match op {
                    UnaryOp::Not => {
                        if t == ValueType::Bool || t == ValueType::Null {
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
            Expr::Member { .. } => {
                // Member expressions return methods/properties
                // We don't know the exact type until runtime
                Some(ValueType::Dynamic)
            }
            Expr::Call { callee, .. } => match callee {
                Expr::Var(func_name, ..) => {
                    if let Some(builtin) = GlobalBuiltin::from_name(func_name) {
                        Some(builtin.return_type())
                    } else {
                        self.lookup_func(func_name).map(|func_sig| func_sig.return_type)
                    }
                }
                Expr::Member { field, .. } => {
                    if let Some(builtin) = MemberBuiltin::from_name(field) {
                        Some(builtin.return_type())
                    } else {
                        Some(ValueType::Dynamic)
                    }
                }
                _ => None,
            },
        }
    }

    fn infer_function_return_type(&self, body: BlockRef<'ast>) -> ValueType {
        let mut return_types = Vec::new_in(self.arena);
        self.collect_return_types(body, &mut return_types);

        if return_types.is_empty() {
            return ValueType::Null;
        }

        let first_type = return_types[0];
        if return_types.iter().all(|t| *t == first_type) { first_type } else { ValueType::Dynamic }
    }

    fn collect_return_types(
        &self,
        block: BlockRef<'ast>,
        return_types: &mut Vec<ValueType, &'res Arena>,
    ) {
        for stmt in block.stmts {
            self.collect_return_types_from_stmt(stmt, return_types);
        }
    }

    fn collect_return_types_from_stmt(
        &self,
        stmt: StmtRef<'ast>,
        return_types: &mut Vec<ValueType, &'res Arena>,
    ) {
        // Nested function bodies are intentionally excluded because their returns
        // do not contribute to the enclosing function's signature.
        match stmt {
            Stmt::Return { expr, .. } => {
                if let Some(expr_ref) = expr {
                    if let Some(typ) = self.infer_expr_type(expr_ref) {
                        return_types.push(typ);
                    } else {
                        return_types.push(ValueType::Dynamic);
                    }
                } else {
                    return_types.push(ValueType::Null);
                }
            }
            Stmt::If { then_b, else_b, .. } => {
                self.collect_return_types(then_b, return_types);
                if let Some(eb) = else_b {
                    self.collect_return_types(eb, return_types);
                }
            }
            Stmt::Loop { body, .. } => {
                self.collect_return_types(body, return_types);
            }
            Stmt::Block { block, .. } => {
                self.collect_return_types(block, return_types);
            }
            _ => {}
        }
    }

    fn emit_analysis_warnings(&mut self) {
        let counts = cfg::count_program(&self.facts, self.arena);
        if let Some(limit) =
            limits::first_exceeded_limit(&self.facts, &counts, limits::DEFAULT_CAPS)
        {
            let span = self.facts.function(self.facts.root_function).body_span;
            let message = arena_format!(
                self.arena,
                "{} for {} (observed {}, limit {})",
                limit.message(),
                limit.metric,
                limit.observed,
                limit.limit
            );
            self.errors.emit(
                span,
                Severity::Warning,
                "analysis",
                limit.message(),
                vec![Label { span, message: ArenaCow::Owned(message) }],
            );
            self.optimization_plan = None;
            return;
        }

        let program = cfg::build_program_with_counts(&self.facts, &counts, self.arena);
        let reachable = reachability::reachable_statement_mask(&program, self.arena);
        let unreachable = reachability::unreachable_statements(&program, self.arena);
        let summaries = summary::compute_summaries(&self.facts, self.arena);
        let function_reachability = analysis_diagnostics::compute_function_reachability(
            &program,
            &self.facts,
            &reachable,
            self.arena,
        );
        let unused_assignments =
            liveness::unused_assignments(&program, &self.facts, &summaries, &reachable, self.arena);
        let unused_variables = analysis_diagnostics::unused_variables(
            &program,
            &self.facts,
            &summaries,
            &reachable,
            &function_reachability,
            self.arena,
        );
        let unused_functions = analysis_diagnostics::unused_functions(
            &self.facts,
            &reachable,
            &function_reachability,
            self.arena,
        );
        self.optimization_plan = Some(opt::build_optimization_plan(
            OptimizationInputs {
                program: &program,
                facts: &self.facts,
                summaries: &summaries,
                reachable: &reachable,
                unused_assignments: &unused_assignments,
                unused_variables: &unused_variables,
                unused_functions: &unused_functions,
            },
            self.facts_arena,
        ));

        let mut warnings = Vec::new_in(self.arena);
        for warning in unreachable {
            warnings.push(WarningRecord {
                stmt_id: warning.stmt_id,
                span: warning.span,
                error: SemanticError::UnreachableCode,
                message: ArenaCow::Borrowed(warning.message),
            });
        }
        for warning in unused_assignments {
            let message = arena_format!(
                self.arena,
                "Value assigned to `{}` is never read",
                warning.local_name
            );
            warnings.push(WarningRecord {
                stmt_id: warning.stmt_id,
                span: warning.span,
                error: SemanticError::UnusedAssignment,
                message: ArenaCow::Owned(message),
            });
        }
        for warning in unused_variables {
            let message =
                arena_format!(self.arena, "Variable `{}` is never read", warning.local_name);
            warnings.push(WarningRecord {
                stmt_id: warning.stmt_id,
                span: warning.span,
                error: SemanticError::UnusedVariable,
                message: ArenaCow::Owned(message),
            });
        }
        for warning in unused_functions {
            let message = arena_format!(self.arena, "Function `{}` is never called", warning.name);
            warnings.push(WarningRecord {
                stmt_id: warning.stmt_id,
                span: warning.span,
                error: SemanticError::UnusedFunction,
                message: ArenaCow::Owned(message),
            });
        }

        warnings.sort_by_key(|warning| warning.stmt_id.0);
        for warning in warnings {
            self.emit_warning(
                warning.span,
                warning.error,
                vec![Label { span: warning.span, message: warning.message }],
            );
        }
    }
}
