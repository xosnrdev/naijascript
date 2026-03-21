//! Binding facts collected during semantic resolution.

use std::ops::Range;

use crate::analysis::effects::ExprClass;
use crate::analysis::ids::{FunctionId, INVALID_SCOPE_ID, LocalId, ScopeId, StmtId};
use crate::arena::Arena;
use crate::diagnostics::Span;
use crate::syntax::parser::{BlockRef, ExprRef, ParamListRef, StmtRef};

/// Stable name used for the synthetic top-level script function.
pub const ROOT_FUNCTION_NAME: &str = "<script>";

/// Program-wide semantic facts that later analysis passes consume.
#[derive(Debug)]
pub struct ProgramFacts<'ast, 'arena> {
    pub root_function: FunctionId,
    pub functions: Vec<FunctionInfo<'ast>, &'arena Arena>,
    pub scopes: Vec<ScopeInfo, &'arena Arena>,
    pub locals: Vec<LocalInfo<'ast>, &'arena Arena>,
    pub function_directs: Vec<FunctionDirectFacts<'arena>, &'arena Arena>,
    pub user_calls: Vec<UserCallBinding<'ast>, &'arena Arena>,
    pub stmt_effects: Vec<StmtEffectFacts<'ast, 'arena>, &'arena Arena>,
}

/// Metadata describing a function body.
#[derive(Debug)]
pub struct FunctionInfo<'ast> {
    pub name: &'ast str,
    pub params: Option<ParamListRef<'ast>>,
    pub parent: Option<FunctionId>,
    pub defining_scope: ScopeId,
    pub def_span: Span,
    pub body_span: Span,
    pub body: BlockRef<'ast>,
    pub def_stmt: Option<StmtId>,
    pub locals_start: u32,
    pub locals_len: u32,
}

/// Metadata describing a lexical scope.
#[derive(Debug)]
pub struct ScopeInfo {
    pub parent: Option<ScopeId>,
    pub owner: FunctionId,
    pub span: Span,
}

/// Metadata describing a local declaration.
#[derive(Debug)]
pub struct LocalInfo<'ast> {
    pub name: &'ast str,
    pub owner: FunctionId,
    pub declaring_scope: ScopeId,
    pub decl_span: Span,
    pub decl_stmt: Option<StmtId>,
    pub kind: LocalKind,
}

/// Origin of a local binding.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LocalKind {
    Parameter,
    Variable,
}

/// Direct interprocedural facts collected while resolving one function body.
#[derive(Debug)]
pub struct FunctionDirectFacts<'arena> {
    pub direct_callees: Vec<FunctionId, &'arena Arena>,
    pub direct_capture_reads: Vec<LocalId, &'arena Arena>,
    pub direct_capture_writes: Vec<LocalId, &'arena Arena>,
}

/// Direct user-function call binding recorded during resolution.
#[derive(Debug)]
pub struct UserCallBinding<'ast> {
    pub call: ExprRef<'ast>,
    pub caller: FunctionId,
    pub callee: FunctionId,
}

/// Per-statement local and effect facts used by later analyses.
#[derive(Debug)]
pub struct StmtEffectFacts<'ast, 'arena> {
    pub stmt: StmtRef<'ast>,
    pub function: FunctionId,
    pub scope: ScopeId,
    pub reads: Vec<LocalId, &'arena Arena>,
    pub writes: Vec<LocalId, &'arena Arena>,
    pub direct_callees: Vec<FunctionId, &'arena Arena>,
    pub expr_class: ExprClass,
}

impl<'ast, 'arena> ProgramFacts<'ast, 'arena> {
    /// Creates an empty set of facts.
    pub fn new(arena: &'arena Arena) -> Self {
        Self {
            root_function: FunctionId(0),
            functions: Vec::new_in(arena),
            scopes: Vec::new_in(arena),
            locals: Vec::new_in(arena),
            function_directs: Vec::new_in(arena),
            user_calls: Vec::new_in(arena),
            stmt_effects: Vec::new_in(arena),
        }
    }

    /// Creates the synthetic top-level function.
    pub fn push_root_function(&mut self, body: BlockRef<'ast>) -> FunctionId {
        debug_assert!(self.functions.is_empty());
        let id = FunctionId(0);
        self.root_function = id;
        self.functions.push(FunctionInfo {
            name: ROOT_FUNCTION_NAME,
            params: None,
            parent: None,
            defining_scope: INVALID_SCOPE_ID,
            def_span: body.span,
            body_span: body.span,
            body,
            def_stmt: None,
            locals_start: 0,
            locals_len: 0,
        });
        self.function_directs.push(FunctionDirectFacts {
            direct_callees: Vec::new_in(self.functions.allocator()),
            direct_capture_reads: Vec::new_in(self.functions.allocator()),
            direct_capture_writes: Vec::new_in(self.functions.allocator()),
        });
        id
    }

    /// Adds a function definition.
    pub fn push_function(
        &mut self,
        name: &'ast str,
        params: ParamListRef<'ast>,
        parent: Option<FunctionId>,
        defining_scope: ScopeId,
        def_span: Span,
        body: BlockRef<'ast>,
    ) -> FunctionId {
        let id = FunctionId(self.functions.len() as u32);
        self.functions.push(FunctionInfo {
            name,
            params: Some(params),
            parent,
            defining_scope,
            def_span,
            body_span: body.span,
            body,
            def_stmt: None,
            locals_start: self.locals.len() as u32,
            locals_len: 0,
        });
        self.function_directs.push(FunctionDirectFacts {
            direct_callees: Vec::new_in(self.functions.allocator()),
            direct_capture_reads: Vec::new_in(self.functions.allocator()),
            direct_capture_writes: Vec::new_in(self.functions.allocator()),
        });
        id
    }

    /// Updates the root function's defining scope once the first lexical block exists.
    pub fn set_root_scope(&mut self, scope: ScopeId) {
        let root = &mut self.functions[self.root_function.0 as usize];
        root.defining_scope = scope;
    }

    /// Records the statement that defines a function.
    pub fn set_function_def_stmt(&mut self, function: FunctionId, stmt: StmtId) {
        self.functions[function.0 as usize].def_stmt = Some(stmt);
    }

    /// Adds a lexical scope.
    pub fn push_scope(
        &mut self,
        parent: Option<ScopeId>,
        owner: FunctionId,
        span: Span,
    ) -> ScopeId {
        let id = ScopeId(self.scopes.len() as u32);
        self.scopes.push(ScopeInfo { parent, owner, span });
        id
    }

    /// Adds a local declaration.
    fn push_local_with_kind(
        &mut self,
        name: &'ast str,
        owner: FunctionId,
        declaring_scope: ScopeId,
        decl_span: Span,
        decl_stmt: Option<StmtId>,
        kind: LocalKind,
    ) -> LocalId {
        let id = LocalId(self.locals.len() as u32);
        self.locals.push(LocalInfo { name, owner, declaring_scope, decl_span, decl_stmt, kind });
        self.functions[owner.0 as usize].locals_len += 1;
        id
    }

    /// Adds a local variable declaration.
    pub fn push_local_decl(
        &mut self,
        name: &'ast str,
        owner: FunctionId,
        declaring_scope: ScopeId,
        decl_span: Span,
        decl_stmt: StmtId,
    ) -> LocalId {
        self.push_local_with_kind(
            name,
            owner,
            declaring_scope,
            decl_span,
            Some(decl_stmt),
            LocalKind::Variable,
        )
    }

    /// Adds a function parameter binding.
    pub fn push_param(
        &mut self,
        name: &'ast str,
        owner: FunctionId,
        declaring_scope: ScopeId,
        decl_span: Span,
    ) -> LocalId {
        self.push_local_with_kind(
            name,
            owner,
            declaring_scope,
            decl_span,
            None,
            LocalKind::Parameter,
        )
    }

    /// Returns the metadata for a function.
    #[must_use]
    pub fn function(&self, id: FunctionId) -> &FunctionInfo<'ast> {
        &self.functions[id.0 as usize]
    }

    /// Returns the arena that owns persistent fact storage.
    #[must_use]
    pub fn arena(&self) -> &'arena Arena {
        self.functions.allocator()
    }

    /// Returns the direct interprocedural facts for a function.
    #[must_use]
    pub fn function_direct(&self, id: FunctionId) -> &FunctionDirectFacts<'arena> {
        &self.function_directs[id.0 as usize]
    }

    /// Finds the function id that owns the given body block.
    #[must_use]
    pub fn function_by_body(&self, body: BlockRef<'ast>) -> Option<FunctionId> {
        self.functions
            .iter()
            .position(|info| std::ptr::eq(info.body, body))
            .map(|idx| FunctionId(idx as u32))
    }

    /// Finds the direct user-function callee bound to this call expression.
    #[must_use]
    pub fn user_call_callee(&self, call: ExprRef<'ast>) -> Option<FunctionId> {
        self.user_calls
            .iter()
            .find(|binding| std::ptr::eq(binding.call, call))
            .map(|binding| binding.callee)
    }

    /// Returns the local-id range owned by a function.
    #[must_use]
    pub fn local_range(&self, function: FunctionId) -> Range<u32> {
        let info = &self.functions[function.0 as usize];
        info.locals_start..(info.locals_start + info.locals_len)
    }

    /// Adds a statement-effect slot during semantic traversal.
    pub fn push_stmt_effect(
        &mut self,
        stmt: StmtRef<'ast>,
        function: FunctionId,
        scope: ScopeId,
    ) -> StmtId {
        let id = StmtId(self.stmt_effects.len() as u32);
        self.stmt_effects.push(StmtEffectFacts {
            stmt,
            function,
            scope,
            reads: Vec::new_in(self.functions.allocator()),
            writes: Vec::new_in(self.functions.allocator()),
            direct_callees: Vec::new_in(self.functions.allocator()),
            expr_class: ExprClass::PureNoTrap,
        });
        id
    }

    /// Returns the effect facts for a statement.
    #[must_use]
    pub fn stmt_effect(&self, stmt: StmtId) -> &StmtEffectFacts<'ast, 'arena> {
        &self.stmt_effects[stmt.0 as usize]
    }

    fn stmt_effect_mut(&mut self, stmt: StmtId) -> &mut StmtEffectFacts<'ast, 'arena> {
        &mut self.stmt_effects[stmt.0 as usize]
    }

    /// Records a current-function local read for the statement.
    pub fn record_stmt_read(&mut self, stmt: StmtId, local: LocalId) {
        let reads = &mut self.stmt_effect_mut(stmt).reads;
        if !reads.contains(&local) {
            reads.push(local);
        }
    }

    /// Records a current-function local write for the statement.
    pub fn record_stmt_write(&mut self, stmt: StmtId, local: LocalId) {
        let writes = &mut self.stmt_effect_mut(stmt).writes;
        if !writes.contains(&local) {
            writes.push(local);
        }
    }

    /// Records a direct user-function callee for the statement.
    pub fn record_stmt_callee(&mut self, stmt: StmtId, callee: FunctionId) {
        let callees = &mut self.stmt_effect_mut(stmt).direct_callees;
        if !callees.contains(&callee) {
            callees.push(callee);
        }
    }

    /// Joins a statement's effect class with another class.
    pub fn join_stmt_expr_class(&mut self, stmt: StmtId, class: ExprClass) {
        let effect = &mut self.stmt_effect_mut(stmt).expr_class;
        *effect = effect.join(class);
    }

    /// Records a direct user-function call.
    pub fn record_direct_callee(&mut self, caller: FunctionId, callee: FunctionId) {
        let callees = &mut self.function_directs[caller.0 as usize].direct_callees;
        if !callees.contains(&callee) {
            callees.push(callee);
        }
    }

    /// Records the bound callee for a direct user-function call expression.
    pub fn record_user_call(
        &mut self,
        call: ExprRef<'ast>,
        caller: FunctionId,
        callee: FunctionId,
    ) {
        if self.user_call_callee(call).is_none() {
            self.user_calls.push(UserCallBinding { call, caller, callee });
        }
    }

    /// Records a direct read of an outer local.
    pub fn record_capture_read(&mut self, function: FunctionId, local: LocalId) {
        let reads = &mut self.function_directs[function.0 as usize].direct_capture_reads;
        if !reads.contains(&local) {
            reads.push(local);
        }
    }

    /// Records a direct write to an outer local.
    pub fn record_capture_write(&mut self, function: FunctionId, local: LocalId) {
        let writes = &mut self.function_directs[function.0 as usize].direct_capture_writes;
        if !writes.contains(&local) {
            writes.push(local);
        }
    }
}
