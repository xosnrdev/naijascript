//! Side CFG used for source-oriented analyses.

use std::fmt::Write;

use crate::analysis::effects::ExprClass;
use crate::analysis::facts::ProgramFacts;
use crate::analysis::ids::{BlockId, FunctionId, LocalId, OpId, ScopeId, StmtId};
use crate::arena::{Arena, ArenaString};
use crate::diagnostics::Span;
use crate::syntax::parser::{BlockRef, Stmt, StmtRef};

/// Reason a later statement in the same lexical sequence cannot execute.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnreachableCause {
    Return,
    Break,
    Continue,
    NoFallthrough,
}

/// Source statement kind for diagnostics and tests.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum StmtKind {
    Assign,
    AssignExisting,
    AssignIndex,
    If,
    Loop,
    Block,
    FunctionDef,
    Return,
    Break,
    Continue,
    Expression,
}

/// Metadata about one source statement in the analysis sidecar.
///
/// `StmtId` remains the stable source-facing identity shared with diagnostics,
/// optimization planning, and the tree-walk runtime. The lowered op id stays an
/// analysis-internal detail even though the current lowering still emits one op
/// per source statement.
#[derive(Debug)]
pub struct StmtInfo<'ast> {
    pub stmt: StmtRef<'ast>,
    pub function: FunctionId,
    pub parent: Option<StmtId>,
    pub span: Span,
    pub kind: StmtKind,
    pub block: BlockId,
    pub op: OpId,
}

/// One lowered analysis op in the linear op stream of a function.
///
/// The current lowering emits exactly one op per source statement so existing
/// dataflow storage can stay statement-shaped. External consumers must still
/// treat `StmtId` as the runtime-facing identity and `OpId` as an analysis-only
/// handle.
#[derive(Debug)]
pub struct LinearOp<'arena> {
    pub id: OpId,
    pub stmt: StmtId,
    pub reads: Vec<LocalId, &'arena Arena>,
    pub writes: Vec<LocalId, &'arena Arena>,
    pub direct_callees: Vec<FunctionId, &'arena Arena>,
    pub expr_class: ExprClass,
    pub span: Span,
}

/// Terminator of a basic block.
#[derive(Debug)]
pub enum Terminator<'arena> {
    Goto {
        target: BlockId,
    },
    Branch {
        stmt: StmtId,
        cond_reads: Vec<LocalId, &'arena Arena>,
        cond_class: ExprClass,
        then_target: BlockId,
        else_target: BlockId,
        span: Span,
    },
    Return {
        stmt: StmtId,
        reads: Vec<LocalId, &'arena Arena>,
        expr_class: ExprClass,
        span: Span,
    },
    Break {
        stmt: StmtId,
        target: Option<BlockId>,
        span: Span,
    },
    Continue {
        stmt: StmtId,
        target: Option<BlockId>,
        span: Span,
    },
    Exit,
}

impl Terminator<'_> {
    /// Visits all successor blocks.
    pub fn successors(&self, mut f: impl FnMut(BlockId)) {
        match self {
            Terminator::Goto { target } => f(*target),
            Terminator::Branch { then_target, else_target, .. } => {
                f(*then_target);
                f(*else_target);
            }
            Terminator::Break { target, .. } | Terminator::Continue { target, .. } => {
                if let Some(target) = target {
                    f(*target);
                }
            }
            Terminator::Return { .. } | Terminator::Exit => {}
        }
    }

    /// Returns the statement that introduced this control transfer, if any.
    #[must_use]
    pub const fn stmt(&self) -> Option<StmtId> {
        match self {
            Terminator::Goto { .. } | Terminator::Exit => None,
            Terminator::Branch { stmt, .. }
            | Terminator::Return { stmt, .. }
            | Terminator::Break { stmt, .. }
            | Terminator::Continue { stmt, .. } => Some(*stmt),
        }
    }
}

/// Straight-line region in the side CFG.
#[derive(Debug)]
pub struct BasicBlock<'arena> {
    pub first_op: u32,
    pub op_count: u32,
    pub kill: Vec<LocalId, &'arena Arena>,
    pub terminator: Option<Terminator<'arena>>,
    pub unreachable_cause: Option<UnreachableCause>,
}

/// CFG for one function body.
#[derive(Debug)]
pub struct CfgFunction<'arena> {
    pub function: FunctionId,
    pub body_parent_stmt: Option<StmtId>,
    pub entry: BlockId,
    pub exit: BlockId,
    pub blocks: Vec<BasicBlock<'arena>, &'arena Arena>,
    pub ops: Vec<LinearOp<'arena>, &'arena Arena>,
}

impl<'arena> CfgFunction<'arena> {
    /// Returns the linear ops that belong to the given block.
    #[must_use]
    pub fn block_ops(&self, block: BlockId) -> &[LinearOp<'arena>] {
        self.block_ops_by_ref(&self.blocks[block.0 as usize])
    }

    /// Returns the linear ops that belong to one block record.
    #[must_use]
    pub fn block_ops_by_ref(&self, block: &BasicBlock<'arena>) -> &[LinearOp<'arena>] {
        let start = block.first_op as usize;
        let end = start + block.op_count as usize;
        &self.ops[start..end]
    }
}

/// CFG for the whole program.
#[derive(Debug)]
pub struct CfgProgram<'ast, 'arena> {
    pub functions: Vec<CfgFunction<'arena>, &'arena Arena>,
    pub stmts: Vec<StmtInfo<'ast>, &'arena Arena>,
}

impl<'ast> CfgProgram<'ast, '_> {
    /// Returns source-facing statement metadata by stable statement id.
    #[must_use]
    pub fn stmt_info(&self, stmt: StmtId) -> &StmtInfo<'ast> {
        &self.stmts[stmt.0 as usize]
    }
}

/// Counted CFG resources used to pre-size analysis storage and validate limits.
#[derive(Debug)]
pub struct ProgramCounts<'arena> {
    pub function_blocks: Vec<u32, &'arena Arena>,
    pub function_ops: Vec<u32, &'arena Arena>,
    pub total_blocks: u32,
    pub total_ops: u32,
    pub total_statements: u32,
}

#[derive(Clone, Copy)]
struct Cursor {
    block: Option<BlockId>,
    dead_cause: Option<UnreachableCause>,
}

#[derive(Clone, Copy)]
struct LoopContext {
    break_target: BlockId,
    continue_target: BlockId,
    body_scope: ScopeId,
}

struct FunctionBuilder<'ast, 'facts, 'arena> {
    facts: &'facts ProgramFacts<'ast, 'ast>,
    function: FunctionId,
    body_parent_stmt: Option<StmtId>,
    expected_blocks: u32,
    expected_ops: u32,
    blocks: Vec<BasicBlock<'arena>, &'arena Arena>,
    ops: Vec<LinearOp<'arena>, &'arena Arena>,
    arena: &'arena Arena,
}

struct ProgramBuilder<'ast, 'facts, 'count, 'arena> {
    facts: &'facts ProgramFacts<'ast, 'ast>,
    block_counts: &'count [u32],
    op_counts: &'count [u32],
    cfgs: Vec<Option<CfgFunction<'arena>>, &'arena Arena>,
    stmts: Vec<StmtInfo<'ast>, &'arena Arena>,
    arena: &'arena Arena,
}

#[derive(Clone, Copy)]
struct CountCursor {
    has_block: bool,
}

struct CountFunctionBuilder {
    blocks: u32,
    ops: u32,
}

struct CountProgramBuilder<'ast, 'facts, 'arena> {
    facts: &'facts ProgramFacts<'ast, 'ast>,
    function_blocks: Vec<u32, &'arena Arena>,
    function_ops: Vec<u32, &'arena Arena>,
    counted: Vec<bool, &'arena Arena>,
    arena: &'arena Arena,
}

impl<'ast, 'facts, 'count, 'arena> ProgramBuilder<'ast, 'facts, 'count, 'arena> {
    fn new(
        facts: &'facts ProgramFacts<'ast, 'ast>,
        total_statements: u32,
        block_counts: &'count [u32],
        op_counts: &'count [u32],
        arena: &'arena Arena,
    ) -> Self {
        let mut cfgs = Vec::with_capacity_in(facts.functions.len(), arena);
        for _ in 0..facts.functions.len() {
            cfgs.push(None);
        }
        Self {
            facts,
            block_counts,
            op_counts,
            cfgs,
            stmts: Vec::with_capacity_in(total_statements as usize, arena),
            arena,
        }
    }

    fn build(mut self) -> CfgProgram<'ast, 'arena> {
        self.build_function(self.facts.root_function, None);

        let mut functions = Vec::with_capacity_in(self.cfgs.len(), self.arena);
        for cfg in self.cfgs {
            functions.push(cfg.expect("Each function fact should lower to a CFG"));
        }

        CfgProgram { functions, stmts: self.stmts }
    }

    fn build_function(&mut self, function: FunctionId, body_parent_stmt: Option<StmtId>) {
        if self.cfgs[function.0 as usize].is_some() {
            return;
        }

        let info = self.facts.function(function);
        let mut builder = FunctionBuilder::with_capacity(
            self.facts,
            function,
            body_parent_stmt,
            self.block_counts[function.0 as usize],
            self.op_counts[function.0 as usize],
            self.arena,
        );
        let cfg = builder.build(info.body, self);
        self.cfgs[function.0 as usize] = Some(cfg);
    }
}

impl<'ast, 'facts, 'arena> FunctionBuilder<'ast, 'facts, 'arena> {
    fn with_capacity(
        facts: &'facts ProgramFacts<'ast, 'ast>,
        function: FunctionId,
        body_parent_stmt: Option<StmtId>,
        block_capacity: u32,
        op_capacity: u32,
        arena: &'arena Arena,
    ) -> Self {
        Self {
            facts,
            function,
            body_parent_stmt,
            expected_blocks: block_capacity,
            expected_ops: op_capacity,
            blocks: Vec::with_capacity_in(block_capacity as usize, arena),
            ops: Vec::with_capacity_in(op_capacity as usize, arena),
            arena,
        }
    }

    fn build(
        &mut self,
        body: BlockRef<'ast>,
        program: &mut ProgramBuilder<'ast, 'facts, '_, 'arena>,
    ) -> CfgFunction<'arena> {
        let root_scope = self
            .facts
            .scope_of_block(body)
            .expect("Each function body should map to a lexical scope");
        let mut scope_stack = Vec::new_in(self.arena);
        scope_stack.push(root_scope);

        let entry = self.new_block(None);
        let exit = self.new_block(None);
        let cursor = self.lower_block(
            body,
            Cursor { block: Some(entry), dead_cause: None },
            None,
            self.body_parent_stmt,
            &mut scope_stack,
            program,
        );

        if let Some(block) = cursor.block {
            self.set_terminator(block, Terminator::Goto { target: exit });
        }
        self.set_terminator(exit, Terminator::Exit);
        debug_assert_eq!(
            self.blocks.len() as u32,
            self.expected_blocks,
            "Count pass and CFG lowering should agree on block counts",
        );
        debug_assert_eq!(
            self.ops.len() as u32,
            self.expected_ops,
            "Count pass and CFG lowering should agree on op counts",
        );

        CfgFunction {
            function: self.function,
            body_parent_stmt: self.body_parent_stmt,
            entry,
            exit,
            blocks: std::mem::replace(&mut self.blocks, Vec::new_in(self.arena)),
            ops: std::mem::replace(&mut self.ops, Vec::new_in(self.arena)),
        }
    }

    fn lower_block(
        &mut self,
        block: BlockRef<'ast>,
        mut cursor: Cursor,
        loop_ctx: Option<LoopContext>,
        parent_stmt: Option<StmtId>,
        scope_stack: &mut Vec<ScopeId, &'arena Arena>,
        program: &mut ProgramBuilder<'ast, 'facts, '_, 'arena>,
    ) -> Cursor {
        debug_assert_eq!(
            scope_stack.last().copied(),
            self.facts.scope_of_block(block),
            "CFG lowering should track the active lexical scope stack",
        );
        for &stmt in block.stmts {
            cursor = self.lower_stmt(stmt, cursor, loop_ctx, parent_stmt, scope_stack, program);
        }
        cursor
    }

    fn lower_stmt(
        &mut self,
        stmt: StmtRef<'ast>,
        mut cursor: Cursor,
        loop_ctx: Option<LoopContext>,
        parent_stmt: Option<StmtId>,
        scope_stack: &mut Vec<ScopeId, &'arena Arena>,
        program: &mut ProgramBuilder<'ast, 'facts, '_, 'arena>,
    ) -> Cursor {
        match stmt {
            Stmt::Assign { span, .. }
            | Stmt::AssignExisting { span, .. }
            | Stmt::AssignIndex { span, .. }
            | Stmt::Expression { span, .. } => {
                let block = self.ensure_block(&mut cursor);
                let _ = self.push_stmt(program, block, stmt, *span, parent_stmt);
                cursor
            }
            Stmt::FunctionDef { body, span, .. } => {
                let block = self.ensure_block(&mut cursor);
                let stmt_id = self.push_stmt(program, block, stmt, *span, parent_stmt);
                if let Some(function) = self.facts.function_by_body(body) {
                    program.build_function(function, Some(stmt_id));
                }
                cursor
            }
            Stmt::Return { span, .. } => {
                let block = self.ensure_block(&mut cursor);
                let stmt_id = self.push_stmt(program, block, stmt, *span, parent_stmt);
                self.set_terminator(block, self.return_terminator(stmt_id, *span));
                Cursor { block: None, dead_cause: Some(UnreachableCause::Return) }
            }
            Stmt::Break { span } => {
                let block = self.ensure_block(&mut cursor);
                let stmt_id = self.push_stmt(program, block, stmt, *span, parent_stmt);
                if let Some(loop_ctx) = loop_ctx {
                    self.kill_scopes_through(block, scope_stack, loop_ctx.body_scope);
                }
                self.set_terminator(
                    block,
                    Terminator::Break {
                        stmt: stmt_id,
                        target: loop_ctx.map(|ctx| ctx.break_target),
                        span: *span,
                    },
                );
                Cursor { block: None, dead_cause: Some(UnreachableCause::Break) }
            }
            Stmt::Continue { span } => {
                let block = self.ensure_block(&mut cursor);
                let stmt_id = self.push_stmt(program, block, stmt, *span, parent_stmt);
                if let Some(loop_ctx) = loop_ctx {
                    self.kill_scopes_through(block, scope_stack, loop_ctx.body_scope);
                }
                self.set_terminator(
                    block,
                    Terminator::Continue {
                        stmt: stmt_id,
                        target: loop_ctx.map(|ctx| ctx.continue_target),
                        span: *span,
                    },
                );
                Cursor { block: None, dead_cause: Some(UnreachableCause::Continue) }
            }
            Stmt::Block { block: nested, span } => {
                let nested_scope = self
                    .facts
                    .scope_of_block(nested)
                    .expect("Each lexical block should map to a scope");
                let block = self.ensure_block(&mut cursor);
                let stmt_id = self.push_stmt(program, block, stmt, *span, parent_stmt);
                scope_stack.push(nested_scope);
                let nested_cursor =
                    self.lower_block(nested, cursor, loop_ctx, Some(stmt_id), scope_stack, program);
                scope_stack.pop();
                if let Some(block) = nested_cursor.block {
                    self.add_scope_kills(block, nested_scope);
                }
                if nested_cursor.block.is_some() {
                    nested_cursor
                } else {
                    Cursor { dead_cause: Some(UnreachableCause::NoFallthrough), ..nested_cursor }
                }
            }
            Stmt::If { then_b, else_b, span, .. } => {
                let then_scope = self
                    .facts
                    .scope_of_block(then_b)
                    .expect("Each then-block should map to a scope");
                let block = self.ensure_block(&mut cursor);
                let stmt_id = self.push_stmt(program, block, stmt, *span, parent_stmt);

                let then_entry = self.new_block(None);
                let else_entry = self.new_block(None);
                self.set_terminator(
                    block,
                    self.branch_terminator(stmt_id, *span, then_entry, else_entry),
                );

                scope_stack.push(then_scope);
                let then_cursor = self.lower_block(
                    then_b,
                    Cursor { block: Some(then_entry), dead_cause: None },
                    loop_ctx,
                    Some(stmt_id),
                    scope_stack,
                    program,
                );
                scope_stack.pop();
                if let Some(block) = then_cursor.block {
                    self.add_scope_kills(block, then_scope);
                }
                let else_cursor = if let Some(else_b) = else_b {
                    let else_scope = self
                        .facts
                        .scope_of_block(else_b)
                        .expect("Each else-block should map to a scope");
                    scope_stack.push(else_scope);
                    let else_cursor = self.lower_block(
                        else_b,
                        Cursor { block: Some(else_entry), dead_cause: None },
                        loop_ctx,
                        Some(stmt_id),
                        scope_stack,
                        program,
                    );
                    scope_stack.pop();
                    if let Some(block) = else_cursor.block {
                        self.add_scope_kills(block, else_scope);
                    }
                    else_cursor
                } else {
                    Cursor { block: Some(else_entry), dead_cause: None }
                };

                match (then_cursor.block, else_cursor.block) {
                    (Some(then_tail), Some(else_tail)) => {
                        let join = self.new_block(None);
                        self.set_terminator(then_tail, Terminator::Goto { target: join });
                        self.set_terminator(else_tail, Terminator::Goto { target: join });
                        Cursor { block: Some(join), dead_cause: None }
                    }
                    (Some(then_tail), None) => {
                        let join = self.new_block(None);
                        self.set_terminator(then_tail, Terminator::Goto { target: join });
                        Cursor { block: Some(join), dead_cause: None }
                    }
                    (None, Some(else_tail)) => {
                        let join = self.new_block(None);
                        self.set_terminator(else_tail, Terminator::Goto { target: join });
                        Cursor { block: Some(join), dead_cause: None }
                    }
                    (None, None) => {
                        Cursor { block: None, dead_cause: Some(UnreachableCause::NoFallthrough) }
                    }
                }
            }
            Stmt::Loop { body, span, .. } => {
                let body_scope =
                    self.facts.scope_of_block(body).expect("Each loop body should map to a scope");
                let pre_loop = self.ensure_block(&mut cursor);
                let cond_block = self.new_block(None);
                self.set_terminator(pre_loop, Terminator::Goto { target: cond_block });

                let stmt_id = self.push_stmt(program, cond_block, stmt, *span, parent_stmt);

                let body_entry = self.new_block(None);
                let exit = self.new_block(None);
                self.set_terminator(
                    cond_block,
                    self.branch_terminator(stmt_id, *span, body_entry, exit),
                );

                let loop_ctx =
                    LoopContext { break_target: exit, continue_target: cond_block, body_scope };
                scope_stack.push(body_scope);
                let body_cursor = self.lower_block(
                    body,
                    Cursor { block: Some(body_entry), dead_cause: None },
                    Some(loop_ctx),
                    Some(stmt_id),
                    scope_stack,
                    program,
                );
                scope_stack.pop();
                if let Some(body_tail) = body_cursor.block {
                    self.add_scope_kills(body_tail, body_scope);
                    self.set_terminator(body_tail, Terminator::Goto { target: cond_block });
                }

                Cursor { block: Some(exit), dead_cause: None }
            }
        }
    }

    fn push_stmt(
        &mut self,
        program: &mut ProgramBuilder<'ast, 'facts, '_, 'arena>,
        block: BlockId,
        stmt: StmtRef<'ast>,
        span: Span,
        parent: Option<StmtId>,
    ) -> StmtId {
        let id = StmtId(program.stmts.len() as u32);
        debug_assert!(
            std::ptr::eq(self.facts.stmt_effect(id).stmt, stmt),
            "Resolver and CFG traversals should assign statement ids in the same order",
        );
        debug_assert_eq!(self.facts.stmt_effect(id).function, self.function);
        let op = self.push_op(block, id, span);
        program.stmts.push(StmtInfo {
            stmt,
            function: self.function,
            parent,
            span,
            kind: stmt_kind(stmt),
            block,
            op,
        });
        id
    }

    fn new_block(&mut self, cause: Option<UnreachableCause>) -> BlockId {
        let id = BlockId(self.blocks.len() as u32);
        self.blocks.push(BasicBlock {
            first_op: 0,
            op_count: 0,
            kill: Vec::new_in(self.arena),
            terminator: None,
            unreachable_cause: cause,
        });
        id
    }

    fn ensure_block(&mut self, cursor: &mut Cursor) -> BlockId {
        if let Some(block) = cursor.block {
            block
        } else {
            let block = self.new_block(cursor.dead_cause);
            cursor.block = Some(block);
            block
        }
    }

    fn push_op(&mut self, block: BlockId, stmt: StmtId, span: Span) -> OpId {
        let op = LinearOp::from_stmt(self.facts, stmt, span, self.arena);
        let op_id = op.id;
        self.append_op_to_block(block, op);
        op_id
    }

    fn append_op_to_block(&mut self, block: BlockId, op: LinearOp<'arena>) {
        let block = &mut self.blocks[block.0 as usize];
        if block.op_count == 0 {
            block.first_op = self.ops.len() as u32;
        } else {
            debug_assert_eq!(
                block.first_op + block.op_count,
                self.ops.len() as u32,
                "Lowering should append each block's ops as one contiguous run",
            );
        }
        block.op_count += 1;
        self.ops.push(op);
    }

    fn set_terminator(&mut self, block: BlockId, terminator: Terminator<'arena>) {
        let slot = &mut self.blocks[block.0 as usize].terminator;
        if slot.is_none() {
            *slot = Some(terminator);
        }
    }

    fn add_scope_kills(&mut self, block: BlockId, scope: ScopeId) {
        let kill = &mut self.blocks[block.0 as usize].kill;
        for &local in self.facts.scope_locals(scope) {
            if self.facts.locals[local.0 as usize].owner == self.function && !kill.contains(&local)
            {
                kill.push(local);
            }
        }
    }

    fn kill_scopes_through(&mut self, block: BlockId, scope_stack: &[ScopeId], boundary: ScopeId) {
        for &scope in scope_stack.iter().rev() {
            self.add_scope_kills(block, scope);
            if scope == boundary {
                break;
            }
        }
    }

    fn branch_terminator(
        &self,
        stmt: StmtId,
        span: Span,
        then_target: BlockId,
        else_target: BlockId,
    ) -> Terminator<'arena> {
        let effect = self.facts.stmt_effect(stmt);
        Terminator::Branch {
            stmt,
            cond_reads: clone_slice_in(&effect.reads, self.arena),
            cond_class: effect.expr_class,
            then_target,
            else_target,
            span,
        }
    }

    fn return_terminator(&self, stmt: StmtId, span: Span) -> Terminator<'arena> {
        let effect = self.facts.stmt_effect(stmt);
        Terminator::Return {
            stmt,
            reads: clone_slice_in(&effect.reads, self.arena),
            expr_class: effect.expr_class,
            span,
        }
    }
}

impl<'ast, 'facts, 'arena> CountProgramBuilder<'ast, 'facts, 'arena> {
    fn new(facts: &'facts ProgramFacts<'ast, 'ast>, arena: &'arena Arena) -> Self {
        let mut function_blocks = Vec::with_capacity_in(facts.functions.len(), arena);
        function_blocks.resize(facts.functions.len(), 0);

        let mut function_ops = Vec::with_capacity_in(facts.functions.len(), arena);
        function_ops.resize(facts.functions.len(), 0);

        let mut counted = Vec::with_capacity_in(facts.functions.len(), arena);
        counted.resize(facts.functions.len(), false);

        Self { facts, function_blocks, function_ops, counted, arena }
    }

    fn build(mut self) -> ProgramCounts<'arena> {
        self.count_function(self.facts.root_function);
        let total_blocks = self.function_blocks.iter().copied().sum();
        let total_ops = self.function_ops.iter().copied().sum();
        debug_assert_eq!(total_ops, self.facts.stmt_effects.len() as u32);
        ProgramCounts {
            function_blocks: self.function_blocks,
            function_ops: self.function_ops,
            total_blocks,
            total_ops,
            total_statements: self.facts.stmt_effects.len() as u32,
        }
    }

    fn count_function(&mut self, function: FunctionId) {
        let slot = &mut self.counted[function.0 as usize];
        if *slot {
            return;
        }
        *slot = true;

        let info = self.facts.function(function);
        let mut builder = CountFunctionBuilder { blocks: 0, ops: 0 };
        let (blocks, ops) = builder.count(info.body);
        self.function_blocks[function.0 as usize] = blocks;
        self.function_ops[function.0 as usize] = ops;

        // The explicit stack bounds nested-function discovery without recursive AST walks.
        let mut stack = Vec::new_in(self.arena);
        for &stmt in info.body.stmts.iter().rev() {
            stack.push(stmt);
        }
        while let Some(stmt) = stack.pop() {
            match stmt {
                Stmt::FunctionDef { body, .. } => {
                    if let Some(function) = self.facts.function_by_body(body) {
                        self.count_function(function);
                    }
                }
                Stmt::If { then_b, else_b, .. } => {
                    if let Some(else_b) = else_b {
                        for &nested in else_b.stmts.iter().rev() {
                            stack.push(nested);
                        }
                    }
                    for &nested in then_b.stmts.iter().rev() {
                        stack.push(nested);
                    }
                }
                Stmt::Loop { body, .. } | Stmt::Block { block: body, .. } => {
                    for &nested in body.stmts.iter().rev() {
                        stack.push(nested);
                    }
                }
                Stmt::Assign { .. }
                | Stmt::AssignExisting { .. }
                | Stmt::AssignIndex { .. }
                | Stmt::Return { .. }
                | Stmt::Break { .. }
                | Stmt::Continue { .. }
                | Stmt::Expression { .. } => {}
            }
        }
    }
}

impl CountFunctionBuilder {
    fn count(&mut self, body: BlockRef<'_>) -> (u32, u32) {
        self.blocks = 2;
        self.ops = 0;
        let _ = self.count_block(body, CountCursor { has_block: true }, false);
        (self.blocks, self.ops)
    }

    fn count_block(
        &mut self,
        block: BlockRef<'_>,
        mut cursor: CountCursor,
        in_loop: bool,
    ) -> CountCursor {
        for &stmt in block.stmts {
            cursor = self.count_stmt(stmt, cursor, in_loop);
        }
        cursor
    }

    fn count_stmt(
        &mut self,
        stmt: StmtRef<'_>,
        mut cursor: CountCursor,
        in_loop: bool,
    ) -> CountCursor {
        self.ops += 1;
        match stmt {
            Stmt::Assign { .. }
            | Stmt::AssignExisting { .. }
            | Stmt::AssignIndex { .. }
            | Stmt::Expression { .. }
            | Stmt::FunctionDef { .. } => {
                self.ensure_block(&mut cursor);
                cursor
            }
            Stmt::Return { .. } => {
                self.ensure_block(&mut cursor);
                CountCursor { has_block: false }
            }
            Stmt::Break { .. } | Stmt::Continue { .. } => {
                self.ensure_block(&mut cursor);
                let _ = in_loop;
                CountCursor { has_block: false }
            }
            Stmt::Block { block, .. } => {
                self.ensure_block(&mut cursor);
                let nested_cursor = self.count_block(block, cursor, in_loop);
                if nested_cursor.has_block {
                    nested_cursor
                } else {
                    CountCursor { has_block: false }
                }
            }
            Stmt::If { then_b, else_b, .. } => {
                self.ensure_block(&mut cursor);
                self.new_block();
                self.new_block();

                let then_cursor =
                    self.count_block(then_b, CountCursor { has_block: true }, in_loop);
                let else_cursor = if let Some(else_b) = else_b {
                    self.count_block(else_b, CountCursor { has_block: true }, in_loop)
                } else {
                    CountCursor { has_block: true }
                };

                if then_cursor.has_block || else_cursor.has_block {
                    self.new_block();
                    CountCursor { has_block: true }
                } else {
                    CountCursor { has_block: false }
                }
            }
            Stmt::Loop { body, .. } => {
                self.ensure_block(&mut cursor);
                self.new_block();
                self.new_block();
                self.new_block();
                let _ = self.count_block(body, CountCursor { has_block: true }, true);
                CountCursor { has_block: true }
            }
        }
    }

    fn ensure_block(&mut self, cursor: &mut CountCursor) {
        if !cursor.has_block {
            self.new_block();
            cursor.has_block = true;
        }
    }

    fn new_block(&mut self) {
        self.blocks += 1;
    }
}

fn stmt_kind(stmt: StmtRef<'_>) -> StmtKind {
    match stmt {
        Stmt::Assign { .. } => StmtKind::Assign,
        Stmt::AssignExisting { .. } => StmtKind::AssignExisting,
        Stmt::AssignIndex { .. } => StmtKind::AssignIndex,
        Stmt::If { .. } => StmtKind::If,
        Stmt::Loop { .. } => StmtKind::Loop,
        Stmt::Block { .. } => StmtKind::Block,
        Stmt::FunctionDef { .. } => StmtKind::FunctionDef,
        Stmt::Return { .. } => StmtKind::Return,
        Stmt::Break { .. } => StmtKind::Break,
        Stmt::Continue { .. } => StmtKind::Continue,
        Stmt::Expression { .. } => StmtKind::Expression,
    }
}

/// Counts CFG resources for the given program.
#[must_use]
pub fn count_program<'ast, 'arena>(
    facts: &ProgramFacts<'ast, 'ast>,
    arena: &'arena Arena,
) -> ProgramCounts<'arena> {
    CountProgramBuilder::new(facts, arena).build()
}

/// Builds the analysis CFG for the given program using precomputed counts.
#[must_use]
pub fn build_program_with_counts<'ast, 'arena>(
    facts: &ProgramFacts<'ast, 'ast>,
    counts: &ProgramCounts<'_>,
    arena: &'arena Arena,
) -> CfgProgram<'ast, 'arena> {
    ProgramBuilder::new(
        facts,
        counts.total_statements,
        &counts.function_blocks,
        &counts.function_ops,
        arena,
    )
    .build()
}

/// Builds the analysis CFG for the given program.
pub fn build_program<'ast, 'arena>(
    facts: &ProgramFacts<'ast, 'ast>,
    arena: &'arena Arena,
) -> CfgProgram<'ast, 'arena> {
    let counts = count_program(facts, arena);
    build_program_with_counts(facts, &counts, arena)
}

/// Renders the CFG into a deterministic text form for debugging and tests.
#[must_use]
pub fn debug_dump_program<'ast, 'arena>(
    program: &CfgProgram<'ast, 'arena>,
    facts: &ProgramFacts<'ast, 'ast>,
    arena: &'arena Arena,
) -> ArenaString<'arena> {
    let mut out = ArenaString::new_in(arena);
    for (function_idx, function) in program.functions.iter().enumerate() {
        let function_id = FunctionId(function_idx as u32);
        let info = facts.function(function_id);
        let _ = writeln!(
            out,
            "fn {} {} entry=b{} exit=b{} ops={}",
            function_id.0,
            info.name,
            function.entry.0,
            function.exit.0,
            function.ops.len()
        );
        for (block_idx, block) in function.blocks.iter().enumerate() {
            let cause = match block.unreachable_cause {
                Some(UnreachableCause::Return) => " return-dead",
                Some(UnreachableCause::Break) => " break-dead",
                Some(UnreachableCause::Continue) => " continue-dead",
                Some(UnreachableCause::NoFallthrough) => " no-fallthrough",
                None => "",
            };
            let _ = writeln!(
                out,
                "  b{block_idx}{cause} first_op={} op_count={}",
                block.first_op, block.op_count
            );
            for op in function.block_ops_by_ref(block) {
                let stmt = &program.stmts[op.stmt.0 as usize];
                let _ = writeln!(
                    out,
                    "    o{} s{} {:?} {}..{}",
                    op.id.0, op.stmt.0, stmt.kind, stmt.span.start, stmt.span.end
                );
            }
            if !block.kill.is_empty() {
                let _ = write!(out, "    kill");
                for &local in &block.kill {
                    let _ = write!(out, " l{}", local.0);
                }
                let _ = writeln!(out);
            }
            let _ = write!(out, "    term ");
            write_terminator_label(&mut out, block.terminator.as_ref());
            let _ = writeln!(out);
        }
    }
    out
}

fn write_terminator_label(out: &mut ArenaString<'_>, terminator: Option<&Terminator<'_>>) {
    match terminator {
        Some(Terminator::Goto { target }) => {
            let _ = write!(out, "goto b{}", target.0);
        }
        Some(Terminator::Branch { stmt, then_target, else_target, .. }) => {
            let _ = write!(out, "branch s{} b{} b{}", stmt.0, then_target.0, else_target.0);
        }
        Some(Terminator::Return { stmt, .. }) => {
            let _ = write!(out, "return s{}", stmt.0);
        }
        Some(Terminator::Break { stmt, target, .. }) => match target {
            Some(target) => {
                let _ = write!(out, "break s{} b{}", stmt.0, target.0);
            }
            None => {
                let _ = write!(out, "break s{} <invalid>", stmt.0);
            }
        },
        Some(Terminator::Continue { stmt, target, .. }) => match target {
            Some(target) => {
                let _ = write!(out, "continue s{} b{}", stmt.0, target.0);
            }
            None => {
                let _ = write!(out, "continue s{} <invalid>", stmt.0);
            }
        },
        Some(Terminator::Exit) => out.push_str("exit"),
        None => out.push_str("<none>"),
    }
}

impl<'arena> LinearOp<'arena> {
    fn from_stmt(
        facts: &ProgramFacts<'_, '_>,
        stmt: StmtId,
        span: Span,
        arena: &'arena Arena,
    ) -> Self {
        let effect = facts.stmt_effect(stmt);
        Self {
            // The current lowering keeps one op per statement so dataflow and
            // source-oriented diagnostics stay aligned while the executor still
            // walks statements directly.
            id: OpId(stmt.0),
            stmt,
            reads: clone_slice_in(&effect.reads, arena),
            writes: clone_slice_in(&effect.writes, arena),
            direct_callees: clone_slice_in(&effect.direct_callees, arena),
            expr_class: effect.expr_class,
            span,
        }
    }
}

fn clone_slice_in<'arena, T: Copy>(items: &[T], arena: &'arena Arena) -> Vec<T, &'arena Arena> {
    let mut cloned = Vec::with_capacity_in(items.len(), arena);
    cloned.extend(items.iter().copied());
    cloned
}
