//! Side CFG used for source-oriented analyses.

use std::fmt::Write;

use crate::analysis::facts::ProgramFacts;
use crate::analysis::ids::{BlockId, FunctionId, StmtId};
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

/// Metadata about a statement lowered into the CFG.
#[derive(Debug)]
pub struct StmtInfo<'ast> {
    pub stmt: StmtRef<'ast>,
    pub function: FunctionId,
    pub parent: Option<StmtId>,
    pub span: Span,
    pub kind: StmtKind,
}

/// Terminator of a basic block.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Terminator {
    Goto { target: BlockId },
    Branch { then_target: BlockId, else_target: BlockId, span: Span },
    Return { span: Span },
    Break { target: Option<BlockId>, span: Span },
    Continue { target: Option<BlockId>, span: Span },
    Exit,
}

impl Terminator {
    /// Visits all successor blocks.
    pub fn successors(self, mut f: impl FnMut(BlockId)) {
        match self {
            Terminator::Goto { target } => f(target),
            Terminator::Branch { then_target, else_target, .. } => {
                f(then_target);
                f(else_target);
            }
            Terminator::Break { target, .. } | Terminator::Continue { target, .. } => {
                if let Some(target) = target {
                    f(target);
                }
            }
            Terminator::Return { .. } | Terminator::Exit => {}
        }
    }
}

/// Straight-line region in the side CFG.
#[derive(Debug)]
pub struct BasicBlock<'arena> {
    pub stmts: Vec<StmtId, &'arena Arena>,
    pub terminator: Option<Terminator>,
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
}

/// CFG for the whole program.
#[derive(Debug)]
pub struct CfgProgram<'ast, 'arena> {
    pub functions: Vec<CfgFunction<'arena>, &'arena Arena>,
    pub stmts: Vec<StmtInfo<'ast>, &'arena Arena>,
}

/// Counted CFG resources used to pre-size analysis storage and validate limits.
#[derive(Debug)]
pub struct ProgramCounts<'arena> {
    pub function_blocks: Vec<u32, &'arena Arena>,
    pub total_blocks: u32,
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
}

struct FunctionBuilder<'ast, 'facts, 'arena> {
    facts: &'facts ProgramFacts<'ast, 'ast>,
    function: FunctionId,
    body_parent_stmt: Option<StmtId>,
    expected_blocks: u32,
    blocks: Vec<BasicBlock<'arena>, &'arena Arena>,
    arena: &'arena Arena,
}

struct ProgramBuilder<'ast, 'facts, 'count, 'arena> {
    facts: &'facts ProgramFacts<'ast, 'ast>,
    block_counts: &'count [u32],
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
}

struct CountProgramBuilder<'ast, 'facts, 'arena> {
    facts: &'facts ProgramFacts<'ast, 'ast>,
    function_blocks: Vec<u32, &'arena Arena>,
    counted: Vec<bool, &'arena Arena>,
    arena: &'arena Arena,
}

impl<'ast, 'facts, 'count, 'arena> ProgramBuilder<'ast, 'facts, 'count, 'arena> {
    fn new(
        facts: &'facts ProgramFacts<'ast, 'ast>,
        total_statements: u32,
        block_counts: &'count [u32],
        arena: &'arena Arena,
    ) -> Self {
        let mut cfgs = Vec::with_capacity_in(facts.functions.len(), arena);
        for _ in 0..facts.functions.len() {
            cfgs.push(None);
        }
        Self {
            facts,
            block_counts,
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
        arena: &'arena Arena,
    ) -> Self {
        Self {
            facts,
            function,
            body_parent_stmt,
            expected_blocks: block_capacity,
            blocks: Vec::with_capacity_in(block_capacity as usize, arena),
            arena,
        }
    }

    fn build(
        &mut self,
        body: BlockRef<'ast>,
        program: &mut ProgramBuilder<'ast, 'facts, '_, 'arena>,
    ) -> CfgFunction<'arena> {
        let entry = self.new_block(None);
        let exit = self.new_block(None);
        let cursor = self.lower_block(
            body,
            Cursor { block: Some(entry), dead_cause: None },
            None,
            self.body_parent_stmt,
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

        CfgFunction {
            function: self.function,
            body_parent_stmt: self.body_parent_stmt,
            entry,
            exit,
            blocks: std::mem::replace(&mut self.blocks, Vec::new_in(self.arena)),
        }
    }

    fn lower_block(
        &mut self,
        block: BlockRef<'ast>,
        mut cursor: Cursor,
        loop_ctx: Option<LoopContext>,
        parent_stmt: Option<StmtId>,
        program: &mut ProgramBuilder<'ast, 'facts, '_, 'arena>,
    ) -> Cursor {
        for &stmt in block.stmts {
            cursor = self.lower_stmt(stmt, cursor, loop_ctx, parent_stmt, program);
        }
        cursor
    }

    fn lower_stmt(
        &mut self,
        stmt: StmtRef<'ast>,
        mut cursor: Cursor,
        loop_ctx: Option<LoopContext>,
        parent_stmt: Option<StmtId>,
        program: &mut ProgramBuilder<'ast, 'facts, '_, 'arena>,
    ) -> Cursor {
        match stmt {
            Stmt::Assign { span, .. }
            | Stmt::AssignExisting { span, .. }
            | Stmt::AssignIndex { span, .. }
            | Stmt::Expression { span, .. } => {
                let block = self.ensure_block(&mut cursor);
                let stmt_id = self.push_stmt(program, stmt, *span, parent_stmt);
                self.blocks[block.0 as usize].stmts.push(stmt_id);
                cursor
            }
            Stmt::FunctionDef { body, span, .. } => {
                let block = self.ensure_block(&mut cursor);
                let stmt_id = self.push_stmt(program, stmt, *span, parent_stmt);
                self.blocks[block.0 as usize].stmts.push(stmt_id);
                if let Some(function) = self.facts.function_by_body(body) {
                    program.build_function(function, Some(stmt_id));
                }
                cursor
            }
            Stmt::Return { span, .. } => {
                let block = self.ensure_block(&mut cursor);
                let stmt_id = self.push_stmt(program, stmt, *span, parent_stmt);
                self.blocks[block.0 as usize].stmts.push(stmt_id);
                self.set_terminator(block, Terminator::Return { span: *span });
                Cursor { block: None, dead_cause: Some(UnreachableCause::Return) }
            }
            Stmt::Break { span } => {
                let block = self.ensure_block(&mut cursor);
                let stmt_id = self.push_stmt(program, stmt, *span, parent_stmt);
                self.blocks[block.0 as usize].stmts.push(stmt_id);
                self.set_terminator(
                    block,
                    Terminator::Break { target: loop_ctx.map(|ctx| ctx.break_target), span: *span },
                );
                Cursor { block: None, dead_cause: Some(UnreachableCause::Break) }
            }
            Stmt::Continue { span } => {
                let block = self.ensure_block(&mut cursor);
                let stmt_id = self.push_stmt(program, stmt, *span, parent_stmt);
                self.blocks[block.0 as usize].stmts.push(stmt_id);
                self.set_terminator(
                    block,
                    Terminator::Continue {
                        target: loop_ctx.map(|ctx| ctx.continue_target),
                        span: *span,
                    },
                );
                Cursor { block: None, dead_cause: Some(UnreachableCause::Continue) }
            }
            Stmt::Block { block: nested, span } => {
                let block = self.ensure_block(&mut cursor);
                let stmt_id = self.push_stmt(program, stmt, *span, parent_stmt);
                self.blocks[block.0 as usize].stmts.push(stmt_id);
                let nested_cursor =
                    self.lower_block(nested, cursor, loop_ctx, Some(stmt_id), program);
                if nested_cursor.block.is_some() {
                    nested_cursor
                } else {
                    Cursor { dead_cause: Some(UnreachableCause::NoFallthrough), ..nested_cursor }
                }
            }
            Stmt::If { then_b, else_b, span, .. } => {
                let block = self.ensure_block(&mut cursor);
                let stmt_id = self.push_stmt(program, stmt, *span, parent_stmt);
                self.blocks[block.0 as usize].stmts.push(stmt_id);

                let then_entry = self.new_block(None);
                let else_entry = self.new_block(None);
                self.set_terminator(
                    block,
                    Terminator::Branch {
                        then_target: then_entry,
                        else_target: else_entry,
                        span: *span,
                    },
                );

                let then_cursor = self.lower_block(
                    then_b,
                    Cursor { block: Some(then_entry), dead_cause: None },
                    loop_ctx,
                    Some(stmt_id),
                    program,
                );
                let else_cursor = if let Some(else_b) = else_b {
                    self.lower_block(
                        else_b,
                        Cursor { block: Some(else_entry), dead_cause: None },
                        loop_ctx,
                        Some(stmt_id),
                        program,
                    )
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
                let pre_loop = self.ensure_block(&mut cursor);
                let cond_block = self.new_block(None);
                self.set_terminator(pre_loop, Terminator::Goto { target: cond_block });

                let stmt_id = self.push_stmt(program, stmt, *span, parent_stmt);
                self.blocks[cond_block.0 as usize].stmts.push(stmt_id);

                let body_entry = self.new_block(None);
                let exit = self.new_block(None);
                self.set_terminator(
                    cond_block,
                    Terminator::Branch { then_target: body_entry, else_target: exit, span: *span },
                );

                let loop_ctx = LoopContext { break_target: exit, continue_target: cond_block };
                let body_cursor = self.lower_block(
                    body,
                    Cursor { block: Some(body_entry), dead_cause: None },
                    Some(loop_ctx),
                    Some(stmt_id),
                    program,
                );
                if let Some(body_tail) = body_cursor.block {
                    self.set_terminator(body_tail, Terminator::Goto { target: cond_block });
                }

                Cursor { block: Some(exit), dead_cause: None }
            }
        }
    }

    fn push_stmt(
        &mut self,
        program: &mut ProgramBuilder<'ast, 'facts, '_, 'arena>,
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
        program.stmts.push(StmtInfo {
            stmt,
            function: self.function,
            parent,
            span,
            kind: stmt_kind(stmt),
        });
        id
    }

    fn new_block(&mut self, cause: Option<UnreachableCause>) -> BlockId {
        let id = BlockId(self.blocks.len() as u32);
        self.blocks.push(BasicBlock {
            stmts: Vec::new_in(self.arena),
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

    fn set_terminator(&mut self, block: BlockId, terminator: Terminator) {
        let slot = &mut self.blocks[block.0 as usize].terminator;
        if slot.is_none() {
            *slot = Some(terminator);
        }
    }
}

impl<'ast, 'facts, 'arena> CountProgramBuilder<'ast, 'facts, 'arena> {
    fn new(facts: &'facts ProgramFacts<'ast, 'ast>, arena: &'arena Arena) -> Self {
        let mut function_blocks = Vec::with_capacity_in(facts.functions.len(), arena);
        function_blocks.resize(facts.functions.len(), 0);

        let mut counted = Vec::with_capacity_in(facts.functions.len(), arena);
        counted.resize(facts.functions.len(), false);

        Self { facts, function_blocks, counted, arena }
    }

    fn build(mut self) -> ProgramCounts<'arena> {
        self.count_function(self.facts.root_function);
        let total_blocks = self.function_blocks.iter().copied().sum();
        ProgramCounts {
            function_blocks: self.function_blocks,
            total_blocks,
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
        let mut builder = CountFunctionBuilder { blocks: 0 };
        let blocks = builder.count(info.body);
        self.function_blocks[function.0 as usize] = blocks;

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
    fn count(&mut self, body: BlockRef<'_>) -> u32 {
        self.blocks = 2;
        let _ = self.count_block(body, CountCursor { has_block: true }, false);
        self.blocks
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
    ProgramBuilder::new(facts, counts.total_statements, &counts.function_blocks, arena).build()
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
            "fn {} {} entry=b{} exit=b{}",
            function_id.0, info.name, function.entry.0, function.exit.0
        );
        for (block_idx, block) in function.blocks.iter().enumerate() {
            let cause = match block.unreachable_cause {
                Some(UnreachableCause::Return) => " return-dead",
                Some(UnreachableCause::Break) => " break-dead",
                Some(UnreachableCause::Continue) => " continue-dead",
                Some(UnreachableCause::NoFallthrough) => " no-fallthrough",
                None => "",
            };
            let _ = writeln!(out, "  b{block_idx}{cause}");
            for &stmt_id in &block.stmts {
                let stmt = &program.stmts[stmt_id.0 as usize];
                let _ = writeln!(
                    out,
                    "    s{} {:?} {}..{}",
                    stmt_id.0, stmt.kind, stmt.span.start, stmt.span.end
                );
            }
            let _ = write!(out, "    term ");
            write_terminator_label(&mut out, block.terminator);
            let _ = writeln!(out);
        }
    }
    out
}

fn write_terminator_label(out: &mut ArenaString<'_>, terminator: Option<Terminator>) {
    match terminator {
        Some(Terminator::Goto { target }) => {
            let _ = write!(out, "goto b{}", target.0);
        }
        Some(Terminator::Branch { then_target, else_target, .. }) => {
            let _ = write!(out, "branch b{} b{}", then_target.0, else_target.0);
        }
        Some(Terminator::Return { .. }) => out.push_str("return"),
        Some(Terminator::Break { target, .. }) => match target {
            Some(target) => {
                let _ = write!(out, "break b{}", target.0);
            }
            None => out.push_str("break <invalid>"),
        },
        Some(Terminator::Continue { target, .. }) => match target {
            Some(target) => {
                let _ = write!(out, "continue b{}", target.0);
            }
            None => out.push_str("continue <invalid>"),
        },
        Some(Terminator::Exit) => out.push_str("exit"),
        None => out.push_str("<none>"),
    }
}
