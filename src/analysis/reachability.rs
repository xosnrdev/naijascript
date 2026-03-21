//! Forward reachability analysis over the side CFG.

use crate::analysis::cfg::{CfgProgram, StmtKind, UnreachableCause};
use crate::analysis::ids::{BlockId, StmtId};
use crate::arena::Arena;
use crate::diagnostics::Span;
use crate::syntax::parser::StmtRef;

/// Source-oriented unreachable statement warning.
#[derive(Debug)]
pub struct UnreachableStmt<'ast> {
    pub stmt_id: StmtId,
    pub stmt: StmtRef<'ast>,
    pub span: Span,
    pub message: &'static str,
}

/// Computes reachability for every lowered statement in the CFG.
pub fn reachable_statement_mask<'arena>(
    program: &'arena CfgProgram<'_, 'arena>,
    arena: &'arena Arena,
) -> Vec<bool, &'arena Arena> {
    let mut stmt_reachable = Vec::with_capacity_in(program.stmts.len(), arena);
    stmt_reachable.resize(program.stmts.len(), false);

    for function in &program.functions {
        let mut block_reachable = Vec::with_capacity_in(function.blocks.len(), arena);
        block_reachable.resize(function.blocks.len(), false);

        let mut worklist = Vec::new_in(arena);
        worklist.push(function.entry);

        while let Some(block) = worklist.pop() {
            let slot = &mut block_reachable[block.0 as usize];
            if *slot {
                continue;
            }
            *slot = true;

            let block_info = &function.blocks[block.0 as usize];
            for stmt in &block_info.stmts {
                stmt_reachable[stmt.0 as usize] = true;
            }

            if let Some(terminator) = block_info.terminator {
                terminator.successors(|successor| {
                    if !block_reachable[successor.0 as usize] {
                        worklist.push(successor);
                    }
                });
            }
        }
    }

    stmt_reachable
}

/// Computes unreachable statements for the given CFG program.
pub fn unreachable_statements<'ast, 'arena>(
    program: &'arena CfgProgram<'ast, 'arena>,
    arena: &'arena Arena,
) -> Vec<UnreachableStmt<'ast>, &'arena Arena> {
    let stmt_reachable = reachable_statement_mask(program, arena);
    let mut warnings = Vec::new_in(arena);
    for (stmt_idx, info) in program.stmts.iter().enumerate() {
        if stmt_reachable[stmt_idx] {
            continue;
        }

        if let Some(parent) = info.parent
            && !stmt_reachable[parent.0 as usize]
        {
            continue;
        }

        let function = &program.functions[info.function.0 as usize];
        let block_id = statement_block(function, StmtId(stmt_idx as u32))
            .expect("Each lowered statement should belong to a block");
        let cause = function.blocks[block_id.0 as usize].unreachable_cause;

        warnings.push(UnreachableStmt {
            stmt_id: StmtId(stmt_idx as u32),
            stmt: info.stmt,
            span: info.span,
            message: unreachable_message(cause, info.kind),
        });
    }

    warnings
}

fn statement_block(
    function: &crate::analysis::cfg::CfgFunction<'_>,
    stmt: StmtId,
) -> Option<BlockId> {
    function
        .blocks
        .iter()
        .enumerate()
        .find_map(|(idx, block)| block.stmts.contains(&stmt).then_some(BlockId(idx as u32)))
}

fn unreachable_message(cause: Option<UnreachableCause>, kind: StmtKind) -> &'static str {
    match cause {
        Some(UnreachableCause::Return) => "Dead code after `return` statement",
        Some(UnreachableCause::Break) => "Dead code after `comot` statement",
        Some(UnreachableCause::Continue) => "Dead code after `next` statement",
        Some(UnreachableCause::NoFallthrough) | None => match kind {
            StmtKind::FunctionDef => "This function definition cannot execute",
            _ => "This statement cannot execute",
        },
    }
}
