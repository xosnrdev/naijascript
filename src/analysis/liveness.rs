//! Backward liveness analysis for local assignments.

use crate::analysis::cfg::{CfgProgram, StmtKind};
use crate::analysis::facts::ProgramFacts;
use crate::analysis::ids::{FunctionId, LocalId, StmtId};
use crate::analysis::summary::FunctionSummary;
use crate::arena::Arena;
use crate::diagnostics::Span;
use crate::syntax::parser::StmtRef;

type BitSet<'arena> = Vec<u64, &'arena Arena>;

/// Source-oriented warning for an assignment whose stored value is never read.
#[derive(Debug)]
pub struct UnusedAssignment<'ast> {
    pub stmt_id: StmtId,
    pub stmt: StmtRef<'ast>,
    pub span: Span,
    pub local: LocalId,
    pub local_name: &'ast str,
}

type SummarySlice<'arena> = [FunctionSummary<'arena>];

/// Computes unused-assignment warnings for reachable assignment statements.
pub fn unused_assignments<'ast, 'arena>(
    program: &'arena CfgProgram<'ast, 'arena>,
    facts: &ProgramFacts<'ast, 'ast>,
    summaries: &'arena SummarySlice<'arena>,
    reachable: &[bool],
    arena: &'arena Arena,
) -> Vec<UnusedAssignment<'ast>, &'arena Arena> {
    let mut warnings = Vec::new_in(arena);

    for function_cfg in &program.functions {
        let function = function_cfg.function;
        let local_range = facts.local_range(function);
        let local_count = local_range.end - local_range.start;
        if local_count == 0 {
            continue;
        }

        let block_count = function_cfg.blocks.len();
        let mut live_in = Vec::with_capacity_in(block_count, arena);
        let mut live_out = Vec::with_capacity_in(block_count, arena);
        for _ in 0..block_count {
            live_in.push(new_bitset(local_count, arena));
            live_out.push(new_bitset(local_count, arena));
        }

        let mut changed = true;
        while changed {
            changed = false;

            for block_idx in (0..block_count).rev() {
                let block = &function_cfg.blocks[block_idx];
                let mut new_live_out = new_bitset(local_count, arena);
                if let Some(terminator) = block.terminator {
                    terminator.successors(|successor| {
                        union_from(&mut new_live_out, &live_in[successor.0 as usize]);
                    });
                }

                let mut new_live_in = clone_bitset(&new_live_out, arena);
                for stmt in block.stmts.iter().rev().copied() {
                    apply_stmt_transfer(
                        &mut new_live_in,
                        stmt,
                        function,
                        facts,
                        summaries,
                        local_range.start,
                    );
                }

                if live_out[block_idx] != new_live_out {
                    live_out[block_idx] = new_live_out;
                    changed = true;
                }

                if live_in[block_idx] != new_live_in {
                    live_in[block_idx] = new_live_in;
                    changed = true;
                }
            }
        }

        for (block_idx, block) in function_cfg.blocks.iter().enumerate() {
            let mut live = clone_bitset(&live_out[block_idx], arena);
            for stmt in block.stmts.iter().rev().copied() {
                let live_after = clone_bitset(&live, arena);
                if reachable[stmt.0 as usize] {
                    let stmt_info = &program.stmts[stmt.0 as usize];
                    if matches!(stmt_info.kind, StmtKind::Assign | StmtKind::AssignExisting)
                        && let Some(local) = facts.stmt_effect(stmt).writes.first().copied()
                        && !contains_local(&live_after, local, local_range.start)
                    {
                        warnings.push(UnusedAssignment {
                            stmt_id: stmt,
                            stmt: stmt_info.stmt,
                            span: stmt_info.span,
                            local,
                            local_name: facts.locals[local.0 as usize].name,
                        });
                    }
                }

                apply_stmt_transfer(&mut live, stmt, function, facts, summaries, local_range.start);
            }
        }
    }

    warnings
}

fn apply_stmt_transfer(
    live: &mut BitSet<'_>,
    stmt: StmtId,
    function: FunctionId,
    facts: &ProgramFacts<'_, '_>,
    summaries: &SummarySlice<'_>,
    local_start: u32,
) {
    let stmt_facts = facts.stmt_effect(stmt);
    for &local in &stmt_facts.writes {
        clear_local(live, local, local_start);
    }
    for &callee in &stmt_facts.direct_callees {
        for &local in &summaries[callee.0 as usize].transitive_capture_writes {
            if facts.locals[local.0 as usize].owner == function {
                clear_local(live, local, local_start);
            }
        }
    }

    for &local in &stmt_facts.reads {
        set_local(live, local, local_start);
    }
    for &callee in &stmt_facts.direct_callees {
        for &local in &summaries[callee.0 as usize].transitive_capture_reads {
            if facts.locals[local.0 as usize].owner == function {
                set_local(live, local, local_start);
            }
        }
    }
}

fn new_bitset(local_count: u32, arena: &Arena) -> BitSet<'_> {
    let mut bits = Vec::with_capacity_in(word_count(local_count), arena);
    bits.resize(word_count(local_count), 0);
    bits
}

fn clone_bitset<'arena>(bits: &[u64], arena: &'arena Arena) -> BitSet<'arena> {
    let mut cloned = Vec::with_capacity_in(bits.len(), arena);
    cloned.extend(bits.iter().copied());
    cloned
}

fn union_from(dst: &mut BitSet<'_>, src: &[u64]) {
    for (dst_word, src_word) in dst.iter_mut().zip(src) {
        *dst_word |= *src_word;
    }
}

fn contains_local(bits: &[u64], local: LocalId, local_start: u32) -> bool {
    let local_idx = (local.0 - local_start) as usize;
    let word_idx = local_idx / u64::BITS as usize;
    let bit_idx = local_idx % u64::BITS as usize;
    (bits[word_idx] & (1_u64 << bit_idx)) != 0
}

fn set_local(bits: &mut BitSet<'_>, local: LocalId, local_start: u32) {
    let local_idx = (local.0 - local_start) as usize;
    let word_idx = local_idx / u64::BITS as usize;
    let bit_idx = local_idx % u64::BITS as usize;
    bits[word_idx] |= 1_u64 << bit_idx;
}

fn clear_local(bits: &mut BitSet<'_>, local: LocalId, local_start: u32) {
    let local_idx = (local.0 - local_start) as usize;
    let word_idx = local_idx / u64::BITS as usize;
    let bit_idx = local_idx % u64::BITS as usize;
    bits[word_idx] &= !(1_u64 << bit_idx);
}

const fn word_count(local_count: u32) -> usize {
    local_count.div_ceil(u64::BITS) as usize
}
