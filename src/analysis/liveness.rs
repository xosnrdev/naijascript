//! Backward liveness analysis for local assignments.

use crate::analysis::cfg::{CfgFunction, CfgProgram, LinearOp, StmtKind};
use crate::analysis::facts::ProgramFacts;
use crate::analysis::ids::{FunctionId, LocalId, StmtId};
use crate::analysis::summary::FunctionSummary;
use crate::arena::Arena;
use crate::diagnostics::Span;
use crate::syntax::parser::StmtRef;

type BitSet<'arena> = Vec<u64, &'arena Arena>;
type SummarySlice<'arena> = [FunctionSummary<'arena>];

/// Source-oriented warning for an assignment whose stored value is never read.
#[derive(Debug)]
pub struct UnusedAssignment<'ast> {
    pub stmt_id: StmtId,
    pub stmt: StmtRef<'ast>,
    pub span: Span,
    pub local: LocalId,
    pub local_name: &'ast str,
}

struct BlockTransferFacts<'arena> {
    uses: BitSet<'arena>,
    defs: BitSet<'arena>,
    kills: BitSet<'arena>,
}

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
        let mut scratch_live_out = new_bitset(local_count, arena);
        let mut scratch_live_in = new_bitset(local_count, arena);
        let block_facts = compute_block_facts(
            function_cfg,
            function,
            facts,
            summaries,
            local_range.start,
            local_count,
            arena,
        );

        let mut changed = true;
        while changed {
            changed = false;

            for block_idx in (0..block_count).rev() {
                clear_bitset(&mut scratch_live_out);
                if let Some(terminator) = function_cfg.blocks[block_idx].terminator.as_ref() {
                    terminator.successors(|successor| {
                        union_from(&mut scratch_live_out, &live_in[successor.0 as usize]);
                    });
                }

                overwrite_bitset(&mut scratch_live_in, &scratch_live_out);
                subtract_mask(&mut scratch_live_in, &block_facts[block_idx].defs);
                subtract_mask(&mut scratch_live_in, &block_facts[block_idx].kills);
                union_from(&mut scratch_live_in, &block_facts[block_idx].uses);

                if replace_bitset_if_changed(&mut live_out[block_idx], &scratch_live_out) {
                    changed = true;
                }

                if replace_bitset_if_changed(&mut live_in[block_idx], &scratch_live_in) {
                    changed = true;
                }
            }
        }

        let mut live = new_bitset(local_count, arena);
        for (block_idx, block) in function_cfg.blocks.iter().enumerate() {
            overwrite_bitset(&mut live, &live_out[block_idx]);
            subtract_mask(&mut live, &block_facts[block_idx].kills);
            for op in function_cfg.block_ops_by_ref(block).iter().rev() {
                if reachable[op.stmt.0 as usize] {
                    let stmt_info = &program.stmts[op.stmt.0 as usize];
                    if matches!(stmt_info.kind, StmtKind::Assign | StmtKind::AssignExisting)
                        && let Some(local) = op.writes.first().copied()
                        && !contains_local(&live, local, local_range.start)
                    {
                        warnings.push(UnusedAssignment {
                            stmt_id: op.stmt,
                            stmt: stmt_info.stmt,
                            span: stmt_info.span,
                            local,
                            local_name: facts.locals[local.0 as usize].name,
                        });
                    }
                }

                apply_op_transfer(
                    &mut live,
                    op,
                    function,
                    facts,
                    summaries,
                    local_range.start,
                    local_count,
                );
            }
        }
    }

    warnings
}

fn compute_block_facts<'ast, 'arena>(
    function_cfg: &CfgFunction<'arena>,
    function: FunctionId,
    facts: &ProgramFacts<'ast, 'ast>,
    summaries: &SummarySlice<'arena>,
    local_start: u32,
    local_count: u32,
    arena: &'arena Arena,
) -> Vec<BlockTransferFacts<'arena>, &'arena Arena> {
    let mut block_facts = Vec::with_capacity_in(function_cfg.blocks.len(), arena);

    for block in &function_cfg.blocks {
        let mut uses = new_bitset(local_count, arena);
        let mut defs = new_bitset(local_count, arena);
        let mut kills = new_bitset(local_count, arena);

        for &local in &block.kill {
            set_local(&mut kills, local, local_start);
        }

        for op in function_cfg.block_ops_by_ref(block) {
            for &local in &op.reads {
                note_use(&mut uses, &defs, local, local_start);
            }
            for &local in &op.writes {
                note_def(&mut defs, local, local_start);
            }

            for &callee in &op.direct_callees {
                let summary = &summaries[callee.0 as usize];
                if !summary.available {
                    set_all_locals(&mut uses, local_count);
                    continue;
                }
                for &local in &summary.transitive_capture_reads {
                    if facts.locals[local.0 as usize].owner == function {
                        note_use(&mut uses, &defs, local, local_start);
                    }
                }
                for &local in &summary.transitive_capture_writes {
                    if facts.locals[local.0 as usize].owner == function {
                        note_def(&mut defs, local, local_start);
                    }
                }
            }
        }

        block_facts.push(BlockTransferFacts { uses, defs, kills });
    }

    block_facts
}

fn apply_op_transfer(
    live: &mut BitSet<'_>,
    op: &LinearOp<'_>,
    function: FunctionId,
    facts: &ProgramFacts<'_, '_>,
    summaries: &SummarySlice<'_>,
    local_start: u32,
    local_count: u32,
) {
    for &local in &op.writes {
        clear_local(live, local, local_start);
    }
    for &callee in &op.direct_callees {
        for &local in &summaries[callee.0 as usize].transitive_capture_writes {
            if facts.locals[local.0 as usize].owner == function {
                clear_local(live, local, local_start);
            }
        }
    }

    for &local in &op.reads {
        set_local(live, local, local_start);
    }
    for &callee in &op.direct_callees {
        let summary = &summaries[callee.0 as usize];
        if !summary.available {
            set_all_locals(live, local_count);
            continue;
        }
        for &local in &summary.transitive_capture_reads {
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

fn clear_bitset(bits: &mut BitSet<'_>) {
    bits.fill(0);
}

fn overwrite_bitset(dst: &mut BitSet<'_>, src: &[u64]) {
    debug_assert_eq!(dst.len(), src.len(), "Bitset dimensions should stay stable per function");
    dst.copy_from_slice(src);
}

fn replace_bitset_if_changed(dst: &mut BitSet<'_>, src: &[u64]) -> bool {
    if dst.as_slice() == src {
        return false;
    }

    overwrite_bitset(dst, src);
    true
}

fn union_from(dst: &mut BitSet<'_>, src: &[u64]) {
    for (dst_word, src_word) in dst.iter_mut().zip(src) {
        *dst_word |= *src_word;
    }
}

fn subtract_mask(dst: &mut BitSet<'_>, mask: &[u64]) {
    for (dst_word, mask_word) in dst.iter_mut().zip(mask) {
        *dst_word &= !mask_word;
    }
}

fn set_all_locals(bits: &mut BitSet<'_>, local_count: u32) {
    let full_words = (local_count / u64::BITS) as usize;
    for word in bits.iter_mut().take(full_words) {
        *word = u64::MAX;
    }

    let remaining = local_count % u64::BITS;
    if remaining != 0 {
        bits[full_words] = (1_u64 << remaining) - 1;
    }
}

fn note_use(use_set: &mut BitSet<'_>, def_set: &[u64], local: LocalId, local_start: u32) {
    if !contains_local(def_set, local, local_start) {
        set_local(use_set, local, local_start);
    }
}

fn note_def(def_set: &mut BitSet<'_>, local: LocalId, local_start: u32) {
    set_local(def_set, local, local_start);
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
