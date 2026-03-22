//! Conservative optimization planning over the analysis sidecar.

use crate::analysis::cfg::CfgProgram;
use crate::analysis::diagnostics::{UnusedFunction, UnusedVariable};
use crate::analysis::effects::ExprClass;
use crate::analysis::facts::ProgramFacts;
use crate::analysis::ids::{FunctionId, OpId, StmtId};
use crate::analysis::liveness::UnusedAssignment;
use crate::analysis::summary::FunctionSummary;
use crate::arena::Arena;
use crate::syntax::parser::Stmt;

type SummarySlice<'arena> = [FunctionSummary<'arena>];
const NO_REFERENCE_STMT: u32 = u32::MAX;

/// Inputs required to build a conservative optimization plan.
#[derive(Clone, Copy)]
pub struct OptimizationInputs<'ast, 'cfg, 'summary, 'input> {
    pub program: &'input CfgProgram<'ast, 'cfg>,
    pub facts: &'input ProgramFacts<'ast, 'ast>,
    pub summaries: &'input SummarySlice<'summary>,
    pub reachable: &'input [bool],
    pub unused_assignments: &'input [UnusedAssignment<'ast>],
    pub unused_variables: &'input [UnusedVariable<'ast>],
    pub unused_functions: &'input [UnusedFunction<'ast>],
}

/// Side-effect-free description of code that later integrations may skip.
#[derive(Debug)]
pub struct OptimizationPlan<'arena> {
    pub removable_ops: Vec<OpId, &'arena Arena>,
    // Statements remain the runtime-facing key because the tree-walk executor still steps
    // source statements directly even though the CFG now stores linear ops separately.
    pub removable_stmts: Vec<StmtId, &'arena Arena>,
    pub removable_function_defs: Vec<FunctionId, &'arena Arena>,
}

impl OptimizationPlan<'_> {
    /// Returns whether the plan marks this statement op as removable.
    #[must_use]
    pub fn contains_removable_op(&self, op: OpId) -> bool {
        self.removable_ops.binary_search_by_key(&op.0, |candidate| candidate.0).is_ok()
    }

    /// Returns whether the plan marks this source statement as removable.
    #[must_use]
    pub fn contains_removable_stmt(&self, stmt: StmtId) -> bool {
        self.removable_stmts.binary_search_by_key(&stmt.0, |candidate| candidate.0).is_ok()
    }

    /// Returns whether the plan marks this function definition as removable.
    #[must_use]
    pub fn contains_removable_function_def(&self, function: FunctionId) -> bool {
        self.removable_function_defs
            .binary_search_by_key(&function.0, |candidate| candidate.0)
            .is_ok()
    }
}

/// Builds the conservative optimization plan supported by the current analyses.
pub fn build_optimization_plan<'arena>(
    inputs: OptimizationInputs<'_, '_, '_, '_>,
    arena: &'arena Arena,
) -> OptimizationPlan<'arena> {
    let OptimizationInputs {
        program,
        facts,
        summaries,
        reachable,
        unused_assignments,
        unused_variables,
        unused_functions,
    } = inputs;
    let mut removable_ops = Vec::new_in(arena);
    let mut removable_stmts = Vec::new_in(arena);
    let mut removable_function_defs = Vec::new_in(arena);
    let max_local_reference_stmt =
        compute_max_local_reference_stmt(facts, summaries, reachable, arena);

    for (stmt_idx, is_reachable) in reachable.iter().copied().enumerate() {
        if !is_reachable {
            let stmt_info = &program.stmts[stmt_idx];
            let stmt = stmt_info.stmt;
            let stmt_id = facts
                .stmt_id(stmt)
                .expect("CFG statement should map back to a stable statement id");
            push_unique(&mut removable_ops, stmt_info.op);
            push_unique(&mut removable_stmts, stmt_id);
        }
    }

    for warning in unused_assignments {
        if stmt_effective_class(warning.stmt_id, facts, summaries) != ExprClass::PureNoTrap {
            continue;
        }
        let stmt = facts.stmt_effect(warning.stmt_id).stmt;
        if matches!(stmt, Stmt::Assign { .. })
            && !declaration_is_runtime_removable(
                warning.local,
                warning.stmt_id,
                &max_local_reference_stmt,
            )
        {
            continue;
        }
        let op = program.stmts[warning.stmt_id.0 as usize].op;
        push_unique(&mut removable_ops, op);
        push_unique(&mut removable_stmts, warning.stmt_id);
    }

    for warning in unused_variables {
        if stmt_effective_class(warning.stmt_id, facts, summaries) != ExprClass::PureNoTrap {
            continue;
        }
        if !declaration_is_runtime_removable(
            warning.local,
            warning.stmt_id,
            &max_local_reference_stmt,
        ) {
            continue;
        }
        let op = program.stmts[warning.stmt_id.0 as usize].op;
        push_unique(&mut removable_ops, op);
        push_unique(&mut removable_stmts, warning.stmt_id);
    }

    for warning in unused_functions {
        push_unique(&mut removable_function_defs, warning.function);
    }

    removable_ops.sort_by_key(|op| op.0);
    removable_stmts.sort_by_key(|stmt| stmt.0);
    removable_function_defs.sort_by_key(|function| function.0);

    OptimizationPlan { removable_ops, removable_stmts, removable_function_defs }
}

fn compute_max_local_reference_stmt<'ast, 'arena>(
    facts: &ProgramFacts<'ast, 'ast>,
    summaries: &SummarySlice<'_>,
    reachable: &[bool],
    arena: &'arena Arena,
) -> Vec<u32, &'arena Arena> {
    let mut max_stmt = Vec::with_capacity_in(facts.locals.len(), arena);
    max_stmt.resize(facts.locals.len(), NO_REFERENCE_STMT);

    for (stmt_idx, stmt_facts) in facts.stmt_effects.iter().enumerate() {
        if !reachable[stmt_idx] {
            continue;
        }

        let stmt_id = u32::try_from(stmt_idx).expect("statement index should fit in u32");
        for &local in &stmt_facts.reads {
            note_max_reference(&mut max_stmt, local, stmt_id);
        }
        for &local in &stmt_facts.writes {
            note_max_reference(&mut max_stmt, local, stmt_id);
        }
        for &callee in &stmt_facts.direct_callees {
            let summary = &summaries[callee.0 as usize];
            if !summary.available {
                for local_idx in facts.local_range(stmt_facts.function) {
                    note_max_reference(
                        &mut max_stmt,
                        crate::analysis::ids::LocalId(local_idx),
                        stmt_id,
                    );
                }
                continue;
            }

            for &local in &summary.transitive_capture_reads {
                if facts.locals[local.0 as usize].owner == stmt_facts.function {
                    note_max_reference(&mut max_stmt, local, stmt_id);
                }
            }
            for &local in &summary.transitive_capture_writes {
                if facts.locals[local.0 as usize].owner == stmt_facts.function {
                    note_max_reference(&mut max_stmt, local, stmt_id);
                }
            }
        }
    }

    max_stmt
}

fn note_max_reference(max_stmt: &mut [u32], local: crate::analysis::ids::LocalId, stmt_id: u32) {
    let entry = &mut max_stmt[local.0 as usize];
    if *entry == NO_REFERENCE_STMT || *entry < stmt_id {
        *entry = stmt_id;
    }
}

fn declaration_is_runtime_removable(
    local: crate::analysis::ids::LocalId,
    stmt: StmtId,
    max_stmt: &[u32],
) -> bool {
    let max_reference = max_stmt[local.0 as usize];
    max_reference == NO_REFERENCE_STMT || max_reference <= stmt.0
}

fn stmt_effective_class(
    stmt: StmtId,
    facts: &ProgramFacts<'_, '_>,
    summaries: &SummarySlice<'_>,
) -> ExprClass {
    let stmt_facts = facts.stmt_effect(stmt);
    stmt_facts.direct_callees.iter().fold(stmt_facts.expr_class, |class, callee| {
        let summary = &summaries[callee.0 as usize];
        if !summary.available {
            return ExprClass::Impure;
        }

        class.join(summary.transitive_class)
    })
}

fn push_unique<T: Copy + PartialEq, A: std::alloc::Allocator>(dst: &mut Vec<T, A>, item: T) {
    if !dst.contains(&item) {
        dst.push(item);
    }
}
