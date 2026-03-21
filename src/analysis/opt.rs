//! Conservative optimization planning over the analysis sidecar.

use crate::analysis::diagnostics::{UnusedFunction, UnusedVariable};
use crate::analysis::effects::ExprClass;
use crate::analysis::facts::ProgramFacts;
use crate::analysis::ids::{FunctionId, OpId, StmtId};
use crate::analysis::liveness::UnusedAssignment;
use crate::analysis::summary::FunctionSummary;
use crate::arena::Arena;

type SummarySlice<'arena> = [FunctionSummary<'arena>];

/// Side-effect-free description of code that later integrations may skip.
#[derive(Debug)]
pub struct OptimizationPlan<'arena> {
    pub removable_ops: Vec<OpId, &'arena Arena>,
    pub removable_function_defs: Vec<FunctionId, &'arena Arena>,
}

/// Builds the conservative optimization plan supported by the current analyses.
pub fn build_optimization_plan<'ast, 'arena>(
    facts: &ProgramFacts<'ast, 'ast>,
    summaries: &SummarySlice<'_>,
    reachable: &[bool],
    unused_assignments: &[UnusedAssignment<'ast>],
    unused_variables: &[UnusedVariable<'ast>],
    unused_functions: &[UnusedFunction<'ast>],
    arena: &'arena Arena,
) -> OptimizationPlan<'arena> {
    let mut removable_ops = Vec::new_in(arena);
    let mut removable_function_defs = Vec::new_in(arena);

    for (stmt_idx, is_reachable) in reachable.iter().copied().enumerate() {
        if !is_reachable {
            push_unique(&mut removable_ops, OpId(stmt_idx as u32));
        }
    }

    for warning in unused_assignments {
        if stmt_effective_class(warning.stmt_id, facts, summaries) == ExprClass::PureNoTrap {
            push_unique(&mut removable_ops, OpId(warning.stmt_id.0));
        }
    }

    for warning in unused_variables {
        if stmt_effective_class(warning.stmt_id, facts, summaries) == ExprClass::PureNoTrap {
            push_unique(&mut removable_ops, OpId(warning.stmt_id.0));
        }
    }

    for warning in unused_functions {
        push_unique(&mut removable_function_defs, warning.function);
    }

    OptimizationPlan { removable_ops, removable_function_defs }
}

fn stmt_effective_class(
    stmt: StmtId,
    facts: &ProgramFacts<'_, '_>,
    summaries: &SummarySlice<'_>,
) -> ExprClass {
    let stmt_facts = facts.stmt_effect(stmt);
    stmt_facts.direct_callees.iter().fold(stmt_facts.expr_class, |class, callee| {
        class.join(summaries[callee.0 as usize].transitive_class)
    })
}

fn push_unique<T: Copy + PartialEq, A: std::alloc::Allocator>(dst: &mut Vec<T, A>, item: T) {
    if !dst.contains(&item) {
        dst.push(item);
    }
}
