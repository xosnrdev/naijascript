//! Interprocedural summaries for direct user-function calls.

use crate::analysis::effects::ExprClass;
use crate::analysis::facts::ProgramFacts;
use crate::analysis::ids::{FunctionId, LocalId};
use crate::arena::Arena;

/// Conservative summary for one function body.
#[derive(Debug)]
pub struct FunctionSummary<'arena> {
    pub direct_callees: Vec<FunctionId, &'arena Arena>,
    pub transitive_callees: Vec<FunctionId, &'arena Arena>,
    pub direct_capture_reads: Vec<LocalId, &'arena Arena>,
    pub direct_capture_writes: Vec<LocalId, &'arena Arena>,
    pub body_class: ExprClass,
    pub transitive_capture_reads: Vec<LocalId, &'arena Arena>,
    pub transitive_capture_writes: Vec<LocalId, &'arena Arena>,
    pub transitive_class: ExprClass,
}

#[derive(Clone, Copy)]
struct PendingVisit {
    function: FunctionId,
    expanded: bool,
}

type Component<'arena> = Vec<FunctionId, &'arena Arena>;
type ComponentList<'arena> = Vec<Component<'arena>, &'arena Arena>;

/// Computes function summaries for the resolved program.
pub fn compute_summaries<'ast, 'arena>(
    facts: &ProgramFacts<'ast, 'ast>,
    arena: &'arena Arena,
) -> Vec<FunctionSummary<'arena>, &'arena Arena> {
    let body_classes = compute_body_classes(facts, arena);
    let reverse_edges = build_reverse_edges(facts, arena);
    let finish_order = compute_finish_order(facts, arena);
    let (components, component_ids) =
        compute_strongly_connected_components(&reverse_edges, &finish_order, arena);
    let component_order = compute_component_order(facts, &components, &component_ids, arena);
    let mut summaries = initialize_summaries(facts, &body_classes, arena);

    for &component_idx in &component_order {
        summarize_component(&components[component_idx as usize], facts, &mut summaries);
    }

    summaries
}

fn compute_body_classes<'ast, 'arena>(
    facts: &ProgramFacts<'ast, 'ast>,
    arena: &'arena Arena,
) -> Vec<ExprClass, &'arena Arena> {
    let mut body_classes = Vec::with_capacity_in(facts.functions.len(), arena);
    body_classes.resize(facts.functions.len(), ExprClass::PureNoTrap);

    for stmt in &facts.stmt_effects {
        let class = &mut body_classes[stmt.function.0 as usize];
        *class = class.join(stmt.expr_class);
    }

    body_classes
}

fn build_reverse_edges<'ast, 'arena>(
    facts: &ProgramFacts<'ast, 'ast>,
    arena: &'arena Arena,
) -> Vec<Vec<FunctionId, &'arena Arena>, &'arena Arena> {
    let mut reverse_edges = Vec::with_capacity_in(facts.functions.len(), arena);
    for _ in 0..facts.functions.len() {
        reverse_edges.push(Vec::new_in(arena));
    }

    for (caller_idx, _) in facts.functions.iter().enumerate() {
        let caller = FunctionId(caller_idx as u32);
        for &callee in &facts.function_direct(caller).direct_callees {
            reverse_edges[callee.0 as usize].push(caller);
        }
    }

    reverse_edges
}

fn compute_finish_order<'ast, 'arena>(
    facts: &ProgramFacts<'ast, 'ast>,
    arena: &'arena Arena,
) -> Vec<FunctionId, &'arena Arena> {
    let mut seen = Vec::with_capacity_in(facts.functions.len(), arena);
    seen.resize(facts.functions.len(), false);

    let mut finish_order = Vec::with_capacity_in(facts.functions.len(), arena);
    let mut stack = Vec::new_in(arena);

    for function_idx in 0..facts.functions.len() {
        let function = FunctionId(function_idx as u32);
        if seen[function_idx] {
            continue;
        }

        stack.push(PendingVisit { function, expanded: false });
        while let Some(visit) = stack.pop() {
            let index = visit.function.0 as usize;
            if visit.expanded {
                finish_order.push(visit.function);
                continue;
            }
            if seen[index] {
                continue;
            }

            seen[index] = true;
            stack.push(PendingVisit { function: visit.function, expanded: true });
            for &callee in facts.function_direct(visit.function).direct_callees.iter().rev() {
                if !seen[callee.0 as usize] {
                    stack.push(PendingVisit { function: callee, expanded: false });
                }
            }
        }
    }

    finish_order
}

fn compute_strongly_connected_components<'arena>(
    reverse_edges: &[Vec<FunctionId, &'arena Arena>],
    finish_order: &[FunctionId],
    arena: &'arena Arena,
) -> (ComponentList<'arena>, Vec<u32, &'arena Arena>) {
    let mut component_ids = Vec::with_capacity_in(reverse_edges.len(), arena);
    component_ids.resize(reverse_edges.len(), u32::MAX);

    let mut components = Vec::new_in(arena);
    let mut stack = Vec::new_in(arena);

    for &function in finish_order.iter().rev() {
        if component_ids[function.0 as usize] != u32::MAX {
            continue;
        }

        let component_idx = components.len() as u32;
        let mut component = Vec::new_in(arena);
        component_ids[function.0 as usize] = component_idx;
        stack.push(function);

        while let Some(member) = stack.pop() {
            component.push(member);
            for &predecessor in reverse_edges[member.0 as usize].iter().rev() {
                let predecessor_idx = predecessor.0 as usize;
                if component_ids[predecessor_idx] == u32::MAX {
                    component_ids[predecessor_idx] = component_idx;
                    stack.push(predecessor);
                }
            }
        }

        component.sort_by_key(|function| function.0);
        components.push(component);
    }

    (components, component_ids)
}

fn compute_component_order<'ast, 'arena>(
    facts: &ProgramFacts<'ast, 'ast>,
    components: &[Vec<FunctionId, &'arena Arena>],
    component_ids: &[u32],
    arena: &'arena Arena,
) -> Vec<u32, &'arena Arena> {
    let mut predecessors = Vec::with_capacity_in(components.len(), arena);
    for _ in 0..components.len() {
        predecessors.push(Vec::new_in(arena));
    }

    let mut pending_outgoing = Vec::with_capacity_in(components.len(), arena);
    pending_outgoing.resize(components.len(), 0_u32);

    for (caller_idx, _) in facts.functions.iter().enumerate() {
        let caller = FunctionId(caller_idx as u32);
        let caller_component = component_ids[caller_idx];
        for &callee in &facts.function_direct(caller).direct_callees {
            let callee_component = component_ids[callee.0 as usize];
            if caller_component == callee_component {
                continue;
            }

            let predecessor_list = &mut predecessors[callee_component as usize];
            if push_unique(predecessor_list, caller_component) {
                pending_outgoing[caller_component as usize] += 1;
            }
        }
    }

    let mut ready = Vec::new_in(arena);
    for component_idx in (0..components.len()).rev() {
        if pending_outgoing[component_idx] == 0 {
            ready.push(component_idx as u32);
        }
    }

    let mut order = Vec::with_capacity_in(components.len(), arena);
    while let Some(component_idx) = ready.pop() {
        order.push(component_idx);
        for &predecessor in &predecessors[component_idx as usize] {
            let pending = &mut pending_outgoing[predecessor as usize];
            *pending -= 1;
            if *pending == 0 {
                ready.push(predecessor);
            }
        }
    }

    debug_assert_eq!(
        order.len(),
        components.len(),
        "SCC scheduling should cover every component exactly once",
    );
    order
}

fn initialize_summaries<'ast, 'arena>(
    facts: &ProgramFacts<'ast, 'ast>,
    body_classes: &[ExprClass],
    arena: &'arena Arena,
) -> Vec<FunctionSummary<'arena>, &'arena Arena> {
    let mut summaries = Vec::with_capacity_in(facts.functions.len(), arena);

    for (function_idx, _) in facts.functions.iter().enumerate() {
        let function = FunctionId(function_idx as u32);
        let directs = facts.function_direct(function);
        let direct_callees = clone_ids(&directs.direct_callees, arena);
        let direct_capture_reads = clone_ids(&directs.direct_capture_reads, arena);
        let direct_capture_writes = clone_ids(&directs.direct_capture_writes, arena);
        let body_class = body_classes[function_idx];

        summaries.push(FunctionSummary {
            transitive_callees: clone_ids(&direct_callees, arena),
            transitive_capture_reads: clone_ids(&direct_capture_reads, arena),
            transitive_capture_writes: clone_ids(&direct_capture_writes, arena),
            transitive_class: body_class,
            direct_callees,
            direct_capture_reads,
            direct_capture_writes,
            body_class,
        });
    }

    summaries
}

fn summarize_component(
    component: &[FunctionId],
    facts: &ProgramFacts<'_, '_>,
    summaries: &mut [FunctionSummary<'_>],
) {
    let mut changed = true;
    while changed {
        changed = false;

        for &function in component {
            let function_idx = function.0 as usize;
            let body_class = summaries[function_idx].body_class;
            let mut transitive_class = body_class;

            for &callee in &facts.function_direct(function).direct_callees {
                if callee == function {
                    continue;
                }

                let callee_idx = callee.0 as usize;
                let (caller_summary, callee_summary) =
                    split_summary_pair(summaries, function_idx, callee_idx);
                if extend_unique(
                    &mut caller_summary.transitive_callees,
                    &callee_summary.transitive_callees,
                ) {
                    changed = true;
                }
                if extend_unique(
                    &mut caller_summary.transitive_capture_reads,
                    &callee_summary.transitive_capture_reads,
                ) {
                    changed = true;
                }
                if extend_unique(
                    &mut caller_summary.transitive_capture_writes,
                    &callee_summary.transitive_capture_writes,
                ) {
                    changed = true;
                }
                transitive_class = transitive_class.join(callee_summary.transitive_class);
            }

            let caller_summary = &mut summaries[function_idx];
            if caller_summary.transitive_class != transitive_class {
                caller_summary.transitive_class = transitive_class;
                changed = true;
            }
        }
    }
}

fn split_summary_pair<'a, 'arena>(
    summaries: &'a mut [FunctionSummary<'arena>],
    caller_idx: usize,
    callee_idx: usize,
) -> (&'a mut FunctionSummary<'arena>, &'a FunctionSummary<'arena>) {
    debug_assert_ne!(caller_idx, callee_idx);

    if caller_idx < callee_idx {
        let (left, right) = summaries.split_at_mut(callee_idx);
        (&mut left[caller_idx], &right[0])
    } else {
        let (left, right) = summaries.split_at_mut(caller_idx);
        (&mut right[0], &left[callee_idx])
    }
}

fn clone_ids<'arena, T: Copy>(items: &[T], arena: &'arena Arena) -> Vec<T, &'arena Arena> {
    let mut cloned = Vec::with_capacity_in(items.len(), arena);
    cloned.extend(items.iter().copied());
    cloned
}

fn extend_unique<T: Copy + PartialEq, A: std::alloc::Allocator>(
    dst: &mut Vec<T, A>,
    src: &[T],
) -> bool {
    let mut changed = false;
    for &item in src {
        changed |= push_unique(dst, item);
    }
    changed
}

fn push_unique<T: Copy + PartialEq, A: std::alloc::Allocator>(
    dst: &mut Vec<T, A>,
    item: T,
) -> bool {
    if dst.contains(&item) {
        return false;
    }

    dst.push(item);
    true
}
