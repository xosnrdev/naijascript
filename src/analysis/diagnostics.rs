//! Source-oriented diagnostics built on top of the analysis facts.

use crate::analysis::cfg::CfgProgram;
use crate::analysis::facts::{LocalKind, ProgramFacts};
use crate::analysis::ids::{FunctionId, LocalId, StmtId};
use crate::analysis::summary::FunctionSummary;
use crate::arena::Arena;
use crate::diagnostics::Span;

type SummarySlice<'arena> = [FunctionSummary<'arena>];

/// Interprocedural reachability split between function definitions and bodies.
#[derive(Debug)]
pub struct FunctionReachability<'arena> {
    pub definition_reachable: Vec<bool, &'arena Arena>,
    pub body_reachable: Vec<bool, &'arena Arena>,
}

/// Source-oriented warning for a local declaration that is never read.
#[derive(Debug)]
pub struct UnusedVariable<'ast> {
    pub stmt_id: StmtId,
    pub local: LocalId,
    pub span: Span,
    pub local_name: &'ast str,
}

/// Source-oriented warning for a reachable function definition that is never called.
#[derive(Debug)]
pub struct UnusedFunction<'ast> {
    pub stmt_id: StmtId,
    pub function: FunctionId,
    pub span: Span,
    pub name: &'ast str,
}

/// Computes which function definitions can execute and which bodies can run.
pub fn compute_function_reachability<'ast, 'arena>(
    program: &'arena CfgProgram<'ast, 'arena>,
    facts: &ProgramFacts<'ast, 'ast>,
    reachable: &[bool],
    arena: &'arena Arena,
) -> FunctionReachability<'arena> {
    let mut definition_reachable = Vec::with_capacity_in(facts.functions.len(), arena);
    definition_reachable.resize(facts.functions.len(), false);

    let mut body_reachable = Vec::with_capacity_in(facts.functions.len(), arena);
    body_reachable.resize(facts.functions.len(), false);

    let mut function_by_def_stmt = Vec::with_capacity_in(facts.stmt_effects.len(), arena);
    function_by_def_stmt.resize(facts.stmt_effects.len(), None);
    for function in 0..facts.functions.len() {
        let function_id = FunctionId(function as u32);
        if let Some(def_stmt) = facts.function(function_id).def_stmt {
            function_by_def_stmt[def_stmt.0 as usize] = Some(function_id);
        }
    }

    definition_reachable[facts.root_function.0 as usize] = true;
    body_reachable[facts.root_function.0 as usize] = true;

    let mut worklist = Vec::new_in(arena);
    worklist.push(facts.root_function);

    while let Some(function) = worklist.pop() {
        let cfg = &program.functions[function.0 as usize];
        for block in &cfg.blocks {
            for op in cfg.block_ops_by_ref(block) {
                let stmt = op.stmt;
                if !reachable[stmt.0 as usize] {
                    continue;
                }

                if let Some(defined_function) = function_by_def_stmt[stmt.0 as usize] {
                    definition_reachable[defined_function.0 as usize] = true;
                }

                for &callee in &facts.stmt_effect(stmt).direct_callees {
                    let callee_reachable = &mut body_reachable[callee.0 as usize];
                    if !*callee_reachable {
                        *callee_reachable = true;
                        worklist.push(callee);
                    }
                }
            }
        }
    }

    FunctionReachability { definition_reachable, body_reachable }
}

/// Computes unused-variable warnings for reachable declarations.
pub fn unused_variables<'ast, 'arena>(
    program: &'arena CfgProgram<'ast, 'arena>,
    facts: &ProgramFacts<'ast, 'ast>,
    summaries: &'arena SummarySlice<'arena>,
    reachable: &[bool],
    function_reachability: &FunctionReachability<'arena>,
    arena: &'arena Arena,
) -> Vec<UnusedVariable<'ast>, &'arena Arena> {
    let mut used_locals = Vec::with_capacity_in(facts.locals.len(), arena);
    used_locals.resize(facts.locals.len(), false);

    for cfg in &program.functions {
        let function = cfg.function;
        if !function_reachability.body_reachable[function.0 as usize] {
            continue;
        }

        for block in &cfg.blocks {
            for op in cfg.block_ops_by_ref(block) {
                let stmt = op.stmt;
                if !reachable[stmt.0 as usize] {
                    continue;
                }

                let stmt_facts = facts.stmt_effect(stmt);
                for &local in &stmt_facts.reads {
                    used_locals[local.0 as usize] = true;
                }
                for &callee in &stmt_facts.direct_callees {
                    let summary = &summaries[callee.0 as usize];
                    if !summary.available {
                        mark_function_locals_used(&mut used_locals, facts, function);
                        continue;
                    }
                    for &local in &summary.transitive_capture_reads {
                        used_locals[local.0 as usize] = true;
                    }
                }
            }
        }
    }

    let mut warnings = Vec::new_in(arena);
    for (local_idx, local) in facts.locals.iter().enumerate() {
        if local.kind != LocalKind::Variable || used_locals[local_idx] {
            continue;
        }

        if !function_reachability.body_reachable[local.owner.0 as usize] {
            continue;
        }

        let Some(decl_stmt) = local.decl_stmt else {
            continue;
        };
        if !reachable[decl_stmt.0 as usize] {
            continue;
        }

        warnings.push(UnusedVariable {
            stmt_id: decl_stmt,
            local: LocalId(local_idx as u32),
            span: local.decl_span,
            local_name: local.name,
        });
    }

    warnings
}

fn mark_function_locals_used(
    used_locals: &mut [bool],
    facts: &ProgramFacts<'_, '_>,
    function: FunctionId,
) {
    for local in facts.local_range(function) {
        used_locals[local as usize] = true;
    }
}

/// Computes unused-function warnings for reachable definitions with no reachable calls.
pub fn unused_functions<'ast, 'arena>(
    facts: &ProgramFacts<'ast, 'ast>,
    reachable: &[bool],
    function_reachability: &FunctionReachability<'arena>,
    arena: &'arena Arena,
) -> Vec<UnusedFunction<'ast>, &'arena Arena> {
    let mut warnings = Vec::new_in(arena);

    for function in 0..facts.functions.len() {
        let function_id = FunctionId(function as u32);
        if function_id == facts.root_function {
            continue;
        }

        let info = facts.function(function_id);
        let Some(def_stmt) = info.def_stmt else {
            continue;
        };

        if !reachable[def_stmt.0 as usize]
            || !function_reachability.definition_reachable[function]
            || function_reachability.body_reachable[function]
        {
            continue;
        }

        warnings.push(UnusedFunction {
            stmt_id: def_stmt,
            function: function_id,
            span: info.def_span,
            name: info.name,
        });
    }

    warnings
}
