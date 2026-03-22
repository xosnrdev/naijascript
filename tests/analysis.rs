use naijascript::analysis::cfg::{
    CfgProgram, build_program, build_program_with_counts, count_program, debug_dump_program,
};
use naijascript::analysis::diagnostics::{
    compute_function_reachability, unused_functions, unused_variables,
};
use naijascript::analysis::facts::ProgramFacts;
use naijascript::analysis::ids::{BlockId, FunctionId, LocalId, OpId};
use naijascript::analysis::limits::{AnalysisCaps, DEFAULT_CAPS, first_exceeded_limit};
use naijascript::analysis::liveness::unused_assignments;
use naijascript::analysis::opt::{OptimizationInputs, build_optimization_plan};
use naijascript::analysis::reachability::reachable_statement_mask;
use naijascript::analysis::summary::{compute_summaries, compute_summaries_with_max_events};
use naijascript::syntax::parser::{Expr, Stmt};

mod common;
use common::with_pipeline;

fn function_id_by_name(
    facts: &ProgramFacts<'_, '_>,
    name: &str,
    parent: Option<FunctionId>,
) -> FunctionId {
    facts
        .functions
        .iter()
        .enumerate()
        .find_map(|(idx, info)| {
            (info.name == name && info.parent == parent).then_some(FunctionId(
                u32::try_from(idx).expect("function index should fit in u32"),
            ))
        })
        .expect("Expected function to exist")
}

fn local_id_by_name(facts: &ProgramFacts<'_, '_>, name: &str, owner: FunctionId) -> LocalId {
    facts
        .locals
        .iter()
        .enumerate()
        .find_map(|(idx, info)| {
            (info.name == name && info.owner == owner)
                .then_some(LocalId(u32::try_from(idx).expect("local index should fit in u32")))
        })
        .expect("Expected local to exist")
}

fn program_stmt_index(program: &CfgProgram<'_, '_>, op: OpId) -> usize {
    program
        .stmts
        .iter()
        .position(|info| info.op == op)
        .expect("Optimization op should resolve to a lowered program statement")
}

#[test]
fn summary_tracks_direct_capture_reads_and_writes() {
    with_pipeline(
        r"
        make total get 0
        do bump() start
            total get total add 1
        end
        ",
        |arena, (root, parse_errors), resolver, _| {
            assert!(parse_errors.diagnostics.is_empty());
            resolver.resolve(root);
            assert!(!resolver.errors.has_errors(), "{:?}", resolver.errors.diagnostics);

            let facts = &resolver.facts;
            let summaries = compute_summaries(facts, arena);
            let bump = function_id_by_name(facts, "bump", Some(facts.root_function));
            let total = local_id_by_name(facts, "total", facts.root_function);
            let summary = &summaries[bump.0 as usize];

            assert!(summary.direct_capture_reads.contains(&total));
            assert!(summary.direct_capture_writes.contains(&total));
            assert!(summary.transitive_capture_reads.contains(&total));
            assert!(summary.transitive_capture_writes.contains(&total));
        },
    );
}

#[test]
fn resolver_records_runtime_local_bindings() {
    with_pipeline(
        r#"
        make x get 1
        x get x add 1
        shout("value {x}")
        "#,
        |_, (root, parse_errors), resolver, _| {
            assert!(parse_errors.diagnostics.is_empty());
            resolver.resolve(root);
            assert!(!resolver.errors.has_errors(), "{:?}", resolver.errors.diagnostics);

            let facts = &resolver.facts;
            let x = local_id_by_name(facts, "x", facts.root_function);

            let assign_stmt = root.stmts[0];
            let reassign_stmt = root.stmts[1];
            let shout_stmt = root.stmts[2];

            assert_eq!(facts.stmt_local(assign_stmt), Some(x));
            assert_eq!(facts.stmt_local(reassign_stmt), Some(x));

            let Stmt::AssignExisting { expr: reassign_expr, .. } = reassign_stmt else {
                panic!("second statement should be a reassignment");
            };
            let Expr::Binary { lhs, .. } = reassign_expr else {
                panic!("reassignment RHS should be a binary expression");
            };
            let Expr::Var(..) = lhs else {
                panic!("binary LHS should be a variable expression");
            };
            assert_eq!(facts.expr_local(lhs), Some(x));

            let Stmt::Expression { expr: shout_expr, .. } = shout_stmt else {
                panic!("third statement should be an expression");
            };
            let Expr::Call { args, .. } = shout_expr else {
                panic!("third statement should be a call");
            };
            let Expr::String { .. } = args.args[0] else {
                panic!("shout argument should be an interpolated string");
            };
            assert_eq!(facts.string_segment_local(args.args[0], 1), Some(x));
        },
    );
}

#[test]
fn summary_propagates_capture_effects_through_direct_calls() {
    with_pipeline(
        r"
        make counter get 0
        do leaf() start
            counter get counter add 1
        end
        do mid() start
            leaf()
        end
        ",
        |arena, (root, parse_errors), resolver, _| {
            assert!(parse_errors.diagnostics.is_empty());
            resolver.resolve(root);
            assert!(!resolver.errors.has_errors(), "{:?}", resolver.errors.diagnostics);

            let facts = &resolver.facts;
            let summaries = compute_summaries(facts, arena);
            let leaf = function_id_by_name(facts, "leaf", Some(facts.root_function));
            let mid = function_id_by_name(facts, "mid", Some(facts.root_function));
            let counter = local_id_by_name(facts, "counter", facts.root_function);
            let summary = &summaries[mid.0 as usize];

            assert!(summary.direct_callees.contains(&leaf));
            assert!(summary.transitive_callees.contains(&leaf));
            assert!(summary.direct_capture_reads.is_empty());
            assert!(summary.direct_capture_writes.is_empty());
            assert!(summary.transitive_capture_reads.contains(&counter));
            assert!(summary.transitive_capture_writes.contains(&counter));
        },
    );
}

#[test]
fn summary_resolves_lexical_function_shadowing() {
    with_pipeline(
        r"
        do outer() start
            do foo() start
            end
            foo()
        end
        do foo() start
        end
        ",
        |arena, (root, parse_errors), resolver, _| {
            assert!(parse_errors.diagnostics.is_empty());
            resolver.resolve(root);
            assert!(!resolver.errors.has_errors(), "{:?}", resolver.errors.diagnostics);

            let facts = &resolver.facts;
            let summaries = compute_summaries(facts, arena);
            let outer = function_id_by_name(facts, "outer", Some(facts.root_function));
            let inner_foo = function_id_by_name(facts, "foo", Some(outer));
            let top_level_foo = function_id_by_name(facts, "foo", Some(facts.root_function));
            let summary = &summaries[outer.0 as usize];

            assert!(summary.direct_callees.contains(&inner_foo));
            assert!(!summary.direct_callees.contains(&top_level_foo));
        },
    );
}

#[test]
fn summary_converges_for_self_recursive_capture_updates() {
    with_pipeline(
        r"
        make total get 0
        do ping(n) start
            total get total add 1
            if to say (n pass 0) start
                return ping(n minus 1)
            end
            return total
        end
        ",
        |arena, (root, parse_errors), resolver, _| {
            assert!(parse_errors.diagnostics.is_empty());
            resolver.resolve(root);
            assert!(!resolver.errors.has_errors(), "{:?}", resolver.errors.diagnostics);

            let facts = &resolver.facts;
            let summaries = compute_summaries(facts, arena);
            let ping = function_id_by_name(facts, "ping", Some(facts.root_function));
            let total = local_id_by_name(facts, "total", facts.root_function);
            let summary = &summaries[ping.0 as usize];

            assert!(summary.direct_callees.contains(&ping));
            assert!(summary.transitive_callees.contains(&ping));
            assert!(summary.direct_capture_reads.contains(&total));
            assert!(summary.direct_capture_writes.contains(&total));
            assert!(summary.transitive_capture_reads.contains(&total));
            assert!(summary.transitive_capture_writes.contains(&total));
        },
    );
}

#[test]
fn summary_marks_callers_unavailable_after_budget_exhaustion() {
    with_pipeline(
        r"
        make x get 0
        do leaf() start
            shout(x)
        end
        do mid() start
            leaf()
        end
        do top() start
            mid()
        end
        ",
        |arena, (root, parse_errors), resolver, _| {
            assert!(parse_errors.diagnostics.is_empty());
            resolver.resolve(root);
            assert!(!resolver.errors.has_errors(), "{:?}", resolver.errors.diagnostics);

            let facts = &resolver.facts;
            let summaries = compute_summaries_with_max_events(facts, 0, arena);
            let leaf = function_id_by_name(facts, "leaf", Some(facts.root_function));
            let mid = function_id_by_name(facts, "mid", Some(facts.root_function));
            let top = function_id_by_name(facts, "top", Some(facts.root_function));

            assert!(summaries[leaf.0 as usize].available);
            assert!(!summaries[mid.0 as usize].available);
            assert!(!summaries[top.0 as usize].available);
            assert!(summaries[mid.0 as usize].transitive_callees.is_empty());
            assert!(summaries[top.0 as usize].transitive_callees.is_empty());
        },
    );
}

#[test]
fn unavailable_summary_suppresses_dead_store_false_positives() {
    with_pipeline(
        r"
        do leaf() start
            shout(0)
        end
        do mid() start
            leaf()
        end
        make x get 0
        x get 1
        mid()
        x get 2
        shout(x)
        ",
        |arena, (root, parse_errors), resolver, _| {
            assert!(parse_errors.diagnostics.is_empty());
            resolver.resolve(root);
            assert!(!resolver.errors.has_errors(), "{:?}", resolver.errors.diagnostics);

            let facts = &resolver.facts;
            let program = build_program(facts, arena);
            let reachable = reachable_statement_mask(&program, arena);
            let summaries = compute_summaries_with_max_events(facts, 0, arena);
            let mid = function_id_by_name(facts, "mid", Some(facts.root_function));
            let warnings = unused_assignments(&program, facts, &summaries, &reachable, arena);

            assert!(!summaries[mid.0 as usize].available);
            assert_eq!(warnings.len(), 1);
            assert!(matches!(
                program.stmts[warnings[0].stmt_id.0 as usize].kind,
                naijascript::analysis::cfg::StmtKind::Assign
            ));
        },
    );
}

#[test]
fn optimization_plan_keeps_calls_with_unavailable_summaries() {
    let src = r"
        do leaf() start
            shout(0)
        end
        do mid() start
            return leaf()
        end
        make x get 0
        x get mid()
        shout(1)
    ";
    with_pipeline(src, |arena, (root, parse_errors), resolver, _| {
        assert!(parse_errors.diagnostics.is_empty());
        resolver.resolve(root);
        assert!(!resolver.errors.has_errors(), "{:?}", resolver.errors.diagnostics);

        let facts = &resolver.facts;
        let program = build_program(facts, arena);
        let reachable = reachable_statement_mask(&program, arena);
        let summaries = compute_summaries_with_max_events(facts, 0, arena);
        let function_reachability =
            compute_function_reachability(&program, facts, &reachable, arena);
        let unused_assignments = unused_assignments(&program, facts, &summaries, &reachable, arena);
        let unused_variables = unused_variables(
            &program,
            facts,
            &summaries,
            &reachable,
            &function_reachability,
            arena,
        );
        let unused_functions = unused_functions(facts, &reachable, &function_reachability, arena);
        let plan = build_optimization_plan(
            OptimizationInputs {
                program: &program,
                facts,
                summaries: &summaries,
                reachable: &reachable,
                unused_assignments: &unused_assignments,
                unused_variables: &unused_variables,
                unused_functions: &unused_functions,
            },
            arena,
        );

        assert!(unused_assignments.iter().any(|warning| {
            matches!(
                program.stmts[warning.stmt_id.0 as usize].kind,
                naijascript::analysis::cfg::StmtKind::AssignExisting
            )
        }));
        assert!(!plan.removable_ops.iter().any(|op| {
            matches!(
                program.stmts[program_stmt_index(&program, *op)].stmt,
                Stmt::AssignExisting { .. }
            )
        }));
    });
}

#[test]
fn cfg_debug_dump_reports_branch_and_dead_tail_shape() {
    with_pipeline(
        "do foo() start return 1 shout(2) end",
        |arena, (root, parse_errors), resolver, _| {
            assert!(parse_errors.diagnostics.is_empty());
            resolver.resolve(root);
            assert!(!resolver.errors.has_errors(), "{:?}", resolver.errors.diagnostics);

            let facts = &resolver.facts;
            let counts = count_program(facts, arena);
            let program = build_program_with_counts(facts, &counts, arena);
            let dump = debug_dump_program(&program, facts, arena);

            assert!(dump.contains("fn 0 <script> entry=b0 exit=b1"));
            assert!(dump.contains("fn 1 foo entry=b0 exit=b1"));
            assert!(dump.contains("s0 FunctionDef"));
            assert!(dump.contains("s1 Return"));
            assert!(dump.contains("s2 Expression"));
            assert!(dump.contains("b2 return-dead"));
            assert!(dump.contains("term return"));
            assert!(dump.contains("term goto b1"));
        },
    );
}

#[test]
fn cfg_blocks_reference_contiguous_op_slices() {
    with_pipeline(
        r"
        if to say (true) start
            shout(1)
        end if not so start
            shout(2)
        end
        shout(3)
        ",
        |arena, (root, parse_errors), resolver, _| {
            assert!(parse_errors.diagnostics.is_empty());
            resolver.resolve(root);
            assert!(!resolver.errors.has_errors(), "{:?}", resolver.errors.diagnostics);

            let facts = &resolver.facts;
            let counts = count_program(facts, arena);
            let program = build_program_with_counts(facts, &counts, arena);
            let root_cfg = &program.functions[facts.root_function.0 as usize];
            let total_ops: usize =
                root_cfg.blocks.iter().map(|block| block.op_count as usize).sum();

            assert_eq!(total_ops, root_cfg.ops.len());

            let mut seen = std::collections::BTreeSet::new();
            for (block_idx, _block) in root_cfg.blocks.iter().enumerate() {
                let block_id = BlockId(block_idx.try_into().expect("fits in u32"));
                for op in root_cfg.block_ops(block_id) {
                    assert!(seen.insert(op.id.0));
                    let stmt = &program.stmts[op.stmt.0 as usize];
                    assert_eq!(stmt.block, block_id);
                    assert_eq!(stmt.op, op.id);
                }
            }

            let all_op_ids = root_cfg.ops.iter().map(|op| op.id.0).collect::<Vec<_>>();
            let seen_op_ids = seen.into_iter().collect::<Vec<_>>();
            assert_eq!(seen_op_ids, all_op_ids);
        },
    );
}

#[test]
fn cfg_records_scope_exit_kills_for_nested_block_fallthrough() {
    with_pipeline(
        "start make x get 1 shout(x) end shout(0)",
        |arena, (root, parse_errors), resolver, _| {
            assert!(parse_errors.diagnostics.is_empty());
            resolver.resolve(root);
            assert!(!resolver.errors.has_errors(), "{:?}", resolver.errors.diagnostics);

            let facts = &resolver.facts;
            let counts = count_program(facts, arena);
            let program = build_program_with_counts(facts, &counts, arena);
            let x = local_id_by_name(facts, "x", facts.root_function);
            let root_cfg = &program.functions[facts.root_function.0 as usize];

            assert!(root_cfg.blocks.iter().any(|block| block.kill.contains(&x)));
        },
    );
}

#[test]
fn cfg_records_scope_exit_kills_for_continue_edges() {
    with_pipeline(
        "jasi (true) start make x get 1 next end",
        |arena, (root, parse_errors), resolver, _| {
            assert!(parse_errors.diagnostics.is_empty());
            resolver.resolve(root);
            assert!(!resolver.errors.has_errors(), "{:?}", resolver.errors.diagnostics);

            let facts = &resolver.facts;
            let counts = count_program(facts, arena);
            let program = build_program_with_counts(facts, &counts, arena);
            let x = local_id_by_name(facts, "x", facts.root_function);
            let root_cfg = &program.functions[facts.root_function.0 as usize];

            assert!(root_cfg.blocks.iter().any(|block| {
                matches!(
                    block.terminator,
                    Some(naijascript::analysis::cfg::Terminator::Continue { .. })
                ) && block.kill.contains(&x)
            }));
        },
    );
}

#[test]
fn analysis_limits_trip_before_expensive_passes_start() {
    with_pipeline("make x get 1", |arena, (root, parse_errors), resolver, _| {
        assert!(parse_errors.diagnostics.is_empty());
        resolver.resolve(root);
        assert!(!resolver.errors.has_errors(), "{:?}", resolver.errors.diagnostics);

        let facts = &resolver.facts;
        let counts = count_program(facts, arena);
        let caps = AnalysisCaps { max_functions: 0, ..DEFAULT_CAPS };
        let limit = first_exceeded_limit(facts, &counts, caps)
            .expect("function cap should trigger before deeper analysis");

        assert_eq!(limit.metric, "functions");
        assert_eq!(limit.observed, 1);
        assert_eq!(limit.limit, 0);
    });
}

#[test]
fn analysis_limits_trip_for_cfg_ops() {
    with_pipeline("make x get 1", |arena, (root, parse_errors), resolver, _| {
        assert!(parse_errors.diagnostics.is_empty());
        resolver.resolve(root);
        assert!(!resolver.errors.has_errors(), "{:?}", resolver.errors.diagnostics);

        let facts = &resolver.facts;
        let counts = count_program(facts, arena);
        let caps = AnalysisCaps { max_total_ops: 0, ..DEFAULT_CAPS };
        let limit = first_exceeded_limit(facts, &counts, caps)
            .expect("op cap should trigger before deeper analysis");

        assert_eq!(limit.metric, "cfg ops");
        assert_eq!(limit.observed, 1);
        assert_eq!(limit.limit, 0);
    });
}

#[test]
fn optimization_plan_marks_pure_unused_assignment_removable() {
    let src = "make x get 1 shout(0)";
    with_pipeline(src, |arena, (root, parse_errors), resolver, _| {
        assert!(parse_errors.diagnostics.is_empty());
        resolver.resolve(root);
        assert!(!resolver.errors.has_errors(), "{:?}", resolver.errors.diagnostics);

        let facts = &resolver.facts;
        let program = build_program(facts, arena);
        let reachable = reachable_statement_mask(&program, arena);
        let summaries = compute_summaries(facts, arena);
        let function_reachability =
            compute_function_reachability(&program, facts, &reachable, arena);
        let unused_assignments = unused_assignments(&program, facts, &summaries, &reachable, arena);
        let unused_variables = unused_variables(
            &program,
            facts,
            &summaries,
            &reachable,
            &function_reachability,
            arena,
        );
        let unused_functions = unused_functions(facts, &reachable, &function_reachability, arena);
        let plan = build_optimization_plan(
            OptimizationInputs {
                program: &program,
                facts,
                summaries: &summaries,
                reachable: &reachable,
                unused_assignments: &unused_assignments,
                unused_variables: &unused_variables,
                unused_functions: &unused_functions,
            },
            arena,
        );

        assert_eq!(plan.removable_ops.len(), 1);
        let removable_stmt =
            program.stmts[program_stmt_index(&program, plan.removable_ops[0])].stmt;
        assert!(matches!(removable_stmt, Stmt::Assign { .. }));
    });
}

#[test]
fn optimization_plan_keeps_declaration_needed_for_later_reassignment() {
    let src = "make x get 1 x get 2 shout(x)";
    with_pipeline(src, |arena, (root, parse_errors), resolver, _| {
        assert!(parse_errors.diagnostics.is_empty());
        resolver.resolve(root);
        assert!(!resolver.errors.has_errors(), "{:?}", resolver.errors.diagnostics);

        let facts = &resolver.facts;
        let program = build_program(facts, arena);
        let reachable = reachable_statement_mask(&program, arena);
        let summaries = compute_summaries(facts, arena);
        let function_reachability =
            compute_function_reachability(&program, facts, &reachable, arena);
        let unused_assignments = unused_assignments(&program, facts, &summaries, &reachable, arena);
        let unused_variables = unused_variables(
            &program,
            facts,
            &summaries,
            &reachable,
            &function_reachability,
            arena,
        );
        let unused_functions = unused_functions(facts, &reachable, &function_reachability, arena);
        let plan = build_optimization_plan(
            OptimizationInputs {
                program: &program,
                facts,
                summaries: &summaries,
                reachable: &reachable,
                unused_assignments: &unused_assignments,
                unused_variables: &unused_variables,
                unused_functions: &unused_functions,
            },
            arena,
        );

        assert!(plan.removable_ops.is_empty());
    });
}

#[test]
fn optimization_plan_marks_unreachable_ops_and_unused_function_defs_removable() {
    let src = r#"
        do outer() start
            return 1
            shout("dead")
        end
        outer()
        do foo() start
        end
    "#;
    with_pipeline(src, |arena, (root, parse_errors), resolver, _| {
        assert!(parse_errors.diagnostics.is_empty());
        resolver.resolve(root);
        assert!(!resolver.errors.has_errors(), "{:?}", resolver.errors.diagnostics);

        let facts = &resolver.facts;
        let program = build_program(facts, arena);
        let reachable = reachable_statement_mask(&program, arena);
        let summaries = compute_summaries(facts, arena);
        let function_reachability =
            compute_function_reachability(&program, facts, &reachable, arena);
        let unused_assignments = unused_assignments(&program, facts, &summaries, &reachable, arena);
        let unused_variables = unused_variables(
            &program,
            facts,
            &summaries,
            &reachable,
            &function_reachability,
            arena,
        );
        let unused_functions = unused_functions(facts, &reachable, &function_reachability, arena);
        let plan = build_optimization_plan(
            OptimizationInputs {
                program: &program,
                facts,
                summaries: &summaries,
                reachable: &reachable,
                unused_assignments: &unused_assignments,
                unused_variables: &unused_variables,
                unused_functions: &unused_functions,
            },
            arena,
        );

        assert!(plan.removable_ops.iter().any(|op| {
            matches!(program.stmts[program_stmt_index(&program, *op)].stmt, Stmt::Expression { .. })
                && !reachable[program_stmt_index(&program, *op)]
        }));
        assert_eq!(unused_functions.len(), 1);
        assert_eq!(plan.removable_function_defs.len(), 1);
    });
}
