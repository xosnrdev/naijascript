use naijascript::analysis::cfg::{
    build_program, build_program_with_counts, count_program, debug_dump_program,
};
use naijascript::analysis::diagnostics::{
    compute_function_reachability, unused_functions, unused_variables,
};
use naijascript::analysis::facts::ProgramFacts;
use naijascript::analysis::ids::{FunctionId, LocalId};
use naijascript::analysis::limits::{AnalysisCaps, DEFAULT_CAPS, first_exceeded_limit};
use naijascript::analysis::liveness::unused_assignments;
use naijascript::analysis::opt::build_optimization_plan;
use naijascript::analysis::reachability::reachable_statement_mask;
use naijascript::analysis::summary::compute_summaries;

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
fn optimization_plan_marks_pure_unused_assignment_removable() {
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
            facts,
            &summaries,
            &reachable,
            &unused_assignments,
            &unused_variables,
            &unused_functions,
            arena,
        );

        assert_eq!(plan.removable_ops.len(), 1);
        let removable_stmt = &program.stmts[plan.removable_ops[0].0 as usize];
        assert!(matches!(removable_stmt.kind, naijascript::analysis::cfg::StmtKind::Assign));
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
            facts,
            &summaries,
            &reachable,
            &unused_assignments,
            &unused_variables,
            &unused_functions,
            arena,
        );

        assert!(plan.removable_ops.iter().any(|op| {
            let stmt = &program.stmts[op.0 as usize];
            matches!(stmt.kind, naijascript::analysis::cfg::StmtKind::Expression)
                && !reachable[op.0 as usize]
        }));
        assert_eq!(unused_functions.len(), 1);
        assert_eq!(plan.removable_function_defs.len(), 1);
    });
}
