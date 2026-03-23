#![feature(allocator_api)]

use naijascript::arena::ArenaCow;
use naijascript::diagnostics::AsStr;
use naijascript::process::HostPolicy;
use naijascript::runtime::{RuntimeErrorKind, Value};

mod common;
use common::{process_helper_path, with_pipeline, with_pipeline_and_host_policy};

fn helper_path() -> &'static str {
    process_helper_path()
        .to_str()
        .expect("process helper path should be valid UTF-8 for NaijaScript string literals")
}

fn helper_dir() -> String {
    process_helper_path()
        .parent()
        .expect("helper binary should have a parent directory")
        .display()
        .to_string()
}

fn ns_string_literal(text: &str) -> String {
    let mut quoted = String::with_capacity(text.len() + 2);
    quoted.push('"');
    for ch in text.chars() {
        match ch {
            '\\' => quoted.push_str("\\\\"),
            '"' => quoted.push_str("\\\""),
            '\n' => quoted.push_str("\\n"),
            '\r' => quoted.push_str("\\r"),
            '\t' => quoted.push_str("\\t"),
            _ => quoted.push(ch),
        }
    }
    quoted.push('"');
    quoted
}

macro_rules! assert_process_output {
    ($src:expr, $expected:expr) => {{
        with_pipeline($src, |_, (root, parse_errors), resolver, runtime| {
            assert!(parse_errors.diagnostics.is_empty(), "{:?}", parse_errors.diagnostics);
            resolver.resolve(root);
            assert!(!resolver.errors.has_errors(), "{:?}", resolver.errors.diagnostics);
            runtime.run_with_analysis(root, &resolver.facts, resolver.optimization_plan.as_ref());
            assert!(!runtime.errors.has_errors(), "{:?}", runtime.errors.diagnostics);
            assert_eq!(runtime.output, $expected);
        })
    }};
    ($src:expr, $expected:expr, policy: $policy:expr) => {{
        with_pipeline_and_host_policy(
            $src,
            $policy,
            |_, (root, parse_errors), resolver, runtime| {
                assert!(parse_errors.diagnostics.is_empty(), "{:?}", parse_errors.diagnostics);
                resolver.resolve(root);
                assert!(!resolver.errors.has_errors(), "{:?}", resolver.errors.diagnostics);
                runtime.run_with_analysis(
                    root,
                    &resolver.facts,
                    resolver.optimization_plan.as_ref(),
                );
                assert!(!runtime.errors.has_errors(), "{:?}", runtime.errors.diagnostics);
                assert_eq!(runtime.output, $expected);
            },
        )
    }};
}

macro_rules! assert_process_error {
    ($src:expr, $expected:expr) => {{
        with_pipeline($src, |_, (root, parse_errors), resolver, runtime| {
            assert!(parse_errors.diagnostics.is_empty(), "{:?}", parse_errors.diagnostics);
            resolver.resolve(root);
            assert!(!resolver.errors.has_errors(), "{:?}", resolver.errors.diagnostics);
            runtime.run_with_analysis(root, &resolver.facts, resolver.optimization_plan.as_ref());
            assert!(
                runtime.errors.diagnostics.iter().any(|diag| diag.message == $expected.as_str()),
                "Expected runtime error {}, got {:?}",
                $expected.as_str(),
                runtime.errors.diagnostics
            );
        })
    }};
    ($src:expr, $expected:expr, policy: $policy:expr) => {{
        with_pipeline_and_host_policy(
            $src,
            $policy,
            |_, (root, parse_errors), resolver, runtime| {
                assert!(parse_errors.diagnostics.is_empty(), "{:?}", parse_errors.diagnostics);
                resolver.resolve(root);
                assert!(!resolver.errors.has_errors(), "{:?}", resolver.errors.diagnostics);
                runtime.run_with_analysis(
                    root,
                    &resolver.facts,
                    resolver.optimization_plan.as_ref(),
                );
                assert!(
                    runtime
                        .errors
                        .diagnostics
                        .iter()
                        .any(|diag| diag.message == $expected.as_str()),
                    "Expected runtime error {}, got {:?}",
                    $expected.as_str(),
                    runtime.errors.diagnostics
                );
            },
        )
    }};
}

#[test]
fn direct_execution_reports_types_and_captured_output() {
    let helper = ns_string_literal(helper_path());
    let src = format!(
        r#"
        make cmd get command({helper})
        cmd.arg("echo-stdout")
        cmd.arg("hello")
        cmd.stdout_capture()
        make res get cmd.run()
        shout(typeof(cmd))
        shout(typeof(res))
        shout(res.success())
        shout(res.exit_code())
        shout(res.stdout())
        shout(res.stderr())
        "#
    );

    assert_process_output!(
        &src,
        vec![
            Value::Str(ArenaCow::borrowed("process_command")),
            Value::Str(ArenaCow::borrowed("process_result")),
            Value::Bool(true),
            Value::Number(0.0),
            Value::Str(ArenaCow::borrowed("hello")),
            Value::Null,
        ]
    );
}

#[test]
fn non_zero_exit_is_result_data() {
    let helper = ns_string_literal(helper_path());
    let src = format!(
        r#"
        make cmd get command({helper})
        cmd.arg("exit")
        cmd.arg("7")
        make res get cmd.run()
        shout(res.success())
        shout(res.exit_code())
        "#
    );

    assert_process_output!(&src, vec![Value::Bool(false), Value::Number(7.0)]);
}

#[test]
fn path_lookup_uses_env_override() {
    let dir = ns_string_literal(&helper_dir());
    let src = format!(
        r#"
        make cmd get command("process_helper")
        cmd.arg("echo-stdout")
        cmd.arg("from_path")
        cmd.env("PATH", {dir})
        cmd.stdout_capture()
        make res get cmd.run()
        shout(res.stdout())
        "#
    );

    assert_process_output!(&src, vec![Value::Str(ArenaCow::borrowed("from_path"))]);
}

#[test]
fn argument_boundaries_are_preserved() {
    let helper = ns_string_literal(helper_path());
    let src = format!(
        r#"
        make cmd get command({helper})
        cmd.arg("print-args")
        cmd.arg("hello world")
        cmd.arg("*literal*")
        cmd.arg("quote \" mark")
        cmd.stdout_capture()
        make res get cmd.run()
        shout(res.stdout())
        "#
    );

    assert_process_output!(
        &src,
        vec![Value::Str(ArenaCow::borrowed("hello world\n*literal*\nquote \" mark"))]
    );
}

#[test]
fn cwd_override_is_observed() {
    let helper = ns_string_literal(helper_path());
    let expected_cwd =
        std::env::current_dir().expect("current directory should exist").display().to_string();
    let cwd = ns_string_literal(&expected_cwd);
    let src = format!(
        r#"
        make cmd get command({helper})
        cmd.arg("print-cwd")
        cmd.cwd({cwd})
        cmd.stdout_capture()
        make res get cmd.run()
        shout(res.stdout())
        "#
    );
    with_pipeline(&src, |_, (root, parse_errors), resolver, runtime| {
        assert!(parse_errors.diagnostics.is_empty(), "{:?}", parse_errors.diagnostics);
        resolver.resolve(root);
        assert!(!resolver.errors.has_errors(), "{:?}", resolver.errors.diagnostics);
        runtime.run_with_analysis(root, &resolver.facts, resolver.optimization_plan.as_ref());
        assert!(!runtime.errors.has_errors(), "{:?}", runtime.errors.diagnostics);
        assert_eq!(
            runtime.output,
            vec![Value::Str(ArenaCow::owned(naijascript::arena::ArenaString::from_str(
                runtime.output.allocator(),
                &expected_cwd
            )))]
        );
    });
}

#[test]
fn env_override_is_observed() {
    let helper = ns_string_literal(helper_path());
    let src = format!(
        r#"
        make cmd get command({helper})
        cmd.arg("print-env")
        cmd.arg("NAIJA_TEST_VALUE")
        cmd.env("NAIJA_TEST_VALUE", "from_env")
        cmd.stdout_capture()
        make res get cmd.run()
        shout(res.stdout())
        "#
    );

    assert_process_output!(&src, vec![Value::Str(ArenaCow::borrowed("from_env"))]);
}

#[test]
fn stdin_text_round_trips() {
    let helper = ns_string_literal(helper_path());
    let src = format!(
        r#"
        make cmd get command({helper})
        cmd.arg("cat-stdin")
        cmd.stdin_text("b\na\n")
        cmd.stdout_capture()
        make res get cmd.run()
        shout(res.stdout())
        "#
    );

    assert_process_output!(&src, vec![Value::Str(ArenaCow::borrowed("b\na\n"))]);
}

#[test]
fn stdin_null_produces_empty_capture() {
    let helper = ns_string_literal(helper_path());
    let src = format!(
        r#"
        make cmd get command({helper})
        cmd.arg("cat-stdin")
        cmd.stdin_null()
        cmd.stdout_capture()
        make res get cmd.run()
        shout(res.stdout())
        "#
    );

    assert_process_output!(&src, vec![Value::Str(ArenaCow::borrowed(""))]);
}

#[test]
fn captures_stdout_and_stderr_independently() {
    let helper = ns_string_literal(helper_path());
    let src = format!(
        r#"
        make cmd get command({helper})
        cmd.arg("echo-both")
        cmd.arg("out")
        cmd.arg("err")
        cmd.stdout_capture()
        cmd.stderr_capture()
        make res get cmd.run()
        shout(res.stdout())
        shout(res.stderr())
        "#
    );

    assert_process_output!(
        &src,
        vec![Value::Str(ArenaCow::borrowed("out")), Value::Str(ArenaCow::borrowed("err")),]
    );
}

#[test]
fn stdout_inherit_clears_capture_and_leaves_stderr_capture_intact() {
    let helper = ns_string_literal(helper_path());
    let src = format!(
        r#"
        make cmd get command({helper})
        cmd.arg("echo-both")
        cmd.arg("inherit_out")
        cmd.arg("captured_err")
        cmd.stdout_capture()
        cmd.stdout_inherit()
        cmd.stderr_capture()
        make res get cmd.run()
        shout(res.stdout())
        shout(res.stderr())
        "#
    );

    assert_process_output!(&src, vec![Value::Null, Value::Str(ArenaCow::borrowed("captured_err"))]);
}

#[test]
fn stdout_null_clears_capture_and_drops_stdout() {
    let helper = ns_string_literal(helper_path());
    let src = format!(
        r#"
        make cmd get command({helper})
        cmd.arg("echo-both")
        cmd.arg("dropped_out")
        cmd.arg("captured_err")
        cmd.stdout_capture()
        cmd.stdout_null()
        cmd.stderr_capture()
        make res get cmd.run()
        shout(res.stdout())
        shout(res.stderr())
        "#
    );

    assert_process_output!(&src, vec![Value::Null, Value::Str(ArenaCow::borrowed("captured_err"))]);
}

#[test]
fn stderr_inherit_clears_capture_and_leaves_stdout_capture_intact() {
    let helper = ns_string_literal(helper_path());
    let src = format!(
        r#"
        make cmd get command({helper})
        cmd.arg("echo-both")
        cmd.arg("captured_out")
        cmd.arg("inherit_err")
        cmd.stdout_capture()
        cmd.stderr_capture()
        cmd.stderr_inherit()
        make res get cmd.run()
        shout(res.stdout())
        shout(res.stderr())
        "#
    );

    assert_process_output!(&src, vec![Value::Str(ArenaCow::borrowed("captured_out")), Value::Null]);
}

#[test]
fn stderr_null_clears_capture_and_drops_stderr() {
    let helper = ns_string_literal(helper_path());
    let src = format!(
        r#"
        make cmd get command({helper})
        cmd.arg("echo-both")
        cmd.arg("captured_out")
        cmd.arg("dropped_err")
        cmd.stdout_capture()
        cmd.stderr_capture()
        cmd.stderr_null()
        make res get cmd.run()
        shout(res.stdout())
        shout(res.stderr())
        "#
    );

    assert_process_output!(&src, vec![Value::Str(ArenaCow::borrowed("captured_out")), Value::Null]);
}

#[test]
fn timeout_reports_runtime_error() {
    let helper = ns_string_literal(helper_path());
    let src = format!(
        r#"
        make cmd get command({helper})
        cmd.arg("sleep-ms")
        cmd.arg("50")
        cmd.timeout_ms(1)
        cmd.run()
        "#
    );

    assert_process_error!(&src, RuntimeErrorKind::ProcessTimeout);
}

#[test]
fn capture_overflow_reports_runtime_error() {
    let helper = ns_string_literal(helper_path());
    let src = format!(
        r#"
        make cmd get command({helper})
        cmd.arg("echo-stdout")
        cmd.arg("hello")
        cmd.stdout_capture()
        cmd.run()
        "#
    );
    let mut policy = HostPolicy::native_default();
    policy.process.max_capture_bytes_per_stream = 4;

    assert_process_error!(&src, RuntimeErrorKind::ProcessOutputLimitExceeded("stdout"), policy: policy);
}

#[test]
fn invalid_utf8_capture_reports_runtime_error() {
    let helper = ns_string_literal(helper_path());
    let src = format!(
        r#"
        make cmd get command({helper})
        cmd.arg("emit-bytes")
        cmd.arg("ff")
        cmd.stdout_capture()
        cmd.run()
        "#
    );

    assert_process_error!(&src, RuntimeErrorKind::ProcessInvalidUtf8("stdout"));
}

#[test]
fn denied_policy_reports_runtime_error() {
    let helper = ns_string_literal(helper_path());
    let src = format!(
        r#"
        make cmd get command({helper})
        cmd.arg("echo-stdout")
        cmd.arg("hello")
        cmd.run()
        "#
    );
    let mut policy = HostPolicy::native_default();
    policy.allow_process = false;

    assert_process_error!(&src, RuntimeErrorKind::ProcessDenied, policy: policy);
}
