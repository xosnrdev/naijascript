use std::borrow::Cow;

use naijascript::diagnostics::AsStr;
use naijascript::runtime::{Interpreter, RuntimeErrorKind, Value};

mod common;
use crate::common::parse_from_source;

#[macro_export]
macro_rules! assert_runtime {
    ($src:expr, output: $expected:expr) => {{
        let mut parser = parse_from_source($src);
        let (root, parse_errors) = parser.parse_program();
        assert!(
            parse_errors.diagnostics.is_empty(),
            "Expected no parse errors, got: {:?}",
            parse_errors.diagnostics
        );
        let mut resolver = naijascript::resolver::SemAnalyzer::new(
            &parser.stmt_arena,
            &parser.expr_arena,
            &parser.block_arena,
            &parser.param_arena,
            &parser.arg_arena,
        );
        resolver.analyze(root);
        assert!(
            !resolver.errors.has_errors(),
            "Expected no semantic errors, got: {:?}",
            resolver.errors.diagnostics
        );
        let mut interp = Interpreter::new(
            &parser.stmt_arena,
            &parser.expr_arena,
            &parser.block_arena,
            &parser.param_arena,
            &parser.arg_arena,
        );
        interp.run(root);
        assert_eq!(interp.output, $expected);
    }};
    ($src:expr, error: $err:expr) => {{
        let mut parser = parse_from_source($src);
        let (root, parse_errors) = parser.parse_program();
        assert!(
            parse_errors.diagnostics.is_empty(),
            "Expected no parse errors, got: {:?}",
            parse_errors.diagnostics
        );
        let mut interp = Interpreter::new(
            &parser.stmt_arena,
            &parser.expr_arena,
            &parser.block_arena,
            &parser.param_arena,
            &parser.arg_arena,
        );
        interp.run(root);
        assert!(
            interp.errors.diagnostics.iter().any(|e| e.message == $err.as_str()),
            "Expected error: {}, got: {:?}",
            $err.as_str(),
            interp.errors.diagnostics
        );
    }};
}

#[test]
fn variable_assignment() {
    assert_runtime!("make x get 5 shout(x)", output: vec![Value::Number(5.0)]);
}

#[test]
fn variable_reassignment() {
    assert_runtime!("make x get 2 x get 7 shout(x)", output: vec![Value::Number(7.0)]);
}

#[test]
fn arithmetic_addition() {
    assert_runtime!("shout(2 add 3)", output: vec![Value::Number(5.0)]);
}

#[test]
fn arithmetic_precedence() {
    assert_runtime!("shout(2 add 3 times 4)", output: vec![Value::Number(14.0)]);
}

#[test]
fn if_then_branch() {
    assert_runtime!("if to say (1 na 1) start shout(42) end", output: vec![Value::Number(42.0)]);
}

#[test]
fn if_else_branch() {
    assert_runtime!("if to say (1 na 2) start shout(1) end if not so start shout(2) end", output: vec![Value::Number(2.0)]);
}

#[test]
fn loop_execution() {
    assert_runtime!("make x get 1 jasi (x small pass 3) start shout(x) x get x add 1 end", output: vec![Value::Number(1.0), Value::Number(2.0)]);
}

#[test]
fn division_by_zero() {
    assert_runtime!("shout(1 divide 0)", error: RuntimeErrorKind::DivisionByZero);
}

#[test]
fn parenthesized_expression() {
    assert_runtime!("shout((2 add 3) times 4)", output: vec![Value::Number(20.0)]);
}

#[test]
fn operator_precedence() {
    assert_runtime!("shout(10 minus 2 divide 2)", output: vec![Value::Number(9.0)]);
}

#[test]
fn comparison_greater_than() {
    assert_runtime!("if to say (5 pass 3) start shout(1) end", output: vec![Value::Number(1.0)]);
}

#[test]
fn string_equality() {
    assert_runtime!(r#"if to say ("abc" na "abc") start shout(1) end"#, output: vec![Value::Number(1.0)]);
}

#[test]
fn string_concatenation() {
    assert_runtime!(r#"shout("foo" add "bar")"#, output: vec![Value::Str(Cow::Owned("foobar".to_string()))]);
}

#[test]
fn loop_with_mutation() {
    assert_runtime!("make sum get 0 make i get 1 jasi (i small pass 3) start sum get sum add i i get i add 1 end shout(sum)", output: vec![Value::Number(3.0)]);
}

#[test]
fn nested_arithmetic() {
    assert_runtime!("shout((1 add 2) times (3 add 4))", output: vec![Value::Number(21.0)]);
}

#[test]
fn multiplication_by_zero() {
    assert_runtime!("shout(5 times 0)", output: vec![Value::Number(0.0)]);
}

#[test]
fn negative_numbers() {
    assert_runtime!("shout(0 minus 5)", output: vec![Value::Number(-5.0)]);
}

#[test]
fn decimal_arithmetic() {
    assert_runtime!("shout(1.5 add 2.5)", output: vec![Value::Number(4.0)]);
}

#[test]
fn comparison_in_condition() {
    assert_runtime!("make x get 10 make y get 5 if to say (x pass y) start shout(x minus y) end", output: vec![Value::Number(5.0)]);
}

#[test]
fn empty_string_concatenation() {
    assert_runtime!(r#"shout("" add "test")"#, output: vec![Value::Str(Cow::Owned("test".to_string()))]);
}

#[test]
fn modulus_by_zero() {
    assert_runtime!("shout(5 mod 0)", error: RuntimeErrorKind::DivisionByZero);
}

#[test]
fn boolean_literals() {
    assert_runtime!("shout(true)", output: vec![Value::Bool(true)]);
    assert_runtime!("shout(false)", output: vec![Value::Bool(false)]);
}

#[test]
fn boolean_assignment() {
    assert_runtime!("make x get true shout(x)", output: vec![Value::Bool(true)]);
    assert_runtime!("make y get false shout(y)", output: vec![Value::Bool(false)]);
}

#[test]
fn boolean_comparison() {
    assert_runtime!("if to say (true na true) start shout(1) end", output: vec![Value::Number(1.0)]);
    assert_runtime!("if to say (true na false) start shout(1) end if not so start shout(2) end", output: vec![Value::Number(2.0)]);
}

#[test]
fn boolean_ordering() {
    assert_runtime!("if to say (false small pass true) start shout(1) end", output: vec![Value::Number(1.0)]);
    assert_runtime!("if to say (true pass false) start shout(1) end", output: vec![Value::Number(1.0)]);
}

#[test]
fn block_scope_shadowing_and_lifetime() {
    assert_runtime!("make x get 1 start make x get 2 shout(x) end shout(x)", output: vec![Value::Number(2.0), Value::Number(1.0)]);
}

#[test]
fn logical_and_operator() {
    assert_runtime!("shout(true and true)", output: vec![Value::Bool(true)]);
    assert_runtime!("shout(true and false)", output: vec![Value::Bool(false)]);
    assert_runtime!("shout(false and true)", output: vec![Value::Bool(false)]);
    assert_runtime!("shout(false and false)", output: vec![Value::Bool(false)]);
}

#[test]
fn logical_or_operator() {
    assert_runtime!("shout(true or true)", output: vec![Value::Bool(true)]);
    assert_runtime!("shout(true or false)", output: vec![Value::Bool(true)]);
    assert_runtime!("shout(false or true)", output: vec![Value::Bool(true)]);
    assert_runtime!("shout(false or false)", output: vec![Value::Bool(false)]);
}

#[test]
fn logical_not_operator() {
    assert_runtime!("shout(not true)", output: vec![Value::Bool(false)]);
    assert_runtime!("shout(not false)", output: vec![Value::Bool(true)]);
}

#[test]
fn logical_operator_precedence() {
    assert_runtime!("shout(true or false and false)", output: vec![Value::Bool(true)]);
    assert_runtime!("shout((true or false) and false)", output: vec![Value::Bool(false)]);
}

#[test]
fn logical_not_precedence() {
    assert_runtime!("shout(not true or true)", output: vec![Value::Bool(true)]);
    assert_runtime!("shout(not (true or true))", output: vec![Value::Bool(false)]);
}

#[test]
fn function_definition_and_call() {
    assert_runtime!("do foo() start shout(42) end foo()", output: vec![Value::Number(42.0)]);
}

#[test]
fn function_with_parameters() {
    assert_runtime!("do sum(a, b) start shout(a add b) end sum(3, 4)", output: vec![Value::Number(7.0)]);
}

#[test]
fn function_with_return_value() {
    assert_runtime!("do square(x) start return x times x end shout(square(5))", output: vec![Value::Number(25.0)]);
}

#[test]
fn function_call_as_expression() {
    assert_runtime!("do double(x) start return x times 2 end make result get double(5) shout(result)", output: vec![Value::Number(10.0)]);
}

#[test]
fn string_parameter_concatenation() {
    assert_runtime!(
        r#"do greet(name) start shout("Hello " add name) end greet("World")"#,
        output: vec![Value::Str(Cow::Owned("Hello World".to_string()))]
    );
}

#[test]
fn number_parameter_string_concatenation() {
    assert_runtime!(
        r#"do format(count) start shout("Items: " add count) end format(42)"#,
        output: vec![Value::Str(Cow::Owned("Items: 42".to_string()))]
    );
}

#[test]
fn string_number_parameter_concatenation() {
    assert_runtime!(
        r#"do formatReverse(count, suffix) start shout(count add suffix) end formatReverse(42, " items")"#,
        output: vec![Value::Str(Cow::Owned("42 items".to_string()))]
    );
}

#[test]
fn mixed_parameter_types_concatenation() {
    assert_runtime!(
        r#"do mixed(prefix, number, suffix) start shout(prefix add number add suffix) end mixed("Count: ", 42, " total")"#,
        output: vec![Value::Str(Cow::Owned("Count: 42 total".to_string()))]
    );
}

#[test]
fn multiple_string_parameters() {
    assert_runtime!(
        r#"do join(first, second, third) start shout(first add " " add second add " " add third) end join("Hello", "beautiful", "world")"#,
        output: vec![Value::Str(Cow::Owned("Hello beautiful world".to_string()))]
    );
}

#[test]
fn decimal_number_string_concatenation() {
    assert_runtime!(
        r#"do price(amount) start shout("$" add amount) end price(99.99)"#,
        output: vec![Value::Str(Cow::Owned("$99.99".to_string()))]
    );
}

#[test]
fn boolean_parameter_operations() {
    assert_runtime!(
        r#"do check(flag) start if to say (flag na true) start shout("YES") end end check(true)"#,
        output: vec![Value::Str(Cow::Owned("YES".to_string()))]
    );
}

#[test]
fn control_flow_else_return_type() {
    assert_runtime!(
        r#"
        do join(a) start
            if to say (true) start
                return a add "baz"
            end
            return a
        end
        shout(join("foo"))
        "#,
        output: vec![Value::Str(Cow::Owned("foobaz".to_string()))]
    );
    assert_runtime!(
        r#"
        do join(a) start
            if to say (false) start
                return a add "baz"
            end
            return a
        end
        shout(join("baz"))
        "#,
        output: vec![Value::Str(Cow::Owned("baz".to_string()))]
    );
}

#[test]
fn do_sum() {
    assert_runtime!(
        r#"
        do sum(a) start
            return a add 5
        end
        shout(sum(5))
        "#,
        output: vec![Value::Number(10.0)]
    );
}

#[test]
fn do_join() {
    assert_runtime!(
        r#"
        do join(a) start
            return a add "baz"
        end
        shout(join("foo"))
        "#,
        output: vec![Value::Str(Cow::Owned("foobaz".to_string()))]
    );
}

#[test]
fn do_dynamic_concatenation() {
    assert_runtime!(
        r#"shout("one" add 2)"#,
        output: vec![Value::Str(Cow::Owned("one2".to_string()))]
    );
}
