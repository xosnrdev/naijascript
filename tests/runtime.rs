#![feature(allocator_api)]

use naijascript::KIBI;
use naijascript::arena::{Arena, ArenaCow, ArenaString};
use naijascript::diagnostics::AsStr;
use naijascript::runtime::{Runtime, RuntimeErrorKind, Value};

mod common;
use crate::common::_parse_from_src;

macro_rules! assert_runtime {
    ($src:expr, output: $expected:expr) => {{
        let arena = Arena::new(KIBI).unwrap();
        let mut parser = _parse_from_src($src, &arena);
        let (root, err) = parser.parse_program();
        assert!(err.diagnostics.is_empty(), "Expected no parse errors, got: {:?}", err.diagnostics);
        let mut resolver = naijascript::resolver::Resolver::new(&arena);
        resolver.resolve(root);
        assert!(
            !resolver.errors.has_errors(),
            "Expected no semantic errors, got: {:?}",
            resolver.errors.diagnostics
        );
        let mut runtime = Runtime::new(&arena);
        runtime.run(root);
        assert_eq!(runtime.output, $expected);
    }};
    ($src:expr, error: $err:expr) => {{
        let arena = naijascript::arena::Arena::new(4 * KIBI).unwrap();
        let mut parser = _parse_from_src($src, &arena);
        let (root, err) = parser.parse_program();
        assert!(err.diagnostics.is_empty(), "Expected no parse errors, got: {:?}", err.diagnostics);
        let mut runtime = Runtime::new(&arena);
        runtime.run(root);
        assert!(
            runtime.errors.diagnostics.iter().any(|e| e.message == $err.as_str()),
            "Expected error: {}, got: {:?}",
            $err.as_str(),
            runtime.errors.diagnostics
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
    assert_runtime!(r#"shout("foo" add "bar")"#, output: vec![Value::Str(ArenaCow::Borrowed("foobar"))]);
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
    assert_runtime!(r#"shout("" add "test")"#, output: vec![Value::Str(ArenaCow::Borrowed("test"))]);
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
fn array_output() {
    let arena = Arena::new(4 * KIBI).unwrap();
    let mut value = Vec::with_capacity_in(3, &arena);
    value.extend_from_slice(&[Value::Number(1.0), Value::Number(2.0), Value::Number(3.0)]);
    assert_runtime!("make nums get [1, 2, 3] shout(nums)", output: vec![Value::Array(value)]);
}

#[test]
fn empty_array() {
    let arena = Arena::new(4 * KIBI).unwrap();
    let value: Vec<Value, &Arena> = Vec::with_capacity_in(0, &arena);
    assert_runtime!("make empty get [] shout(empty)", output: vec![Value::Array(value)]);
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
        output: vec![Value::Str(ArenaCow::Borrowed("Hello World"))]
    );
}

#[test]
fn number_parameter_string_concatenation() {
    assert_runtime!(
        r#"do format(count) start shout("Items: " add count) end format(42)"#,
        output: vec![Value::Str(ArenaCow::Borrowed("Items: 42"))]
    );
}

#[test]
fn string_number_parameter_concatenation() {
    assert_runtime!(
        r#"do formatReverse(count, suffix) start shout(count add suffix) end formatReverse(42, " items")"#,
        output: vec![Value::Str(ArenaCow::Borrowed("42 items"))]
    );
}

#[test]
fn mixed_parameter_types_concatenation() {
    assert_runtime!(
        r#"do mixed(prefix, number, suffix) start shout(prefix add number add suffix) end mixed("Count: ", 42, " total")"#,
        output: vec![Value::Str(ArenaCow::Borrowed("Count: 42 total"))]
    );
}

#[test]
fn multiple_string_parameters() {
    assert_runtime!(
        r#"do join(first, second, third) start shout(first add " " add second add " " add third) end join("Hello", "beautiful", "world")"#,
        output: vec![Value::Str(ArenaCow::Borrowed("Hello beautiful world"))]
    );
}

#[test]
fn decimal_number_string_concatenation() {
    assert_runtime!(
        r#"do price(amount) start shout("$" add amount) end price(99.99)"#,
        output: vec![Value::Str(ArenaCow::Borrowed("$99.99"))]
    );
}

#[test]
fn boolean_parameter_operations() {
    assert_runtime!(
        r#"do check(flag) start if to say (flag na true) start shout("YES") end end check(true)"#,
        output: vec![Value::Str(ArenaCow::Borrowed("YES"))]
    );
}

#[test]
fn control_flow_else_return_type() {
    let arena = Arena::new(KIBI).unwrap();
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
        output: vec![Value::Str(ArenaCow::Owned(ArenaString::from_str(&arena, "foobaz")))]
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
        output: vec![Value::Str(ArenaCow::Borrowed("baz"))]
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
    let arena = Arena::new(KIBI).unwrap();
    assert_runtime!(
        r#"
        do join(a) start
            return a add "baz"
        end
        shout(join("foo"))
        "#,
        output: vec![Value::Str(ArenaCow::Owned(ArenaString::from_str(&arena, "foobaz")))]
    );
}

#[test]
fn do_dynamic_concatenation() {
    assert_runtime!(
        r#"shout("one" add 2)"#,
        output: vec![Value::Str(ArenaCow::Borrowed("one2"))]
    );
}

#[test]
fn unary_minus() {
    assert_runtime!("shout(minus 5)", output: vec![Value::Number(-5.0)]);
}

#[test]
fn unary_minus_in_parentheses() {
    assert_runtime!("shout((minus 5))", output: vec![Value::Number(-5.0)]);
}

#[test]
fn unary_minus_with_variable() {
    assert_runtime!("make x get 3 shout(minus x)", output: vec![Value::Number(-3.0)]);
}

#[test]
fn unary_minus_precedence() {
    // Should parse as (-3) * 4 = -12
    assert_runtime!("shout(minus 3 times 4)", output: vec![Value::Number(-12.0)]);
}

#[test]
fn double_unary_minus() {
    // Should parse as -(-5) = 5
    assert_runtime!("shout(minus minus 5)", output: vec![Value::Number(5.0)]);
}

#[test]
fn unary_minus_in_arithmetic() {
    // Should parse as 3 + (-2) = 1
    assert_runtime!("shout(3 add minus 2)", output: vec![Value::Number(1.0)]);
}

#[test]
fn mixed_binary_and_unary_minus() {
    // Should parse as 5 - (-3) = 8
    assert_runtime!("shout(5 minus minus 3)", output: vec![Value::Number(8.0)]);
}

#[test]
fn unary_minus_with_decimal() {
    assert_runtime!("shout(minus 3.12)", output: vec![Value::Number(-3.12)]);
}

#[test]
fn string_interpolation() {
    assert_runtime!(
        r#"
        make name get "World"
        shout("Hello {name}")
        "#,
        output: vec![Value::Str(ArenaCow::Borrowed("Hello World"))]
    );
}

#[test]
fn string_interpolation_multiple() {
    assert_runtime!(
        r#"
        make name get "John"
        make age get 25
        shout("{name} get {age} years")
        "#,
        output: vec![Value::Str(ArenaCow::Borrowed("John get 25 years"))]
    );
}

#[test]
fn string_interpolation_numbers() {
    assert_runtime!(
        r#"
        make price get 99.99
        shout("Price: ${price}")
        "#,
        output: vec![Value::Str(ArenaCow::Borrowed("Price: $99.99"))]
    );
}

#[test]
fn string_interpolation_booleans() {
    assert_runtime!(
        r#"
        make available get true
        shout("Available: {available}")
        "#,
        output: vec![Value::Str(ArenaCow::Borrowed("Available: true"))]
    );
}

#[test]
fn string_interpolation_literal_braces() {
    assert_runtime!(
        r#"shout("Use {{x}} for literal braces")"#,
        output: vec![Value::Str(ArenaCow::Borrowed("Use {x} for literal braces"))]
    );
}

#[test]
fn string_interpolation_single_brace() {
    assert_runtime!(
        r#"shout("{")"#,
        output: vec![Value::Str(ArenaCow::Borrowed("{"))]
    );
}

#[test]
fn string_interpolation_empty_placeholder() {
    assert_runtime!(
        r#"shout("{}")"#,
        output: vec![Value::Str(ArenaCow::Borrowed("{}"))]
    );
}

#[test]
fn string_interpolation_nested_braces() {
    assert_runtime!(
        r#"shout("{a{b}c}")"#,
        output: vec![Value::Str(ArenaCow::Borrowed("{a{b}c}"))]
    );
}

#[test]
fn string_interpolation_resolve_whitespace() {
    assert_runtime!(
        r#"
        make name get "World"
        shout("Hello { name }")
        "#,
        output: vec![Value::Str(ArenaCow::Borrowed("Hello World"))]
    );
}

#[test]
fn uninit_variable() {
    assert_runtime!("make x x get 5 shout(x)", output: vec![Value::Number(5.0)]);
}
