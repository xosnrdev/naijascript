use naijascript::resolver::SemanticError;

mod common;
use crate::common::with_pipeline;

macro_rules! assert_resolve {
    ($src:expr, $err:expr) => {{
        with_pipeline($src, |_, (root, parse_errors), resolver, _| {
            use naijascript::diagnostics::AsStr;

            assert!(
                parse_errors.diagnostics.is_empty(),
                "Expected no parse errors, got: {:?}",
                parse_errors.diagnostics
            );
            resolver.resolve(root);
            assert!(
                resolver.errors.diagnostics.iter().any(|e| e.message == $err.as_str()),
                "Expected resolution error: {}, got: {:?}",
                $err.as_str(),
                resolver.errors.diagnostics
            );
        })
    }};
    ($src:expr) => {{
        with_pipeline($src, |_, (root, parse_errors), resolver, _| {
            assert!(
                parse_errors.diagnostics.is_empty(),
                "Expected no parse errors, got: {:?}",
                parse_errors.diagnostics
            );
            resolver.resolve(root);
            assert!(
                !resolver.errors.has_errors(),
                "Expected no resolution errors, got: {:?}",
                resolver.errors.diagnostics
            );
        })
    }};
}
#[test]
fn test_shadowing_variable_declaration() {
    assert_resolve!("make x get 5 make x get 10");
}

#[test]
fn test_unused_assignment_after_overwrite() {
    assert_resolve!("make x get 1 x get 2 shout(x)", SemanticError::UnusedAssignment);
}

#[test]
fn test_unused_assignment_respects_capture_read() {
    with_pipeline(
        r"
        make x get 1
        do read_x() start
            shout(x)
        end
        read_x()
        ",
        |_, (root, parse_errors), resolver, _| {
            use naijascript::diagnostics::AsStr;

            assert!(parse_errors.diagnostics.is_empty());
            resolver.resolve(root);
            assert!(!resolver.errors.has_errors(), "{:?}", resolver.errors.diagnostics);
            assert!(
                resolver
                    .errors
                    .diagnostics
                    .iter()
                    .all(|diag| diag.message != SemanticError::UnusedAssignment.as_str()),
                "Expected no unused-assignment warning, got: {:?}",
                resolver.errors.diagnostics
            );
        },
    );
}

#[test]
fn test_unused_assignment_detects_capture_write_overwrite() {
    assert_resolve!(
        r"
        make x get 1
        do set_x() start
            x get 2
        end
        set_x()
        ",
        SemanticError::UnusedAssignment
    );
}

#[test]
fn test_unused_assignment_treats_mutating_capture_call_as_read() {
    with_pipeline(
        r"
        make items get []
        do fill() start
            items.push(1)
        end
        fill()
        ",
        |_, (root, parse_errors), resolver, _| {
            use naijascript::diagnostics::AsStr;

            assert!(parse_errors.diagnostics.is_empty());
            resolver.resolve(root);
            assert!(!resolver.errors.has_errors(), "{:?}", resolver.errors.diagnostics);
            assert!(
                resolver
                    .errors
                    .diagnostics
                    .iter()
                    .all(|diag| diag.message != SemanticError::UnusedAssignment.as_str()),
                "Expected no unused-assignment warning, got: {:?}",
                resolver.errors.diagnostics
            );
        },
    );
}

#[test]
fn test_unused_variable_after_never_read_decl() {
    assert_resolve!("make x get 1", SemanticError::UnusedVariable);
}

#[test]
fn test_unused_variable_not_emitted_when_local_is_read() {
    with_pipeline("make x get 1 shout(x)", |_, (root, parse_errors), resolver, _| {
        use naijascript::diagnostics::AsStr;

        assert!(parse_errors.diagnostics.is_empty());
        resolver.resolve(root);
        assert!(!resolver.errors.has_errors(), "{:?}", resolver.errors.diagnostics);
        assert!(
            resolver
                .errors
                .diagnostics
                .iter()
                .all(|diag| diag.message != SemanticError::UnusedVariable.as_str()),
            "Expected no unused-variable warning, got: {:?}",
            resolver.errors.diagnostics
        );
    });
}

#[test]
fn test_unused_variable_ignores_uncalled_capture_read() {
    assert_resolve!(
        r"
        make x get 1
        do read_x() start
            shout(x)
        end
        ",
        SemanticError::UnusedVariable
    );
}

#[test]
fn test_unused_variable_respects_called_capture_read() {
    with_pipeline(
        r"
        make x get 1
        do read_x() start
            shout(x)
        end
        read_x()
        ",
        |_, (root, parse_errors), resolver, _| {
            use naijascript::diagnostics::AsStr;

            assert!(parse_errors.diagnostics.is_empty());
            resolver.resolve(root);
            assert!(!resolver.errors.has_errors(), "{:?}", resolver.errors.diagnostics);
            assert!(
                resolver
                    .errors
                    .diagnostics
                    .iter()
                    .all(|diag| diag.message != SemanticError::UnusedVariable.as_str()),
                "Expected no unused-variable warning, got: {:?}",
                resolver.errors.diagnostics
            );
        },
    );
}

#[test]
fn test_unused_function_for_top_level_definition() {
    assert_resolve!("do foo() start end", SemanticError::UnusedFunction);
}

#[test]
fn test_unused_function_not_emitted_when_called() {
    with_pipeline(
        r"
        do foo() start
        end
        foo()
        ",
        |_, (root, parse_errors), resolver, _| {
            use naijascript::diagnostics::AsStr;

            assert!(parse_errors.diagnostics.is_empty());
            resolver.resolve(root);
            assert!(!resolver.errors.has_errors(), "{:?}", resolver.errors.diagnostics);
            assert!(
                resolver
                    .errors
                    .diagnostics
                    .iter()
                    .all(|diag| diag.message != SemanticError::UnusedFunction.as_str()),
                "Expected no unused-function warning, got: {:?}",
                resolver.errors.diagnostics
            );
        },
    );
}

#[test]
fn test_unused_function_for_reachable_nested_definition() {
    assert_resolve!(
        r"
        do outer() start
            do inner() start
            end
        end
        outer()
        ",
        SemanticError::UnusedFunction
    );
}

#[test]
fn test_unused_function_not_emitted_for_unreachable_nested_definition() {
    with_pipeline(
        r"
        do outer() start
            return 1
            do inner() start
            end
        end
        outer()
        ",
        |_, (root, parse_errors), resolver, _| {
            use naijascript::diagnostics::AsStr;

            assert!(parse_errors.diagnostics.is_empty());
            resolver.resolve(root);
            assert!(!resolver.errors.has_errors(), "{:?}", resolver.errors.diagnostics);
            assert!(
                resolver
                    .errors
                    .diagnostics
                    .iter()
                    .all(|diag| diag.message != SemanticError::UnusedFunction.as_str()),
                "Expected no unused-function warning, got: {:?}",
                resolver.errors.diagnostics
            );
        },
    );
}

#[test]
fn test_use_undeclared_variable() {
    assert_resolve!("shout(x)", SemanticError::UndeclaredIdentifier);
}

#[test]
fn test_assign_to_undeclared_variable() {
    assert_resolve!("x get 5", SemanticError::AssignmentToUndeclared);
}

#[test]
fn test_undeclared_variable_in_condition() {
    assert_resolve!("if to say (x na 1) start end", SemanticError::UndeclaredIdentifier);
}

#[test]
fn test_string_number_comparison() {
    assert_resolve!(r#"if to say ("foo" na 1) start end"#, SemanticError::TypeMismatch);
}

#[test]
fn test_string_subtraction() {
    assert_resolve!(r#"make x get "foo" minus "bar""#, SemanticError::TypeMismatch);
}

#[test]
fn test_string_number_comparison_in_loop() {
    assert_resolve!(r#"jasi ("foo" pass 1) start end"#, SemanticError::TypeMismatch);
}

#[test]
fn test_break_outside_loop() {
    assert_resolve!("comot", SemanticError::UnreachableCode);
}

#[test]
fn test_continue_outside_loop() {
    assert_resolve!("next", SemanticError::UnreachableCode);
}

#[test]
fn test_string_modulus() {
    assert_resolve!(r#"make x get "foo" mod "bar""#, SemanticError::TypeMismatch);
}

#[test]
fn test_boolean_number_comparison() {
    assert_resolve!("if to say (true na 1) start end", SemanticError::TypeMismatch);
}

#[test]
fn test_boolean_string_comparison() {
    assert_resolve!(r#"if to say (true na "test") start end"#, SemanticError::TypeMismatch);
}

#[test]
fn test_boolean_arithmetic_operations() {
    assert_resolve!("make x get true add false", SemanticError::TypeMismatch);
}

#[test]
fn test_block_scope_variable_not_visible_outside() {
    assert_resolve!("start make x get 1 end shout(x)", SemanticError::UndeclaredIdentifier);
}

#[test]
fn test_logical_and_type_error() {
    assert_resolve!("make x get true and 1", SemanticError::TypeMismatch);
    assert_resolve!("make x get 1 and true", SemanticError::TypeMismatch);
    assert_resolve!("make x get 1 and 2", SemanticError::TypeMismatch);
}

#[test]
fn test_logical_or_type_error() {
    assert_resolve!("make x get true or 1", SemanticError::TypeMismatch);
    assert_resolve!("make x get 1 or true", SemanticError::TypeMismatch);
    assert_resolve!("make x get 1 or 2", SemanticError::TypeMismatch);
}

#[test]
fn test_logical_not_type_error() {
    assert_resolve!("make x get not 1", SemanticError::TypeMismatch);
    assert_resolve!(r#"make x get not "foo""#, SemanticError::TypeMismatch);
}

#[test]
fn test_duplicate_function_definition() {
    assert_resolve!("do foo() start end do foo() start end", SemanticError::DuplicateIdentifier);
}

#[test]
fn test_call_undeclared_function() {
    assert_resolve!("foo()", SemanticError::UndeclaredIdentifier);
}

#[test]
fn test_function_call_arity() {
    assert_resolve!("do foo(x, y) start end foo(1)", SemanticError::FunctionCallArity);
}

#[test]
fn test_return_outside_function() {
    assert_resolve!("return 42", SemanticError::UnreachableCode);
}

#[test]
fn test_dead_code_after_return() {
    assert_resolve!(
        "
        do foo() start
            return 42
            make x get 10
            shout(x)
        end
        ",
        SemanticError::UnreachableCode
    );
}

#[test]
fn test_dead_code_after_break_in_loop() {
    assert_resolve!(
        "
        jasi (true) start
            comot
            shout(1)
        end
        ",
        SemanticError::UnreachableCode
    );
}

#[test]
fn test_dead_code_after_continue_in_loop() {
    assert_resolve!(
        "
        jasi (true) start
            next
            shout(1)
        end
        ",
        SemanticError::UnreachableCode
    );
}

#[test]
fn test_dead_code_after_non_fallthrough_if() {
    assert_resolve!(
        "
        do foo() start
            if to say (true) start
                return 1
            end
            if not so start
                return 2
            end
            shout(3)
        end
        ",
        SemanticError::UnreachableCode
    );
}

#[test]
fn test_parameters_not_visible_outside_function() {
    assert_resolve!("do foo(x) start end shout(x)", SemanticError::UndeclaredIdentifier);
}

#[test]
fn test_duplicate_parameter_names() {
    assert_resolve!("do foo(x, x) start end", SemanticError::DuplicateIdentifier);
}

#[test]
fn test_function_not_visible_outside_block() {
    assert_resolve!(
        r"
        start
            do foo() start end
        end
        foo()
        ",
        SemanticError::UndeclaredIdentifier
    );
}

#[test]
fn test_duplicate_function_in_same_block() {
    assert_resolve!(
        r"
        start
            do foo() start end
            do foo() start end
        end
        ",
        SemanticError::DuplicateIdentifier
    );
}

#[test]
fn test_reserved_keyword_as_variable_name() {
    assert_resolve!("make shout get 1", SemanticError::ReservedKeyword);
}

#[test]
fn test_reserved_keyword_as_function_name() {
    assert_resolve!("do shout() start end", SemanticError::ReservedKeyword);
}

#[test]
fn test_reserved_keyword_as_parameter_name() {
    assert_resolve!("do foo(shout) start end", SemanticError::ReservedKeyword);
}

#[test]
fn test_function_return_type_sub_mismatch() {
    assert_resolve!(r#"do sub(a) start return a minus "b" end"#, SemanticError::TypeMismatch);
}

#[test]
fn test_function_return_type_mod_mismatch() {
    assert_resolve!(r#"do rem(a) start return a mod "b" end"#, SemanticError::TypeMismatch);
}

#[test]
fn test_function_return_type_div_mismatch() {
    assert_resolve!(r#"do div(a) start return a divide "b" end"#, SemanticError::TypeMismatch);
}

#[test]
fn test_function_return_type_mul_mismatch() {
    assert_resolve!(r#"do mul(a) start return a times "b" end"#, SemanticError::TypeMismatch);
}

#[test]
fn test_nested_function_return_does_not_widen_outer_return_type() {
    assert_resolve!(
        r"
        do outer() start
            do inner() start
                return 1
            end
        end
        make x get outer() minus 1
        ",
        SemanticError::TypeMismatch
    );
}

#[test]
fn test_non_array_type_index() {
    assert_resolve!("make val get 1[0]", SemanticError::TypeMismatch);
}

#[test]
fn test_array_index_type_not_number() {
    assert_resolve!("make nums get [1, 2] make val get nums[true]", SemanticError::TypeMismatch);
}

#[test]
fn test_array_index_assignment_resolves() {
    assert_resolve!("make nums get [1, 2] nums[0] get 5");
}

#[test]
fn test_array_index_assignment_invalid_target() {
    assert_resolve!("shout(1)[0] get 5", SemanticError::TypeMismatch);
}

#[test]
fn test_unary_minus_with_string() {
    assert_resolve!(r#"make x get minus "hello""#, SemanticError::TypeMismatch);
}

#[test]
fn test_unary_minus_with_boolean() {
    assert_resolve!(r"make x get minus true", SemanticError::TypeMismatch);
}

#[test]
fn test_unary_minus_with_undeclared_variable() {
    assert_resolve!(r"make x get minus y", SemanticError::UndeclaredIdentifier);
}

#[test]
fn test_unresolved_template_variable() {
    assert_resolve!(r#"shout("{foo}")"#, SemanticError::UndeclaredIdentifier);
}
