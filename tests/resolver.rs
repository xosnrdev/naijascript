use naijascript::diagnostics::AsStr;
use naijascript::resolver::{Resolver, SemanticError};

mod common;
use crate::common::parse_from_source;

macro_rules! assert_resolve {
    ($src:expr, $err:expr) => {{
        let mut parser = parse_from_source($src);
        let (root, parser_errors) = parser.parse_program();
        assert!(
            parser_errors.diagnostics.is_empty(),
            "Expected no parse errors, got: {:?}",
            parser_errors.diagnostics
        );
        let mut analyzer = Resolver::new(
            &parser.stmt_arena,
            &parser.expr_arena,
            &parser.block_arena,
            &parser.param_arena,
            &parser.arg_arena,
        );
        analyzer.analyze(root);
        assert!(
            analyzer.errors.diagnostics.iter().any(|e| e.message == $err.as_str()),
            "Expected error: {}, got: {:?}",
            $err.as_str(),
            analyzer.errors.diagnostics
        );
    }};
}

#[test]
fn test_duplicate_variable_declaration() {
    assert_resolve!("make x get 5 make x get 10", SemanticError::DuplicateIdentifier);
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
    assert_resolve!(r#"make x get "foo" minus "bar""#, SemanticError::InvalidStringOperation);
}

#[test]
fn test_string_number_comparison_in_loop() {
    assert_resolve!(r#"jasi ("foo" pass 1) start end"#, SemanticError::TypeMismatch);
}

#[test]
fn test_string_modulus() {
    assert_resolve!(r#"make x get "foo" mod "bar""#, SemanticError::InvalidStringOperation);
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
fn test_block_scope_duplicate_in_same_block() {
    assert_resolve!("start make x get 1 make x get 2 end", SemanticError::DuplicateIdentifier);
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
        r#"
        start
            do foo() start end
        end
        foo()
        "#,
        SemanticError::UndeclaredIdentifier
    );
}

#[test]
fn test_duplicate_function_in_same_block() {
    assert_resolve!(
        r#"
        start
            do foo() start end
            do foo() start end
        end
        "#,
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
fn test_inconsistent_return_type() {
    assert_resolve!(
        r#"
        do foo() start
            if to say (true) start
                return 1
            end
            return "two"
        end
        "#,
        SemanticError::TypeMismatch
    );
}

#[test]
fn test_unary_minus_with_string() {
    assert_resolve!(r#"make x get minus "hello""#, SemanticError::TypeMismatch);
}

#[test]
fn test_unary_minus_with_boolean() {
    assert_resolve!(r#"make x get minus true"#, SemanticError::TypeMismatch);
}

#[test]
fn test_unary_minus_with_undeclared_variable() {
    assert_resolve!(r#"make x get minus y"#, SemanticError::UndeclaredIdentifier);
}
