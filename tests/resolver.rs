use naijascript::diagnostics::AsStr;
use naijascript::resolver::{SemAnalyzer, SemanticError};

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
        let mut analyzer = SemAnalyzer::new(
            &parser.stmt_arena,
            &parser.expr_arena,
            &parser.cond_arena,
            &parser.block_arena,
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
    assert_resolve!("make x get 5 make x get 10", SemanticError::DuplicateDeclaration);
}

#[test]
fn test_use_undeclared_variable() {
    assert_resolve!("shout(x)", SemanticError::UseOfUndeclared);
}

#[test]
fn test_assign_to_undeclared_variable() {
    assert_resolve!("x get 5", SemanticError::AssignmentToUndeclared);
}

#[test]
fn test_undeclared_variable_in_condition() {
    assert_resolve!("if to say (x na 1) start end", SemanticError::UseOfUndeclared);
}

#[test]
fn test_string_number_comparison() {
    assert_resolve!(r#"if to say ("foo" na 1) start end"#, SemanticError::TypeMismatch);
}

#[test]
fn test_string_number_addition() {
    assert_resolve!(r#"make x get "foo" add 1"#, SemanticError::TypeMismatch);
}

#[test]
fn test_string_subtraction() {
    assert_resolve!(r#"make x get "foo" minus "bar""#, SemanticError::InvalidStringOperation);
}

#[test]
fn test_string_number_comparison_in_loop() {
    assert_resolve!(r#"jasi ("foo" pass 1) start end"#, SemanticError::TypeMismatch);
}
