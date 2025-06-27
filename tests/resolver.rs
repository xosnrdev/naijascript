use naijascript::diagnostics::AsStr;
use naijascript::resolver::{SemAnalyzer, SemanticError};
use naijascript::syntax::parser::Parser;

#[test]
fn test_semantic_duplicate_declaration() {
    let src = "make x get 1 make x get 2";
    let mut parser = Parser::new(src);
    let (root, parse_errors) = parser.parse_program();
    assert!(parse_errors.diagnostics.is_empty(), "Parse errors: {parse_errors:?}");
    let mut analyzer = SemAnalyzer::new(
        &parser.stmt_arena,
        &parser.expr_arena,
        &parser.cond_arena,
        &parser.block_arena,
    );
    analyzer.analyze(root);
    assert!(
        analyzer
            .errors
            .diagnostics
            .iter()
            .any(|e| e.message == SemanticError::DuplicateDeclaration.as_str()),
        "Expected duplicate declaration error"
    );
}

#[test]
fn test_semantic_undeclared_variable() {
    let src = "shout(x)";
    let mut parser = Parser::new(src);
    let (root, parse_errors) = parser.parse_program();
    assert!(parse_errors.diagnostics.is_empty(), "Parse errors: {parse_errors:?}");
    let mut analyzer = SemAnalyzer::new(
        &parser.stmt_arena,
        &parser.expr_arena,
        &parser.cond_arena,
        &parser.block_arena,
    );
    analyzer.analyze(root);
    assert!(
        analyzer
            .errors
            .diagnostics
            .iter()
            .any(|e| e.message == SemanticError::UseOfUndeclared.as_str()),
        "Expected undeclared variable error"
    );
}

#[test]
fn test_semantic_valid_program() {
    let src = "make x get 5 shout(x)";
    let mut parser = Parser::new(src);
    let (root, parse_errors) = parser.parse_program();
    assert!(parse_errors.diagnostics.is_empty(), "Parse errors: {parse_errors:?}");
    let mut analyzer = SemAnalyzer::new(
        &parser.stmt_arena,
        &parser.expr_arena,
        &parser.cond_arena,
        &parser.block_arena,
    );
    analyzer.analyze(root);
    assert!(
        analyzer.errors.diagnostics.is_empty(),
        "Expected no semantic errors, got: {:#?}",
        analyzer.errors
    );
}

#[test]
fn test_assignment_to_undeclared_variable() {
    let src = "x get 5";
    let mut parser = Parser::new(src);
    let (root, parse_errors) = parser.parse_program();
    assert!(parse_errors.diagnostics.is_empty(), "Parse errors: {parse_errors:?}");
    let mut analyzer = SemAnalyzer::new(
        &parser.stmt_arena,
        &parser.expr_arena,
        &parser.cond_arena,
        &parser.block_arena,
    );
    analyzer.analyze(root);
    assert!(
        analyzer
            .errors
            .diagnostics
            .iter()
            .any(|e| e.message == SemanticError::AssignmentToUndeclared.as_str()),
        "Expected assignment to undeclared variable error"
    );
}

#[test]
fn test_reassignment_to_declared_variable() {
    let src = "make x get 1 x get 2";
    let mut parser = Parser::new(src);
    let (root, parse_errors) = parser.parse_program();
    assert!(parse_errors.diagnostics.is_empty(), "Parse errors: {parse_errors:?}");
    let mut analyzer = SemAnalyzer::new(
        &parser.stmt_arena,
        &parser.expr_arena,
        &parser.cond_arena,
        &parser.block_arena,
    );
    analyzer.analyze(root);
    assert!(
        analyzer.errors.diagnostics.is_empty(),
        "Expected no semantic errors, got: {:#?}",
        analyzer.errors
    );
}

#[test]
fn test_if_with_undeclared_variable_in_condition() {
    let src = "if to say (y na 1) start shout(1) end";
    let mut parser = Parser::new(src);
    let (root, parse_errors) = parser.parse_program();
    assert!(parse_errors.diagnostics.is_empty(), "Parse errors: {parse_errors:?}");
    let mut analyzer = SemAnalyzer::new(
        &parser.stmt_arena,
        &parser.expr_arena,
        &parser.cond_arena,
        &parser.block_arena,
    );
    analyzer.analyze(root);
    assert!(
        analyzer
            .errors
            .diagnostics
            .iter()
            .any(|e| e.message == SemanticError::UseOfUndeclared.as_str()),
        "Expected use of undeclared variable in if condition"
    );
}

#[test]
fn test_type_mismatch_in_condition() {
    let src = r#"if to say ("abc" na 1) start shout(1) end"#;
    let mut parser = Parser::new(src);
    let (root, parse_errors) = parser.parse_program();
    assert!(parse_errors.diagnostics.is_empty(), "Parse errors: {parse_errors:?}");
    let mut analyzer = SemAnalyzer::new(
        &parser.stmt_arena,
        &parser.expr_arena,
        &parser.cond_arena,
        &parser.block_arena,
    );
    analyzer.analyze(root);
    assert!(
        analyzer
            .errors
            .diagnostics
            .iter()
            .any(|e| e.message == SemanticError::TypeMismatch.as_str()),
        "Expected type mismatch error in condition"
    );
}

#[test]
fn test_invalid_string_addition() {
    let src = r#"make x get "abc" add "def""#;
    let mut parser = Parser::new(src);
    let (root, parse_errors) = parser.parse_program();
    assert!(parse_errors.diagnostics.is_empty(), "Parse errors: {parse_errors:?}");
    let mut analyzer = SemAnalyzer::new(
        &parser.stmt_arena,
        &parser.expr_arena,
        &parser.cond_arena,
        &parser.block_arena,
    );
    analyzer.analyze(root);
    assert!(
        analyzer
            .errors
            .diagnostics
            .iter()
            .any(|e| e.message == SemanticError::InvalidStringOperation.as_str()),
        "Expected invalid string operation error for add"
    );
}

#[test]
fn test_type_mismatch_in_arithmetic() {
    let src = r#"make x get "abc" add 1"#;
    let mut parser = Parser::new(src);
    let (root, parse_errors) = parser.parse_program();
    assert!(parse_errors.diagnostics.is_empty(), "Parse errors: {parse_errors:?}");
    let mut analyzer = SemAnalyzer::new(
        &parser.stmt_arena,
        &parser.expr_arena,
        &parser.cond_arena,
        &parser.block_arena,
    );
    analyzer.analyze(root);
    assert!(
        analyzer
            .errors
            .diagnostics
            .iter()
            .any(|e| e.message == SemanticError::TypeMismatch.as_str()),
        "Expected type mismatch error in arithmetic"
    );
}

#[test]
fn test_invalid_string_minus() {
    let src = r#"make x get "abc" minus "def""#;
    let mut parser = Parser::new(src);
    let (root, parse_errors) = parser.parse_program();
    assert!(parse_errors.diagnostics.is_empty(), "Parse errors: {parse_errors:?}");
    let mut analyzer = SemAnalyzer::new(
        &parser.stmt_arena,
        &parser.expr_arena,
        &parser.cond_arena,
        &parser.block_arena,
    );
    analyzer.analyze(root);
    assert!(
        analyzer
            .errors
            .diagnostics
            .iter()
            .any(|e| e.message == SemanticError::InvalidStringOperation.as_str()),
        "Expected invalid string operation error for minus"
    );
}

#[test]
fn test_type_mismatch_in_loop_condition() {
    let src = r#"jasi ("abc" pass 1) start shout(1) end"#;
    let mut parser = Parser::new(src);
    let (root, parse_errors) = parser.parse_program();
    assert!(parse_errors.diagnostics.is_empty(), "Parse errors: {parse_errors:?}");
    let mut analyzer = SemAnalyzer::new(
        &parser.stmt_arena,
        &parser.expr_arena,
        &parser.cond_arena,
        &parser.block_arena,
    );
    analyzer.analyze(root);
    assert!(
        analyzer
            .errors
            .diagnostics
            .iter()
            .any(|e| e.message == SemanticError::TypeMismatch.as_str()),
        "Expected type mismatch error in loop condition"
    );
}
