use naijascript::diagnostics::AsStr;
use naijascript::resolver::{SemAnalyzer, SemanticError};
use naijascript::syntax::parser::Parser;

#[test]
fn test_semantic_duplicate_declaration() {
    let src = "make x get 1\nmake x get 2";
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
    let src = "make x get 5\nshout(x)";
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
    let src = "make x get 1\nx get 2";
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
fn test_loop_with_type_mismatch_in_condition() {
    let src = "make x get \"hello\"\njasi (x pass 1) start shout(1) end";
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
        "Expected type mismatch in loop condition"
    );
}

#[test]
fn test_add_string_and_number() {
    let src = "make x get \"foo\" add 1";
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
        "Expected type mismatch for string add number"
    );
}

#[test]
fn test_add_string_and_string() {
    let src = "make x get \"foo\" add \"bar\"";
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
        "Expected invalid string operation for string add string"
    );
}

#[test]
fn test_minus_string_and_number() {
    let src = "make x get \"foo\" minus 1";
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
        "Expected type mismatch for string minus number"
    );
}

#[test]
fn test_minus_string_and_string() {
    let src = "make x get \"foo\" minus \"bar\"";
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
        "Expected invalid string operation for string minus string"
    );
}

#[test]
fn test_condition_string_vs_number() {
    let src = "make x get \"foo\"\nif to say (x na 1) start shout(1) end";
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
        "Expected type mismatch in if condition (string vs number)"
    );
}

#[test]
fn test_condition_string_vs_string() {
    let src = "make x get \"foo\"\nmake y get \"bar\"\nif to say (x na y) start shout(1) end";
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
    // Should be valid, so no errors
    assert!(
        analyzer.errors.diagnostics.is_empty(),
        "Expected no semantic errors for string vs string in condition, got: {:#?}",
        analyzer.errors
    );
}
