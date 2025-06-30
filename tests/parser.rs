use naijascript::diagnostics::AsStr;
use naijascript::syntax::parser::SyntaxError;

mod common;
use crate::common::parse_from_source;

macro_rules! assert_parse {
    ($src:expr) => {{
        let mut parser = parse_from_source($src);
        let (_, errors) = parser.parse_program();
        assert!(errors.diagnostics.is_empty(), "Expected no errors, got {errors:?}");
    }};
    ($src:expr, $err:expr) => {{
        let mut parser = parse_from_source($src);
        let (_, errors) = parser.parse_program();
        assert!(!errors.diagnostics.is_empty(), "Expected errors, got none");
        assert!(
            errors.diagnostics.iter().any(|e| e.message == $err.as_str()),
            "Expected error: {}, got: {:?}",
            $err.as_str(),
            errors.diagnostics
        );
    }};
}

#[test]
fn test_parse_assignment() {
    let src = "make x get 5";
    assert_parse!(src);
}

#[test]
fn test_parse_reassignment() {
    let src = "make x get 5 x get 10";
    assert_parse!(src);
}

#[test]
fn test_parse_shout_statement() {
    let src = "shout(42)";
    assert_parse!(src);
}

#[test]
fn test_parse_if_statement() {
    let src = "if to say (x na 1) start shout(1) end";
    assert_parse!(src);
}

#[test]
fn test_parse_if_else_statement() {
    let src = "if to say (x na 1) start shout(1) end if not so start shout(2) end";
    assert_parse!(src);
}

#[test]
fn test_parse_loop_statement() {
    let src = "jasi (x pass 0) start shout(x) end";
    assert_parse!(src);
}

#[test]
fn test_parse_missing_identifier_after_make() {
    let src = "make get 5";
    assert_parse!(src, SyntaxError::ExpectedIdentifierAfterMake);
}

#[test]
fn test_parse_arithmetic_expression() {
    let src = "make x get 1 add 2 times 3";
    assert_parse!(src);
}

#[test]
fn test_parse_parenthesized_expression() {
    let src = "make x get (1 add 2) times 3";
    assert_parse!(src);
}

#[test]
fn test_parse_condition_na() {
    let src = "if to say (x na 1) start end";
    assert_parse!(src);
}

#[test]
fn test_parse_condition_pass() {
    let src = "if to say (x pass 1) start end";
    assert_parse!(src);
}

#[test]
fn test_parse_condition_small_pass() {
    let src = "if to say (x small pass 1) start end";
    assert_parse!(src);
}

#[test]
fn test_parse_invalid_statement() {
    let src = "x 5";
    assert_parse!(src, SyntaxError::ExpectedStatement);
}

#[test]
fn test_parse_shout_missing_parenthesis() {
    let src = "shout 42";
    assert_parse!(src, SyntaxError::ExpectedLParenAfterShout);
}

#[test]
fn test_parse_if_missing_start() {
    let src = "if to say (x na 1) shout(1) end";
    assert_parse!(src, SyntaxError::ExpectedStartForThenBlock);
}

#[test]
fn test_parse_loop_missing_end() {
    let src = "jasi (x pass 0) start shout(x)";
    assert_parse!(src, SyntaxError::UnterminatedLoopBody);
}

#[test]
fn test_parse_trailing_tokens() {
    let src = "make x get 5 123";
    assert_parse!(src, SyntaxError::TrailingTokensAfterProgramEnd);
}
