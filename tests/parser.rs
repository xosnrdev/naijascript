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
    assert_parse!(src, SyntaxError::ReservedKeyword);
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
    assert_parse!(src, SyntaxError::ExpectedStatement);
}

#[test]
fn test_parse_if_missing_start() {
    let src = "if to say (x na 1) shout(1) end";
    assert_parse!(src, SyntaxError::ExpectedStartBlock);
}

#[test]
fn test_parse_loop_missing_end() {
    let src = "jasi (x pass 0) start shout(x)";
    assert_parse!(src, SyntaxError::UnterminatedBlock);
}

#[test]
fn test_parse_trailing_tokens() {
    let src = "make x get 5 123";
    assert_parse!(src, SyntaxError::TrailingTokensAfterProgramEnd);
}

#[test]
fn test_parse_boolean_literals() {
    let src = "make x get true make y get false";
    assert_parse!(src);
}

#[test]
fn test_parse_boolean_comparison() {
    let src = "if to say (true na false) start end";
    assert_parse!(src);
}

#[test]
fn test_reserved_keyword_as_identifier() {
    let src = "make make get 5";
    assert_parse!(src, SyntaxError::ReservedKeyword);
}

#[test]
fn test_parse_function_def_no_params() {
    let src = "do foo() start end";
    assert_parse!(src);
}

#[test]
fn test_parse_function_def_with_params() {
    let src = "do sum(a, b) start return a add b end";
    assert_parse!(src);
}

#[test]
fn test_parse_function_def_trailing_comma() {
    let src = "do foo(a, b,) start end";
    assert_parse!(src);
}

#[test]
fn test_parse_function_def_missing_parens() {
    let src = "do foo start end";
    assert_parse!(src, SyntaxError::ExpectedLParen);
}

#[test]
fn test_parse_function_def_missing_start() {
    let src = "do foo() shout(1) end";
    assert_parse!(src, SyntaxError::ExpectedStartBlock);
}

#[test]
fn test_parse_function_def_missing_end() {
    let src = "do foo() start shout(1)";
    assert_parse!(src, SyntaxError::UnterminatedBlock);
}

#[test]
fn test_parse_function_def_keyword_as_param() {
    let src = "do foo(jasi) start end";
    assert_parse!(src, SyntaxError::ReservedKeyword);
}

#[test]
fn test_parse_function_def_reserved_keyword_name() {
    let src = "do make() start end";
    assert_parse!(src, SyntaxError::ReservedKeyword);
}

#[test]
fn test_parse_function_def_empty_body() {
    let src = "do foo() start end";
    assert_parse!(src);
}

#[test]
fn test_parse_function_def_with_return() {
    let src = "do foo() start return 1 end";
    assert_parse!(src);
}

#[test]
fn test_parse_nested_function_def() {
    let src = "do outer() start do inner() start end end";
    assert_parse!(src);
}

#[test]
fn test_parse_function_call_no_args() {
    let src = "foo()";
    assert_parse!(src);
}

#[test]
fn test_parse_function_call_with_args() {
    let src = "foo(1, 2, 3)";
    assert_parse!(src);
}

#[test]
fn test_parse_function_call_trailing_comma() {
    let src = "foo(1, 2,)";
    assert_parse!(src);
}

#[test]
fn test_parse_function_call_nested() {
    let src = "foo(bar(1), 2)";
    assert_parse!(src);
}

#[test]
fn test_parse_function_call_in_expression() {
    let src = "foo(1) add 2";
    assert_parse!(src);
}

#[test]
fn test_parse_function_call_missing_rparen() {
    let src = "foo(1, 2";
    assert_parse!(src, SyntaxError::ExpectedRParen);
}

#[test]
fn test_parse_return_no_value() {
    let src = "return";
    assert_parse!(src);
}

#[test]
fn test_parse_return_with_value() {
    let src = "return 42";
    assert_parse!(src);
}
