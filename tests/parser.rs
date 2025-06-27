use naijascript::diagnostics::AsStr;
use naijascript::syntax::parser::{Parser, SyntaxError};

#[test]
fn test_parse_assignment() {
    let src = "make x get 42";
    let mut parser = Parser::new(src);
    let (_block_id, errors) = parser.parse_program();
    assert!(errors.diagnostics.is_empty(), "Expected no errors, got {errors:?}");
}

#[test]
fn test_parse_shout() {
    let src = "shout(1 add 2)";
    let mut parser = Parser::new(src);
    let (_block_id, errors) = parser.parse_program();
    assert!(errors.diagnostics.is_empty(), "Expected no errors, got {errors:?}");
}

#[test]
fn test_parse_invalid() {
    let src = "make get 5";
    let mut parser = Parser::new(src);
    let (_block_id, errors) = parser.parse_program();
    assert!(!errors.diagnostics.is_empty(), "Expected errors for invalid syntax");
    assert!(
        errors
            .diagnostics
            .iter()
            .any(|e| e.message == SyntaxError::ExpectedIdentifierAfterMake.as_str()
                || e.message == SyntaxError::ExpectedGetAfterIdentifier.as_str())
    );
}

#[test]
fn test_parse_reassignment() {
    let src = "x get 5";
    let mut parser = Parser::new(src);
    let (_block_id, errors) = parser.parse_program();
    assert!(errors.diagnostics.is_empty(), "Expected no errors, got {errors:?}");
}

#[test]
fn test_parse_if_then() {
    let src = "if to say (x na 1) start shout(42) end";
    let mut parser = Parser::new(src);
    let (_block_id, errors) = parser.parse_program();
    assert!(errors.diagnostics.is_empty(), "Expected no errors, got {errors:?}");
}

#[test]
fn test_parse_if_then_else() {
    let src = "if to say (x na 1) start shout(1) end if not so start shout(2) end";
    let mut parser = Parser::new(src);
    let (_block_id, errors) = parser.parse_program();
    assert!(errors.diagnostics.is_empty(), "Expected no errors, got {errors:?}");
}

#[test]
fn test_parse_loop() {
    let src = "jasi (x pass 0) start shout(x) end";
    let mut parser = Parser::new(src);
    let (_block_id, errors) = parser.parse_program();
    assert!(errors.diagnostics.is_empty(), "Expected no errors, got {errors:?}");
}

#[test]
fn test_parse_expression_minus_times_divide() {
    let src = "make y get 1 minus 2 times 3 divide 4";
    let mut parser = Parser::new(src);
    let (_block_id, errors) = parser.parse_program();
    assert!(errors.diagnostics.is_empty(), "Expected no errors, got {errors:?}");
}

#[test]
fn test_parse_variable_factor() {
    let src = "make x get 1 shout(x)";
    let mut parser = Parser::new(src);
    let (_block_id, errors) = parser.parse_program();
    assert!(errors.diagnostics.is_empty(), "Expected no errors, got {errors:?}");
}

#[test]
fn test_parse_parenthesized_expression() {
    let src = "shout((1 add 2) times 3)";
    let mut parser = Parser::new(src);
    let (_block_id, errors) = parser.parse_program();
    assert!(errors.diagnostics.is_empty(), "Expected no errors, got {errors:?}");
}

#[test]
fn test_parse_condition_operators() {
    let src = "if to say (x na 1) start end if to say (x pass 1) start end if to say (x small pass 1) start end";
    let mut parser = Parser::new(src);
    let (_block_id, errors) = parser.parse_program();
    assert!(errors.diagnostics.is_empty(), "Expected no errors, got {errors:?}");
}

#[test]
fn test_parse_reassignment_missing_get() {
    let src = "x 5";
    let mut parser = Parser::new(src);
    let (_block_id, errors) = parser.parse_program();
    assert!(!errors.diagnostics.is_empty(), "Expected error for missing 'get'");
    assert!(
        errors.diagnostics.iter().any(|e| e.message == SyntaxError::ExpectedStatement.as_str())
    );
}

#[test]
fn test_parse_shout_missing_paren() {
    let src = "shout 1";
    let mut parser = Parser::new(src);
    let (_block_id, errors) = parser.parse_program();
    assert!(!errors.diagnostics.is_empty(), "Expected error for missing '(' in shout");
    assert!(
        errors
            .diagnostics
            .iter()
            .any(|e| e.message == SyntaxError::ExpectedLParenAfterShout.as_str())
    );
}

#[test]
fn test_parse_if_missing_start() {
    let src = "if to say (x na 1) shout(1) end";
    let mut parser = Parser::new(src);
    let (_block_id, errors) = parser.parse_program();
    assert!(!errors.diagnostics.is_empty(), "Expected error for missing 'start' in if block");
    assert!(
        errors
            .diagnostics
            .iter()
            .any(|e| e.message == SyntaxError::ExpectedStartForThenBlock.as_str())
    );
}

#[test]
fn test_parse_loop_missing_end() {
    let src = "jasi (x pass 0) start shout(x)";
    let mut parser = Parser::new(src);
    let (_block_id, errors) = parser.parse_program();
    assert!(!errors.diagnostics.is_empty(), "Expected error for missing 'end' in loop block");
    assert!(
        errors.diagnostics.iter().any(|e| e.message == SyntaxError::UnterminatedLoopBody.as_str())
    );
}

#[test]
fn test_parse_invalid_identifier() {
    let src = "make 1x get 5";
    let mut parser = Parser::new(src);
    let (_block_id, errors) = parser.parse_program();
    assert!(!errors.diagnostics.is_empty(), "Expected error for invalid identifier");
}

#[test]
fn test_parse_trailing_tokens() {
    let src = "make x get 1 123";
    let mut parser = Parser::new(src);
    let (_block_id, errors) = parser.parse_program();
    assert!(!errors.diagnostics.is_empty(), "Expected error for trailing tokens");
    assert!(
        errors
            .diagnostics
            .iter()
            .any(|e| e.message == SyntaxError::TrailingTokensAfterProgramEnd.as_str())
    );
}

#[test]
fn test_parse_assignment_string() {
    let src = r#"make x get "hello""#;
    let mut parser = Parser::new(src);
    let (_block_id, errors) = parser.parse_program();
    assert!(errors.diagnostics.is_empty(), "Expected no errors, got {errors:?}");
}

#[test]
fn test_parse_reassignment_string() {
    let src = r#"x get "world""#;
    let mut parser = Parser::new(src);
    let (_block_id, errors) = parser.parse_program();
    assert!(errors.diagnostics.is_empty(), "Expected no errors, got {errors:?}");
}

#[test]
fn test_parse_shout_string() {
    let src = r#"shout("hi")"#;
    let mut parser = Parser::new(src);
    let (_block_id, errors) = parser.parse_program();
    assert!(errors.diagnostics.is_empty(), "Expected no errors, got {errors:?}");
}

#[test]
fn test_parse_if_condition_string() {
    let src = r#"if to say (x na "foo") start end"#;
    let mut parser = Parser::new(src);
    let (_block_id, errors) = parser.parse_program();
    assert!(errors.diagnostics.is_empty(), "Expected no errors, got {errors:?}");
}

#[test]
fn test_parse_if_string_left_side() {
    let src = r#"if to say ("foo" na x) start end"#;
    let mut parser = Parser::new(src);
    let (_block_id, errors) = parser.parse_program();
    assert!(errors.diagnostics.is_empty(), "Expected no errors, got {errors:?}");
}

#[test]
fn test_parse_if_string_both_sides() {
    let src = r#"if to say ("foo" na "bar") start end"#;
    let mut parser = Parser::new(src);
    let (_block_id, errors) = parser.parse_program();
    assert!(errors.diagnostics.is_empty(), "Expected no errors, got {errors:?}");
}

#[test]
fn test_parse_parenthesized_string() {
    let src = r#"shout(("hello"))"#;
    let mut parser = Parser::new(src);
    let (_block_id, errors) = parser.parse_program();
    assert!(errors.diagnostics.is_empty(), "Expected no errors, got {errors:?}");
}

#[test]
fn test_parse_loop_condition_string() {
    let src = r#"jasi ("foo" na x) start shout(x) end"#;
    let mut parser = Parser::new(src);
    let (_block_id, errors) = parser.parse_program();
    assert!(errors.diagnostics.is_empty(), "Expected no errors, got {errors:?}");
}
