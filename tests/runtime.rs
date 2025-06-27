use naijascript::diagnostics::AsStr;
use naijascript::resolver::SemAnalyzer;
use naijascript::runtime::{Interpreter, RuntimeErrorKind, Value};
use naijascript::syntax::parser::Parser;

#[test]
fn test_assignment_and_shout() {
    let src = "make x get 5 shout(x)";
    let mut parser = Parser::new(src);
    let (root, parse_errors) = parser.parse_program();
    assert!(parse_errors.diagnostics.is_empty());
    let mut interp = Interpreter::new(
        &parser.stmt_arena,
        &parser.expr_arena,
        &parser.cond_arena,
        &parser.block_arena,
    );
    interp.run(root);
    assert_eq!(interp.output, vec![Value::Number(5.0)]);
}

#[test]
fn test_reassignment() {
    let src = "make x get 2 x get 7 shout(x)";
    let mut parser = Parser::new(src);
    let (root, parse_errors) = parser.parse_program();
    assert!(parse_errors.diagnostics.is_empty());
    let mut interp = Interpreter::new(
        &parser.stmt_arena,
        &parser.expr_arena,
        &parser.cond_arena,
        &parser.block_arena,
    );
    interp.run(root);
    assert_eq!(interp.output, vec![Value::Number(7.0)]);
}

#[test]
fn test_expression_arithmetic() {
    let src = "make x get 2 add 3 times 4 shout(x)"; // 2 + (3*4) = 14
    let mut parser = Parser::new(src);
    let (root, parse_errors) = parser.parse_program();
    assert!(parse_errors.diagnostics.is_empty());
    let mut interp = Interpreter::new(
        &parser.stmt_arena,
        &parser.expr_arena,
        &parser.cond_arena,
        &parser.block_arena,
    );
    interp.run(root);
    assert_eq!(interp.output, vec![Value::Number(14.0)]);
}

#[test]
fn test_if_statement_then() {
    let src = "make x get 1 if to say (x na 1) start shout(42) end";
    let mut parser = Parser::new(src);
    let (root, parse_errors) = parser.parse_program();
    assert!(parse_errors.diagnostics.is_empty());
    let mut interp = Interpreter::new(
        &parser.stmt_arena,
        &parser.expr_arena,
        &parser.cond_arena,
        &parser.block_arena,
    );
    interp.run(root);
    assert_eq!(interp.output, vec![Value::Number(42.0)]);
}

#[test]
fn test_if_statement_else() {
    let src = "make x get 2 if to say (x na 1) start shout(1) end if not so start shout(2) end";
    let mut parser = Parser::new(src);
    let (root, parse_errors) = parser.parse_program();
    assert!(parse_errors.diagnostics.is_empty());
    let mut interp = Interpreter::new(
        &parser.stmt_arena,
        &parser.expr_arena,
        &parser.cond_arena,
        &parser.block_arena,
    );
    interp.run(root);
    assert_eq!(interp.output, vec![Value::Number(2.0)]);
}

#[test]
fn test_loop_statement() {
    let src = "make x get 1 jasi (x small pass 4) start shout(x) x get x add 1 end";
    let mut parser = Parser::new(src);
    let (root, parse_errors) = parser.parse_program();
    assert!(parse_errors.diagnostics.is_empty());
    let mut interp = Interpreter::new(
        &parser.stmt_arena,
        &parser.expr_arena,
        &parser.cond_arena,
        &parser.block_arena,
    );
    interp.run(root);
    assert_eq!(interp.output, vec![Value::Number(1.0), Value::Number(2.0), Value::Number(3.0)]);
}

#[test]
fn test_division_by_zero_error() {
    let src = "make x get 1 divide 0 shout(x)";
    let mut parser = Parser::new(src);
    let (root, parse_errors) = parser.parse_program();
    assert!(parse_errors.diagnostics.is_empty());
    let mut interp = Interpreter::new(
        &parser.stmt_arena,
        &parser.expr_arena,
        &parser.cond_arena,
        &parser.block_arena,
    );
    interp.run(root);
    assert!(
        interp
            .errors
            .diagnostics
            .iter()
            .any(|e| e.message == RuntimeErrorKind::DivisionByZero.as_str())
    );
}

#[test]
fn test_string_assignment_and_output() {
    let src = r#"make s get "hello" shout(s)"#;
    let mut parser = Parser::new(src);
    let (root, parse_errors) = parser.parse_program();
    assert!(parse_errors.diagnostics.is_empty());
    let mut analyzer = SemAnalyzer::new(
        &parser.stmt_arena,
        &parser.expr_arena,
        &parser.cond_arena,
        &parser.block_arena,
    );
    analyzer.analyze(root);
    assert!(analyzer.errors.diagnostics.is_empty());
    let mut interp = Interpreter::new(
        &parser.stmt_arena,
        &parser.expr_arena,
        &parser.cond_arena,
        &parser.block_arena,
    );
    interp.run(root);
    assert_eq!(interp.output, vec![Value::Str("hello")]);
}

#[test]
fn test_string_comparison_condition() {
    let src = r#"make s get "abc" if to say (s na "abc") start shout(1) end if not so start shout(2) end"#;
    let mut parser = Parser::new(src);
    let (root, parse_errors) = parser.parse_program();
    assert!(parse_errors.diagnostics.is_empty());
    let mut analyzer = SemAnalyzer::new(
        &parser.stmt_arena,
        &parser.expr_arena,
        &parser.cond_arena,
        &parser.block_arena,
    );
    analyzer.analyze(root);
    assert!(analyzer.errors.diagnostics.is_empty());
    let mut interp = Interpreter::new(
        &parser.stmt_arena,
        &parser.expr_arena,
        &parser.cond_arena,
        &parser.block_arena,
    );
    interp.run(root);
    assert_eq!(interp.output, vec![Value::Number(1.0)]);
}

#[test]
fn test_parenthesized_expression() {
    let src = "make x get (2 add 3) times 4 shout(x)";
    let mut parser = Parser::new(src);
    let (root, parse_errors) = parser.parse_program();
    assert!(parse_errors.diagnostics.is_empty());
    let mut interp = Interpreter::new(
        &parser.stmt_arena,
        &parser.expr_arena,
        &parser.cond_arena,
        &parser.block_arena,
    );
    interp.run(root);
    assert_eq!(interp.output, vec![Value::Number(20.0)]);
}

#[test]
fn test_minus_and_divide_expression() {
    let src = "make x get 10 minus 2 divide 2 shout(x)";
    let mut parser = Parser::new(src);
    let (root, parse_errors) = parser.parse_program();
    assert!(parse_errors.diagnostics.is_empty());
    let mut interp = Interpreter::new(
        &parser.stmt_arena,
        &parser.expr_arena,
        &parser.cond_arena,
        &parser.block_arena,
    );
    interp.run(root);
    // 10 - (2 / 2) = 9
    assert_eq!(interp.output, vec![Value::Number(9.0)]);
}

#[test]
fn test_condition_pass_operator() {
    let src = "make x get 5 if to say (x pass 3) start shout(1) end if not so start shout(2) end";
    let mut parser = Parser::new(src);
    let (root, parse_errors) = parser.parse_program();
    assert!(parse_errors.diagnostics.is_empty());
    let mut interp = Interpreter::new(
        &parser.stmt_arena,
        &parser.expr_arena,
        &parser.cond_arena,
        &parser.block_arena,
    );
    interp.run(root);
    assert_eq!(interp.output, vec![Value::Number(1.0)]);
}

#[test]
fn test_nested_blocks() {
    let src = "make x get 1 if to say (x na 1) start if to say (x na 1) start shout(42) end end";
    let mut parser = Parser::new(src);
    let (root, parse_errors) = parser.parse_program();
    assert!(parse_errors.diagnostics.is_empty());
    let mut interp = Interpreter::new(
        &parser.stmt_arena,
        &parser.expr_arena,
        &parser.cond_arena,
        &parser.block_arena,
    );
    interp.run(root);
    assert_eq!(interp.output, vec![Value::Number(42.0)]);
}

#[test]
fn test_shout_string_literal() {
    let src = r#"shout("direct")"#;
    let mut parser = Parser::new(src);
    let (root, parse_errors) = parser.parse_program();
    assert!(parse_errors.diagnostics.is_empty());
    let mut interp = Interpreter::new(
        &parser.stmt_arena,
        &parser.expr_arena,
        &parser.cond_arena,
        &parser.block_arena,
    );
    interp.run(root);
    assert_eq!(interp.output, vec![Value::Str("direct")]);
}

#[test]
fn test_string_escape_sequences() {
    let src = r#"make s get "line1\nline2"
shout(s)"#;
    let mut parser = Parser::new(src);
    let (root, parse_errors) = parser.parse_program();
    assert!(parse_errors.diagnostics.is_empty());
    let mut analyzer = SemAnalyzer::new(
        &parser.stmt_arena,
        &parser.expr_arena,
        &parser.cond_arena,
        &parser.block_arena,
    );
    analyzer.analyze(root);
    assert!(analyzer.errors.diagnostics.is_empty());
    let mut interp = Interpreter::new(
        &parser.stmt_arena,
        &parser.expr_arena,
        &parser.cond_arena,
        &parser.block_arena,
    );
    interp.run(root);
    assert_eq!(interp.output, vec![Value::Str("line1\nline2")]);
}

#[test]
fn test_string_inequality_condition() {
    let src = r#"make s get "abc"
if to say (s na "def") start shout(1) end if not so start shout(2) end"#;
    let mut parser = Parser::new(src);
    let (root, parse_errors) = parser.parse_program();
    assert!(parse_errors.diagnostics.is_empty());
    let mut analyzer = SemAnalyzer::new(
        &parser.stmt_arena,
        &parser.expr_arena,
        &parser.cond_arena,
        &parser.block_arena,
    );
    analyzer.analyze(root);
    assert!(analyzer.errors.diagnostics.is_empty());
    let mut interp = Interpreter::new(
        &parser.stmt_arena,
        &parser.expr_arena,
        &parser.cond_arena,
        &parser.block_arena,
    );
    interp.run(root);
    assert_eq!(interp.output, vec![Value::Number(2.0)]);
}
