use std::borrow::Cow;

use naijascript::diagnostics::AsStr;
use naijascript::syntax::scanner::{LexError, Lexer, Token};

macro_rules! assert_tokens {
    ($lexer:expr, $($expected:expr),+ $(,)?) => {{
        let mut lexer = $lexer;
        $(
            assert_eq!(&lexer.next_token().token, &$expected);
        )+
    }};
}

//------------------------------------------------------------------------
// KEYWORDS
//------------------------------------------------------------------------

#[test]
fn test_scan_keywords() {
    let src = "make get add minus times divide shout jasi start end na pass small pass if to say if not so";
    assert_tokens!(
        Lexer::new(src),
        Token::Make,
        Token::Get,
        Token::Add,
        Token::Minus,
        Token::Times,
        Token::Divide,
        Token::Shout,
        Token::Jasi,
        Token::Start,
        Token::End,
        Token::Na,
        Token::Pass,
        Token::SmallPass,
        Token::IfToSay,
        Token::IfNotSo,
        Token::EOF
    );
}

//------------------------------------------------------------------------
// PUNCTUATION
//------------------------------------------------------------------------

#[test]
fn test_scan_punctuation() {
    let src = "( )";
    assert_tokens!(Lexer::new(src), Token::LParen, Token::RParen, Token::EOF);
}

//------------------------------------------------------------------------
// IDENTIFIERS
//------------------------------------------------------------------------

#[test]
fn test_scan_identifiers() {
    let src = "foo bar foo1 bar2";
    assert_tokens!(
        Lexer::new(src),
        Token::Identifier("foo"),
        Token::Identifier("bar"),
        Token::Identifier("foo1"),
        Token::Identifier("bar2"),
        Token::EOF
    );
}

#[test]
fn test_scan_numbers() {
    let src = "42 3.14 0.5";
    assert_tokens!(
        Lexer::new(src),
        Token::Number("42"),
        Token::Number("3.14"),
        Token::Number("0.5"),
        Token::EOF
    );
}

#[test]
fn test_scan_strings() {
    let src = r#""hello world" """#;
    assert_tokens!(
        Lexer::new(src),
        Token::String(Cow::Borrowed("hello world")),
        Token::String(Cow::Borrowed("")),
        Token::EOF
    );
}

//------------------------------------------------------------------------
// STRING LITERALS
//------------------------------------------------------------------------

#[test]
fn test_scan_string_with_invalid_escape() {
    let src = r#""bad\xescape""#;
    let mut lexer = Lexer::new(src);
    let _ = lexer.next_token();
    let errors = lexer.errors;
    let label = &errors.diagnostics[0].labels[0];
    assert_eq!(label.span, 4..6);
    assert!(errors.diagnostics.iter().any(|e| e.message == LexError::InvalidStringEscape.as_str()));
}

#[test]
fn test_scan_unterminated_string() {
    let src = r#""unterminated string"#;
    let mut lexer = Lexer::new(src);
    let _ = lexer.next_token();
    let errors = lexer.errors;
    let label = &errors.diagnostics[0].labels[0];
    assert_eq!(label.span, 0..src.len());
    assert!(errors.diagnostics.iter().any(|e| e.message == LexError::UnterminatedString.as_str()));
}

#[test]
fn test_scan_invalid_identifier() {
    let src = "1foo";
    let mut lexer = Lexer::new(src);
    let _ = lexer.next_token();
    let errors = lexer.errors;
    let label = &errors.diagnostics[0].labels[0];
    assert_eq!(label.span, 0..1);
    assert!(errors.diagnostics.iter().any(|e| e.message == LexError::InvalidIdentifier.as_str()));
}

#[test]
fn test_scan_invalid_number() {
    let src = "1..2";
    let mut lexer = Lexer::new(src);
    let _ = lexer.next_token();
    let errors = lexer.errors;
    let label = &errors.diagnostics[0].labels[0];
    assert_eq!(label.span, 1..2);
    assert!(errors.diagnostics.iter().any(|e| e.message == LexError::InvalidNumber.as_str()));
}

#[test]
fn test_scan_plain_string() {
    let src = r#""hello world""#;
    assert_tokens!(Lexer::new(src), Token::String(Cow::Borrowed("hello world")), Token::EOF);
}

#[test]
fn test_scan_string_with_newline_escape() {
    let src = r#""line\nnext""#;
    assert_tokens!(
        Lexer::new(src),
        Token::String(Cow::Owned("line\nnext".to_string())),
        Token::EOF
    );
}

#[test]
fn test_scan_string_with_tab_escape() {
    let src = r#""tab\tend""#;
    assert_tokens!(Lexer::new(src), Token::String(Cow::Owned("tab\tend".to_string())), Token::EOF);
}

#[test]
fn test_scan_string_with_escaped_quote() {
    let src = r#""foo\"bar""#;
    assert_tokens!(Lexer::new(src), Token::String(Cow::Owned("foo\"bar".to_string())), Token::EOF);
}

#[test]
fn test_scan_string_with_escaped_backslash() {
    let src = r#""foo\\bar""#;
    assert_tokens!(Lexer::new(src), Token::String(Cow::Owned("foo\\bar".to_string())), Token::EOF);
}

#[test]
fn test_scan_empty_string() {
    let src = r#""""#;
    assert_tokens!(Lexer::new(src), Token::String(Cow::Borrowed("")), Token::EOF);
}

#[test]
fn test_scan_string_with_only_quote() {
    let src = r#""\"""#;
    assert_tokens!(Lexer::new(src), Token::String(Cow::Owned("\"".to_string())), Token::EOF);
}

#[test]
fn test_scan_string_with_only_backslash() {
    let src = r#""\\""#;
    assert_tokens!(Lexer::new(src), Token::String(Cow::Owned("\\".to_string())), Token::EOF);
}

#[test]
fn test_scan_string_with_all_valid_escapes() {
    let src = r#""\n\t\"\\""#;
    assert_tokens!(Lexer::new(src), Token::String(Cow::Owned("\n\t\"\\".to_string())), Token::EOF);
}

#[test]
fn test_scan_string_with_mixed_valid_and_invalid_escapes() {
    let src = r#""foo\xbar\n""#;
    let mut lexer = Lexer::new(src);
    let mut expected = String::from("foo");
    expected.push('x');
    expected.push_str("bar");
    expected.push('\n');
    assert_eq!(lexer.next_token().token, Token::String(Cow::Owned(expected)));
    let errors = lexer.errors;
    let label = &errors.diagnostics[0].labels[0];
    assert_eq!(label.span, 4..6);
    assert!(errors.diagnostics.iter().any(|e| e.message == LexError::InvalidStringEscape.as_str()));
}
