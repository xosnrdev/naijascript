use std::borrow::Cow;

use naijascript::diagnostics::AsStr;
use naijascript::syntax::scanner::{LexError, Lexer};
use naijascript::syntax::token::{SpannedToken, Token};

macro_rules! assert_tokens {
    ($src:expr, $exp:expr $(,)?) => {{
        let lexer = Lexer::new($src);
        let spanned_tokens: Vec<SpannedToken> = lexer.collect();
        let tokens: Vec<Token> = spanned_tokens.into_iter().map(|st| st.token).collect();
        assert_eq!(tokens, $exp, "Token mismatch");
    }};
}

//------------------------------------------------------------------------
// KEYWORDS
//------------------------------------------------------------------------

#[test]
fn test_scan_keywords() {
    let src = "make get add minus times divide mod jasi start end na pass small pass if to say if not so true false and not or do return ,";
    assert_tokens!(
        src,
        &[
            Token::Make,
            Token::Get,
            Token::Add,
            Token::Minus,
            Token::Times,
            Token::Divide,
            Token::Mod,
            Token::Jasi,
            Token::Start,
            Token::End,
            Token::Na,
            Token::Pass,
            Token::SmallPass,
            Token::IfToSay,
            Token::IfNotSo,
            Token::True,
            Token::False,
            Token::And,
            Token::Not,
            Token::Or,
            Token::Do,
            Token::Return,
            Token::Comma,
        ]
    );
}

//------------------------------------------------------------------------
// PUNCTUATION
//------------------------------------------------------------------------

#[test]
fn test_scan_punctuation() {
    let src = "( )";
    assert_tokens!(src, &[Token::LParen, Token::RParen]);
}

//------------------------------------------------------------------------
// IDENTIFIERS
//------------------------------------------------------------------------

#[test]
fn test_scan_identifier() {
    let src = "foo bar foo1 bar2";
    assert_tokens!(
        src,
        &[
            Token::Identifier("foo"),
            Token::Identifier("bar"),
            Token::Identifier("foo1"),
            Token::Identifier("bar2"),
        ]
    );
}

#[test]
fn test_scan_invalid_identifier() {
    let src = "1foo";
    let mut lexer = Lexer::new(src);
    let _ = lexer.next();
    let errors = lexer.errors;
    let label = &errors.diagnostics[0].labels[0];
    assert_eq!(label.span, 0..1);
    assert!(errors.diagnostics.iter().any(|e| e.message == LexError::InvalidIdentifier.as_str()));
}

#[test]
fn test_scan_identifier_with_underscores() {
    let src = "_foo_bar_123";
    assert_tokens!(src, &[Token::Identifier("_foo_bar_123"),]);
}

//------------------------------------------------------------------------
// NUMBERS
//------------------------------------------------------------------------

#[test]
fn test_scan_number() {
    let src = "42 3.14 0.5";
    assert_tokens!(src, &[Token::Number("42"), Token::Number("3.14"), Token::Number("0.5")]);
}

#[test]
fn test_scan_invalid_number() {
    let src = "1..2";
    let mut lexer = Lexer::new(src);
    let _ = lexer.next();
    let errors = lexer.errors;
    let label = &errors.diagnostics[0].labels[0];
    assert_eq!(label.span, 1..2);
    assert!(errors.diagnostics.iter().any(|e| e.message == LexError::InvalidNumber.as_str()));
}

#[test]
fn test_scan_number_with_underscore_error() {
    let src = "123_foo";
    let mut lexer = Lexer::new(src);
    let _ = lexer.next();
    let errors = lexer.errors;
    let label = &errors.diagnostics[0].labels[0];
    assert_eq!(label.span, 0..3);
    assert!(errors.diagnostics.iter().any(|e| e.message == LexError::InvalidIdentifier.as_str()));
}

//------------------------------------------------------------------------
// STRING
//------------------------------------------------------------------------

#[test]
fn test_scan_plain_string() {
    let src = r#""foo bar""#;
    assert_tokens!(src, &[Token::String(Cow::Borrowed("foo bar"))]);
}

#[test]
fn test_scan_empty_string() {
    let src = r#"""#;
    assert_tokens!(src, &[Token::String(Cow::Borrowed(""))]);
}

#[test]
fn test_scan_string_with_newline_escape() {
    let src = r#""foo\nbar""#;
    assert_tokens!(src, &[Token::String(Cow::Owned("foo\nbar".to_string()))]);
}

#[test]
fn test_scan_string_with_tab_escape() {
    let src = r#""foo\tbar""#;
    assert_tokens!(src, &[Token::String(Cow::Owned("foo\tbar".to_string()))]);
}

#[test]
fn test_scan_string_with_escaped_quote() {
    let src = r#""foo\"bar""#;
    assert_tokens!(src, &[Token::String(Cow::Owned("foo\"bar".to_string()))]);
}

#[test]
fn test_scan_string_with_escaped_backslash() {
    let src = r#""foo\\bar""#;
    assert_tokens!(src, &[Token::String(Cow::Owned("foo\\bar".to_string()))]);
}

#[test]
fn test_scan_string_with_only_quote() {
    let src = r#""\"""#;
    assert_tokens!(src, &[Token::String(Cow::Owned("\"".to_string()))]);
}

#[test]
fn test_scan_string_with_only_backslash() {
    let src = r#""\\""#;
    assert_tokens!(src, &[Token::String(Cow::Owned("\\".to_string()))]);
}

#[test]
fn test_scan_string_with_all_valid_escapes() {
    let src = r#""\n\t\"\\""#;
    assert_tokens!(src, &[Token::String(Cow::Owned("\n\t\"\\".to_string()))]);
}

#[test]
fn test_scan_string_with_invalid_escape() {
    let src = r#""foo\xbar""#;
    let mut lexer = Lexer::new(src);
    let _ = lexer.next();
    let errors = lexer.errors;
    let label = &errors.diagnostics[0].labels[0];
    assert_eq!(label.span, 4..6);
    assert!(errors.diagnostics.iter().any(|e| e.message == LexError::InvalidStringEscape.as_str()));
}

#[test]
fn test_scan_string_with_mixed_valid_and_invalid_escapes() {
    let src = r#""foo\xbar\n""#;
    let mut lexer = Lexer::new(src);
    let st = lexer.next().unwrap_or_default();
    assert_eq!(st.token, Token::String(Cow::Owned(String::from("fooxbar\n"))));
    let errors = lexer.errors;
    let label = &errors.diagnostics[0].labels[0];
    assert_eq!(label.span, 4..6);
    assert!(errors.diagnostics.iter().any(|e| e.message == LexError::InvalidStringEscape.as_str()));
}

#[test]
fn test_scan_unterminated_string() {
    let src = r#""foo bar"#;
    let mut lexer = Lexer::new(src);
    let _ = lexer.next();
    let errors = lexer.errors;
    let label = &errors.diagnostics[0].labels[0];
    assert_eq!(label.span, 0..src.len());
    assert!(errors.diagnostics.iter().any(|e| e.message == LexError::UnterminatedString.as_str()));
}

//------------------------------------------------------------------------
// COMMENTS
//------------------------------------------------------------------------

#[test]
fn test_scan_line_comment() {
    let src = "# foo bar";
    assert_tokens!(src, &[]);
}

#[test]
fn test_scan_trailing_comment() {
    let src = "make x get 1 # trailing comment";
    assert_tokens!(src, &[Token::Make, Token::Identifier("x"), Token::Get, Token::Number("1")]);
}

//------------------------------------------------------------------------
// UNKNOWN
//------------------------------------------------------------------------

#[test]
fn test_scan_unexpected_character() {
    let src = "@";
    let mut lexer = Lexer::new(src);
    let _ = lexer.next();
    let errors = lexer.errors;
    let label = &errors.diagnostics[0].labels[0];
    assert_eq!(label.span, 0..0);
    assert!(errors.diagnostics.iter().any(|e| e.message == LexError::UnexpectedChar.as_str()));
}

#[test]
fn test_scan_utf8_boundary_panic() {
    let src = "😆";
    let mut lexer = Lexer::new(src);
    let _ = lexer.next();
    let errors = lexer.errors;
    let label = &errors.diagnostics[0].labels[0];
    assert_eq!(label.span, 0..4);
    assert!(errors.diagnostics.iter().any(|e| e.message == LexError::UnexpectedChar.as_str()));
}
