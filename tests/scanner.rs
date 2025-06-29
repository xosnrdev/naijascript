use std::borrow::Cow;

use naijascript::diagnostics::AsStr;
use naijascript::syntax::scanner::{LexError, Lexer, Token};

#[test]
fn test_minus_and_divide_keywords() {
    let src = "minus divide";
    let mut lexer = Lexer::new(src);
    assert_eq!(lexer.next_token().token, Token::Minus);
    assert_eq!(lexer.next_token().token, Token::Divide);
    assert_eq!(lexer.next_token().token, Token::EOF);
}

#[test]
fn test_shout_and_jasi_keywords() {
    let src = "shout jasi";
    let mut lexer = Lexer::new(src);
    assert_eq!(lexer.next_token().token, Token::Shout);
    assert_eq!(lexer.next_token().token, Token::Jasi);
    assert_eq!(lexer.next_token().token, Token::EOF);
}

#[test]
fn test_pass_and_small_pass_keywords() {
    let src = "pass small pass";
    let mut lexer = Lexer::new(src);
    assert_eq!(lexer.next_token().token, Token::Pass);
    assert_eq!(lexer.next_token().token, Token::SmallPass);
    assert_eq!(lexer.next_token().token, Token::EOF);
}

#[test]
fn test_if_not_so_keyword() {
    let src = "if not so";
    let mut lexer = Lexer::new(src);
    assert_eq!(lexer.next_token().token, Token::IfNotSo);
    assert_eq!(lexer.next_token().token, Token::EOF);
}

#[test]
fn test_make_get_add_times_keywords() {
    let src = "make get add times";
    let mut lexer = Lexer::new(src);
    assert_eq!(lexer.next_token().token, Token::Make);
    assert_eq!(lexer.next_token().token, Token::Get);
    assert_eq!(lexer.next_token().token, Token::Add);
    assert_eq!(lexer.next_token().token, Token::Times);
    assert_eq!(lexer.next_token().token, Token::EOF);
}

#[test]
fn test_start_end_na_if_to_say_keywords() {
    let src = "start end na if to say";
    let mut lexer = Lexer::new(src);
    assert_eq!(lexer.next_token().token, Token::Start);
    assert_eq!(lexer.next_token().token, Token::End);
    assert_eq!(lexer.next_token().token, Token::Na);
    assert_eq!(lexer.next_token().token, Token::IfToSay);
    assert_eq!(lexer.next_token().token, Token::EOF);
}

#[test]
fn test_parentheses() {
    let src = "( )";
    let mut lexer = Lexer::new(src);
    assert_eq!(lexer.next_token().token, Token::LParen);
    assert_eq!(lexer.next_token().token, Token::RParen);
    assert_eq!(lexer.next_token().token, Token::EOF);
}

#[test]
fn test_integer_number() {
    let src = "42";
    let mut lexer = Lexer::new(src);
    assert_eq!(lexer.next_token().token, Token::Number("42"));
    assert_eq!(lexer.next_token().token, Token::EOF);
}

#[test]
fn test_number_with_decimal() {
    let src = "3.14 0.5";
    let mut lexer = Lexer::new(src);
    assert_eq!(lexer.next_token().token, Token::Number("3.14"));
    assert_eq!(lexer.next_token().token, Token::Number("0.5"));
    assert_eq!(lexer.next_token().token, Token::EOF);
}

#[test]
fn test_identifier_letters_only() {
    let src = "foo bar";
    let mut lexer = Lexer::new(src);
    assert_eq!(lexer.next_token().token, Token::Identifier("foo"));
    assert_eq!(lexer.next_token().token, Token::Identifier("bar"));
    assert_eq!(lexer.next_token().token, Token::EOF);
}

#[test]
fn test_identifier_with_digits() {
    let src = "foo1 bar2";
    let mut lexer = Lexer::new(src);
    assert_eq!(lexer.next_token().token, Token::Identifier("foo1"));
    assert_eq!(lexer.next_token().token, Token::Identifier("bar2"));
    assert_eq!(lexer.next_token().token, Token::EOF);
}

#[test]
fn test_string_with_invalid_escape() {
    let src = r#""bad\xescape""#;
    let mut lexer = Lexer::new(src);
    let _ = lexer.next_token();
    let errors = lexer.errors;
    let label = &errors.diagnostics[0].labels[0];
    assert_eq!(label.span, 4..6);
    assert!(errors.diagnostics.iter().any(|e| e.message == LexError::InvalidStringEscape.as_str()));
}

#[test]
fn test_unterminated_string() {
    let src = r#""unterminated string"#;
    let mut lexer = Lexer::new(src);
    let _ = lexer.next_token();
    let errors = lexer.errors;
    let label = &errors.diagnostics[0].labels[0];
    assert_eq!(label.span, 0..src.len());
    assert!(errors.diagnostics.iter().any(|e| e.message == LexError::UnterminatedString.as_str()));
}

#[test]
fn test_invalid_identifier() {
    let src = "1foo";
    let mut lexer = Lexer::new(src);
    let _ = lexer.next_token();
    let errors = lexer.errors;
    let label = &errors.diagnostics[0].labels[0];
    assert_eq!(label.span, 0..1);
    assert!(errors.diagnostics.iter().any(|e| e.message == LexError::InvalidIdentifier.as_str()));
}

#[test]
fn test_invalid_number() {
    let src = "1..2";
    let mut lexer = Lexer::new(src);
    let _ = lexer.next_token();
    let errors = lexer.errors;
    let label = &errors.diagnostics[0].labels[0];
    assert_eq!(label.span, 1..2);
    assert!(errors.diagnostics.iter().any(|e| e.message == LexError::InvalidNumber.as_str()));
}

#[test]
fn test_whitespace_between_tokens() {
    let src = "make   x   get   1";
    let mut lexer = Lexer::new(src);
    assert_eq!(lexer.next_token().token, Token::Make);
    assert_eq!(lexer.next_token().token, Token::Identifier("x"));
    assert_eq!(lexer.next_token().token, Token::Get);
    assert_eq!(lexer.next_token().token, Token::Number("1"));
    assert_eq!(lexer.next_token().token, Token::EOF);
}

#[test]
fn test_plain_string() {
    let src = r#""hello world""#;
    let mut lexer = Lexer::new(src);
    assert_eq!(lexer.next_token().token, Token::String(Cow::Borrowed("hello world")));
    assert_eq!(lexer.next_token().token, Token::EOF);
}

#[test]
fn test_string_with_newline_escape() {
    let src = r#""line\nnext""#;
    let mut lexer = Lexer::new(src);
    assert_eq!(
        lexer.next_token().token,
        Token::String(Cow::Owned("line\nnext".replace("\\n", "\n")))
    );
    assert_eq!(lexer.next_token().token, Token::EOF);
}

#[test]
fn test_string_with_tab_escape() {
    let src = r#""tab\tend""#;
    let mut lexer = Lexer::new(src);
    assert_eq!(
        lexer.next_token().token,
        Token::String(Cow::Owned("tab\tend".replace("\\t", "\t")))
    );
    assert_eq!(lexer.next_token().token, Token::EOF);
}

#[test]
fn test_string_with_escaped_quote() {
    let src = r#""foo\"bar""#;
    let mut lexer = Lexer::new(src);
    assert_eq!(
        lexer.next_token().token,
        Token::String(Cow::Owned("foo\"bar".replace("\\\"", "\"")))
    );
    assert_eq!(lexer.next_token().token, Token::EOF);
}

#[test]
fn test_string_with_escaped_backslash() {
    let src = r#""foo\\bar""#;
    let mut lexer = Lexer::new(src);
    assert_eq!(
        lexer.next_token().token,
        Token::String(Cow::Owned("foo\\bar".replace("\\\\", "\\")))
    );
    assert_eq!(lexer.next_token().token, Token::EOF);
}

#[test]
fn test_empty_string() {
    let src = r#"""#;
    let mut lexer = Lexer::new(src);
    assert_eq!(lexer.next_token().token, Token::String(Cow::Borrowed("")));
    assert_eq!(lexer.next_token().token, Token::EOF);
}

#[test]
fn test_string_with_only_quote() {
    let src = r#""\"""#;
    let mut lexer = Lexer::new(src);
    assert_eq!(lexer.next_token().token, Token::String(Cow::Owned("\"".replace("\\\"", "\""))));
    assert_eq!(lexer.next_token().token, Token::EOF);
}

#[test]
fn test_string_with_only_backslash() {
    let src = r#""\\""#;
    let mut lexer = Lexer::new(src);
    assert_eq!(lexer.next_token().token, Token::String(Cow::Owned("\\".replace("\\\\", "\\"))));
    assert_eq!(lexer.next_token().token, Token::EOF);
}

#[test]
fn test_string_with_all_valid_escapes() {
    let src = r#""\n\t\"\\""#;
    let mut lexer = Lexer::new(src);
    let expected = "\n\t\"\\"
        .replace("\\n", "\n")
        .replace("\\t", "\t")
        .replace("\\\"", "\"")
        .replace("\\\\", "\\");
    assert_eq!(lexer.next_token().token, Token::String(Cow::Owned(expected)));
    assert_eq!(lexer.next_token().token, Token::EOF);
}

#[test]
fn test_string_with_mixed_valid_and_invalid_escapes() {
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

#[test]
fn test_string_with_all_ascii_except_quote_and_backslash() {
    let src = "\"abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789!@#$%^&*()_+-=;:',.<>/?|[]{}~`\"";
    let mut lexer = Lexer::new(src);
    let expected = Cow::Borrowed(
        "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789!@#$%^&*()_+-=;:',.<>/?|[]{}~`",
    );
    assert_eq!(lexer.next_token().token, Token::String(expected));
    assert_eq!(lexer.next_token().token, Token::EOF);
}
