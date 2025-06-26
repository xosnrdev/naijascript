use naijascript::diagnostics::AsStr;
use naijascript::syntax::scanner::{LexError, Lexer, Token};

#[test]
fn test_minus_and_divide_keywords() {
    let src = "minus divide";
    let mut lexer = Lexer::new(src);
    assert_eq!(lexer.next_token(), Token::Minus);
    assert_eq!(lexer.next_token(), Token::Divide);
    assert_eq!(lexer.next_token(), Token::EOF);
}

#[test]
fn test_shout_and_jasi_keywords() {
    let src = "shout jasi";
    let mut lexer = Lexer::new(src);
    assert_eq!(lexer.next_token(), Token::Shout);
    assert_eq!(lexer.next_token(), Token::Jasi);
    assert_eq!(lexer.next_token(), Token::EOF);
}

#[test]
fn test_pass_and_small_pass_keywords() {
    let src = "pass small pass";
    let mut lexer = Lexer::new(src);
    assert_eq!(lexer.next_token(), Token::Pass);
    assert_eq!(lexer.next_token(), Token::SmallPass);
    assert_eq!(lexer.next_token(), Token::EOF);
}

#[test]
fn test_if_not_so_keyword() {
    let src = "if not so";
    let mut lexer = Lexer::new(src);
    assert_eq!(lexer.next_token(), Token::IfNotSo);
    assert_eq!(lexer.next_token(), Token::EOF);
}

#[test]
fn test_make_get_add_times_keywords() {
    let src = "make get add times";
    let mut lexer = Lexer::new(src);
    assert_eq!(lexer.next_token(), Token::Make);
    assert_eq!(lexer.next_token(), Token::Get);
    assert_eq!(lexer.next_token(), Token::Add);
    assert_eq!(lexer.next_token(), Token::Times);
    assert_eq!(lexer.next_token(), Token::EOF);
}

#[test]
fn test_start_end_na_if_to_say_keywords() {
    let src = "start end na if to say";
    let mut lexer = Lexer::new(src);
    assert_eq!(lexer.next_token(), Token::Start);
    assert_eq!(lexer.next_token(), Token::End);
    assert_eq!(lexer.next_token(), Token::Na);
    assert_eq!(lexer.next_token(), Token::IfToSay);
    assert_eq!(lexer.next_token(), Token::EOF);
}

#[test]
fn test_parentheses() {
    let src = "( )";
    let mut lexer = Lexer::new(src);
    assert_eq!(lexer.next_token(), Token::LParen);
    assert_eq!(lexer.next_token(), Token::RParen);
    assert_eq!(lexer.next_token(), Token::EOF);
}

#[test]
fn test_integer_number() {
    let src = "42";
    let mut lexer = Lexer::new(src);
    assert_eq!(lexer.next_token(), Token::Number("42"));
    assert_eq!(lexer.next_token(), Token::EOF);
}

#[test]
fn test_number_with_decimal() {
    let src = "3.14 0.5";
    let mut lexer = Lexer::new(src);
    assert_eq!(lexer.next_token(), Token::Number("3.14"));
    assert_eq!(lexer.next_token(), Token::Number("0.5"));
    assert_eq!(lexer.next_token(), Token::EOF);
}

#[test]
fn test_identifier_letters_only() {
    let src = "foo bar";
    let mut lexer = Lexer::new(src);
    assert_eq!(lexer.next_token(), Token::Identifier("foo"));
    assert_eq!(lexer.next_token(), Token::Identifier("bar"));
    assert_eq!(lexer.next_token(), Token::EOF);
}

#[test]
fn test_identifier_with_digits() {
    let src = "foo1 bar2";
    let mut lexer = Lexer::new(src);
    assert_eq!(lexer.next_token(), Token::Identifier("foo1"));
    assert_eq!(lexer.next_token(), Token::Identifier("bar2"));
    assert_eq!(lexer.next_token(), Token::EOF);
}

#[test]
fn test_string_literal_and_escapes() {
    let src = "\"hello\" \"line\\nnext\" \"tab\\tend\"";
    let mut lexer = Lexer::new(src);
    assert_eq!(lexer.next_token(), Token::String("hello"));
    assert_eq!(lexer.next_token(), Token::String("line\\nnext"));
    assert_eq!(lexer.next_token(), Token::String("tab\\tend"));
    assert_eq!(lexer.next_token(), Token::EOF);
}

#[test]
fn test_string_with_invalid_escape() {
    let src = "\"bad\\xescape\"";
    let mut lexer = Lexer::new(src);
    let _ = lexer.next_token();
    let errors = lexer.errors;
    assert!(errors.diagnostics.iter().any(|e| e.message == LexError::InvalidStringEscape.as_str()));
}

#[test]
fn test_unterminated_string() {
    let src = "\"unterminated string";
    let mut lexer = Lexer::new(src);
    let _ = lexer.next_token();
    let errors = lexer.errors;
    assert!(errors.diagnostics.iter().any(|e| e.message == LexError::UnterminatedString.as_str()));
}

#[test]
fn test_invalid_identifier() {
    let src = "1foo";
    let mut lexer = Lexer::new(src);
    let _ = lexer.next_token();
    let errors = lexer.errors;
    assert!(errors.diagnostics.iter().any(|e| e.message == LexError::InvalidIdentifier.as_str()));
}

#[test]
fn test_invalid_number() {
    let src = "1..2";
    let mut lexer = Lexer::new(src);
    let _ = lexer.next_token();
    let errors = lexer.errors;
    assert!(errors.diagnostics.iter().any(|e| e.message == LexError::InvalidNumber.as_str()));
}

#[test]
fn test_whitespace_between_tokens() {
    let src = "make   x   get   1";
    let mut lexer = Lexer::new(src);
    assert_eq!(lexer.next_token(), Token::Make);
    assert_eq!(lexer.next_token(), Token::Identifier("x"));
    assert_eq!(lexer.next_token(), Token::Get);
    assert_eq!(lexer.next_token(), Token::Number("1"));
    assert_eq!(lexer.next_token(), Token::EOF);
}
