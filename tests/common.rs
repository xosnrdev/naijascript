use naijascript::syntax::parser::Parser;
use naijascript::syntax::scanner::Lexer;

#[cfg(test)]
pub fn parse_from_source<'src>(src: &'src str) -> Parser<'src, Lexer<'src>> {
    let lexer = Lexer::new(src);
    Parser::new(lexer)
}
