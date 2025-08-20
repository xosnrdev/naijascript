use naijascript::arena::Arena;
use naijascript::syntax::parser::Parser;
use naijascript::syntax::scanner::Lexer;

pub fn _parse_from_src<'src, 'ast>(
    src: &'src str,
    arena: &'ast Arena,
) -> Parser<'src, 'ast, Lexer<'src>> {
    let lexer = Lexer::new(src);
    Parser::new(lexer, arena)
}
