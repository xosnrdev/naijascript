use naijascript::arena::Arena;
use naijascript::syntax::parser::Parser;
use naijascript::syntax::scanner::Lexer;

pub fn parse_from_src<'src, 'ast>(
    src: &'src str,
    arena: &'ast Arena,
) -> Parser<'src, 'ast, Lexer<'ast, 'src>> {
    let lexer = Lexer::new(src, arena);
    Parser::new(lexer, arena)
}
