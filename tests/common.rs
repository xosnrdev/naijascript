#![allow(clippy::missing_panics_doc)]

use naijascript::arena::Arena;
use naijascript::diagnostics::Diagnostics;
use naijascript::helpers::KIBI;
use naijascript::resolver::Resolver;
use naijascript::runtime::Runtime;
use naijascript::syntax::parser::{BlockRef, Parser};
use naijascript::syntax::scanner::Lexer;

pub fn with_pipeline<F, R>(src: &str, f: F) -> R
where
    for<'a> F: FnOnce(
        &'a Arena,
        (BlockRef<'a>, &'a Diagnostics),
        &mut Resolver<'a, 'a>,
        &mut Runtime<'a>,
    ) -> R,
{
    let arena = Arena::new(4 * KIBI).unwrap();

    let lexer = Lexer::new(src, &arena);
    let mut parser = Parser::new(lexer, &arena);
    let (root, parse_errors) = parser.parse_program();
    let mut resolver = Resolver::new(&arena);
    let mut runtime = Runtime::new(&arena, None);

    f(&arena, (root, parse_errors), &mut resolver, &mut runtime)
}
