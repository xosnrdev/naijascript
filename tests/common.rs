#![allow(clippy::missing_panics_doc, unused)]

use naijascript::arena::Arena;
use naijascript::diagnostics::Diagnostics;
use naijascript::helpers::MEBI;
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
    let arena = Arena::new(4 * MEBI).unwrap();
    let frame = Arena::new(2 * MEBI).unwrap();

    let lexer = Lexer::new(src, &arena);
    let mut parser = Parser::new(lexer, &arena);
    let (root, parse_errors) = parser.parse_program();
    let mut resolver = Resolver::new(&arena);
    let mut runtime = Runtime::new(&arena, Some(&frame));

    f(&arena, (root, parse_errors), &mut resolver, &mut runtime)
}

/// Same as [`with_pipeline`] but with larger arenas and a separate frame arena
/// for stress testing memory management under load.
pub fn with_stress_pipeline<F, R>(src: &str, f: F) -> R
where
    for<'a> F: FnOnce(
        &'a Arena,
        (BlockRef<'a>, &'a Diagnostics),
        &mut Resolver<'a, 'a>,
        &mut Runtime<'a>,
    ) -> R,
{
    let arena = Arena::new(64 * MEBI).unwrap();
    let frame = Arena::new(16 * MEBI).unwrap();

    let lexer = Lexer::new(src, &arena);
    let mut parser = Parser::new(lexer, &arena);
    let (root, parse_errors) = parser.parse_program();
    let mut resolver = Resolver::new(&arena);
    let mut runtime = Runtime::new(&arena, Some(&frame));

    f(&arena, (root, parse_errors), &mut resolver, &mut runtime)
}
