#![cfg(target_family = "wasm")]

use naijascript::arena::{self, scratch_arena};
use naijascript::helpers::MEBI;
use naijascript::resolver::Resolver;
use naijascript::runtime::Runtime;
use naijascript::syntax::parser::Parser;
use naijascript::syntax::scanner::Lexer;
use naijascript::syntax::token::SpannedToken;
use wasm_bindgen::prelude::*;

#[wasm_bindgen]
pub fn run_source(src: &str, filename: &str) -> String {
    if let Err(err) = arena::init(256 * MEBI) {
        return format!("Failed to initialize arena: {err}");
    }
    let arena = scratch_arena(None);

    let mut lexer = Lexer::new(src, &arena);
    let tokens: Vec<SpannedToken> = (&mut lexer).collect();
    if !lexer.errors.diagnostics.is_empty() {
        return report_html(&lexer.errors.render_ansi(src, filename));
    }

    let mut parser = Parser::new(tokens.into_iter(), &arena);
    let (root, err) = parser.parse_program();
    if !err.diagnostics.is_empty() {
        return report_html(&err.render_ansi(src, filename));
    }

    let mut resolver = Resolver::new(&arena);
    resolver.resolve(root);
    if resolver.errors.has_errors() {
        return report_html(&resolver.errors.render_ansi(src, filename));
    }
    let mut non_err = String::with_capacity(src.len() / 2);
    if !resolver.errors.diagnostics.is_empty() {
        non_err.push_str(&report_html(&resolver.errors.render_ansi(src, filename)));
    }

    let mut runtime = Runtime::new(&arena);
    let err = runtime.run(root);
    if err.has_errors() {
        return report_html(&err.render_ansi(src, filename));
    }
    if !err.diagnostics.is_empty() {
        non_err.push_str(&report_html(&err.render_ansi(src, filename)));
    }

    let res = runtime.output.iter().map(ToString::to_string).collect::<Vec<_>>().join("\n");
    if !non_err.is_empty() {
        non_err.push_str(&res);
        return non_err;
    }
    res
}

fn report_html(ansi: &str) -> String {
    ansi_to_html::convert(ansi).unwrap()
}
