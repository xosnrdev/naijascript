#![cfg(target_family = "wasm")]

use naijascript::arena::{self, scratch_arena};
use naijascript::helpers::MEBI;
use naijascript::resolver::Resolver;
use naijascript::runtime::Runtime;
use naijascript::syntax::parser::Parser;
use naijascript::syntax::scanner::Lexer;
use wasm_bindgen::prelude::*;

#[wasm_bindgen]
pub fn run_source(src: &str, filename: &str) -> String {
    if let Err(err) = arena::init(16 * MEBI) {
        return format!("Failed to initialize arena: {err}");
    }
    let arena = scratch_arena(None);

    let lexer = Lexer::new(src, &arena);
    let mut parser = Parser::new(lexer, &arena);
    let (root, err) = parser.parse_program();
    if !err.diagnostics.is_empty() {
        return report_html(&err.render_ansi(src, filename));
    }

    // Resolver uses a separate scratch arena that is freed after resolution.
    let mut non_err = String::with_capacity(src.len() / 2);
    {
        let res_arena = scratch_arena(Some(&arena));
        let mut resolver = Resolver::with_facts_arena(&res_arena, &arena);
        resolver.resolve(root);
        if resolver.errors.has_errors() {
            return report_html(&resolver.errors.render_ansi(src, filename));
        }
        if !resolver.errors.diagnostics.is_empty() {
            non_err.push_str(&report_html(&resolver.errors.render_ansi(src, filename)));
        }
        let (facts, optimization_plan) = resolver.into_artifacts();

        // After resolver scope drops, scratch[1] is free for use as frame arena.
        let frame = scratch_arena(Some(&arena));
        let mut runtime = Runtime::new(&arena, Some(&frame));
        let err = runtime.run_with_analysis(root, &facts, optimization_plan.as_ref());
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
}

fn report_html(ansi: &str) -> String {
    ansi_to_html::convert(ansi).unwrap()
}
