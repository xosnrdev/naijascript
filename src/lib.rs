pub mod builtins;
pub mod diagnostics;
pub mod resolver;
pub mod runtime;
pub mod syntax;

#[cfg(target_arch = "wasm32")]
use wasm_bindgen::prelude::*;
#[cfg(target_arch = "wasm32")]
use {
    crate::resolver::SemAnalyzer, crate::runtime::Interpreter, crate::syntax::parser::Parser,
    crate::syntax::scanner::Lexer, crate::syntax::token::SpannedToken,
};

#[cfg(target_arch = "wasm32")]
#[wasm_bindgen]
pub fn run_source(src: &str, filename: &str) -> String {
    let mut lexer = Lexer::new(src);
    let tokens: Vec<SpannedToken> = (&mut lexer).collect();
    if !lexer.errors.diagnostics.is_empty() {
        return lexer.errors.report_html(src, filename);
    }

    let mut parser = Parser::new(tokens.into_iter());
    let (root, parse_errors) = parser.parse_program();
    if !parse_errors.diagnostics.is_empty() {
        return parse_errors.report_html(src, filename);
    }

    let mut resolver = SemAnalyzer::new(
        &parser.stmt_arena,
        &parser.expr_arena,
        &parser.cond_arena,
        &parser.block_arena,
        &parser.param_arena,
        &parser.arg_arena,
    );
    resolver.analyze(root);
    if resolver.errors.has_errors() {
        return resolver.errors.report_html(src, filename);
    }
    if !resolver.errors.diagnostics.is_empty() {
        resolver.errors.report_html(src, filename);
    }

    let mut rt = Interpreter::new(
        &parser.stmt_arena,
        &parser.expr_arena,
        &parser.cond_arena,
        &parser.block_arena,
        &parser.param_arena,
        &parser.arg_arena,
    );
    let err = rt.run(root);
    if err.has_errors() {
        return err.report_html(src, filename);
    }
    if !err.diagnostics.is_empty() {
        err.report_html(src, filename);
    }
    rt.output.iter().map(ToString::to_string).collect::<Vec<_>>().join("\n")
}
