#![feature(allocator_api, cold_path)]
#![cfg_attr(
    target_arch = "loongarch64",
    feature(stdarch_loongarch, stdarch_loongarch_feature_detection, loongarch_target_feature)
)]

#[macro_use]
pub mod arena;
pub mod builtins;
pub mod diagnostics;
pub mod resolver;
pub mod runtime;
pub mod simd;
pub mod syntax;
pub mod sys;

pub const KIBI: usize = 1024;
pub const MEBI: usize = 1024 * 1024;
pub const GIBI: usize = 1024 * 1024 * 1024;

#[cfg(target_arch = "wasm32")]
use {
    crate::{
        arena::scratch_arena,
        resolver::Resolver,
        runtime::Runtime,
        syntax::{parser::Parser, scanner::Lexer, token::SpannedToken},
    },
    wasm_bindgen::prelude::*,
};

#[cfg(target_arch = "wasm32")]
#[wasm_bindgen]
pub fn run_source(src: &str, filename: &str) -> String {
    arena::init(128 * MEBI).unwrap();
    let arena = scratch_arena(None);

    let mut lexer = Lexer::new(src, &arena);
    let tokens: Vec<SpannedToken> = (&mut lexer).collect();
    if !lexer.errors.diagnostics.is_empty() {
        return lexer.errors.report_html(src, filename);
    }

    let mut parser = Parser::new(tokens.into_iter(), &arena);
    let (root, err) = parser.parse_program();
    if !err.diagnostics.is_empty() {
        return err.report_html(src, filename);
    }

    let mut resolver = Resolver::new(&arena);
    resolver.resolve(root);
    if resolver.errors.has_errors() {
        return resolver.errors.report_html(src, filename);
    }
    let mut non_err = String::with_capacity(src.len() / 2);
    if !resolver.errors.diagnostics.is_empty() {
        non_err.push_str(&resolver.errors.report_html(src, filename));
    }

    let mut runtime = Runtime::new(&arena);
    let err = runtime.run(root);
    if err.has_errors() {
        return err.report_html(src, filename);
    }
    if !err.diagnostics.is_empty() {
        non_err.push_str(&err.report_html(src, filename));
    }

    let res = runtime.output.iter().map(ToString::to_string).collect::<Vec<_>>().join("\n");
    if !non_err.is_empty() {
        non_err.push_str(&res);
        non_err.shrink_to_fit();
        return non_err;
    }
    res
}
