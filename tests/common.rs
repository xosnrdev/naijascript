#![allow(clippy::missing_panics_doc, unused)]

use std::ffi::OsString;
use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;
use std::sync::OnceLock;

use naijascript::arena::Arena;
use naijascript::diagnostics::Diagnostics;
use naijascript::helpers::MEBI;
use naijascript::process::HostPolicy;
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

pub fn with_pipeline_and_host_policy<F, R>(src: &str, host_policy: HostPolicy, f: F) -> R
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
    let mut runtime = Runtime::new_with_host_policy(&arena, Some(&frame), host_policy);

    f(&arena, (root, parse_errors), &mut resolver, &mut runtime)
}

pub fn process_helper_path() -> &'static Path {
    static HELPER_PATH: OnceLock<PathBuf> = OnceLock::new();
    HELPER_PATH.get_or_init(compile_process_helper).as_path()
}

fn compile_process_helper() -> PathBuf {
    let manifest_dir = Path::new(env!("CARGO_MANIFEST_DIR"));
    let source = manifest_dir.join("tests/fixtures/process_helper.rs");
    let out_dir = manifest_dir.join("target/process-test-helper");
    fs::create_dir_all(&out_dir).expect("process helper output directory should be creatable");

    let helper_name = if cfg!(windows) { "process_helper.exe" } else { "process_helper" };
    let output = out_dir.join(helper_name);
    let rustc = std::env::var_os("RUSTC").unwrap_or_else(|| OsString::from("rustc"));

    // The helper stays out of Cargo's production binary surface and is compiled
    // only when the process integration tests need it.
    let status = Command::new(rustc)
        .arg("--edition=2024")
        .arg(&source)
        .arg("-o")
        .arg(&output)
        .status()
        .expect("rustc should compile the process helper fixture");
    assert!(status.success(), "process helper fixture compilation should succeed");

    output
}
