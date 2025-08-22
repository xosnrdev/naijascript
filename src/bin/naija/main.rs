//! The command line interface for NaijaScript interpreter.

use std::fs;
use std::io::{self, IsTerminal, Read};
use std::process::ExitCode;

use clap::Parser as ClapParser;
use clap_cargo::style::CLAP_STYLING;
use naijascript::MEBI;
use naijascript::arena::{self, Arena, scratch_arena};
use naijascript::resolver::Resolver;
use naijascript::runtime::Runtime;
use naijascript::syntax::parser::Parser;
use naijascript::syntax::scanner::Lexer;
use naijascript::syntax::token::SpannedToken;

// Command line arguments for the NaijaScript interpreter.
#[derive(Debug, ClapParser)]
#[command(
    about = "Scripting language wey you fit use learn, run things, and catch cruise.",
    version,
    styles = CLAP_STYLING,
    arg_required_else_help = true
)]
struct Cli {
    /// Script file wey you wan run, e.g. naija script.ns
    script: Option<String>,
    /// Code wey you wan run sharp sharp, e.g. naija --eval "shout("hello world")"
    #[arg(short, long)]
    eval: Option<String>,
}

#[cfg(target_pointer_width = "32")]
const SCRATCH_ARENA_CAPACITY: usize = 128 * MEBI;
#[cfg(target_pointer_width = "64")]
const SCRATCH_ARENA_CAPACITY: usize = 512 * MEBI;

// Entry point for the NaijaScript CLI.
//
// Parses command line arguments and dispatches to the appropriate mode (eval, script, stdin).
fn main() -> ExitCode {
    let cli = Cli::parse();

    arena::init(SCRATCH_ARENA_CAPACITY).unwrap();
    let arena = scratch_arena(None);

    if let Some(code) = cli.eval {
        run_source("<eval>", &code, &arena)
    } else if let Some(script) = cli.script {
        if script == "-" { run_stdin(&arena) } else { run_file(&script, &arena) }
    } else if !io::stdin().is_terminal() {
        run_stdin(&arena)
    } else {
        ExitCode::FAILURE
    }
}

// Run source code from a &str, with full diagnostics and semantic analysis.
fn run_source(filename: &str, src: &str, arena: &Arena) -> ExitCode {
    let mut lexer = Lexer::new(src, arena);
    let tokens: Vec<SpannedToken> = (&mut lexer).collect();
    if !lexer.errors.diagnostics.is_empty() {
        lexer.errors.report(src, filename);
        return ExitCode::FAILURE;
    }

    let mut parser = Parser::new(tokens.into_iter(), arena);
    let (root, err) = parser.parse_program();
    if !err.diagnostics.is_empty() {
        err.report(src, filename);
        return ExitCode::FAILURE;
    }

    let mut resolver = Resolver::new(arena);
    resolver.resolve(root);
    if resolver.errors.has_errors() {
        resolver.errors.report(src, filename);
        return ExitCode::FAILURE;
    }
    if !resolver.errors.diagnostics.is_empty() {
        resolver.errors.report(src, filename);
    }

    let mut runtime = Runtime::new(arena);
    let err = runtime.run(root);
    if err.has_errors() {
        err.report(src, filename);
        return ExitCode::FAILURE;
    }
    if !err.diagnostics.is_empty() {
        err.report(src, filename);
    }
    for value in &runtime.output {
        println!("{value}")
    }

    ExitCode::SUCCESS
}

// Runs a script file from disk.
fn run_file(script: &str, arena: &Arena) -> ExitCode {
    let stem = script.split_once('.').map_or(script, |(s, _)| s);
    if !(script.ends_with(".ns") || script.ends_with(".naija")) {
        eprintln!(
            "E be like say your script no end with .ns or .naija\n -> You pass {script}\n -> Abeg rename am to {stem}.ns or {stem}.naija",
        );
        return ExitCode::FAILURE;
    }
    let source = match fs::read_to_string(script) {
        Ok(s) => s,
        Err(_) => {
            eprintln!(
                "E be like say I no fit read file `{script}`\n-> Maybe file no dey or permission block am\n -> Check say the file dey and you get read rights"
            );
            return ExitCode::FAILURE;
        }
    };
    run_source(script, &source, arena)
}

// Reads code from standard input and runs it as a script.
fn run_stdin(arena: &Arena) -> ExitCode {
    let mut buffer = String::new();
    if io::stdin().read_to_string(&mut buffer).is_err() {
        eprintln!("E be like say I no fit read from stdin\n -> Check wetin you pipe");
        return ExitCode::FAILURE;
    }
    run_source("<stdin>", &buffer, arena)
}

#[test]
fn verify_cli() {
    use clap::CommandFactory;
    Cli::command().debug_assert();
}
