//! The command line interface for NaijaScript interpreter.

use std::fs;
use std::io::{self, IsTerminal, Read, Write};
use std::process::ExitCode;

use clap::Parser as ClapParser;
use clap_cargo::style::CLAP_STYLING;
use naijascript::resolver::SemAnalyzer;
use naijascript::runtime::Interpreter;
use naijascript::syntax::parser::Parser;
use naijascript::syntax::scanner::Lexer;
use naijascript::syntax::token::SpannedToken;

/// Command line arguments for the NaijaScript interpreter.
#[derive(Debug, ClapParser)]
#[command(
    about = "Scripting language wey you fit use learn, run things, and catch cruise.",
    version,
    styles = CLAP_STYLING,
    arg_required_else_help = true
)]
struct Cli {
    /// Script file wey you wan run, e.g. naija script.ns
    pub script: Option<String>,
    /// Code wey you wan run sharp sharp, e.g. naija --eval "shout("hello world")"
    #[arg(short, long)]
    pub eval: Option<String>,
    /// Enter REPL mode to dey run code one by one (naija --interactive)
    #[arg(short, long)]
    pub interactive: bool,
}

/// Entry point for the NaijaScript CLI.
///
/// Parses command line arguments and dispatches to the appropriate mode (eval, script, REPL, stdin).
fn main() -> ExitCode {
    let cli = Cli::parse();

    if let Some(code) = cli.eval {
        run_source("<eval>", &code)
    } else if let Some(script) = cli.script {
        if script == "-" { run_stdin() } else { run_file(&script) }
    } else if cli.interactive {
        run_repl()
    } else if !io::stdin().is_terminal() {
        run_stdin()
    } else {
        ExitCode::FAILURE
    }
}

/// Run source code from a &str, with full diagnostics and semantic analysis.
fn run_source(filename: &str, src: &str) -> ExitCode {
    let mut lexer = Lexer::new(src);
    let tokens: Vec<SpannedToken> = (&mut lexer).collect();
    if !lexer.errors.diagnostics.is_empty() {
        lexer.errors.report(src, filename);
        return ExitCode::FAILURE;
    }

    let mut parser = Parser::new(tokens.into_iter());
    let (root, parse_errors) = parser.parse_program();
    if !parse_errors.diagnostics.is_empty() {
        parse_errors.report(src, filename);
        return ExitCode::FAILURE;
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
        resolver.errors.report(src, filename);
        return ExitCode::FAILURE;
    }
    if !resolver.errors.diagnostics.is_empty() {
        resolver.errors.report(src, filename);
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
        err.report(src, filename);
        return ExitCode::FAILURE;
    }
    if !err.diagnostics.is_empty() {
        err.report(src, filename);
    }
    for value in &rt.output {
        println!("{value}")
    }

    ExitCode::SUCCESS
}

/// Runs a script file from disk.
fn run_file(script: &str) -> ExitCode {
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
    run_source(script, &source)
}

/// Reads code from standard input and runs it as a script.
fn run_stdin() -> ExitCode {
    let mut buffer = String::new();
    if io::stdin().read_to_string(&mut buffer).is_err() {
        eprintln!("E be like say I no fit read from stdin\n -> Check wetin you pipe");
        return ExitCode::FAILURE;
    }
    run_source("<stdin>", &buffer)
}

/// Starts the interactive Read-Eval-Print Loop (REPL).
fn run_repl() -> ExitCode {
    println!("NaijaScript REPL (type 'exit' or Ctrl+D to comot)");
    let stdin = io::stdin();
    let mut stdout = io::stdout();
    loop {
        print!("> ");
        stdout.flush().ok();
        let mut line = String::new();
        let n = stdin.read_line(&mut line);
        match n {
            Ok(0) => {
                println!("Oya, bye bye!");
                break;
            }
            Ok(_) if line.trim() == "exit" => {
                println!("Oya, bye bye!");
                break;
            }
            Ok(_) => {
                let trimmed = line.trim();
                if trimmed.is_empty() || trimmed.starts_with("\x1b[") {
                    continue;
                }
                run_source("<repl>", trimmed);
            }
            Err(_) => {
                println!("Oya, bye bye!");
                break;
            }
        }
    }
    ExitCode::SUCCESS
}

#[test]
fn verify_cli() {
    use clap::CommandFactory;
    Cli::command().debug_assert();
}
