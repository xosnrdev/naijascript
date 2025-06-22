//! The command line interface for NaijaScript interpreter.

use std::io::{self, IsTerminal, Read, Write};

use clap::Parser as ClapParser;
use clap_cargo::style::CLAP_STYLING;

use crate::diagnostics::{AsStr, Diagnostics, Severity};
use crate::interpreter::Interpreter;
use crate::resolver::SemAnalyzer;
use crate::syntax::parser::Parser;

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
    /// Code wey you wan run sharp sharp, e.g. naija --eval "shout(1)"
    #[arg(short, long)]
    pub eval: Option<String>,
    /// Enter REPL mode to dey run code one by one (naija --interactive)
    #[arg(short, long)]
    pub interactive: bool,
}

/// Represents errors that can occur during command execution.
#[derive(Debug)]
pub enum CmdError {
    /// I/O error (file, stdin, etc.)
    Io(std::io::Error),
    /// Parsing error
    Parse,
    /// Semantic analysis error
    Semantic,
    /// Runtime error
    Runtime,
    /// Invalid script file extension
    InvalidScriptExtension(String),
    /// Other miscellaneous error
    Other,
}

/// Result type for command execution.
pub type CmdResult<T> = Result<T, CmdError>;

/// Entry point for the NaijaScript CLI.
///
/// Parses command line arguments and dispatches to the appropriate mode (eval, script, REPL, stdin).
pub fn run() {
    let cli = Cli::parse();
    let exit_code = if let Some(code) = cli.eval {
        match run_source("<eval>", &code) {
            Ok(()) => 0,
            Err(_) => 1,
        }
    } else if let Some(script) = cli.script {
        if script == "-" {
            match run_stdin() {
                Ok(()) => 0,
                Err(_) => 1,
            }
        } else {
            match run_file(&script) {
                Ok(()) => 0,
                Err(_) => 1,
            }
        }
    } else if cli.interactive {
        match run_repl() {
            Ok(()) => 0,
            Err(_) => 1,
        }
    } else if !io::stdin().is_terminal() {
        match run_stdin() {
            Ok(()) => 0,
            Err(_) => 1,
        }
    } else {
        1
    };
    std::process::exit(exit_code);
}

/// Run source code from a &str, with full diagnostics and semantic analysis.
fn run_source(filename: &str, src: &str) -> CmdResult<()> {
    let mut parser = Parser::new(src);
    let (root, parse_errors) = parser.parse_program();
    let has_parse_errors = !parse_errors.diagnostics.is_empty();
    if has_parse_errors {
        parse_errors.report(src, filename);
        return Err(CmdError::Parse);
    }
    let mut analyzer = SemAnalyzer::new(
        &parser.stmt_arena,
        &parser.expr_arena,
        &parser.cond_arena,
        &parser.block_arena,
    );
    analyzer.analyze(root);
    let has_semantic_errors = !analyzer.errors.diagnostics.is_empty();
    if has_semantic_errors {
        analyzer.errors.report(src, filename);
        return Err(CmdError::Semantic);
    }
    let mut output = |v: f64| println!("{v}");
    let mut interp = Interpreter::with_output(
        &parser.stmt_arena,
        &parser.expr_arena,
        &parser.cond_arena,
        &parser.block_arena,
        &mut output,
    );
    match interp.run(root) {
        Ok(()) => Ok(()),
        Err(e) => {
            // Report runtime error with diagnostics
            let mut diag = Diagnostics::default();
            diag.emit(e.span.clone(), Severity::Error, "runtime", e.kind.as_str(), &[]);
            diag.report(src, filename);
            Err(CmdError::Runtime)
        }
    }
}

/// Runs a script file from disk.
fn run_file(script: &str) -> CmdResult<()> {
    if !(script.ends_with(".ns") || script.ends_with(".naija")) {
        return Err(CmdError::InvalidScriptExtension(script.to_string()));
    }
    let source = std::fs::read_to_string(script).map_err(CmdError::Io)?;
    run_source(script, &source)
}

/// Reads code from standard input and runs it as a script.
fn run_stdin() -> CmdResult<()> {
    let mut buffer = String::new();
    if let Err(e) = io::stdin().read_to_string(&mut buffer) {
        return Err(CmdError::Io(e));
    }
    run_source("<stdin>", &buffer)
}

/// Starts the interactive Read-Eval-Print Loop (REPL).
fn run_repl() -> CmdResult<()> {
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
                let _ = run_source("<repl>", trimmed);
            }
            Err(_) => {
                println!("Oya, bye bye!");
                break;
            }
        }
    }
    Ok(())
}

#[test]
fn verify_cli() {
    use clap::CommandFactory;
    Cli::command().debug_assert();
}
