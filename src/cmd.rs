use std::io::{self, IsTerminal, Read, Write};

use clap::Parser as ClapParser;
use clap_cargo::style::CLAP_STYLING;

use crate::diagnostics::StderrHandler;
use crate::interpreter::{Interpreter, InterpreterError};
use crate::syntax::Lexer;
use crate::syntax::parser::{ParseError, Parser};

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

/// Error type for command-line operations.
/// Wraps IO, parse, interpreter, and script extension errors.
#[derive(Debug)]
pub enum CmdError<'a> {
    Io(std::io::Error),
    Parse(ParseError<'a>),
    Interpreter(InterpreterError<'a>),
    InvalidScriptExtension(String),
    Other(String),
}

/// Alias for command result type, for ergonomic error handling
pub type CmdResult<'a, T> = Result<T, CmdError<'a>>;

/// Entry point for the command-line tool.
/// Dispatches to eval, script, REPL, or stdin mode based on arguments.
pub fn run() {
    let cli = Cli::parse();
    let exit_code = if let Some(code) = cli.eval {
        match run_eval(&code) {
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
            match run_script(&script) {
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

/// Evaluate code passed as a string (for --eval).
/// Returns error if parsing or execution fails.
fn run_eval<'a>(code: &'a str) -> CmdResult<'a, ()> {
    let mut handler = StderrHandler;
    let mut parser = Parser::new(Lexer::new(code));
    match parser.parse_program_with_handler(Some(&mut handler), code) {
        Ok(program) => {
            let mut interpreter = Interpreter::new();
            interpreter
                .eval_program_with_handler(&program, Some(&mut handler), code)
                .map_err(CmdError::Interpreter)
        }
        Err(e) => Err(CmdError::Parse(e)),
    }
}

/// Run a script file from disk.
/// Only .ns and .naija extensions are allowed for safety and convention.
fn run_script(script: &str) -> CmdResult<'static, ()> {
    if !(script.ends_with(".ns") || script.ends_with(".naija")) {
        return Err(CmdError::InvalidScriptExtension(script.to_string()));
    }
    let source = std::fs::read_to_string(script).map_err(CmdError::Io)?;
    // Leak the source to extend its lifetime to 'static for error reporting
    let source: &'static str = Box::leak(source.into_boxed_str());
    let mut handler = StderrHandler;
    let mut parser = Parser::new(Lexer::new(source));
    match parser.parse_program_with_handler(Some(&mut handler), source) {
        Ok(program) => {
            let mut interpreter = Interpreter::new();
            interpreter
                .eval_program_with_handler(&program, Some(&mut handler), source)
                .map_err(CmdError::Interpreter)
        }
        Err(e) => Err(CmdError::Parse(e)),
    }
}

/// Start the interactive REPL.
fn run_repl<'a>() -> CmdResult<'a, ()> {
    println!("NaijaScript REPL (type 'exit' or Ctrl+D to comot)");
    let mut interpreter = Interpreter::new();
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
                if trimmed.is_empty() {
                    continue;
                }
                let input = trimmed.to_owned().into_boxed_str();
                // Leak the buffer to extend its lifetime to 'static for error reporting
                let static_input: &'static str = Box::leak(input);
                let mut handler = StderrHandler;
                let mut parser = Parser::new(Lexer::new(static_input));
                if let Ok(program) =
                    parser.parse_program_with_handler(Some(&mut handler), static_input)
                {
                    let _ = interpreter.eval_program_with_handler(
                        &program,
                        Some(&mut handler),
                        static_input,
                    );
                }
            }
            Err(_) => {
                println!("Oya, bye bye!");
                break;
            }
        }
    }
    Ok(())
}

/// Run code from stdin (for piping or file redirection).
/// Reads all input, then parses and executes.
fn run_stdin() -> CmdResult<'static, ()> {
    let mut buffer = String::new();
    if let Err(e) = io::stdin().read_to_string(&mut buffer) {
        return Err(CmdError::Io(e));
    }
    // Leak the buffer to extend its lifetime to 'static for error reporting
    let buffer: &'static str = Box::leak(buffer.into_boxed_str());
    let mut handler = StderrHandler;
    let mut parser = Parser::new(Lexer::new(buffer));
    match parser.parse_program_with_handler(Some(&mut handler), buffer) {
        Ok(program) => {
            let mut interpreter = Interpreter::new();
            interpreter
                .eval_program_with_handler(&program, Some(&mut handler), buffer)
                .map_err(CmdError::Interpreter)
        }
        Err(e) => Err(CmdError::Parse(e)),
    }
}

#[test]
fn verify_cli() {
    use clap::CommandFactory;
    Cli::command().debug_assert();
}
