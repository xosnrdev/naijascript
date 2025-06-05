use std::io::{self, Read, Write};

use clap::Parser as ClapParser;
use clap_cargo::style::CLAP_STYLING;

use crate::interpreter::{Interpreter, InterpreterError};
use crate::syntax::Lexer;
use crate::syntax::parser::{ParseError, Parser};

#[derive(Debug, ClapParser)]
#[command(about, version, styles = CLAP_STYLING)]
struct Cli {
    /// Script file to run
    pub script: Option<String>,
    /// Evaluate code passed as a string
    #[arg(short, long)]
    pub eval: Option<String>,
    /// Start REPL
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

impl<'a> std::fmt::Display for CmdError<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CmdError::Io(e) => write!(f, "Chai! I/O wahala: {e}"),
            CmdError::Parse(e) => write!(f, "{e}"),
            CmdError::Interpreter(e) => write!(f, "{e}"),
            CmdError::InvalidScriptExtension(s) => {
                write!(f, "Omo! Only .ns or .naija files dey allowed as script. You give me: {s}")
            }
            CmdError::Other(msg) => write!(f, "Cmd wahala: {msg}"),
        }
    }
}

impl<'a> std::error::Error for CmdError<'a> {}

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
    } else if !atty::is(atty::Stream::Stdin) {
        // If stdin is not a tty, read from stdin
        match run_stdin() {
            Ok(()) => 0,
            Err(_) => 1,
        }
    } else {
        println!("No input provided. Use --help for usage.");
        1
    };
    std::process::exit(exit_code);
}

/// Evaluate code passed as a string (for --eval).
/// Returns error if parsing or execution fails.
fn run_eval<'a>(code: &'a str) -> CmdResult<'a, ()> {
    let mut parser = Parser::new(Lexer::new(code));
    match parser.parse_program() {
        Ok(program) => {
            let mut interpreter = Interpreter::new();
            interpreter.eval_program(&program).map_err(CmdError::Interpreter)
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
    let mut parser = Parser::new(Lexer::new(&source));
    match parser.parse_program() {
        Ok(program) => {
            let mut interpreter = Interpreter::new();
            // Workaround: parser/interpreter errors are stringified for static lifetime
            interpreter.eval_program(&program).map_err(|e| CmdError::Other(e.to_string()))
        }
        Err(e) => Err(CmdError::Other(e.to_string())),
    }
}

/// Start the interactive REPL.
/// Uses Box::leak to extend input lifetime for parser/interpreter.
/// This is a workaround for lifetime constraints in the REPL loop.
fn run_repl<'a>() -> CmdResult<'a, ()> {
    println!("NaijaScript REPL (type 'exit' or Ctrl+D to comot)");
    let mut interpreter = Interpreter::new();
    let stdin = io::stdin();
    let mut stdout = io::stdout();
    loop {
        print!("> ");
        stdout.flush().ok();
        let mut line = String::new();
        // Fix: handle EOF (Ctrl+D) by checking if read_line returns 0
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
                // Box::leak is used here to give the input a 'static lifetime for the parser.
                // This is safe in a REPL since the process is short-lived and input is small.
                let input = trimmed.to_owned().into_boxed_str();
                let static_input: &'static str = Box::leak(input);
                let mut parser = Parser::new(Lexer::new(static_input));
                match parser.parse_program() {
                    Ok(program) => {
                        if let Err(e) = interpreter.eval_program(&program) {
                            eprintln!("{e}");
                        }
                    }
                    Err(e) => eprintln!("{e}"),
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
    let mut parser = Parser::new(Lexer::new(&buffer));
    match parser.parse_program() {
        Ok(program) => {
            let mut interpreter = Interpreter::new();
            // Workaround: parser/interpreter errors are stringified for static lifetime
            interpreter.eval_program(&program).map_err(|e| CmdError::Other(e.to_string()))
        }
        Err(e) => Err(CmdError::Other(e.to_string())),
    }
}
