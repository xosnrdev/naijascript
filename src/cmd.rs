use std::io::{self, IsTerminal, Read, Write};

use clap::Parser as ClapParser;
use clap_cargo::style::CLAP_STYLING;

use crate::diagnostics::StderrHandler;
use crate::interpreter::Interpreter;
use crate::syntax::Lexer;
use crate::syntax::parser::Parser;

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

#[derive(Debug)]
pub enum CmdError {
    Io(std::io::Error),
    Parse(String),
    Interpreter(String),
    InvalidScriptExtension(String),
    Other(String),
}

pub type CmdResult<T> = Result<T, CmdError>;

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

fn run_eval(code: &str) -> CmdResult<()> {
    let mut handler = StderrHandler;
    let mut parser = Parser::new(Lexer::new(code));
    match parser.parse_program_with_handler(Some(&mut handler), code) {
        Ok(program) => {
            let mut interpreter = Interpreter::new();
            interpreter
                .eval_program_with_handler(&program, Some(&mut handler), code)
                .map_err(|e| CmdError::Interpreter(format!("{e:?}")))
        }
        Err(e) => Err(CmdError::Parse(format!("{e:?}"))),
    }
}

struct ScriptInput {
    source: String,
}

impl ScriptInput {
    fn as_str(&self) -> &str {
        &self.source
    }
}

fn run_script(script: &str) -> CmdResult<()> {
    if !(script.ends_with(".ns") || script.ends_with(".naija")) {
        return Err(CmdError::InvalidScriptExtension(script.to_string()));
    }
    let source = std::fs::read_to_string(script).map_err(CmdError::Io)?;
    let input = ScriptInput { source };
    let src = input.as_str();
    let mut handler = StderrHandler;
    let mut parser = Parser::new(Lexer::new(src));
    match parser.parse_program_with_handler(Some(&mut handler), src) {
        Ok(program) => {
            let mut interpreter = Interpreter::new();
            interpreter
                .eval_program_with_handler(&program, Some(&mut handler), src)
                .map_err(|e| CmdError::Interpreter(format!("{e:?}")))
        }
        Err(e) => Err(CmdError::Parse(format!("{e:?}"))),
    }
}

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
                if trimmed.is_empty() {
                    continue;
                }
                {
                    let input = ScriptInput { source: trimmed.to_owned() };
                    let mut handler = StderrHandler;
                    if let Ok(program) = Parser::new(Lexer::new(input.as_str()))
                        .parse_program_with_handler(Some(&mut handler), input.as_str())
                    {
                        let _ = Interpreter::new().eval_program_with_handler(
                            &program,
                            Some(&mut handler),
                            input.as_str(),
                        );
                    }
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

fn run_stdin() -> CmdResult<()> {
    let mut buffer = String::new();
    if let Err(e) = io::stdin().read_to_string(&mut buffer) {
        return Err(CmdError::Io(e));
    }
    let input = ScriptInput { source: buffer };
    let src = input.as_str();
    let mut handler = StderrHandler;
    let mut parser = Parser::new(Lexer::new(src));
    match parser.parse_program_with_handler(Some(&mut handler), src) {
        Ok(program) => {
            let mut interpreter = Interpreter::new();
            interpreter
                .eval_program_with_handler(&program, Some(&mut handler), src)
                .map_err(|e| CmdError::Interpreter(format!("{e:?}")))
        }
        Err(e) => Err(CmdError::Parse(format!("{e:?}"))),
    }
}

#[test]
fn verify_cli() {
    use clap::CommandFactory;
    Cli::command().debug_assert();
}
