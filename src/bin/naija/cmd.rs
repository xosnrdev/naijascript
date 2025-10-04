use std::fs;
use std::io::{self, IsTerminal, Read};
use std::process::ExitCode;

use clap::Subcommand;
use clap::builder::NonEmptyStringValueParser;
use clap_cargo::style::CLAP_STYLING;
use naijascript::resolver::Resolver;
use naijascript::runtime::Runtime;
use naijascript::syntax::parser::Parser;
use naijascript::syntax::scanner::Lexer;
use naijascript::syntax::token::SpannedToken;

use crate::arena::{Arena, ArenaString};
use crate::{print_error, toolchain};

#[derive(Debug, clap::Parser)]
#[command(
    about = "The NaijaScript Interpreter",
    version,
    styles = CLAP_STYLING,
    arg_required_else_help = true
)]
pub struct Cli {
    /// Script to run
    script: Option<String>,
    /// Evaluate code from the command line
    #[arg(short, long, conflicts_with = "script")]
    eval: Option<String>,
    #[command(subcommand)]
    command: Option<Command>,
}

#[derive(Debug, Subcommand)]
enum Command {
    #[command(name = "self")]
    Self_ {
        #[command(subcommand)]
        command: SelfCommand,
    },
}

#[derive(Debug, Subcommand)]
enum SelfCommand {
    /// Download and install one or more versions
    Install {
        /// One or more versions to install
        #[arg(value_name = "version", required = true, value_parser = NonEmptyStringValueParser::new())]
        versions: Vec<String>,
    },
    /// List installed versions
    List,
    /// Fetch available versions online
    Available,
    /// Uninstall one or more versions
    #[command(arg_required_else_help = true)]
    Uninstall {
        /// One or more versions to uninstall
        #[arg(value_name = "version", value_parser = NonEmptyStringValueParser::new())]
        versions: Vec<String>,
        /// Uninstall all versions
        #[arg(long, conflicts_with = "versions")]
        all: bool,
    },
}

impl Cli {
    pub fn run(self, arena: &Arena) -> ExitCode {
        if let Some(code) = self.eval {
            run_source("<eval>", &code, arena)
        } else if let Some(script) = self.script {
            if script == "-" { run_stdin(arena) } else { run_file(&script, arena) }
        } else if !io::stdin().is_terminal() {
            run_stdin(arena)
        } else if let Some(Command::Self_ { command }) = self.command {
            match command {
                SelfCommand::Install { versions } => {
                    report_result(toolchain::install_version(&versions))
                }
                SelfCommand::List => report_result(toolchain::list_installed_version()),
                SelfCommand::Available => report_result(toolchain::fetch_available_version()),
                SelfCommand::Uninstall { versions, all } => {
                    report_result(toolchain::uninstall_version(&versions, all))
                }
            }
        } else {
            ExitCode::FAILURE
        }
    }
}

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

fn run_file(path: &str, arena: &Arena) -> ExitCode {
    match fs::read_to_string(path) {
        Ok(src) => run_source(path, &src, arena),
        Err(err) => {
            print_error!("Failed to read file '{path}': {err}");
            ExitCode::FAILURE
        }
    }
}

fn run_stdin(arena: &Arena) -> ExitCode {
    let mut reader = io::stdin().lock();
    let mut buf = Vec::new_in(arena);
    let mut chunk = [0u8; 8192];

    loop {
        match reader.read(&mut chunk) {
            Ok(0) => break,
            Ok(n) => buf.extend_from_slice(&chunk[..n]),
            Err(err) if err.kind() == io::ErrorKind::Interrupted => continue,
            Err(err) => {
                print_error!("Failed to read from stdin: {err}");
                return ExitCode::FAILURE;
            }
        }
    }

    let src = match std::str::from_utf8(&buf) {
        Ok(_) => unsafe { ArenaString::from_utf8_unchecked(buf) },
        Err(err) => {
            let err = io::Error::new(io::ErrorKind::InvalidData, err);
            print_error!("Failed to read from stdin: {err}");
            return ExitCode::FAILURE;
        }
    };

    run_source("<stdin>", &src, arena)
}

fn report_result(result: Result<(), String>) -> ExitCode {
    match result {
        Ok(()) => ExitCode::SUCCESS,
        Err(err) => {
            print_error!("{err}");
            ExitCode::FAILURE
        }
    }
}

#[test]
fn verify_cli() {
    use clap::CommandFactory;
    Cli::command().debug_assert();
}
