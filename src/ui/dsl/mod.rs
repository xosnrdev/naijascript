//! The `.nui` declarative UI DSL front end: scanner, parser, AST, and resolver.
//!
//! The evaluator that turns a resolved program into renderer-neutral IR lives in
//! [`crate::ui::engine`], since it needs retained session state.

pub mod ast;
pub mod parser;
pub mod resolver;
pub mod scanner;

pub use ast::Program;
pub use parser::parse;
pub use resolver::resolve;

/// Parses and resolves `.nui` source into a validated [`Program`].
///
/// Returns every compile diagnostic on failure.
pub fn compile(source: &str) -> Result<Program, Vec<crate::ui::error::UiError>> {
    let program = parse(source).map_err(|error| vec![error])?;
    resolve(&program)?;
    Ok(program)
}
