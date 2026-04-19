#![feature(allocator_api)]
#![allow(
    clippy::many_single_char_names,
    clippy::similar_names,
    clippy::missing_errors_doc,
    clippy::missing_panics_doc,
    clippy::too_many_lines,
    clippy::cast_possible_truncation
)]

#[macro_use]
pub mod arena;
pub mod analysis;
pub mod builtins;
pub mod diagnostics;
pub mod helpers;
pub mod process;
pub mod resolver;
pub mod runtime;
pub mod syntax;
pub mod sys;
