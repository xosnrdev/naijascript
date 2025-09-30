use std::process::ExitCode;

use clap::Parser;
use naijascript::arena::{self, scratch_arena};
use naijascript::cmd::Cli;
use naijascript::{GIBI, print_error};

#[cfg(target_pointer_width = "32")]
const SCRATCH_ARENA_CAPACITY: usize = 1 * GIBI;
#[cfg(target_pointer_width = "64")]
const SCRATCH_ARENA_CAPACITY: usize = 4 * GIBI;

fn main() -> ExitCode {
    let cli = Cli::parse();

    if let Err(err) = arena::init(SCRATCH_ARENA_CAPACITY) {
        print_error!("Failed to initialize arena: {err}");
        return ExitCode::FAILURE;
    }
    let arena = scratch_arena(None);
    cli.run(&arena)
}
