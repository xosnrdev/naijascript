//! Arena allocators. Small and fast.

mod bump;
mod cow;
#[cfg(debug_assertions)]
mod debug;
mod scratch;
mod string;

#[cfg(any(doc, not(debug_assertions)))]
pub use bump::Arena;
pub use cow::ArenaCow;
#[cfg(all(not(doc), debug_assertions))]
pub use debug::Arena;
pub use scratch::{ScratchArena, init, scratch_arena};
pub use string::ArenaString;
