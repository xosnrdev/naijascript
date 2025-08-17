//! Platform abstractions.

#[cfg(windows)]
mod windows;

#[cfg(unix)]
mod unix;

#[cfg(unix)]
pub use unix::*;
#[cfg(windows)]
pub use windows::*;
