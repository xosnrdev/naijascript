//! Platform abstractions.

#![allow(non_camel_case_types)]

#[cfg(windows)]
mod windows;

#[cfg(unix)]
mod unix;

#[cfg(target_family = "wasm")]
mod wasm;

use std::io;
use std::ptr::NonNull;

#[cfg(unix)]
use unix::{UnixStdin, UnixVirtualMemory};
#[cfg(target_family = "wasm")]
use wasm::*;
#[cfg(windows)]
pub use windows::*;

use crate::arena::{Arena, ArenaString};
use crate::runtime::Value;

#[cfg(unix)]
pub type virtual_memory = UnixVirtualMemory;
#[cfg(target_family = "wasm")]
pub type virtual_memory = WasmVirtualMemory;
#[cfg(windows)]
pub type virtual_memory = WindowsVirtualMemory;

#[cfg(unix)]
pub type stdin = UnixStdin;
#[cfg(windows)]
pub type stdin = WindowsStdin;
#[cfg(target_family = "wasm")]
pub type stdin = WasmStdin;

pub trait VirtualMemory {
    /// Reserves a virtual memory region of the given size.
    /// To commit the memory, use `commit`.
    /// To release the memory, use `release`.
    ///
    /// # Safety
    ///
    /// This function is unsafe because it uses raw pointers.
    /// Don't forget to release the memory when you're done with it or you'll leak it.
    unsafe fn reserve(size: usize) -> Result<NonNull<u8>, u32>;

    /// Commits a virtual memory region of the given size.
    ///
    /// # Safety
    ///
    /// This function is unsafe because it uses raw pointers.
    /// Make sure to only pass pointers acquired from `reserve`
    /// and to pass a size less than or equal to the size passed to `reserve`.
    unsafe fn commit(base: NonNull<u8>, size: usize) -> Result<(), u32>;

    /// Releases a virtual memory region of the given size.
    ///
    /// # Safety
    ///
    /// This function is unsafe because it uses raw pointers.
    /// Make sure to only pass pointers acquired from `reserve`.
    unsafe fn release(base: NonNull<u8>, size: usize);
}

pub trait Stdin {
    /// Reads a line from the standard input.
    ///
    /// Unlike the `read_line` from [`std::io::stdin`] that's hardcoded to [`std::string::String`] type, this
    /// one allocates with [`ArenaString`].
    fn read_line<'arena>(
        prompt: &Value<'arena, '_>,
        arena: &'arena Arena,
    ) -> Result<ArenaString<'arena>, io::Error>;
}
