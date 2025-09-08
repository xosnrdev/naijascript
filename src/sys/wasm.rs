use std::alloc::{Layout, alloc, alloc_zeroed, dealloc};
use std::io;
use std::ptr::NonNull;

use super::{Stdin, VirtualMemory};
use crate::KIBI;
use crate::arena::{Arena, ArenaString};

const WASM_PAGE: usize = 64 * KIBI;
const ZERO_ON_RESERVE: bool = false;

#[inline]
const fn round_up_to_pages(n: usize) -> usize {
    (n + (WASM_PAGE - 1)) & !(WASM_PAGE - 1)
}

#[inline]
fn layout_for(size: usize) -> Result<Layout, u32> {
    let size = round_up_to_pages(size);
    Layout::from_size_align(size, WASM_PAGE).map_err(|_| 12)
}

pub struct WasmVirtualMemory;

impl VirtualMemory for WasmVirtualMemory {
    unsafe fn reserve(size: usize) -> Result<NonNull<u8>, u32> {
        let layout = layout_for(size)?;
        let ptr = unsafe { if ZERO_ON_RESERVE { alloc_zeroed(layout) } else { alloc(layout) } };
        match NonNull::new(ptr) {
            Some(p) => Ok(p.cast()),
            None => Err(12),
        }
    }

    unsafe fn commit(_base: NonNull<u8>, _size: usize) -> Result<(), u32> {
        Ok(())
    }

    unsafe fn release(base: NonNull<u8>, size: usize) {
        if let Ok(layout) = layout_for(size) {
            unsafe {
                dealloc(base.as_ptr(), layout);
            }
        }
    }
}

pub struct WasmStdin;

impl Stdin for WasmStdin {
    fn read_line<'arena>(
        _prompt: &str,
        _arena: &'arena Arena,
    ) -> Result<ArenaString<'arena>, io::Error> {
        Err(io::Error::new(io::ErrorKind::Unsupported, "Dis platform lack I/O support"))
    }
}
