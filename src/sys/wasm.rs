use std::alloc::{Layout, alloc, dealloc};
use std::ptr::NonNull;
use std::{io, mem};

use super::{Stdin, VirtualMemory};
use crate::arena::{Arena, ArenaString};
use crate::runtime::Value;

pub struct WasmVirtualMemory;

impl VirtualMemory for WasmVirtualMemory {
    unsafe fn reserve(size: usize) -> Result<NonNull<u8>, u32> {
        let layout =
            Layout::from_size_align(size, mem::align_of::<usize>()).map_err(|_| u32::MAX)?;
        NonNull::new(unsafe { alloc(layout) }).ok_or(u32::MAX)
    }

    unsafe fn commit(_base: NonNull<u8>, _size: usize) -> Result<(), u32> {
        Ok(())
    }

    unsafe fn release(base: NonNull<u8>, size: usize) {
        if let Ok(layout) = Layout::from_size_align(size, mem::align_of::<usize>()) {
            unsafe { dealloc(base.as_ptr(), layout) };
        }
    }
}

pub struct WasmStdin;

impl Stdin for WasmStdin {
    fn read_line<'arena, 'src>(
        _prompt: &Value<'arena, 'src>,
        _arena: &'arena Arena,
    ) -> Result<ArenaString<'arena>, io::Error> {
        Err(io::Error::new(io::ErrorKind::Unsupported, "Dis platform lack I/O support"))
    }
}
