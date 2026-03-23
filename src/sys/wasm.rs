use std::alloc::{Layout, alloc, dealloc};
use std::ptr::NonNull;
use std::{io, mem};

use super::{ProcessRunner, Stdin, VirtualMemory};
use crate::arena::{Arena, ArenaString};
use crate::process::{ProcessCaps, ProcessError, ProcessResult, ProcessSpec};
use crate::runtime::Value;

pub struct WasmVirtualMemory;

impl VirtualMemory for WasmVirtualMemory {
    unsafe fn reserve(size: usize) -> Result<NonNull<u8>, u32> {
        let layout =
            Layout::from_size_align(size, mem::align_of::<usize>()).map_err(|_| u32::MAX)?;
        NonNull::new(unsafe { alloc(layout) }).ok_or(u32::MAX)
    }

    unsafe fn commit(_: NonNull<u8>, _: usize) -> Result<(), u32> {
        Ok(())
    }

    unsafe fn decommit(_: NonNull<u8>, _: usize) {}

    unsafe fn release(base: NonNull<u8>, size: usize) {
        if let Ok(layout) = Layout::from_size_align(size, mem::align_of::<usize>()) {
            unsafe { dealloc(base.as_ptr(), layout) };
        }
    }
}

pub struct WasmStdin;

impl Stdin for WasmStdin {
    fn read_line<'a>(_: &Value<'a>, _: &'a Arena) -> Result<ArenaString<'a>, io::Error> {
        Err(io::Error::new(io::ErrorKind::Unsupported, "Dis platform lack I/O support"))
    }
}

pub struct WasmProcessRunner;

impl ProcessRunner for WasmProcessRunner {
    fn run<'arena>(
        _: &ProcessSpec<'_>,
        _: &ProcessCaps,
        _: &'arena Arena,
    ) -> Result<ProcessResult<'arena>, ProcessError> {
        Err(ProcessError::Unsupported)
    }
}
