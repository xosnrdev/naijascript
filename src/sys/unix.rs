//! Unix-specific platform code.
//!
//! Read the `windows` module for reference.

use std::ffi::c_int;
use std::ptr::{self, NonNull, null_mut};

use super::VirtualMemory;

#[cfg(target_os = "netbsd")]
const fn desired_mprotect(flags: c_int) -> c_int {
    // NetBSD allows an mmap(2) caller to specify what protection flags they
    // will use later via mprotect. It does not allow a caller to move from
    // PROT_NONE to PROT_READ | PROT_WRITE.
    //
    // see PROT_MPROTECT in man 2 mmap
    flags << 3
}

#[cfg(not(target_os = "netbsd"))]
const fn desired_mprotect(_: c_int) -> c_int {
    libc::PROT_NONE
}

pub struct UnixVirtualMemory;

impl VirtualMemory for UnixVirtualMemory {
    unsafe fn reserve(size: usize) -> Result<NonNull<u8>, u32> {
        unsafe {
            let ptr = libc::mmap(
                null_mut(),
                size,
                desired_mprotect(libc::PROT_READ | libc::PROT_WRITE),
                libc::MAP_PRIVATE | libc::MAP_ANONYMOUS,
                -1,
                0,
            );
            if ptr.is_null() || ptr::eq(ptr, libc::MAP_FAILED) {
                Err(libc::ENOMEM as u32)
            } else {
                Ok(NonNull::new_unchecked(ptr as *mut u8))
            }
        }
    }

    unsafe fn commit(base: NonNull<u8>, size: usize) -> Result<(), u32> {
        unsafe {
            let status =
                libc::mprotect(base.cast().as_ptr(), size, libc::PROT_READ | libc::PROT_WRITE);
            if status != 0 { Err(libc::ENOMEM as u32) } else { Ok(()) }
        }
    }

    unsafe fn release(base: NonNull<u8>, size: usize) {
        unsafe {
            libc::munmap(base.cast().as_ptr(), size);
        }
    }
}
