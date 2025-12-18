//! Unix-specific platform code.
//!
//! Read the `windows` module for reference.

use std::ffi::c_int;
use std::io::{self, Write};
use std::ptr::{self, NonNull, null_mut};
use std::slice;

use memchr_rs::memchr;

use super::{Stdin, VirtualMemory};
use crate::KIBI;
use crate::arena::{Arena, ArenaString};
use crate::runtime::Value;

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
                Ok(NonNull::new_unchecked(ptr.cast::<u8>()))
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

pub struct UnixStdin;

impl Stdin for UnixStdin {
    fn read_line<'arena>(
        prompt: &Value<'arena, '_>,
        arena: &'arena Arena,
    ) -> Result<ArenaString<'arena>, io::Error> {
        print!("{prompt}");
        io::stdout().flush()?;

        let mut cap = 8 * KIBI;
        let mut buf = ArenaString::with_capacity_in(cap, arena);
        let mut len = 0;

        loop {
            if len == cap {
                cap *= 2;
                buf.reserve_exact(cap - buf.capacity());
            }

            let count = cap - len;
            let base = buf.as_ptr();

            let n = unsafe {
                libc::read(libc::STDIN_FILENO, base.add(len) as *mut libc::c_void, count)
            };
            if n < 0 {
                return Err(io::Error::last_os_error());
            }
            if n == 0 {
                // EOF
                break;
            }
            let n = n.cast_unsigned();

            len += n;

            let hay = unsafe { slice::from_raw_parts(base, len) };
            let index = memchr(b'\n', hay, len - n);
            if index < len {
                len = index;
                break;
            }
        }

        unsafe {
            buf.as_mut_vec().set_len(len);
        }

        Ok(buf)
    }
}
