use std::io::{self, Write};
use std::ptr::{NonNull, null_mut};
use std::{char, slice};

use memchr_rs::memchr;
use windows_sys::Win32::Foundation::{self, INVALID_HANDLE_VALUE};
use windows_sys::Win32::Storage::FileSystem::ReadFile;
use windows_sys::Win32::System::Console::{
    GetConsoleMode, GetStdHandle, ReadConsoleW, STD_INPUT_HANDLE,
};
use windows_sys::Win32::System::Memory;

use super::{Stdin, VirtualMemory};
use crate::KIBI;
use crate::arena::{Arena, ArenaString};
use crate::runtime::Value;

pub struct WindowsVirtualMemory;

impl VirtualMemory for WindowsVirtualMemory {
    unsafe fn reserve(size: usize) -> Result<NonNull<u8>, u32> {
        unsafe {
            #[allow(unused)]
            let mut base = null_mut();

            // In debug builds, we use fixed addresses to aid in debugging.
            // Makes it possible to immediately tell which address space a pointer belongs to.
            #[cfg(all(debug_assertions, not(target_pointer_width = "32")))]
            {
                static mut S_BASE_GEN: usize = 0x0000_1000_0000_0000; // 16 TiB
                S_BASE_GEN += 0x0000_0010_0000_0000; // 64 GiB
                base = S_BASE_GEN as *mut _;
            }

            check_ptr_return(
                Memory::VirtualAlloc(base, size, Memory::MEM_RESERVE, Memory::PAGE_READWRITE)
                    .cast::<u8>(),
            )
        }
    }

    unsafe fn commit(base: NonNull<u8>, size: usize) -> Result<(), u32> {
        unsafe {
            check_ptr_return(Memory::VirtualAlloc(
                base.as_ptr().cast(),
                size,
                Memory::MEM_COMMIT,
                Memory::PAGE_READWRITE,
            ))
            .map(|_| ())
        }
    }

    unsafe fn release(base: NonNull<u8>, _size: usize) {
        unsafe {
            // NOTE: `VirtualFree` fails if the pointer isn't
            // a valid base address or if the size isn't zero.
            Memory::VirtualFree(base.as_ptr().cast(), 0, Memory::MEM_RELEASE);
        }
    }
}

fn check_ptr_return<T>(ret: *mut T) -> Result<NonNull<T>, u32> {
    NonNull::new(ret).ok_or_else(get_last_error)
}

#[cold]
fn get_last_error() -> u32 {
    unsafe { gle_to_apperr(Foundation::GetLastError()) }
}

const fn gle_to_apperr(gle: u32) -> u32 {
    if gle == 0 { 0x8000_FFFF } else { 0x8007_0000 | gle }
}

pub struct WindowsStdin;

impl Stdin for WindowsStdin {
    fn read_line<'arena>(
        prompt: &Value<'arena, '_>,
        arena: &'arena Arena,
    ) -> Result<ArenaString<'arena>, io::Error> {
        print!("{prompt}");
        io::stdout().flush()?;

        if is_console() { read_line_console(arena) } else { read_line_pipe(arena) }
    }
}

fn read_line_console(arena: &Arena) -> Result<ArenaString<'_>, io::Error> {
    let hconsoleinput = unsafe { GetStdHandle(STD_INPUT_HANDLE) };
    if hconsoleinput == INVALID_HANDLE_VALUE {
        return Err(io::Error::last_os_error());
    }

    let mut cap = 16 * KIBI;
    let mut buf16: Vec<u16, &Arena> = Vec::with_capacity_in(cap, arena);
    let (mut total, mut scan_start) = (0, 0);

    loop {
        if total == cap {
            cap *= 2;
            buf16.reserve_exact(cap - buf16.capacity());
        }

        #[allow(clippy::cast_possible_truncation)]
        let nnumberofcharstoread = (cap - total) as u32;
        let base = buf16.as_ptr();
        let mut lpnumberofcharsread = 0;

        let n = unsafe {
            ReadConsoleW(
                hconsoleinput,
                base.add(total) as *mut _,
                nnumberofcharstoread,
                &raw mut lpnumberofcharsread,
                null_mut(),
            )
        };
        if n == 0 {
            return Err(io::Error::last_os_error());
        }

        if lpnumberofcharsread == 0 {
            // EOF
            break;
        }

        total += lpnumberofcharsread as usize;
        let len = total - scan_start;

        let hay = unsafe { slice::from_raw_parts(base.add(scan_start), len) };
        if let Some(pos) = hay.iter().position(|&c| c == 0x1A) {
            total = scan_start + pos;
            break;
        }
        if let Some(pos) = hay.iter().position(|&c| c == u16::from(b'\n')) {
            let pos = scan_start + pos;
            // Check for and strip the preceding Carriage Return for CRLF endings
            if pos > 0 && unsafe { *base.add(pos - 1) } == u16::from(b'\r') {
                total = pos - 1;
            } else {
                total = pos;
            }
            break;
        }

        scan_start = total;
    }

    unsafe {
        buf16.set_len(total);
    }

    let mut buf8: Vec<u8, &Arena> = Vec::with_capacity_in(total * 3 + 4, arena);
    for ch in char::decode_utf16(buf16.into_iter()) {
        let ch = ch.unwrap_or(char::REPLACEMENT_CHARACTER);
        let mut dst = [0u8; 4];
        let enc = ch.encode_utf8(&mut dst);
        buf8.extend_from_slice(enc.as_bytes());
    }

    let buf = unsafe { ArenaString::from_utf8_unchecked(buf8) };

    Ok(buf)
}

fn read_line_pipe(arena: &Arena) -> Result<ArenaString<'_>, io::Error> {
    let hfile = unsafe { GetStdHandle(STD_INPUT_HANDLE) };
    if hfile == INVALID_HANDLE_VALUE {
        return Err(io::Error::last_os_error());
    }

    let mut cap = 8 * KIBI;
    let mut buf = ArenaString::with_capacity_in(cap, arena);
    let mut total = 0;

    loop {
        if total == cap {
            cap *= 2;
            buf.reserve_exact(cap - buf.capacity());
        }

        let base = buf.as_ptr();
        #[allow(clippy::cast_possible_truncation)]
        let nnumberofbytestoread = (cap - total) as u32;
        let mut lpnumberofbytesread = 0;

        let n = unsafe {
            ReadFile(
                hfile,
                base.add(total).cast_mut(),
                nnumberofbytestoread,
                &raw mut lpnumberofbytesread,
                null_mut(),
            )
        };
        if n == 0 {
            return Err(io::Error::last_os_error());
        }

        if lpnumberofbytesread == 0 {
            // EOF
            break;
        }

        let n = lpnumberofbytesread as usize;
        total += n;

        let hay = unsafe { slice::from_raw_parts(base, total) };
        let index = memchr(b'\n', hay, total - n);
        if index < total {
            total = index;
            break;
        }
    }

    unsafe {
        buf.as_mut_vec().set_len(total);
    }

    Ok(buf)
}

fn is_console() -> bool {
    unsafe {
        let hconsolehandle = GetStdHandle(STD_INPUT_HANDLE);
        if hconsolehandle == INVALID_HANDLE_VALUE {
            return false;
        }
        let mut lpmode = 0;
        GetConsoleMode(hconsolehandle, &raw mut lpmode) != 0
    }
}
