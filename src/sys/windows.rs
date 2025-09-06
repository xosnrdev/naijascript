use std::ffi::OsStr;
use std::io::{self, Write};
use std::os::windows::ffi::OsStrExt;
use std::path::Path;
use std::ptr::{NonNull, null_mut};
use std::{char, slice};

use windows_sys::Win32::Foundation::{self, ERROR_MORE_DATA, ERROR_SUCCESS, INVALID_HANDLE_VALUE};
use windows_sys::Win32::Storage::FileSystem::ReadFile;
use windows_sys::Win32::System::Console::{
    GetConsoleMode, GetStdHandle, ReadConsoleW, STD_INPUT_HANDLE,
};
use windows_sys::Win32::System::Memory;
use windows_sys::Win32::System::Registry::{
    HKEY, HKEY_CURRENT_USER, KEY_READ, KEY_WRITE, REG_SZ, RegCloseKey, RegOpenKeyExW,
    RegQueryValueExW, RegSetValueExW,
};

use super::{Stdin, VirtualMemory};
use crate::KIBI;
use crate::arena::{Arena, ArenaString};
use crate::simd::memchr;

pub fn add_to_path(dir: &Path) -> io::Result<()> {
    let dir_str = dir.to_string_lossy();
    let current_path = get_current_path()?;

    if current_path.to_lowercase().contains(&normalize_path(dir)) {
        return Ok(());
    }

    let new_path = if current_path.is_empty() {
        dir_str.to_string()
    } else {
        format!("{dir_str};{current_path}")
    };

    set_path(&new_path)
}

pub fn remove_from_path(dir: &Path) -> io::Result<()> {
    let target = normalize_path(dir);
    let current_path = get_current_path()?;

    let new_path: Vec<&str> =
        current_path.split(';').filter(|p| normalize_path(Path::new(p.trim())) != target).collect();

    set_path(&new_path.join(";"))
}

fn get_current_path() -> io::Result<String> {
    unsafe {
        let mut hkey: HKEY = null_mut();
        let environment_key = to_utf16("Environment");
        let path_value = to_utf16("PATH");

        // Open the registry key
        let result =
            RegOpenKeyExW(HKEY_CURRENT_USER, environment_key.as_ptr(), 0, KEY_READ, &mut hkey);

        if result != ERROR_SUCCESS {
            return Ok(String::new());
        }

        // First, query the size needed
        let mut size = 0;
        let mut value_type = 0;
        let query_result = RegQueryValueExW(
            hkey,
            path_value.as_ptr(),
            null_mut(),
            &mut value_type,
            null_mut(),
            &mut size,
        );

        if query_result != ERROR_SUCCESS && query_result != ERROR_MORE_DATA {
            RegCloseKey(hkey);
            return Ok(String::new());
        }

        // Allocate buffer and read the value
        let buffer_size = (size / 2) as usize;
        let mut buffer = vec![0; buffer_size];
        let read_result = RegQueryValueExW(
            hkey,
            path_value.as_ptr(),
            null_mut(),
            &mut value_type,
            buffer.as_mut_ptr() as *mut u8,
            &mut size,
        );

        RegCloseKey(hkey);

        if read_result == ERROR_SUCCESS {
            Ok(from_utf16_buffer(&buffer))
        } else {
            Ok(String::new())
        }
    }
}

fn set_path(path: &str) -> io::Result<()> {
    unsafe {
        let mut hkey: HKEY = null_mut();
        let environment_key = to_utf16("Environment");
        let path_value = to_utf16("PATH");
        let path_data = to_utf16(path);

        // Open the registry key for writing
        let result =
            RegOpenKeyExW(HKEY_CURRENT_USER, environment_key.as_ptr(), 0, KEY_WRITE, &mut hkey);

        if result != ERROR_SUCCESS {
            return Err(win32_error_to_io_error(result));
        }

        // Set the value
        let set_result = RegSetValueExW(
            hkey,
            path_value.as_ptr(),
            0,
            REG_SZ,
            path_data.as_ptr() as *const u8,
            (path_data.len() * 2) as u32,
        );

        RegCloseKey(hkey);

        if set_result == ERROR_SUCCESS { Ok(()) } else { Err(win32_error_to_io_error(set_result)) }
    }
}

fn normalize_path(path: &Path) -> String {
    path.to_string_lossy().to_lowercase()
}

fn to_utf16(s: &str) -> Vec<u16> {
    OsStr::new(s).encode_wide().chain(std::iter::once(0)).collect()
}

fn from_utf16_buffer(buffer: &[u16]) -> String {
    // Find the first null terminator
    let len = buffer.iter().position(|&c| c == 0).unwrap_or(buffer.len());
    String::from_utf16_lossy(&buffer[..len])
}

fn win32_error_to_io_error(error_code: u32) -> io::Error {
    io::Error::from_raw_os_error(error_code as i32)
}

pub struct WindowsVirtualMemory;

impl VirtualMemory for WindowsVirtualMemory {
    unsafe fn reserve(size: usize) -> Result<NonNull<u8>, u32> {
        unsafe {
            let mut _base = null_mut();

            // In debug builds, we use fixed addresses to aid in debugging.
            // Makes it possible to immediately tell which address space a pointer belongs to.
            #[cfg(all(debug_assertions, not(target_pointer_width = "32")))]
            {
                static mut S_BASE_GEN: usize = 0x0000100000000000; // 16 TiB
                S_BASE_GEN += 0x0000001000000000; // 64 GiB
                _base = S_BASE_GEN as *mut _;
            }

            check_ptr_return(Memory::VirtualAlloc(
                _base,
                size,
                Memory::MEM_RESERVE,
                Memory::PAGE_READWRITE,
            ) as *mut u8)
        }
    }

    unsafe fn commit(base: NonNull<u8>, size: usize) -> Result<(), u32> {
        unsafe {
            check_ptr_return(Memory::VirtualAlloc(
                base.as_ptr() as *mut _,
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
            Memory::VirtualFree(base.as_ptr() as *mut _, 0, Memory::MEM_RELEASE);
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

#[inline]
const fn gle_to_apperr(gle: u32) -> u32 {
    if gle == 0 { 0x8000FFFF } else { 0x80070000 | gle }
}

pub struct WindowsStdin;

impl Stdin for WindowsStdin {
    fn read_line<'arena>(
        prompt: &str,
        arena: &'arena Arena,
    ) -> Result<ArenaString<'arena>, io::Error> {
        print!("{prompt}");
        io::stdout().flush()?;

        if is_console() { read_line_console(arena) } else { read_line_pipe(arena) }
    }
}

fn read_line_console<'arena>(arena: &'arena Arena) -> Result<ArenaString<'arena>, io::Error> {
    let handle = unsafe { GetStdHandle(STD_INPUT_HANDLE) };
    if handle == INVALID_HANDLE_VALUE {
        return Err(io::Error::last_os_error());
    }

    let mut cap = 16 * KIBI;
    let mut buf16: Vec<u16, &Arena> = Vec::with_capacity_in(cap, arena);
    let mut total = 0;
    let mut scan_start = 0;

    loop {
        if total == cap {
            cap = cap
                .checked_mul(2)
                .ok_or(win32_error_to_io_error(Foundation::ERROR_NOT_ENOUGH_MEMORY))?;
            buf16.reserve_exact(cap.saturating_sub(buf16.capacity()));
        }

        let avail = cap - total;
        let base = buf16.as_mut_ptr();
        let mut chars_read = 0;

        let n = unsafe {
            ReadConsoleW(
                handle,
                base.add(total) as *mut _,
                avail as u32,
                &mut chars_read,
                null_mut(),
            )
        };

        if n == 0 {
            return Err(io::Error::last_os_error());
        }
        if chars_read == 0 {
            // EOF
            break;
        }

        total = total.saturating_add(chars_read as usize);
        let len = total - scan_start;

        let hay = unsafe { slice::from_raw_parts(base.add(scan_start), len) };

        if let Some(pos) = hay.iter().position(|&c| c == 0x1A) {
            total = scan_start + pos;
            break;
        }
        if let Some(pos) = hay.iter().position(|&c| c == (b'\n' as u16)) {
            let pos = scan_start + pos;
            // Check for and strip the preceding Carriage Return for CRLF endings
            if pos > 0 && unsafe { *base.add(pos - 1) } == (b'\r' as u16) {
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

    let len: usize = char::decode_utf16(buf16.iter().cloned())
        .map(|r| r.unwrap_or(char::REPLACEMENT_CHARACTER).len_utf8())
        .sum();

    let mut buf8: Vec<u8, &Arena> = Vec::with_capacity_in(len, arena);
    let mut i = 0;
    for ch in char::decode_utf16(buf16.into_iter()) {
        let ch = ch.unwrap_or(char::REPLACEMENT_CHARACTER);
        let enc = ch.encode_utf8(&mut buf8[i..]);
        i += enc.len();
    }

    let res = unsafe { ArenaString::from_utf8_unchecked(buf8) };

    Ok(res)
}

fn read_line_pipe<'arena>(arena: &'arena Arena) -> Result<ArenaString<'arena>, io::Error> {
    let handle = unsafe { GetStdHandle(STD_INPUT_HANDLE) };
    if handle == INVALID_HANDLE_VALUE {
        return Err(io::Error::last_os_error());
    }

    let mut cap = 8 * KIBI;
    let mut buf = ArenaString::with_capacity_in(cap, arena);
    let mut total = 0;

    loop {
        if total == cap {
            cap = cap
                .checked_mul(2)
                .ok_or(win32_error_to_io_error(Foundation::ERROR_NOT_ENOUGH_MEMORY))?;
            buf.reserve_exact(cap.saturating_sub(buf.capacity()));
        }

        let avail = cap - total;
        let base = buf.as_ptr();
        let mut bytes_read = 0;

        let n = unsafe {
            ReadFile(handle, base.add(total) as *mut _, avail as u32, &mut bytes_read, null_mut())
        };

        if n == 0 {
            return Err(io::Error::last_os_error());
        }
        if bytes_read == 0 {
            // EOF
            break;
        }

        let n = bytes_read as usize;
        total = total.saturating_add(n);

        let hay = unsafe { slice::from_raw_parts(base, total) };
        let pos = memchr(b'\n', hay, total - n);
        if pos < total {
            total = pos + 1;
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
        let handle = GetStdHandle(STD_INPUT_HANDLE);
        if handle == INVALID_HANDLE_VALUE {
            return false;
        }
        let mut mode = 0;
        GetConsoleMode(handle, &mut mode) != 0
    }
}
