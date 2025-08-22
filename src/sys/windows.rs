use std::ffi::OsStr;
use std::io;
use std::os::windows::ffi::OsStrExt;
use std::path::Path;
use std::ptr::{NonNull, null_mut};

use windows_sys::Win32::Foundation::{self, ERROR_MORE_DATA, ERROR_SUCCESS};
use windows_sys::Win32::System::Memory;
use windows_sys::Win32::System::Registry::{
    HKEY, HKEY_CURRENT_USER, KEY_READ, KEY_WRITE, REG_SZ, RegCloseKey, RegOpenKeyExW,
    RegQueryValueExW, RegSetValueExW,
};

use super::VirtualMemory;

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
        let mut size = 0u32;
        let mut value_type = 0u32;
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
        let mut buffer = vec![0u16; buffer_size];
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
