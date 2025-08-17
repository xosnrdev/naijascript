use std::path::Path;
use std::ptr::{NonNull, null_mut};

use windows_sys::Win32::Foundation;
use windows_sys::Win32::System::Memory;
use winreg::RegKey;
use winreg::enums::*;

pub fn add_to_path(dir: &Path) -> std::io::Result<()> {
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

pub fn remove_from_path(dir: &Path) -> std::io::Result<()> {
    let target = normalize_path(dir);
    let current_path = get_current_path()?;

    let new_path: Vec<&str> =
        current_path.split(';').filter(|p| normalize_path(Path::new(p.trim())) != target).collect();

    set_path(&new_path.join(";"))
}

fn get_env_key() -> std::io::Result<RegKey> {
    let hkcu = RegKey::predef(HKEY_CURRENT_USER);
    hkcu.open_subkey_with_flags("Environment", KEY_READ | KEY_WRITE)
}

fn get_current_path() -> std::io::Result<String> {
    get_env_key()?.get_value("PATH").or(Ok(String::new()))
}

fn set_path(path: &str) -> std::io::Result<()> {
    get_env_key()?.set_value("PATH", &path)
}

fn normalize_path(path: &Path) -> String {
    path.to_string_lossy().to_lowercase()
}

/// Reserves a virtual memory region of the given size.
/// To commit the memory, use [`virtual_commit`].
/// To release the memory, use [`virtual_release`].
///
/// # Safety
///
/// This function is unsafe because it uses raw pointers.
/// Don't forget to release the memory when you're done with it or you'll leak it.
pub unsafe fn virtual_reserve(size: usize) -> Result<NonNull<u8>, u32> {
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

/// Releases a virtual memory region of the given size.
///
/// # Safety
///
/// This function is unsafe because it uses raw pointers.
/// Make sure to only pass pointers acquired from [`virtual_reserve`].
pub unsafe fn virtual_release(base: NonNull<u8>, _size: usize) {
    unsafe {
        // NOTE: `VirtualFree` fails if the pointer isn't
        // a valid base address or if the size isn't zero.
        Memory::VirtualFree(base.as_ptr() as *mut _, 0, Memory::MEM_RELEASE);
    }
}

/// Commits a virtual memory region of the given size.
///
/// # Safety
///
/// This function is unsafe because it uses raw pointers.
/// Make sure to only pass pointers acquired from [`virtual_reserve`]
/// and to pass a size less than or equal to the size passed to [`virtual_reserve`].
pub unsafe fn virtual_commit(base: NonNull<u8>, size: usize) -> Result<(), u32> {
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
