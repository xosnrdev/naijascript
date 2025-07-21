use std::path::Path;

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
