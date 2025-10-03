//! Toolchain management abstractions.

use std::borrow::Cow;
use std::io::Cursor;
#[cfg(unix)]
use std::os::unix::fs::PermissionsExt;
use std::path::{Path, PathBuf};
use std::{env, fs, io};

use sha2::{Digest, Sha256};
#[cfg(unix)]
use tar::Archive;
use tempfile::Builder;
#[cfg(unix)]
use xz2::read::XzDecoder;
#[cfg(windows)]
use zip::ZipArchive;

macro_rules! report_error {
    ($msg:expr) => {
        |err| format!("{}: {err}", $msg)
    };
}

macro_rules! print_info {
    ($($arg:tt)*) => {
        println!("\x1b[1;34minfo:\x1b[0m {}", format!($($arg)*))
    }
}

macro_rules! print_warn {
    ($($arg:tt)*) => {
        println!("\x1b[1;33mwarn:\x1b[0m {}", format!($($arg)*))
    }
}

macro_rules! print_success {
    ($($arg:tt)*) => {
        println!("\x1b[1;32mok:\x1b[0m {}", format!($($arg)*))
    }
}

#[macro_export]
macro_rules! print_error {
    ($($arg:tt)*) => {
        eprintln!("\x1b[1;31merror:\x1b[0m {}", format!($($arg)*))
    }
}

const REPO: &str = "xosnrdev/naijascript";
const ASSET_PREFIX: &str = "naija";

pub fn install_version(versions: &[String]) -> Result<(), String> {
    for version in versions {
        let version = {
            resolve_version(version)?;
            normalize_version(version)
        };
        let dir = version_dir(version);
        if dir.exists() {
            print_warn!("Version '{version}' is already installed");
            continue;
        } else {
            print_info!("Installing version '{version}'...");
            create_dir(&dir)?;
            match download_and_install(version, &dir) {
                Ok(()) => print_success!("Installation successful"),
                Err(err) => {
                    let _ = remove_path(&dir);
                    return Err(err);
                }
            }
        }
    }

    Ok(())
}

fn resolve_version<'a>(version: &'a str) -> Result<Cow<'a, str>, String> {
    print_info!("Resolving version '{version}'...");
    if version.to_lowercase() == "latest" {
        let version = fetch_latest_version()?;
        Ok(Cow::Owned(version))
    } else {
        Ok(Cow::Borrowed(version))
    }
}

fn fetch_latest_version() -> Result<String, String> {
    print_info!("Fetching latest version...");
    let url = format!("https://api.github.com/repos/{REPO}/releases/latest");
    let res = minreq::get(url)
        .with_header("User-Agent", "naijascript")
        .send()
        .map_err(report_error!("Failed to fetch latest version"))?;
    let status = res.status_code;
    if !status.is_success() {
        return Err(format!("Failed to fetch latest version (status code: {status})"));
    }
    let res = res.as_str().map_err(|err| err.to_string())?;
    let value = serde_json::from_str::<serde_json::Value>(res).map_err(|err| err.to_string())?;
    extract_tag_names(&[value])
        .into_iter()
        .next()
        .ok_or("Failed to fetch latest version".to_string())
}

fn extract_tag_names(releases: &[serde_json::Value]) -> Vec<String> {
    print_info!("Extracting tag names...");
    let (os, arch) = get_platform();
    let ext = archive_ext();
    let target = format!("{arch}-{os}");
    releases
        .iter()
        .filter_map(|release| {
            let tag = release.get("tag_name")?.as_str()?;
            let asset = format!("{ASSET_PREFIX}-{tag}-{target}.{ext}");
            release
                .get("assets")
                .and_then(|a| a.as_array())
                .into_iter()
                .flatten()
                .any(|v| {
                    v.get("browser_download_url")
                        .and_then(|u| u.as_str())
                        .is_some_and(|u| u.ends_with(&asset))
                })
                .then(|| tag.strip_prefix('v').unwrap_or(tag).to_owned())
        })
        .collect()
}

fn get_platform() -> (&'static str, &'static str) {
    let arch = if cfg!(target_arch = "x86_64") {
        "x86_64"
    } else if cfg!(target_arch = "aarch64") {
        "aarch64"
    } else {
        "unknown"
    };
    let os = if cfg!(target_os = "macos") {
        "apple-darwin"
    } else if cfg!(target_os = "linux") {
        "unknown-linux-gnu"
    } else if cfg!(windows) {
        "pc-windows-msvc"
    } else {
        "unknown"
    };
    (os, arch)
}

fn archive_ext() -> &'static str {
    if cfg!(windows) { "zip" } else { "tar.xz" }
}

fn normalize_version(version: &str) -> &str {
    version.strip_prefix(['v', 'V']).unwrap_or(version)
}

fn version_dir(version: &str) -> PathBuf {
    versions_dir().join(normalize_version(version))
}

fn versions_dir() -> PathBuf {
    home_dir().join(".naijascript/versions")
}

fn home_dir() -> PathBuf {
    let home = if cfg!(windows) { "%USERPROFILE%" } else { "$HOME" };
    env::home_dir()
        .filter(|p| !p.as_os_str().is_empty())
        .unwrap_or_else(|| panic!("{home} should be set"))
}

fn create_dir(path: &Path) -> Result<(), String> {
    print_info!("Creating directory '{}'...", path.display());
    // `std::fs::create_dir_all` is idempotent we can call it without checking if the directory exists
    fs::create_dir_all(path).map_err(report_error!("Failed to create directory"))
}

fn remove_path(path: &Path) -> Result<(), String> {
    print_info!("Removing path '{}'...", path.display());
    let ft = match fs::symlink_metadata(path) {
        Ok(m) => m.file_type(),
        Err(err) => {
            if err.kind() == io::ErrorKind::NotFound {
                return Ok(());
            } else {
                return Err(format!("Failed to check existing path: {err}"));
            }
        }
    };

    if ft.is_dir() {
        if let Err(err) = fs::remove_dir_all(path)
            && err.kind() != io::ErrorKind::NotFound
        {
            return Err(format!("Failed to remove existing directory: {err}"));
        }
    } else if let Err(err) = fs::remove_file(path)
        && err.kind() != io::ErrorKind::NotFound
    {
        return Err(format!("Failed to remove existing file: {err}"));
    }
    Ok(())
}

fn download_and_install(version: &str, version_dir: &Path) -> Result<(), String> {
    let (os, arch) = get_platform();
    if os == "unknown" || arch == "unknown" {
        return Err("Unsupported platform".to_string());
    }
    let version =
        if version.starts_with('v') { version.to_string() } else { format!("v{version}") };
    let version = normalize_version(&version);
    let ext = archive_ext();
    let target = format!("{arch}-{os}");
    let archive_name = format!("naija-{version}-{target}.{ext}");
    let url = format!("https://github.com/{REPO}/releases/download/{version}/{archive_name}");
    print_info!("Downloading binary for {arch}-{os}...");
    let res = minreq::get(&url)
        .with_header("User-Agent", "naijascript")
        .send()
        .map_err(report_error!("Failed to download binary"))?;
    let status = res.status_code;
    if !status.is_success() {
        return Err(format!("Failed to download binary (status code: {status})"));
    }
    let bytes = res.as_bytes();

    // Verify checksum ?
    let url = format!(
        "https://github.com/{REPO}/releases/download/{version}/naija-{version}-{target}.sha256"
    );
    print_info!("Downloading checksum file...");
    let res = minreq::get(&url)
        .with_header("User-Agent", "naijascript")
        .send()
        .map_err(report_error!("Failed to download checksum file"))?;
    let status = res.status_code;
    if !status.is_success() {
        return Err(format!("Failed to download checksum file (status code: {status})"));
    }
    let res = res.as_str().map_err(|err| err.to_string())?;
    let expected = res.split_whitespace().next().ok_or("Malformed checksum file")?;
    let mut hasher = Sha256::new();
    hasher.update(bytes);
    let actual = format!("{:x}", hasher.finalize());
    if actual != expected {
        return Err("Checksum verification failed: archive is corrupted or tampered".to_string());
    }
    let bin_name = bin_name();
    let out_path = version_dir.join(bin_name);
    extract_bin_from_archive(bytes, bin_name, &out_path, ext)
}

fn extract_bin_from_archive(
    bytes: &[u8],
    bin_name: &str,
    out_path: &Path,
    ext: &str,
) -> Result<(), String> {
    print_info!("Extracting binary from archive...");
    let parent = out_path.parent().ok_or("Invalid output path")?;
    let cursor = Cursor::new(bytes);
    #[cfg(unix)]
    extract_bin(bin_name, out_path, ext, parent, cursor)?;
    #[cfg(windows)]
    extract_bin(bin_name, out_path, ext, parent, cursor)?;
    Ok(())
}

#[cfg(unix)]
fn extract_bin(
    bin_name: &str,
    out_path: &Path,
    ext: &str,
    parent: &Path,
    cursor: Cursor<&[u8]>,
) -> Result<(), String> {
    if ext != "tar.xz" {
        return Err(format!("Unsupported archive format '{ext}' for Unix installation"));
    }
    let decoder = XzDecoder::new(cursor);
    let mut archive = Archive::new(decoder);
    let mut entries = archive.entries().map_err(report_error!("Failed to read archive entries"))?;
    let mut entry = entries
        .next()
        .ok_or_else(|| format!("Binary '{bin_name}' not found in archive"))?
        .map_err(report_error!("Failed to read archive entry"))?;
    let mode = entry.header().mode().unwrap_or(0o755);
    let mut temp = Builder::new()
        .prefix(".naija-")
        .tempfile_in(parent)
        .map_err(report_error!("Failed to create temporary file"))?;
    {
        let file = temp.as_file_mut();
        io::copy(&mut entry, file)
            .map_err(report_error!("Failed to write binary to temporary file"))?;
        file.sync_all().map_err(report_error!("Failed to flush binary to disk"))?;
    }
    {
        fs::set_permissions(temp.path(), fs::Permissions::from_mode(mode))
            .map_err(report_error!("Failed to set binary permissions"))?;
    }
    let temp_path = temp.into_temp_path();
    fs::rename(&temp_path, out_path).map_err(report_error!("Failed to replace binary"))?;
    if let Err(err) = temp_path.close()
        && err.kind() != io::ErrorKind::NotFound
    {
        return Err(format!("Failed to cleanup temporary file: {err}"));
    }
    Ok(())
}

#[cfg(windows)]
fn extract_bin(
    bin_name: &str,
    out_path: &Path,
    ext: &str,
    parent: &Path,
    cursor: Cursor<&[u8]>,
) -> Result<(), String> {
    if ext != "zip" {
        return Err(format!("Unsupported archive format '{ext}' for Windows installation"));
    }
    let mut archive =
        ZipArchive::new(cursor).map_err(report_error!("Failed to read ZIP archive"))?;
    let mut entry =
        archive.by_name(bin_name).map_err(report_error!("Failed to read archive entry"))?;

    let mut temp = Builder::new()
        .prefix(".naija-")
        .tempfile_in(parent)
        .map_err(report_error!("Failed to create temporary file"))?;
    {
        let mut file = temp.as_file_mut();
        io::copy(&mut entry, &mut file)
            .map_err(report_error!("Failed to write binary to temporary file"))?;
        file.sync_all().map_err(report_error!("Failed to flush binary to disk"))?;
    }

    let temp_path = temp.into_temp_path();
    fs::rename(&temp_path, out_path).map_err(report_error!("Failed to replace binary"))?;
    if let Err(err) = temp_path.close()
        && err.kind() != io::ErrorKind::NotFound
    {
        return Err(format!("Failed to cleanup temporary file: {err}"));
    }
    Ok(())
}

pub fn list_installed_version() -> Result<(), String> {
    let entries =
        fs::read_dir(versions_dir()).map_err(report_error!("Failed to read versions directory"))?;
    let versions: Vec<String> =
        entries.flatten().map(|entry| entry.file_name().to_string_lossy().to_string()).collect();
    if versions.is_empty() {
        print_info!("No versions installed");
    } else {
        print_info!("Installed versions:");
        for version in versions {
            println!("\n- {version}");
        }
    }
    Ok(())
}

pub fn fetch_available_version() -> Result<(), String> {
    print_info!("Fetching available versions...");
    let url = format!("https://api.github.com/repos/{REPO}/releases");
    let res = minreq::get(url)
        .with_header("User-Agent", "naijascript")
        .send()
        .map_err(report_error!("Failed to fetch available versions"))?;
    let status = res.status_code;
    if !status.is_success() {
        return Err(format!("Failed to fetch available versions (status code: {status})"));
    }
    let res = res.as_str().map_err(|err| err.to_string())?;
    let value = serde_json::from_str::<serde_json::Value>(res).map_err(|err| err.to_string())?;
    let releases = value.as_array().ok_or("Expected array of releases")?;
    let versions = extract_tag_names(releases);
    if versions.is_empty() {
        print_info!("No available versions found");
    } else {
        print_info!("Available versions:");
        for version in versions {
            println!("\n- {version}");
        }
    }
    Ok(())
}

fn bin_name() -> &'static str {
    if cfg!(windows) { "naija.exe" } else { "naija" }
}
trait StatusCodeExt {
    fn is_success(&self) -> bool;
}

impl StatusCodeExt for i32 {
    fn is_success(&self) -> bool {
        (200..300).contains(self)
    }
}
