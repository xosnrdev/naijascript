//! Toolchain management abstractions.

use std::borrow::Cow;
use std::collections::HashSet;
use std::io::{self, BufRead, Cursor, Write};
#[cfg(unix)]
use std::os::unix::fs::PermissionsExt;
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};
use std::{env, fs, os};

use minreq::Response;
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
        |err| Cow::Owned(format!("{}: {err}", $msg))
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

macro_rules! print_hint {
    ($($arg:tt)*) => {
        println!("\n\x1b[1;36mhint:\x1b[0m {}", format!($($arg)*))
    };
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

pub fn install_version(versions: &[String]) -> Result<(), Cow<'static, str>> {
    for version in versions.iter().map(|s| s.to_ascii_lowercase()).collect::<HashSet<_>>() {
        let version = {
            let version = resolve_version(&version)?;
            normalize_version(&version)
        };
        let dir = version_dir(&version);
        if dir.exists() || get_toolchain_version().as_deref().is_some_and(|v| v == version) {
            print_warn!("Version '{version}' already exists, skipping install.");
            continue;
        } else {
            print_info!("Installing version '{version}'...");
            create_dir(&dir)?;
            match download_and_install(&version, &dir) {
                Ok(()) => print_success!("Installation complete."),
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
    if version.eq_ignore_ascii_case("latest") {
        let version = fetch_latest_version()?;
        Ok(Cow::Owned(version))
    } else {
        Ok(Cow::Borrowed(version))
    }
}

fn fetch_latest_version() -> Result<String, Cow<'static, str>> {
    print_info!("Fetching latest version...");
    let url = format!("https://api.github.com/repos/{REPO}/releases/latest");
    let res = minreq::get(url)
        .with_header("User-Agent", "naijascript")
        .send()
        .map_err(report_error!("Failed to fetch latest version"))?;
    if !ResponseExt(&res).is_ok() {
        return Err(Cow::Owned(format!(
            "Failed to fetch latest version (status code: {})",
            res.status_code
        )));
    }
    let res = res.as_str().map_err(|err| Cow::Owned(err.to_string()))?;
    let value = serde_json::from_str::<serde_json::Value>(res)
        .map_err(|err| Cow::Owned(err.to_string()))?;
    extract_tag_names(&[value]).next().ok_or(Cow::Borrowed("Failed to fetch latest version"))
}

fn extract_tag_names(releases: &[serde_json::Value]) -> impl Iterator<Item = String> {
    print_info!("Extracting tag names...");
    let (os, arch) = get_platform();
    let ext = archive_ext();
    let target = format!("{arch}-{os}");
    releases.iter().filter_map(move |release| {
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

fn normalize_version(version: &str) -> String {
    version.strip_prefix(['v', 'V']).unwrap_or(version).to_owned()
}

fn version_dir(version: &str) -> PathBuf {
    versions_dir().join(normalize_version(version))
}

fn versions_dir() -> PathBuf {
    naijascript_dir().join("versions")
}

fn naijascript_dir() -> PathBuf {
    home_dir().join(".naijascript")
}

fn home_dir() -> PathBuf {
    let home = if cfg!(windows) { "%USERPROFILE%" } else { "$HOME" };
    env::home_dir()
        .filter(|p| !p.as_os_str().is_empty())
        .unwrap_or_else(|| panic!("{home} should be set"))
}

fn create_dir(path: &Path) -> Result<(), Cow<'static, str>> {
    print_info!("Creating directory '{}'...", path.display());
    // `std::fs::create_dir_all` is idempotent we can call it without checking if the directory exists
    fs::create_dir_all(path).map_err(report_error!("Failed to create directory"))
}

fn remove_path(path: &Path) -> Result<(), Cow<'static, str>> {
    print_info!("Removing path '{}'...", path.display());
    let ft = match fs::symlink_metadata(path) {
        Ok(m) => m.file_type(),
        Err(err) => {
            if err.kind() == io::ErrorKind::NotFound {
                return Ok(());
            } else {
                return Err(report_error!("Failed to check existing path")(err));
            }
        }
    };

    if ft.is_dir() {
        if let Err(err) = fs::remove_dir_all(path)
            && err.kind() != io::ErrorKind::NotFound
        {
            return Err(report_error!("Failed to remove existing directory")(err));
        }
    } else if let Err(err) = fs::remove_file(path)
        && err.kind() != io::ErrorKind::NotFound
    {
        return Err(report_error!("Failed to remove existing file")(err));
    }
    Ok(())
}

fn download_and_install(version: &str, version_dir: &Path) -> Result<(), Cow<'static, str>> {
    let (os, arch) = get_platform();
    if os == "unknown" || arch == "unknown" {
        return Err(Cow::Borrowed("Unsupported platform"));
    }
    let version =
        if version.starts_with('v') { version.to_string() } else { format!("v{version}") };
    let ext = archive_ext();
    let target = format!("{arch}-{os}");
    let archive_name = format!("naija-{version}-{target}.{ext}");
    let url = format!("https://github.com/{REPO}/releases/download/{version}/{archive_name}");
    print_info!("Downloading binary for {arch}-{os}...");
    let res = minreq::get(&url)
        .with_header("User-Agent", "naijascript")
        .send()
        .map_err(report_error!("Failed to download binary"))?;
    if !ResponseExt(&res).is_ok() {
        return Err(Cow::Owned(format!(
            "Failed to download binary (status code: {})",
            res.status_code
        )));
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
    if !ResponseExt(&res).is_ok() {
        return Err(Cow::Owned(format!(
            "Failed to download checksum file (status code: {})",
            res.status_code
        )));
    }
    let res = res.as_str().map_err(|err| err.to_string())?;
    let expected = res.split_whitespace().next().ok_or("Malformed checksum file")?;
    let mut hasher = Sha256::new();
    hasher.update(bytes);
    let actual = format!("{:x}", hasher.finalize());
    if actual != expected {
        return Err(Cow::Borrowed(
            "Checksum verification failed: archive is corrupted or tampered",
        ));
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
) -> Result<(), Cow<'static, str>> {
    print_info!("Extracting binary from archive...");
    let parent = out_path.parent().ok_or(Cow::Borrowed("Invalid output path"))?;
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
) -> Result<(), Cow<'static, str>> {
    if ext != "tar.xz" {
        return Err(Cow::Owned(format!(
            "Unsupported archive format '{ext}' for Unix installation"
        )));
    }
    let decoder = XzDecoder::new(cursor);
    let mut archive = Archive::new(decoder);
    let mut entries = archive.entries().map_err(report_error!("Failed to read archive entries"))?;
    let mut entry = entries
        .next()
        .ok_or_else(|| format!("Binary '{bin_name}' not found in archive"))?
        .map_err(report_error!("Failed to read archive entry"))?;
    let mode = entry.header().mode().unwrap_or(0o755);
    let mut temp_file = Builder::new()
        .prefix(".naija-install-")
        .tempfile_in(parent)
        .map_err(report_error!("Failed to create temporary file"))?;
    {
        let file = temp_file.as_file_mut();
        io::copy(&mut entry, file)
            .map_err(report_error!("Failed to write binary to temporary file"))?;
        file.sync_all().map_err(report_error!("Failed to flush binary to disk"))?;
    }
    {
        fs::set_permissions(temp_file.path(), fs::Permissions::from_mode(mode))
            .map_err(report_error!("Failed to set binary permissions"))?;
    }
    let temp_path = temp_file.into_temp_path();
    fs::rename(&temp_path, out_path).map_err(report_error!("Failed to replace binary"))?;
    if let Err(err) = temp_path.close()
        && err.kind() != io::ErrorKind::NotFound
    {
        return Err(report_error!("Failed to cleanup temporary file")(err));
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
) -> Result<(), Cow<'static, str>> {
    if ext != "zip" {
        return Err(Cow::Owned(format!(
            "Unsupported archive format '{ext}' for Windows installation"
        )));
    }
    let mut archive =
        ZipArchive::new(cursor).map_err(report_error!("Failed to read ZIP archive"))?;
    let mut entry =
        archive.by_name(bin_name).map_err(report_error!("Failed to read archive entry"))?;

    let mut temp_file = Builder::new()
        .prefix(".naija-install-")
        .tempfile_in(parent)
        .map_err(report_error!("Failed to create temporary file"))?;
    {
        let mut file = temp_file.as_file_mut();
        io::copy(&mut entry, &mut file)
            .map_err(report_error!("Failed to write binary to temporary file"))?;
        file.sync_all().map_err(report_error!("Failed to flush binary to disk"))?;
    }

    let temp_path = temp_file.into_temp_path();
    fs::rename(&temp_path, out_path).map_err(report_error!("Failed to replace binary"))?;
    if let Err(err) = temp_path.close()
        && err.kind() != io::ErrorKind::NotFound
    {
        return Err(report_error!("Failed to cleanup temporary file")(err));
    }
    Ok(())
}

pub fn list_installed_version() -> Result<(), Cow<'static, str>> {
    let mut versions = Vec::new();
    match fs::read_dir(versions_dir()) {
        Ok(entries) => {
            entries
                .flatten()
                .map(|entry| entry.file_name().to_string_lossy().to_string())
                .for_each(|entry| versions.push(entry));
        }
        Err(err)
            if err.kind() == io::ErrorKind::NotFound
                && let Some(version) = get_toolchain_version() =>
        {
            versions.push(version);
        }
        Err(err) => return Err(report_error!("Failed to read versions directory")(err)),
    }

    if versions.is_empty() {
        print_info!("No versions installed");
    } else {
        print_info!("Installed versions:\n");
        for version in versions {
            println!("- {version}");
        }
    }

    Ok(())
}

pub fn fetch_available_version() -> Result<(), Cow<'static, str>> {
    print_info!("Fetching available versions...");
    let url = format!("https://api.github.com/repos/{REPO}/releases");
    let res = minreq::get(url)
        .with_header("User-Agent", "naijascript")
        .send()
        .map_err(report_error!("Failed to fetch available versions"))?;
    if !ResponseExt(&res).is_ok() {
        return Err(Cow::Owned(format!(
            "Failed to fetch available versions (status code: {})",
            res.status_code
        )));
    }
    let res = res.as_str().map_err(|err| Cow::Owned(err.to_string()))?;
    let value = serde_json::from_str::<serde_json::Value>(res)
        .map_err(|err| Cow::Owned(err.to_string()))?;
    let releases = value.as_array().ok_or(Cow::Borrowed("Expected array of releases"))?;
    let versions: Vec<String> = extract_tag_names(releases).collect();
    if versions.is_empty() {
        print_info!("No available versions found");
    } else {
        print_info!("Available versions:\n");
        for version in versions {
            println!("- {version}");
        }
    }
    Ok(())
}

pub fn uninstall_version(versions: &[String], all: bool) -> Result<(), Cow<'static, str>> {
    if all {
        return uninstall_all();
    }
    for version in versions.iter().collect::<HashSet<_>>() {
        print_info!("Uninstalling version '{version}'...");
        let version = normalize_version(version);
        let version_dir = version_dir(&version);
        let is_default_version = get_toolchain_version().as_deref().is_some_and(|v| v == version);
        if !version_dir.exists() && !is_default_version {
            print_warn!("Version '{version}' does not exist, skipping uninstall.");
            continue;
        }
        if is_default_version {
            print_warn!("Version '{version}' is currently set as default.");
            print_hint!("Change the default version before uninstalling.");
            continue;
        }
        remove_path(&version_dir)?;
        print_success!("Uninstall complete.");
    }
    Ok(())
}

fn bin_name() -> &'static str {
    if cfg!(windows) { "naija.exe" } else { "naija" }
}

fn config_file() -> PathBuf {
    naijascript_dir().join("config.toml")
}

fn get_toolchain_version() -> Option<String> {
    read_file_trimmed(Path::new(".naijascript-toolchain")).map_or_else(
        || {
            read_file_trimmed(&config_file()).and_then(|c| {
                c.lines().find_map(|line| {
                    line.strip_prefix("default = ").and_then(|suffix| {
                        let suffix = suffix.trim_matches(['"', '\'', ' ']);
                        (!suffix.is_empty()).then(|| normalize_version(suffix))
                    })
                })
            })
        },
        |version| Some(normalize_version(&version)),
    )
}

fn read_file_trimmed(path: &Path) -> Option<String> {
    fs::read(path).ok().and_then(|s| {
        let s = String::from_utf8_lossy(s.strip_prefix(&[0xEF, 0xBB, 0xBF]).unwrap_or(&s));
        let s = s.trim();
        (!s.is_empty()).then(|| s.to_owned())
    })
}

struct ResponseExt<'a>(&'a Response);

impl<'a> ResponseExt<'a> {
    fn is_ok(&self) -> bool {
        (200..300).contains(&self.0.status_code)
    }
}

fn uninstall_all() -> Result<(), Cow<'static, str>> {
    let root_dir = naijascript_dir();

    if !try_confirm_uninstall(&root_dir)? {
        return Ok(());
    }

    let versions_dir = versions_dir();
    let default_version_dir = get_toolchain_version().map(|version| versions_dir.join(&version));

    try_purge_stale_versions(default_version_dir.as_deref(), &root_dir, &versions_dir)?;

    let script_path = try_build_uninstall_script(&root_dir)?;

    print_info!("Uninstalling default version...");
    let spawn_result = {
        #[cfg(unix)]
        {
            Command::new("sh")
                .arg(&script_path)
                .stdin(Stdio::null())
                .stdout(Stdio::null())
                .stderr(Stdio::null())
                .spawn()
        }
        #[cfg(windows)]
        {
            Command::new("cmd")
                .arg("/C")
                .arg(&script_path)
                .stdin(Stdio::null())
                .stdout(Stdio::null())
                .stderr(Stdio::null())
                .spawn()
        }
    };

    match spawn_result {
        Ok(_) => {
            print_success!("Uninstall complete.");
            print_hint!("You may need to restart your terminal for PATH changes to take effect.");
            std::process::exit(0)
        }
        Err(err) => {
            let _ = fs::remove_file(&script_path);
            Err(report_error!("Failed to uninstall default version")(err))
        }
    }
}

fn try_confirm_uninstall(root_dir: &Path) -> Result<bool, Cow<'static, str>> {
    print_warn!("This will remove all installed versions in '{}'", root_dir.display());
    print_warn!("Type 'y' or 'yes' to continue");
    print!("> ");
    io::stdout().flush().map_err(report_error!("Failed to flush confirmation prompt"))?;
    io::stdin().lock().lines().next().transpose().map_or_else(
        |err| Err(report_error!("Failed to read confirmation input")(err)),
        |line| {
            Ok(line.is_some_and(|line| {
                let yes = matches!(line.trim().to_ascii_lowercase().as_str(), "y" | "yes");
                if !yes {
                    print_info!("Uninstall aborted by user");
                }
                yes
            }))
        },
    )
}

fn try_purge_stale_versions(
    default_version_dir: Option<&Path>,
    root_dir: &Path,
    versions_dir: &Path,
) -> Result<(), Cow<'static, str>> {
    print_info!("Purging stale versions...");
    default_version_dir.map_or(Ok(()), |dir| {
        let staging_dir = Builder::new()
            .prefix(".naijascript-staging-")
            .tempdir_in(root_dir)
            .map_err(report_error!("Failed to create staging directory"))?;

        fs::read_dir(versions_dir).map_or_else(
            |err| {
                if err.kind() != io::ErrorKind::NotFound {
                    Err(report_error!("Failed to read version directory")(err))
                } else {
                    Ok(())
                }
            },
            |entries| {
                for entry in entries {
                    let entry =
                        entry.map_err(report_error!("Failed to read versions directory entry"))?;
                    let entry_path = entry.path();
                    let ft = entry
                        .file_type()
                        .map_err(report_error!("Failed to inspect versions entry"))?;
                    if entry_path == *dir || !ft.is_dir() {
                        continue;
                    }
                    let dest = staging_dir.path().join(entry.file_name());
                    fs::rename(&entry_path, &dest)
                        .map_err(report_error!("Failed to replace versions directory"))?;
                }
                staging_dir.close().map_err(report_error!("Failed to delete staged versions"))
            },
        )
    })
}

fn try_build_uninstall_script(root_dir: &Path) -> Result<PathBuf, Cow<'static, str>> {
    let bin_root = get_bin_root();
    let link_path = bin_root.join(bin_name());
    let link_path = link_path.to_string_lossy();
    let bin_root = bin_root.to_string_lossy();
    let root_dir = root_dir.to_string_lossy();
    let mut script = Builder::new()
        .prefix("naija-uninstall-")
        .suffix(if cfg!(windows) { ".bat" } else { ".sh" })
        .tempfile_in(env::temp_dir())
        .map_err(report_error!("Failed to create uninstall script"))?;

    #[cfg(unix)]
    {
        let script_contents = format!(
            r#"
                #!/bin/sh
                set -eu
                sleep 2
                PATTERN="export PATH=\"${bin_root}:\$PATH\""
                for PROFILE in "${{HOME}}/.zshrc" "${{HOME}}/.bashrc" "${{HOME}}/.profile"; do
                    if [ -f "${{PROFILE}}" ]; then
                        TMP=$(mktemp)
                        grep -F -v "${{PATTERN}}" "${{PROFILE}}" > "${{TMP}}" || true
                        mv "${{TMP}}" "${{PROFILE}}"
                    fi
                done
                rm -rf "{root_dir}"
                rm -f "{link_path}"
                rm -f "$0"
            "#
        );
        script
            .write_all(script_contents.as_bytes())
            .map_err(report_error!("Failed to write uninstall script"))?;
        fs::set_permissions(script.path(), fs::Permissions::from_mode(0o700))
            .map_err(report_error!("Failed to set uninstall script executable"))?;
    }

    #[cfg(windows)]
    {
        let script_contents = format!(
            r#"
                @echo off
                setlocal
                timeout /t 3 /nobreak >nul
                set BIN_ROOT={bin_root}
                powershell -NoProfile -ExecutionPolicy Bypass -Command "$binRoot = $env:BIN_ROOT; $profilePath = $PROFILE; if (Test-Path $profilePath) {{ $pattern = [regex]::Escape($binRoot) + ';`$env:Path'; $content = Get-Content $profilePath; $filtered = $content | Where-Object {{ $_ -notmatch $pattern }}; if ($filtered.Count -ne $content.Count) {{ $filtered | Set-Content -Path $profilePath -Encoding UTF8 }} }}; $userPath = [Environment]::GetEnvironmentVariable('Path','User'); if ($userPath) {{ $parts = $userPath.Split(';') | Where-Object {{ $_ -and $_.TrimEnd('\\') -ne $binRoot.TrimEnd('\\') }}; [Environment]::SetEnvironmentVariable('Path', ($parts -join ';'), 'User') }}"
                rmdir /s /q "{root_dir}"
                if exist "{link_path}" del "{link_path}"
                del "%~f0"
            "#
        );
        script
            .write_all(script_contents.as_bytes())
            .map_err(report_error!("Failed to write uninstall script"))?;
    }

    script.as_file().sync_all().map_err(report_error!("Failed to sync uninstall script"))?;
    script.into_temp_path().keep().map_err(report_error!("Failed to persist uninstall script"))
}

pub fn set_default_version(version: &str) -> Result<(), Cow<'static, str>> {
    let version = {
        let version = resolve_version(version)?;
        normalize_version(&version)
    };
    if get_toolchain_version().as_deref().is_some_and(|v| v == version) {
        print_warn!("Version '{version}' is already set as default.");
        return Ok(());
    }
    print_info!("Setting default version to '{version}'...");
    let config_path = config_file();
    create_dir(config_path.parent().ok_or("Invalid config path")?)?;
    fs::write(&config_path, format!("default = \"{version}\"\n"))
        .map_err(report_error!("Failed to write config file"))?;
    let bin_path = version_dir(&version).join(bin_name());
    let (temp_path_buf, script_path) = try_build_symlink_script(&bin_path)?;
    let spawn_result = {
        #[cfg(unix)]
        {
            Command::new("sh")
                .arg(&script_path)
                .stdin(Stdio::null())
                .stdout(Stdio::null())
                .stderr(Stdio::null())
                .spawn()
        }
        #[cfg(windows)]
        {
            Command::new("cmd")
                .arg("/C")
                .arg(&script_path)
                .stdin(Stdio::null())
                .stdout(Stdio::null())
                .stderr(Stdio::null())
                .spawn()
        }
    };

    match spawn_result {
        Ok(_) => {
            print_success!("Default version set to '{version}'");
            std::process::exit(0)
        }
        Err(err) => {
            let _ = fs::remove_file(&temp_path_buf);
            let _ = fs::remove_file(&script_path);
            Err(report_error!("Failed to replace existing symlink")(err))
        }
    }
}

fn try_build_symlink_script(bin_path: &Path) -> Result<(PathBuf, PathBuf), Cow<'static, str>> {
    if !bin_path.exists() {
        return Err(Cow::Owned(format!("Binary path '{}' does not exist", bin_path.display())));
    }

    print_info!("Creating symlink to '{}'", bin_path.display());

    let bin_root = get_bin_root();
    let link = bin_root.join(bin_name());

    create_dir(&bin_root)?;

    let temp_path = Builder::new().prefix(".naija-symlink-").tempfile_in(&bin_root).map_or_else(
        |err| Err(report_error!("Failed to reserve temporary symlink location")(err)),
        |file| Ok(file.into_temp_path()),
    )?;

    // We only need the path for the symlink, not the temporary FD
    let temp_path_buf = temp_path.to_path_buf();
    temp_path.close().map_err(report_error!("Failed to reserve temporary symlink location"))?;

    let mut script = Builder::new()
        .prefix("naija-symlink-")
        .suffix(if cfg!(windows) { ".bat" } else { ".sh" })
        .tempfile_in(env::temp_dir())
        .map_err(report_error!("Failed to create symlink script"))?;

    #[cfg(unix)]
    {
        os::unix::fs::symlink(bin_path, &temp_path_buf)
            .map_err(report_error!("Failed to create symlink to version"))?;

        let script_content = format!(
            r#"
                #!/bin/sh
                set -eu
                sleep 2
                mv -f "{}" "{}"
                rm -f "$0"
            "#,
            temp_path_buf.to_string_lossy(),
            link.to_string_lossy()
        );
        script
            .write_all(script_content.as_bytes())
            .map_err(report_error!("Failed to write symlink script"))?;
        fs::set_permissions(script.path(), fs::Permissions::from_mode(0o700))
            .map_err(report_error!("Failed to set symlink script executable"))?;
    }

    #[cfg(windows)]
    {
        os::windows::fs::symlink_file(bin_path, &temp_path_buf).map_err(|err| {
            if err.raw_os_error() == Some(1314) {
                return Cow::Borrowed(
                    r#"Failed to create symlink: Windows denied the request (OS error 1314).

                You can fix this by either: 
                1. Enabling Developer Mode in Windows Settings, or 
                2. Re-running this command from an elevated (Administrator) terminal."#,
                );
            }
            report_error!("Failed to create symlink to version")(err)
        })?;

        let script_content = format!(
            r#"
                @echo off
                timeout /t 3 /nobreak >nul
                move /Y "{}" "{}"
                del "%~f0"
            "#,
            temp_path_buf.to_string_lossy(),
            link.to_string_lossy()
        );
        script
            .write_all(script_content.as_bytes())
            .map_err(report_error!("Failed to write symlink script"))?;
    }
    script.as_file().sync_all().map_err(report_error!("Failed to sync symlink script"))?;
    let script = script
        .into_temp_path()
        .keep()
        .map_err(report_error!("Failed to persist symlink script"))?;

    Ok((temp_path_buf, script))
}

fn get_bin_root() -> PathBuf {
    naijascript_dir().join("bin")
}
