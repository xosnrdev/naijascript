//! The official Toolchain Manager for NaijaScript

#[cfg(target_os = "windows")]
mod windows;

use std::borrow::Cow;
use std::fs;
use std::fs::File;
use std::io::{self, Cursor, Write};
use std::path::{Path, PathBuf};
use std::process::Command;
use std::sync::OnceLock;

use clap::{Parser, Subcommand};
use clap_cargo::style::CLAP_STYLING;
use regex_lite::Regex;

const REPO: &str = "xosnrdev/naijascript";

#[derive(Parser)]
#[command(
    version,
    about = "Toolchain manager for NaijaScript",
    display_name = "naijaup",
    styles = CLAP_STYLING
)]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    /// Install one NaijaScript version for your system
    Install { version: String },
    /// Show all NaijaScript versions wey you don install
    List,
    /// Set which version go be your default NaijaScript
    Default { version: String },
    /// Run your script with the NaijaScript version wey you choose
    Run {
        script: String,
        #[arg(last = true)]
        args: Vec<String>,
    },
    #[command(name = "self")]
    /// Naijaup self-management (update, uninstall, etc)
    Self_ {
        #[command(subcommand)]
        action: SelfAction,
    },
    /// Comot one NaijaScript version from your system
    Uninstall { version: String },
    /// Show all NaijaScript versions wey dey online
    Available,
}

#[derive(Subcommand)]
enum SelfAction {
    /// Update naijaup itself (make sure you dey use latest)
    Update,
    /// Comot naijaup and all NaijaScript data from your system
    Uninstall {
        /// No ask me for confirmation (I sabi wetin I dey do)
        #[arg(long)]
        yes: bool,
    },
}

macro_rules! report_err {
    ($msg:expr) => {
        |e| format!(concat!($msg, ": {}"), e)
    };
}

macro_rules! print_info {
    ($($arg:tt)*) => { println!("\x1b[34m[info]\x1b[0m {}", format!($($arg)*)) } // Blue
}
macro_rules! print_warn {
    ($($arg:tt)*) => { println!("\x1b[33m[warn]\x1b[0m {}", format!($($arg)*)) } // Yellow
}
macro_rules! print_success {
    ($($arg:tt)*) => { println!("\x1b[32m[ok]\x1b[0m {}", format!($($arg)*)) } // Green
}
macro_rules! print_error {
    ($($arg:tt)*) => { eprintln!("\x1b[31m[err]\x1b[0m {}", format!($($arg)*)) } // Red
}

fn main() {
    if let Err(e) = run() {
        print_error!("{e}");
        std::process::exit(1);
    }
}

fn ensure_dir_exists(path: &Path) -> Result<(), String> {
    if !path.exists() {
        fs::create_dir_all(path).map_err(report_err!("I no fit create directory"))?;
    }
    Ok(())
}

fn remove_if_exists(path: &Path) -> Result<(), String> {
    // Strip trailing slashes to avoid ENOTDIR errors on symlink removal (Unix).
    let path_buf;
    let path = {
        let s = path.as_os_str().to_string_lossy();
        let trimmed = s.trim_end_matches(['/', '\\']);
        if trimmed.len() == s.len() {
            path
        } else {
            path_buf = PathBuf::from(trimmed);
            &path_buf
        }
    };
    // Use symlink_metadata, not exists(), so broken symlinks are not missed.
    let meta = match fs::symlink_metadata(path) {
        Ok(m) => m,
        Err(_) => {
            // If metadata fails, the path truly does not exist (not even as a broken symlink)
            return Ok(());
        }
    };
    let ft = meta.file_type();
    // Always use remove_file for symlinks to avoid following the link.
    if ft.is_symlink() {
        fs::remove_file(path).map_err(report_err!("I no fit remove symlink"))?;
    } else if ft.is_dir() {
        // Only use remove_dir_all for real directories, never for symlinks.
        fs::remove_dir_all(path).map_err(report_err!("I no fit remove directory"))?;
    } else {
        fs::remove_file(path).map_err(report_err!("I no fit remove file"))?;
    }
    Ok(())
}

fn installed_versions() -> Vec<String> {
    let vdir = versions_dir();
    match fs::read_dir(&vdir) {
        Ok(entries) => {
            entries.flatten().map(|entry| entry.file_name().to_string_lossy().to_string()).collect()
        }
        Err(_) => Vec::new(),
    }
}

fn read_trimmed_file(path: &Path) -> Option<String> {
    if let Ok(buf) = fs::read(path)
        && let Some((start, end)) = find_non_whitespace(&buf)
        && let Ok(s) = std::str::from_utf8(&buf[start..end])
    {
        let trimmed = s.trim();
        if !trimmed.is_empty() {
            return Some(trimmed.to_string());
        }
    }
    None
}

// Extract tag names that have a matching binary asset in the JSON response.
fn extract_tag_names(json: &str) -> Vec<String> {
    static RE: OnceLock<Regex> = OnceLock::new();
    let re = RE.get_or_init(|| Regex::new(r#"tag_name"\s*:\s*"([^"]+)""#).unwrap());
    let (os, arch) = get_platform();
    let ext = archive_ext();
    let target = format!("{arch}-{os}");

    re.captures_iter(json)
        .filter_map(|cap| {
            let tag = cap.get(1)?.as_str();
            let asset = format!("naija-{tag}-{target}.{ext}");
            json.contains(&asset).then(|| tag.trim_start_matches('v').to_owned())
        })
        .collect()
}

fn fetch_latest_version_tag(client: &reqwest::blocking::Client) -> Result<String, String> {
    let url = &format!("https://api.github.com/repos/{REPO}/releases/latest");
    let res = client.get(url).send().map_err(report_err!("I no fit reach GitHub"))?;
    let status = res.status();
    if !status.is_success() {
        return Err(format!("Request fail with status: {status}. Try again later."));
    }
    let text = res.text().map_err(report_err!("I no fit read GitHub response"))?;
    extract_tag_names(&text)
        .into_iter()
        .next()
        .ok_or("I no fit process latest version tag.".to_string())
}

fn resolve_version<'a>(
    version: &'a str,
    client: &reqwest::blocking::Client,
) -> Result<Cow<'a, str>, String> {
    if version.trim().is_empty() {
        return Err("Oga, you no fit leave version empty. Abeg enter correct value.".to_string());
    }
    if version.to_lowercase() == "latest" {
        let tag = fetch_latest_version_tag(client)?;
        print_info!("I don resolve 'latest' to version: {tag}");
        Ok(Cow::Owned(tag))
    } else {
        Ok(Cow::Borrowed(version))
    }
}

fn normalize_version(version: &str) -> String {
    version.trim_start_matches(['v', 'V']).to_string()
}

fn run() -> Result<(), String> {
    let cli = Cli::parse();
    let client = reqwest::blocking::Client::builder()
        .user_agent("naijaup")
        .build()
        .map_err(report_err!("I no fit create HTTP client"))?;
    match &cli.command {
        Commands::Install { version } => {
            let resolved_version = resolve_version(version, &client)?;
            let norm_version = normalize_version(&resolved_version);
            let vdir = version_dir(&norm_version);
            if vdir.exists() {
                print_info!("Version {norm_version} don already dey your system.");
            } else {
                ensure_dir_exists(&vdir)?;
                match download_and_install(&norm_version, &vdir) {
                    Ok(()) => {
                        print_success!("I don install version: {norm_version}")
                    }
                    Err(e) => {
                        let _ = remove_if_exists(&vdir);
                        return Err(e);
                    }
                }
            }
        }
        Commands::List => {
            let versions = installed_versions();
            if versions.is_empty() {
                print_info!("I no see any installed version.");
            } else {
                print_success!("See all the versions wey you don install:");
                for v in versions {
                    print_info!("  - {v}");
                }
            }
        }
        Commands::Default { version } => {
            let resolved_version = resolve_version(version, &client)?;
            let norm_version = normalize_version(&resolved_version);
            let vdir = version_dir(&norm_version);
            if !vdir.exists() {
                return Err(format!(
                    "You never install version {norm_version} yet. Run `naijaup install {norm_version}` first"
                ));
            }
            let current_default = find_toolchain_version().map(|v| normalize_version(&v));
            if let Some(def) = current_default
                && def == norm_version
            {
                print_success!("Version {norm_version} na default already.");
                return Ok(());
            }
            let conf = config_file();
            if let Some(parent) = conf.parent() {
                ensure_dir_exists(parent)?;
            }
            fs::write(&conf, format!("default = \"{norm_version}\"\n"))
                .map_err(report_err!("I no fit write config"))?;
            print_success!("I don set {norm_version} as your default NaijaScript version");
            update_default_symlink(&norm_version)?;
        }
        Commands::Run { script, args } => {
            let version = find_toolchain_version().ok_or_else(||
                "I no sabi the NaijaScript version to use. Set default or add .naijascript-toolchain.".to_string()
            )?;
            let norm_version = normalize_version(&version);
            let bin = version_bin_path(&norm_version);
            if !bin.exists() {
                return Err(format!(
                    "I no see NaijaScript version {norm_version} for your system. Run `naijaup install {norm_version}` first."
                ));
            }
            let mut cmd = Command::new(bin);
            cmd.arg(script);
            for a in args {
                cmd.arg(a);
            }
            let status = cmd.status();
            match status {
                Ok(s) if s.success() => {}
                Ok(s) => std::process::exit(s.code().unwrap_or(1)),
                Err(e) => return Err(format!("I no fit run your script: {e}")),
            }
        }
        Commands::Self_ { action } => match action {
            SelfAction::Update => self_update(&client)?,
            SelfAction::Uninstall { yes } => self_uninstall(*yes)?,
        },
        Commands::Uninstall { version } => {
            let norm_version = normalize_version(version);
            let vdir = version_dir(&norm_version);
            if !vdir.exists() {
                print_info!("Version {norm_version} no dey your system.");
                return Ok(());
            }
            let default_version = find_toolchain_version().map(|v| normalize_version(&v));
            if let Some(def) = default_version
                && def == norm_version
            {
                print_warn!(
                    "You wan remove your default version ({norm_version})? Set another default first."
                );
                return Ok(());
            }
            remove_if_exists(&vdir)?;
            print_success!("I don comot NaijaScript version: {norm_version}");
        }
        Commands::Available => {
            print_info!("I dey fetch all NaijaScript versions wey dey online...");
            let versions = fetch_available_versions(&client)?;
            if versions.is_empty() {
                print_info!("I no see any available version online.");
            } else {
                print_success!("See all the available version online:");
                for v in versions {
                    print_info!("  - {v}");
                }
            }
        }
    }
    Ok(())
}

fn get_platform() -> (&'static str, &'static str) {
    // Returns platform/arch as used in release artifact naming.
    // This must match the release workflow and bootstrap scripts for cross-platform correctness.
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
    } else if cfg!(target_os = "windows") {
        "pc-windows-msvc"
    } else {
        "unknown"
    };
    (os, arch)
}

fn version_dir(version: &str) -> PathBuf {
    versions_dir().join(normalize_version(version))
}

fn version_bin_path(version: &str) -> PathBuf {
    let bin = bin_name();
    version_dir(version).join(bin)
}

fn bin_name() -> &'static str {
    if cfg!(windows) { "naija.exe" } else { "naija" }
}

fn archive_ext() -> &'static str {
    if cfg!(windows) { "zip" } else { "tar.xz" }
}

fn version_tag(version: &str) -> String {
    if version.starts_with('v') { version.to_string() } else { format!("v{version}") }
}

fn download_and_install(version: &str, vdir: &Path) -> Result<(), String> {
    let (os, arch) = get_platform();
    if os == "unknown" || arch == "unknown" {
        return Err("I no sabi your OS or architecture".to_string());
    }
    let version_tag = version_tag(&normalize_version(version));
    let ext = archive_ext();
    let bin_name = bin_name();
    let target = format!("{arch}-{os}");
    let archive_name = format!("naija-{version_tag}-{target}.{ext}");
    let url = format!("https://github.com/{REPO}/releases/download/{version_tag}/{archive_name}");
    print_info!("I dey download NaijaScript from: {url}");
    let client = reqwest::blocking::Client::builder()
        .user_agent("naijaup")
        .build()
        .map_err(report_err!("I no fit create HTTP client"))?;
    let res = client.get(&url).send().map_err(report_err!("I no fit download"))?;
    let status = res.status();
    if !status.is_success() {
        return Err(format!("Request fail with status: {status}. Try again later."));
    }
    let bytes = res.bytes().map_err(report_err!("I no fit read download"))?;
    let out_path = vdir.join(bin_name);
    extract_bin_from_archive(&bytes, bin_name, &out_path, ext)?;
    Ok(())
}

// Extract the named binary from a release archive (zip or tar.xz) and write it to out_path.
fn extract_bin_from_archive(
    bytes: &[u8],
    bin_name: &str,
    out_path: &Path,
    ext: &str,
) -> Result<(), String> {
    let mut found = false;
    if ext == "zip" {
        let reader = Cursor::new(bytes);
        let mut zip = zip::ZipArchive::new(reader).map_err(report_err!("I no fit open zip"))?;
        let mut file =
            zip.by_name(bin_name).map_err(|e| format!("I no see {bin_name} inside zip: {e}"))?;
        let mut out = File::create(out_path).map_err(report_err!("I no fit save file"))?;
        io::copy(&mut file, &mut out).map_err(report_err!("I no fit write file"))?;
        found = true;
    } else {
        let reader = Cursor::new(bytes);
        let xz = xz2::read::XzDecoder::new(reader);
        let mut archive = tar::Archive::new(xz);
        for entry in archive.entries().map_err(report_err!("I no fit read tar"))? {
            let mut entry = entry.map_err(report_err!("I no fit read tar entry"))?;
            let path = entry.path().map_err(report_err!("I no fit get tar path"))?;
            if path.file_name().map(|n| n == bin_name).unwrap_or(false) {
                let mut out = File::create(out_path).map_err(report_err!("I no fit save file"))?;
                io::copy(&mut entry, &mut out).map_err(report_err!("I no fit write file"))?;
                #[cfg(unix)]
                {
                    use std::os::unix::fs::PermissionsExt;
                    let mut perms = fs::metadata(out_path)
                        .map_err(report_err!("I no fit get permissions"))?
                        .permissions();
                    perms.set_mode(0o755);
                    fs::set_permissions(out_path, perms)
                        .map_err(report_err!("I no fit set permissions"))?;
                }
                found = true;
                break;
            }
        }
    }
    if !found {
        // Fail if the expected binary is not present, to avoid silent or partial installs.
        return Err(format!("I no see {bin_name} inside archive."));
    }
    Ok(())
}

fn naijaup_dir() -> PathBuf {
    dirs::home_dir().expect("I no fit find your home directory").join(".naijaup")
}

fn versions_dir() -> PathBuf {
    naijaup_dir().join("versions")
}

fn config_file() -> PathBuf {
    naijaup_dir().join("config.toml")
}

fn find_toolchain_version() -> Option<String> {
    // Project-local toolchain override is checked first to allow per-project version pinning, which is critical for reproducible builds.
    if let Some(ver) = read_trimmed_file(Path::new(".naijascript-toolchain")) {
        return Some(normalize_version(&ver));
    }
    // If no project override, fallback to user config file for the default version. This ensures a global default is always available if no local override is set.
    if let Some(cfg_str) = read_trimmed_file(&config_file()) {
        for line in cfg_str.lines() {
            if let Some(rest) = line.strip_prefix("default = ") {
                let rest = rest.trim_matches(['"', '\'', ' '].as_ref()).trim();
                if !rest.is_empty() {
                    return Some(normalize_version(rest));
                }
            }
        }
    }
    None
}

fn find_non_whitespace(bytes: &[u8]) -> Option<(usize, usize)> {
    let start = bytes.iter().position(|b| !b.is_ascii_whitespace())?;
    let end = bytes.iter().rposition(|b| !b.is_ascii_whitespace())? + 1;
    Some((start, end))
}

fn self_update(client: &reqwest::blocking::Client) -> Result<(), String> {
    let (os, arch) = get_platform();
    if os == "unknown" || arch == "unknown" {
        return Err("I no sabi your OS or architecture".to_string());
    }
    let tag_url = &format!("https://api.github.com/repos/{REPO}/releases/latest");
    let res = client.get(tag_url).send().map_err(report_err!("I no fit reach GitHub"))?;
    let status = res.status();
    if !status.is_success() {
        return Err(format!("Request fail with status: {status}. Try again later."));
    }
    let text = res.text().map_err(report_err!("I no fit read GitHub response"))?;
    let tag = extract_tag_names(&text)
        .into_iter()
        .next()
        .ok_or("I no fit process latest version for `naijaup` update.")?;
    let latest = tag.as_str();
    let current = env!("CARGO_PKG_VERSION");
    if latest == current {
        print_success!("Naijaup dey up-to-date (v{current})");
        return Ok(());
    }
    print_info!("I dey update naijaup from {current} to {latest}...");
    let ext = archive_ext();
    let bin_name = if os == "windows" { "naijaup.exe" } else { "naijaup" };
    let target = format!("{arch}-{os}");
    let archive_name = format!("{bin_name}-v{latest}-{target}.{ext}");
    let url = format!("https://github.com/{REPO}/releases/download/v{latest}/{archive_name}");
    print_info!("I dey download latest naijaup from: {url}");
    let res = client.get(&url).send().map_err(report_err!("I no fit download"))?;
    let status = res.status();
    if !status.is_success() {
        return Err(format!("Request fail with status: {status}. Try again later."));
    }
    let bytes = res.bytes().map_err(report_err!("I no fit read Github response"))?;
    let exe = std::env::current_exe().map_err(report_err!("I no fit find current exe"))?;
    let tmp_path = exe.with_extension("tmp");
    extract_bin_from_archive(&bytes, bin_name, &tmp_path, ext)?;
    fs::rename(&tmp_path, &exe).map_err(report_err!("I no fit update to latest version"))?;
    print_success!("I don update naijaup to version {latest}!");
    Ok(())
}

fn print_removal_result(result: std::io::Result<()>, path: &Path) {
    let path = path.display();
    match result {
        Ok(_) => print_success!("I don comot: {path}"),
        Err(e) => {
            print_warn!("I no fit comot {path} because of: {e}")
        }
    }
}

fn fetch_available_versions(client: &reqwest::blocking::Client) -> Result<Vec<String>, String> {
    let url = &format!("https://api.github.com/repos/{REPO}/releases");
    let res = client.get(url).send().map_err(report_err!("I no fit reach GitHub"))?;
    let status = res.status();
    if !status.is_success() {
        return Err(format!("Request fail with status: {status}. Try again later."));
    }
    let text = res.text().map_err(report_err!("I no fit read Github response"))?;
    let versions = extract_tag_names(&text);
    if versions.is_empty() {
        println!("I no see any version available online.");
    }
    Ok(versions)
}

fn self_uninstall(yes: bool) -> Result<(), String> {
    let home = naijaup_dir();
    let exe = std::env::current_exe().map_err(report_err!("I no fit find path"))?;
    if !yes {
        print!(
            "\x1b[33m[warn]\x1b[0m You sure say you wan uninstall NaijaScript? You no fit undo dis action. Type 'y' or 'yes' to confirm: "
        );
        io::stdout().flush().ok();
        let mut input = String::new();
        io::stdin().read_line(&mut input).ok();
        if !matches!(input.trim().to_lowercase().as_str(), "y" | "yes") {
            println!("I don cancel am.");
            return Ok(());
        }
    }
    print_removal_result(fs::remove_dir_all(&home), &home);
    // Drop dangling symlinks to `naija` binary BEFORE removing the executable.
    #[cfg(unix)]
    {
        if let Some(home) = dirs::home_dir() {
            let symlink_path = home.join(".local/bin/naija");
            let result = remove_if_exists(&symlink_path).map_err(std::io::Error::other);
            print_removal_result(result, &symlink_path);
        }
    }
    #[cfg(windows)]
    {
        if let Some(home) = dirs::home_dir() {
            let bin_dir = home.join(r".naijaup\bin");
            let symlink_path = bin_dir.join("naija.exe");
            for (result, path) in [
                (remove_if_exists(&symlink_path).map_err(std::io::Error::other), &symlink_path),
                (windows::remove_from_path(&bin_dir).map_err(std::io::Error::other), &bin_dir),
            ] {
                print_removal_result(result, path);
            }
        }
    }
    print_removal_result(fs::remove_file(&exe), &exe);
    print_success!("I don comot NaijaScript from your system. Try restart your shell.");
    Ok(())
}

// Create a symlink for the default `naija` binary to enable running `naija` from anywhere in the shell.
fn update_default_symlink(version: &str) -> Result<(), String> {
    let norm_version = normalize_version(version);
    let vdir = versions_dir().join(&norm_version);
    let bin_path = vdir.join(bin_name());
    if !bin_path.exists() {
        // Avoid creating a symlink to a missing binary, which would break the user's PATH and lead to hard-to-debug errors.
        return Err(format!("I no see binary for version {norm_version}."));
    }
    #[cfg(unix)]
    {
        use std::os::unix::fs::symlink;
        let home = dirs::home_dir().ok_or("I no fit find your home directory.")?;
        let local_bin = home.join(".local/bin");
        ensure_dir_exists(&local_bin)?;
        let symlink_path = local_bin.join("naija");
        remove_if_exists(&symlink_path)?;
        // Always recreate the symlink to ensure the default is up to date, even if the user previously set a different version.
        symlink(&bin_path, &symlink_path).map_err(report_err!("I no fit create symlink"))?;
        print_info!("I don set symlink: {} -> {}", symlink_path.display(), bin_path.display());
        print_info!("Make sure ~/.local/bin dey your PATH to use `naija` directly.");
    }
    #[cfg(windows)]
    {
        use std::os::windows::fs::symlink_file;
        let bin_dir = naijaup_dir().join("bin");
        ensure_dir_exists(&bin_dir)?;
        let symlink_path = bin_dir.join("naija.exe");
        remove_if_exists(&symlink_path)?;
        // On Windows, symlink creation may fail if not run as admin or on some filesystems; handle gracefully and inform the user.
        match symlink_file(&bin_path, &symlink_path) {
            Ok(_) => {
                print_info!(
                    "I don set symlink: {} -> {}",
                    symlink_path.display(),
                    bin_path.display()
                );
                match windows::add_to_path(&bin_dir) {
                    Ok(_) => print_info!(
                        "I don add {} to your PATH. Try restart your shell.",
                        bin_dir.display()
                    ),
                    Err(e) => print_warn!("I no fit add {} to your PATH: {}", bin_dir.display(), e),
                }
            }
            _ => {
                print_warn!(
                    "I no fit create symlink for {}. Try run `naijaup` as administrator or add {} to your PATH manually.",
                    symlink_path.display(),
                    bin_dir.display()
                );
            }
        }
    }
    Ok(())
}

#[test]
fn verify_cli() {
    use clap::CommandFactory;
    Cli::command().debug_assert();
}
