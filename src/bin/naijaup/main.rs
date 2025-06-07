use std::fs;
use std::fs::File;
use std::io::{self, Cursor, Write};
use std::path::{Path, PathBuf};
use std::process::Command;

use clap::{Parser, Subcommand};
use clap_cargo::style::CLAP_STYLING;

const REPO: &str = "xosnrdev/naijascript";

#[derive(Parser)]
#[command(
    version,
    about = "Toolchain manager for NaijaScript",
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
    SelfCmd {
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

fn main() {
    if let Err(e) = run() {
        eprintln!("{e}");
        std::process::exit(1);
    }
}

macro_rules! report_err {
    ($msg:expr) => {
        |e| format!(concat!($msg, ": {}"), e)
    };
}

fn run() -> Result<(), String> {
    let cli = Cli::parse();
    match &cli.command {
        Commands::Install { version } => {
            let vdir = versions_dir().join(version);
            if vdir.exists() {
                // Avoids redundant downloads and ensures idempotency for repeated installs.
                println!("Version {version} don already dey your system.");
            } else {
                fs::create_dir_all(&vdir)
                    .map_err(report_err!("Wahala! I no fit create directory"))?;
                download_and_install(version, &vdir)?;
                println!("I don install NaijaScript version: {version}");
            }
        }
        Commands::List => list_installed_versions(),
        Commands::Default { version } => {
            let vdir = versions_dir().join(version);
            if !vdir.exists() {
                // Prevents setting a default to a version that isn't installed, which would break determinism and could cause confusing errors later.
                return Err(format!(
                    "Oga, you never install version {version} yet. Run 'naijaup install {version}' first."
                ));
            }
            let conf = config_file();
            if let Some(parent) = conf.parent() {
                fs::create_dir_all(parent)
                    .map_err(report_err!("Wahala! I no fit create directory"))?;
            }
            // We overwrite the config here because only one default version is ever supported
            // This avoids ambiguity and ensures deterministic toolchain selection for the user.
            fs::write(&conf, format!("default = \"{version}\"\n"))
                .map_err(report_err!("Wahala! I no fit write config"))?;
            println!("I don set {version} as your default NaijaScript version");
        }
        Commands::Run { script, args } => {
            // Resolving the toolchain version here ensures deterministic execution and prevents accidental use of the wrong version, especially in CI or scripting scenarios where implicit defaults could cause subtle, hard-to-debug issues.
            let version = find_toolchain_version().ok_or_else(||
                "Oga, I no sabi which NaijaScript version to use. Set default or add .naijascript-toolchain.".to_string()
            )?;
            let bin = versions_dir().join(&version).join(if cfg!(windows) {
                "naija.exe"
            } else {
                "naija"
            });
            if !bin.exists() {
                // This handles the edge case where a user manually deletes a version directory, ensuring we don't try to run a missing binary and instead provide a clear recovery path.
                return Err(format!(
                    "Wahala! I no see NaijaScript version {version} for your system. Run 'naijaup install {version}' first."
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
                Err(e) => return Err(format!("Wahala! I no fit run your script: {e}")),
            }
        }
        Commands::SelfCmd { action } => match action {
            SelfAction::Update => self_update()?,
            SelfAction::Uninstall { yes } => self_uninstall(*yes)?,
        },
        Commands::Uninstall { version } => {
            let vdir = versions_dir().join(version);
            if !vdir.exists() {
                // Early return here ensures idempotency: uninstalling a non-existent version is a no-op, not an error, which matches user expectations and avoids unnecessary failure states.
                println!("Oga, version {version} no dey your system.");
                return Ok(());
            }
            let default_version = find_toolchain_version();
            if let Some(def) = default_version
                && def == *version
            {
                // Prevents removal of the default version to avoid breaking the user's workflow and ensures 'run' always has a valid toolchain.
                println!(
                    "You wan uninstall your default version ({version})? Set another default first."
                );
                return Ok(());
            }
            fs::remove_dir_all(&vdir).map_err(report_err!("Wahala! I no fit remove directory"))?;
            println!("I don comot NaijaScript version: {version}");
        }
        Commands::Available => {
            println!("I dey fetch all NaijaScript versions wey dey online...");
            let versions = fetch_available_versions()?;
            if versions.is_empty() {
                println!("I no see any version for online.");
            } else {
                println!("See all the versions wey dey online:");
                for v in versions {
                    println!("  - {v}");
                }
            }
        }
    }
    Ok(())
}

fn get_platform() -> (&'static str, &'static str) {
    // Returns platform/arch as used in release artifact naming. This must match the release workflow and bootstrap scripts for cross-platform correctness.
    let os = if cfg!(target_os = "macos") {
        "macos"
    } else if cfg!(target_os = "linux") {
        "linux"
    } else if cfg!(target_os = "windows") {
        "windows"
    } else {
        "unknown"
    };
    let arch = if cfg!(target_arch = "x86_64") {
        "x86_64"
    } else if cfg!(target_arch = "aarch64") {
        "aarch64"
    } else {
        "unknown"
    };
    (os, arch)
}

fn download_and_install(version: &str, vdir: &Path) -> Result<(), String> {
    let (os, arch) = get_platform();
    if os == "unknown" || arch == "unknown" {
        return Err("I no sabi your OS or architecture".to_string());
    }
    let version_tag =
        if version.starts_with('v') { version.to_string() } else { format!("v{version}") };
    let ext = if os == "windows" { "zip" } else { "tar.gz" };
    let bin_name = if os == "windows" { "naija.exe" } else { "naija" };
    let archive_name = format!("naija-{os}-{arch}-{version_tag}.{ext}");
    let url = format!("https://github.com/{REPO}/releases/download/{version_tag}/{archive_name}");
    println!("I dey download NaijaScript from: {url}");
    let resp = reqwest::blocking::get(&url).map_err(report_err!("Wahala! I no fit download"))?;
    if !resp.status().is_success() {
        return Err(format!(
            "Wahala! I no see version {version} for online (status: {})",
            resp.status()
        ));
    }
    let bytes = resp.bytes().map_err(report_err!("Wahala! I no fit read download"))?;
    let out_path = vdir.join(bin_name);
    extract_bin_from_archive(&bytes, bin_name, &out_path, ext)?;
    Ok(())
}

// Helper to extract a named binary from an archive (zip or tar.gz) and write to out_path.
// Only used because std does not provide tar/zip/gzip extraction; this is the minimal, robust, cross-platform approach.
fn extract_bin_from_archive(
    bytes: &[u8],
    bin_name: &str,
    out_path: &Path,
    ext: &str,
) -> Result<(), String> {
    let mut found = false;
    if ext == "zip" {
        let reader = Cursor::new(bytes);
        let mut zip =
            zip::ZipArchive::new(reader).map_err(report_err!("Wahala! I no fit open zip"))?;
        let mut file = zip
            .by_name(bin_name)
            .map_err(|e| format!("Wahala! I no see {bin_name} inside zip: {e}"))?;
        let mut out = File::create(out_path).map_err(report_err!("Wahala! I no fit save file"))?;
        io::copy(&mut file, &mut out).map_err(report_err!("Wahala! I no fit write file"))?;
        found = true;
    } else {
        let reader = Cursor::new(bytes);
        let mut archive = tar::Archive::new(flate2::read::GzDecoder::new(reader));
        for entry in archive.entries().map_err(report_err!("Wahala! I no fit read tar"))? {
            let mut entry = entry.map_err(report_err!("Wahala! I no fit read tar entry"))?;
            let path = entry.path().map_err(report_err!("Wahala! I no fit get tar path"))?;
            if path.file_name().map(|n| n == bin_name).unwrap_or(false) {
                let mut out =
                    File::create(out_path).map_err(report_err!("Wahala! I no fit save file"))?;
                io::copy(&mut entry, &mut out)
                    .map_err(report_err!("Wahala! I no fit write file"))?;
                #[cfg(unix)]
                {
                    use std::os::unix::fs::PermissionsExt;
                    let mut perms = fs::metadata(out_path)
                        .map_err(report_err!("Wahala! I no fit get permissions"))?
                        .permissions();
                    perms.set_mode(0o755);
                    fs::set_permissions(out_path, perms)
                        .map_err(report_err!("Wahala! I no fit set permissions"))?;
                }
                found = true;
                break;
            }
        }
    }
    if !found {
        return Err(format!("Wahala! I no see {bin_name} inside archive."));
    }
    Ok(())
}

fn naijaup_dir() -> PathBuf {
    // Home directory is required for correct operation; error if missing to avoid silent misplacement of toolchain data.
    dirs::home_dir()
        .expect("Oga, I no fit find your home directory. Naijaup need am to work.")
        .join(".naijaup")
}

fn versions_dir() -> PathBuf {
    naijaup_dir().join("versions")
}

fn config_file() -> PathBuf {
    naijaup_dir().join("config.toml")
}

// Returns the toolchain version as an owned String. Always allocates because config/toolchain files are read from disk and not kept alive.
fn find_toolchain_version() -> Option<String> {
    // Project-local toolchain override is checked first to allow per-project version pinning, which is critical for reproducible builds and team consistency.
    if let Ok(ver) = std::fs::read(".naijascript-toolchain")
        && let Some((start, end)) = find_non_whitespace(&ver)
        && let Ok(s) = std::str::from_utf8(&ver[start..end])
    {
        return Some(s.trim().to_string());
    }
    // If no project override, fallback to user config file for the default version. This ensures a global default is always available if no local override is set.
    if let Ok(cfg) = std::fs::read(config_file())
        && let Ok(cfg_str) = std::str::from_utf8(&cfg)
    {
        for line in cfg_str.lines() {
            if let Some(rest) = line.strip_prefix("default = ") {
                let rest = rest.trim_matches(['"', '\'', ' '].as_ref()).trim();
                if !rest.is_empty() {
                    return Some(rest.to_string());
                }
            }
        }
    }
    None
}

// Returns the first and last non-whitespace byte indices in a buffer, or None if all whitespace.
// Used to robustly trim config/toolchain files, which may have trailing newlines or spaces.
fn find_non_whitespace(bytes: &[u8]) -> Option<(usize, usize)> {
    let start = bytes.iter().position(|b| !b.is_ascii_whitespace())?;
    let end = bytes.iter().rposition(|b| !b.is_ascii_whitespace())? + 1;
    Some((start, end))
}

fn self_update() -> Result<(), String> {
    let (os, arch) = get_platform();
    if os == "unknown" || arch == "unknown" {
        return Err("I no sabi your OS or architecture".to_string());
    }
    let tag_url = &format!("https://api.github.com/repos/{REPO}/releases/latest");
    let client = reqwest::blocking::Client::new();
    let resp = client
        .get(tag_url)
        .header("User-Agent", "naijaup")
        .send()
        .map_err(report_err!("Wahala! I no fit reach GitHub"))?;
    if !resp.status().is_success() {
        return Err(format!("Wahala! GitHub no gree (status: {})", resp.status()));
    }
    let text = resp.text().map_err(report_err!("Wahala! I no fit read GitHub response"))?;
    let tag = text
        .lines()
        .find_map(|line| {
            let tag = line.find("\"tag_name\": ")?;
            let start = line[tag..].find('"')? + tag + 1;
            let end = line[start..].find('"')? + start;
            Some(line[start..end].to_string())
        })
        .ok_or_else(|| {
            "Wahala! I no fit find latest version tag. Maybe GitHub API don change.".to_string()
        })?;
    let ext = if os == "windows" { "zip" } else { "tar.gz" };
    let bin_name = if os == "windows" { "naijaup.exe" } else { "naijaup" };
    let archive_name = format!("naijaup-{os}-{arch}-{tag}.{ext}");
    let url = format!("https://github.com/{REPO}/releases/download/{tag}/{archive_name}");
    println!("I dey download latest naijaup from: {url}");
    let resp = reqwest::blocking::get(&url).map_err(report_err!("Wahala! I no fit download"))?;
    if !resp.status().is_success() {
        return Err(format!(
            "Wahala! I no see latest naijaup for online (status: {})",
            resp.status()
        ));
    }
    let bytes = resp.bytes().map_err(report_err!("Wahala! I no fit read download"))?;
    let exe = std::env::current_exe().map_err(report_err!("Wahala! I no fit find myself"))?;
    let tmp_path = exe.with_extension("tmp");
    extract_bin_from_archive(&bytes, bin_name, &tmp_path, ext)?;
    fs::rename(&tmp_path, &exe).map_err(report_err!("Wahala! I no fit update myself"))?;
    println!("Naijaup don update finish! Try run am again.");
    Ok(())
}

fn print_removal_result(result: std::io::Result<()>, path: &Path) {
    match result {
        Ok(_) => println!("I don comot: {}", path.display()),
        Err(e) => {
            // This message highlights the edge case where file or directory removal fails, which may require manual intervention due to permissions or OS locks.
            println!("Omo, I no fit comot {} (abeg try remove am by yourself): {e}", path.display())
        }
    }
}

fn self_uninstall(yes: bool) -> Result<(), String> {
    let home = naijaup_dir();
    let exe = std::env::current_exe()
        .map_err(|e| format!("Oga, I no fit find where naijaup dey run from: {e}"))?;
    if !yes {
        print!(
            "Oga, this one go comot ALL NaijaScript versions, config, and naijaup itself. You sure? [y/N]: "
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
    print_removal_result(fs::remove_file(&exe), &exe);
    println!("E don finish! Naijaup and all NaijaScript don comot.");
    Ok(())
}

fn fetch_available_versions() -> Result<Vec<String>, String> {
    // Parses the GitHub releases API response as plain text for performance and minimal dependencies. This is fragile if GitHub changes their API, but avoids pulling in serde_json for a single field, which would increase compile times and binary size for little gain.
    let url = &format!("https://api.github.com/repos/{REPO}/releases");
    let client = reqwest::blocking::Client::new();
    let resp = client
        .get(url)
        .header("User-Agent", "naijaup")
        .send()
        .map_err(report_err!("Wahala! I no fit reach GitHub"))?;
    if !resp.status().is_success() {
        return Err(format!("Wahala! GitHub no gree (status: {})", resp.status()));
    }
    let text = resp.text().map_err(report_err!("Wahala! I no fit read GitHub response"))?;
    let versions = text
        .lines()
        .filter_map(|line| {
            let tag = line.find("\"tag_name\": ")?;
            let start = line[tag..].find('"')? + tag + 1;
            let end = line[start..].find('"')? + start;
            let v = &line[start..end];
            Some(v.trim_start_matches('v').to_string())
        })
        .collect::<Vec<_>>();
    if versions.is_empty() {
        // This can happen if GitHub changes their API or there are no releases. Warn user.
        println!(
            "Oga, I no fit find any version for GitHub releases. Maybe dem change API or no release dey."
        );
    }
    Ok(versions)
}

fn list_installed_versions() {
    let vdir = versions_dir();
    match fs::read_dir(&vdir) {
        Ok(entries) => {
            let versions: Vec<_> = entries.flatten().collect();
            if versions.is_empty() {
                println!("Oga, you never install any version yet.");
            } else {
                println!("See all the versions wey you don install:");
                for entry in versions {
                    println!("  - {}", entry.file_name().to_string_lossy());
                }
            }
        }
        Err(_) => println!("Oga, you never install any version yet."),
    }
}

#[test]
fn verify_cli() {
    use clap::CommandFactory;
    Cli::command().debug_assert();
}
