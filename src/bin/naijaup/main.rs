use std::borrow::Cow;
use std::fs;
use std::io::{self, Write};
use std::path::{Path, PathBuf};
use std::process::Command;

use clap::{Parser, Subcommand};
use clap_cargo::style::CLAP_STYLING;

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

fn run() -> Result<(), String> {
    let cli = Cli::parse();
    match &cli.command {
        Commands::Install { version } => {
            let vdir = versions_dir().join(version);
            if vdir.exists() {
                // Avoid redundant downloads; user already has this version.
                println!("Version {version} don already dey your system.");
            } else {
                fs::create_dir_all(&vdir).map_err(|e| e.to_string())?;
                download_and_install(version, &vdir)?;
                println!("I don install NaijaScript version: {version}");
            }
        }
        Commands::List => list_installed_versions(),
        Commands::Default { version } => {
            let vdir = versions_dir().join(version);
            if !vdir.exists() {
                // Prevent user from setting a default to a non-installed version.
                return Err(format!(
                    "Oga, you never install version {version} yet. Run 'naijaup install {version}' first."
                ));
            }
            let conf = config_file();
            if let Some(parent) = conf.parent() {
                fs::create_dir_all(parent).map_err(|e| e.to_string())?;
            }
            // Overwrite config with new default; only one default is supported.
            fs::write(&conf, format!("default = \"{version}\"\n")).map_err(|e| e.to_string())?;
            println!("I don set {version} as your default NaijaScript version");
        }
        Commands::Run { script, args } => {
            // Always resolve the toolchain version before running.
            let version = find_toolchain_version().ok_or_else(||
                "Oga, I no sabi which NaijaScript version to use. Set default or add .naijascript-toolchain.".to_string()
            )?;
            let bin = versions_dir().join(&*version).join(if cfg!(windows) {
                "naija.exe"
            } else {
                "naija"
            });
            if !bin.exists() {
                // User may have deleted the version manually; prompt to reinstall.
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
                // Nothing to do if version is not installed.
                println!("Oga, version {version} no dey your system.");
                return Ok(());
            }
            let default_version = find_toolchain_version();
            if let Some(def) = default_version
                && def == *version
            {
                // Prevent user from uninstalling the default version, which would break 'run'.
                println!(
                    "You wan uninstall your default version ({version})? Set another default first."
                );
                return Ok(());
            }
            fs::remove_dir_all(&vdir).map_err(|e| e.to_string())?;
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
    let url = format!(
        "https://github.com/xosnrdev/naijascript/releases/download/v{version}/naija-{os}-{arch}"
    );
    let bin_path = vdir.join(if os == "windows" { "naija.exe" } else { "naija" });
    println!("I dey download NaijaScript from: {url}");
    let resp =
        reqwest::blocking::get(&url).map_err(|e| format!("Wahala! I no fit download: {e}"))?;
    if !resp.status().is_success() {
        return Err(format!(
            "Wahala! I no see version {version} for online (status: {})",
            resp.status()
        ));
    }
    let bytes = resp.bytes().map_err(|e| format!("Wahala! I no fit read download: {e}"))?;
    fs::write(&bin_path, &bytes).map_err(|e| format!("Wahala! I no fit save file: {e}"))?;
    #[cfg(unix)]
    {
        use std::os::unix::fs::PermissionsExt;
        let mut perms = fs::metadata(&bin_path).map_err(|e| e.to_string())?.permissions();
        perms.set_mode(0o755);
        fs::set_permissions(&bin_path, perms).map_err(|e| e.to_string())?;
    }
    Ok(())
}

fn naijaup_dir() -> PathBuf {
    dirs::home_dir().unwrap_or_else(|| PathBuf::from(".")).join(".naijaup")
}

fn versions_dir() -> PathBuf {
    naijaup_dir().join("versions")
}

fn config_file() -> PathBuf {
    naijaup_dir().join("config.toml")
}

// We use Cow here to allow for both borrowed and owned version strings, avoiding leaks and unnecessary allocations.
fn find_toolchain_version() -> Option<Cow<'static, str>> {
    // 1. Prefer project-local toolchain override if present.
    if let Ok(ver) = std::fs::read(".naijascript-toolchain")
        && let Some((start, end)) = find_non_whitespace(&ver)
        && let Ok(s) = std::str::from_utf8(&ver[start..end])
    {
        // Always own the string to ensure 'static lifetime for downstream use.
        return Some(Cow::Owned(s.to_string()));
    }
    // 2. Fallback to user config file for default version.
    if let Ok(cfg) = std::fs::read(config_file())
        && let Ok(cfg_str) = std::str::from_utf8(&cfg)
    {
        for line in cfg_str.lines() {
            if let Some(rest) = line.strip_prefix("default = ") {
                let rest = rest.trim_matches(['"', '\'', ' '].as_ref());
                if !rest.is_empty() {
                    // Config may contain whitespace or quotes; we sanitize here.
                    return Some(Cow::Owned(rest.to_string()));
                }
            }
        }
    }
    // No version found; caller must handle this case.
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
    let url = format!(
        "https://github.com/xosnrdev/naijascript/releases/latest/download/naijaup-{os}-{arch}"
    );
    println!("I dey download latest naijaup from: {url}");
    let resp =
        reqwest::blocking::get(&url).map_err(|e| format!("Wahala! I no fit download: {e}"))?;
    if !resp.status().is_success() {
        return Err(format!(
            "Wahala! I no see latest naijaup for online (status: {})",
            resp.status()
        ));
    }
    let bytes = resp.bytes().map_err(|e| format!("Wahala! I no fit read download: {e}"))?;
    let exe = std::env::current_exe().map_err(|e| format!("Wahala! I no fit find myself: {e}"))?;
    let tmp_path = exe.with_extension("tmp");
    fs::write(&tmp_path, &bytes).map_err(|e| format!("Wahala! I no fit save update: {e}"))?;
    #[cfg(unix)]
    {
        use std::os::unix::fs::PermissionsExt;
        let mut perms = fs::metadata(&tmp_path).map_err(|e| e.to_string())?.permissions();
        perms.set_mode(0o755);
        fs::set_permissions(&tmp_path, perms).map_err(|e| e.to_string())?;
    }
    fs::rename(&tmp_path, &exe).map_err(|e| format!("Wahala! I no fit update myself: {e}"))?;
    println!("Naijaup don update finish! Try run am again.");
    Ok(())
}

fn print_removal_result(result: std::io::Result<()>, path: &Path) {
    match result {
        Ok(_) => println!("I don comot: {}", path.display()),
        Err(_) => {
            println!("Omo, I no fit comot {} (abeg try remove am by yourself)", path.display())
        }
    }
}

fn self_uninstall(yes: bool) -> Result<(), String> {
    let home = naijaup_dir();
    let exe = match std::env::current_exe() {
        Ok(path) => path,
        Err(e) => {
            eprintln!("Oga, I no fit find where naijaup dey run from: {e}");
            PathBuf::from("naijaup")
        }
    };
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
    // We parse the GitHub releases API response as plain text for performance and minimal dependencies.
    // This is fragile if GitHub changes their API, but avoids pulling in serde_json for a single field.
    let url = "https://api.github.com/repos/xosnrdev/naijascript/releases";
    let client = reqwest::blocking::Client::new();
    let resp = client
        .get(url)
        .header("User-Agent", "naijaup")
        .send()
        .map_err(|e| format!("Wahala! I no fit reach GitHub: {e}"))?;
    if !resp.status().is_success() {
        return Err(format!("Wahala! GitHub no gree (status: {})", resp.status()));
    }
    let text = resp.text().map_err(|e| format!("Wahala! I no fit read GitHub response: {e}"))?;
    let versions = text
        .lines()
        .filter_map(|line| {
            // We look for lines containing the tag_name field, which is how GitHub encodes release versions.
            let tag = line.find("\"tag_name\": ")?;
            let start = line[tag..].find('"')? + tag + 1;
            let end = line[start..].find('"')? + start;
            let v = &line[start..end];
            // Remove leading 'v' for consistency with local version naming.
            Some(v.trim_start_matches('v').to_string())
        })
        .collect();
    Ok(versions)
}

fn list_installed_versions() {
    let vdir = versions_dir();
    println!("See all the versions wey you don install:");
    match fs::read_dir(&vdir) {
        Ok(entries) => entries
            .flatten()
            .for_each(|entry| println!("  - {}", entry.file_name().to_string_lossy())),
        Err(_) => println!("  (No version dey yet)"),
    }
}

#[test]
fn verify_cli() {
    use clap::CommandFactory;
    Cli::command().debug_assert();
}
