# Naijaup – The Toolchain Manager

`naijaup` is the official utility for installing and managing NaijaScript interpreters.

## Installation

### Recommended Script Installation

- **Linux or macOS**
  1. Open a terminal window.
  2. Run:
     ```sh
     curl --proto '=https' --tlsv1.2 -LsSf https://raw.githubusercontent.com/xosnrdev/naijascript/master/scripts/install.sh | sh
     ```
- **Windows (PowerShell)**
  1. Launch PowerShell.
  2. Run:
     ```powershell
     powershell -ExecutionPolicy ByPass -c "irm https://raw.githubusercontent.com/xosnrdev/naijascript/master/scripts/install.ps1 | iex"
     ```

The installer places `naijaup` in `$HOME/.local/bin` (Linux/macOS) or `%USERPROFILE%\.naijaup\bin` (Windows).

## Using Naijaup

Run `naijaup --help` to review every available command.

### Core Tasks

#### 1. Install a NaijaScript Version

- Install the latest release:
  ```sh
  naijaup install latest
  ```
- Install a specific version (for example, `1.2.3`):
  ```sh
  naijaup install 1.2.3
  ```

#### 2. List Installed Versions

- Display all locally installed interpreters:
  ```sh
  naijaup list
  ```

#### 3. Choose the Default Version

- Set the interpreter used by default:
  ```sh
  naijaup default <version>
  # Example
  naijaup default latest
  ```

#### 4. Discover Available Versions

- View every release hosted online:
  ```sh
  naijaup available
  ```

#### 5. Uninstall a Version

- Remove an installed interpreter:
  ```sh
  naijaup uninstall <version>
  ```

#### 6. Run a Script with a Chosen Version

- Execute a script using any installed interpreter:
  ```sh
  naijaup run <script.ns> [args...]
  ```

#### 7. Update Naijaup

- Upgrade the toolchain manager itself:
  ```sh
  naijaup self update
  ```

#### 8. Uninstall Naijaup

- Remove `naijaup` and all installed interpreters:
  ```sh
  naijaup self uninstall --yes
  ```

## How Naijaup Manages Versions

- **Default Version**
  - Configure the default interpreter using `naijaup default <version>`.
  - The selection is stored in `~/.naijaup/config.toml`.
- **Per-Project Version**
  - Place a `.naijascript-toolchain` file in a project directory to override the default.
- **Symlinks**
  - On Linux/macOS, a `naija` symlink is created in `~/.local/bin/naija`.
  - On Windows, the shim lives in `%USERPROFILE%\.naijaup\bin\naija.exe`.
  - Ensure these directories appear in your `PATH`.

## Troubleshooting

- **`naijaup` command not found**
  - Add the installer directory to your `PATH` and restart the terminal session.
  - Still blocked? [Open an issue on GitHub](https://github.com/xosnrdev/naijascript/issues).
- **Permission errors on Unix-like systems**
  - Adjust file permissions for the installation directory or rerun the installer with elevated rights.
- **Windows shim is missing**
  - Run the terminal as Administrator, or manually add `%USERPROFILE%\.naijaup\bin` to your `PATH`.
- **Network timeouts or download failures**
  - Confirm you have a stable internet connection; the installer fetches artifacts from GitHub.
- **Manual installation**
  - Download binaries from the [latest release](https://github.com/xosnrdev/naijascript/releases/latest) and follow the included instructions.

## Command Reference

### `install <version>`

Install the specified NaijaScript interpreter version.

### `list`

Show every version currently installed.

### `default <version>`

Set the interpreter invoked by default.

### `available`

List all versions published upstream.

### `uninstall <version>`

Remove the chosen interpreter from your system.

### `run <script> [args...]`

Execute a script with a specific interpreter version.

### `self update`

Update `naijaup` to the newest release.

### `self uninstall --yes`

Uninstall `naijaup` and delete all NaijaScript data (confirmation required).
