# Naijaup Toolchain Manager

The official toolchain manager for NaijaScript.

## Installation

### Install via Script (Recommended)

- **Linux/macOS:**
  1. Open your terminal.
  2. Run:
     ```sh
     curl --proto '=https' --tlsv1.2 -LsSf https://raw.githubusercontent.com/xosnrdev/naijascript/master/scripts/install.sh | sh
     ```
- **Windows (PowerShell):**
  1. Open PowerShell.
  2. Run:
     ```powershell
     powershell -ExecutionPolicy ByPass -c "irm https://raw.githubusercontent.com/xosnrdev/naijascript/master/scripts/install.ps1 | iex"
     ```

This command installs `naijaup` to your user-local bin directory (e.g., `$HOME/.local/bin` or `%USERPROFILE%\.naijascript\bin`).

## Usage

Run `naijaup --help` to see all available commands and options.

### Common Tasks

#### 1. Install a NaijaScript Interpreter Version

- To install the latest version, run:
  ```sh
  naijaup install latest
  ```
- To install a specific version (for example, 1.2.3), run:
  ```sh
  naijaup install 1.2.3
  ```

#### 2. List Installed Versions

- To list all installed versions, run:
  ```sh
  naijaup list
  ```

#### 3. Set the Default Interpreter Version

- To set the default version, run:
  ```sh
  naijaup default <version>
  # Example:
  naijaup default latest
  ```

#### 4. Show All Available Versions Online

- To see all available versions, run:
  ```sh
  naijaup available
  ```

#### 5. Uninstall a Version

- To remove a specific version, run:
  ```sh
  naijaup uninstall <version>
  ```

#### 6. Run a Script with a Specific Version

- To run a script with the selected interpreter version, run:
  ```sh
  naijaup run <script.ns> [args...]
  ```

#### 7. Update `naijaup` Itself

- To update `naijaup` to the latest version, run:
  ```sh
  naijaup self update
  ```

#### 8. Uninstall `naijaup` and All Data

- To remove `naijaup` and all NaijaScript data, run:
  ```sh
  naijaup self uninstall --yes
  ```

## Configuration and Environment

- **Default Version:**
  - Set the default version with `naijaup default <version>`.
  - The default is stored in `~/.naijaup/config.toml`.
- **Project-local Version:**
  - Place a `.naijascript-toolchain` file in your project directory to override the default version.
- **Symlinks:**
  - On Linux/macOS, a symlink to the default `naija` binary is created in `~/.local/bin/naija`.
  - On Windows, a symlink is created in `%USERPROFILE%\.naijaup\bin\naija.exe`.
  - Make sure these directories are in your `PATH`.

## Troubleshooting

- **Command Not Found:**
  - Add your install directory to your `PATH`.
  - Restart your terminal after installation.
  - If you continue to have issues, please [open an issue on GitHub](https://github.com/xosnrdev/naijascript/issues).
- **Permission Issues:**
  - On Unix, adjust permissions for the install directory if needed.
- **Symlink Creation Fails (Windows):**
  - Run your terminal as administrator, or manually add the binary directory to your `PATH`.
- **Network Issues:**
  - Ensure you have internet access to fetch releases from GitHub.
- **Manual Installation:**
  - Visit the [Releases page](https://github.com/xosnrdev/naijascript/releases/latest).

## Command Reference

### `install <version>`

Install a specific version of the NaijaScript interpreter.

### `list`

Show all installed versions.

### `default <version>`

Set the default interpreter version.

### `available`

Show all available versions online.

### `uninstall <version>`

Remove a specific version from your system.

### `run <script> [args...]`

Run a script with the selected interpreter version.

### `self update`

Update `naijaup` to the latest version.

### `self uninstall --yes`

Uninstall `naijaup` and all NaijaScript data (requires confirmation).
