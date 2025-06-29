# Naijaup - The Toolchain Manager

Na the official tool wey you go use manage NaijaScript Interpreter.

## How to Install Am

### How to Install with Script (We Recommend Dis One)

- **For Linux/macOS:**
  1. Open your terminal.
  2. Run:
     ```sh
     curl --proto '=https' --tlsv1.2 -LsSf https://raw.githubusercontent.com/xosnrdev/naijascript/master/scripts/install.sh | sh
     ```
- **For Windows (PowerShell):**
  1. Open PowerShell.
  2. Run:
     ```powershell
     powershell -ExecutionPolicy ByPass -c "irm https://raw.githubusercontent.com/xosnrdev/naijascript/master/scripts/install.ps1 | iex"
     ```

This command go install `naijaup` for your computer inside `$HOME/.local/bin` (for Linux/macOS) or `%USERPROFILE%\.naijaup\bin` (for Windows).

## How to Use Am

Run `naijaup --help` make you see everything wey you fit do.

### Things Wey You Go Dey Do

#### 1. How to Install NaijaScript Version

- To install the latest version, run:
  ```sh
  naijaup install latest
  ```
- To install another version (e.g., 1.2.3), run:
  ```sh
  naijaup install 1.2.3
  ```

#### 2. See All the Versions Wey You Don Install

- To see all the versions wey you don install, run:
  ```sh
  naijaup list
  ```

#### 3. Choose Your Default Version

- To choose the version wey you wan dey use everytime, run:
  ```sh
  naijaup default <version>
  # For example:
  naijaup default latest
  ```

#### 4. See All the Versions Wey Dey Online

- To see all the versions wey you fit download, run:
  ```sh
  naijaup available
  ```

#### 5. How to Comot Version

- To comot any version, run:
  ```sh
  naijaup uninstall <version>
  ```

#### 6. Run Script with the Version Wey You Choose

- To run your script with any version wey you like, run:
  ```sh
  naijaup run <script.ns> [args...]
  ```

#### 7. How to Update `naijaup`

- To update `naijaup` to the latest version, run:
  ```sh
  naijaup self update
  ```

#### 8. How to Comot `naijaup` and Everything

- To comot `naijaup` and all NaijaScript data from your system, run:
  ```sh
  naijaup self uninstall --yes
  ```

## How E Take Arrange Things

- **Default Version:**
  - You fit set your default version with `naijaup default <version>`.
  - We dey save the default inside `~/.naijaup/config.toml`.
- **Project Version:**
  - If you wan use another version for your project, just put `.naijascript-toolchain` file inside your project folder.
- **Symlinks:**
  - For Linux/macOS, we dey create shortcut to the `naija` program inside `~/.local/bin/naija`.
  - For Windows, the shortcut dey inside `%USERPROFILE%\.naijaup\bin\naija.exe`.
  - Make sure say these folders dey your `PATH`.

## If Wahala Happen (Troubleshooting)

- **Command No Dey:**
  - Add the folder where you install am to your `PATH`.
  - Restart your terminal after you install am.
  - If the wahala still dey, abeg [open issue for us for GitHub](https://github.com/xosnrdev/naijascript/issues).
- **Permission Wahala:**
  - For Unix, you fit change the permission for the folder where you install am.
- **Shortcut No Wan Create (for Windows):**
  - Run your terminal as administrator, or add the folder to your `PATH` by yourself.
- **Network Wahala:**
  - Make sure say you get internet to download from GitHub.
- **Manual Installation:**
  - Go the [Releases page](https://github.com/xosnrdev/naijascript/releases/latest) to download am by yourself.

## All the Commands

### `install <version>`

E go install the NaijaScript version wey you choose.

### `list`

E go show you all the versions wey you don install.

### `default <version>`

E go set the version wey you go dey use everytime.

### `available`

E go show you all the versions wey you fit download.

### `uninstall <version>`

E go comot the version wey you choose from your system.

### `run <script> [args...]`

E go run your script with the version wey you choose.

### `self update`

E go update `naijaup` to the latest version.

### `self uninstall --yes`

E go comot `naijaup` and all NaijaScript data (you must gree to am).
