# NaijaScript

A scripting language for learning, automation, and fun with Naija (Nigerian) lingo.

## Quick Start Instructions

Follow these steps to get up and running with NaijaScript:

### 1. Check System Requirements

- Make sure you are using a supported OS:
  - Linux (x86_64, aarch64)
  - macOS (x86_64, aarch64)
  - Windows (x86_64, aarch64)
- Ensure you have the following dependencies:
  - POSIX: `curl`, `tar`, `shasum` or `sha256sum`
  - Windows: PowerShell 5.0+
- Confirm your OS version is at least:
  - macOS 10.13+, Ubuntu 18.04+, Windows 10+

### 2. Install the Toolchain Manager (`naijaup`)

- **On Linux/macOS:**
  1. Open your terminal.
  2. Run:
     ```sh
     curl --proto '=https' --tlsv1.2 -LsSf https://raw.githubusercontent.com/xosnrdev/naijascript/master/scripts/install.sh | sh
     ```
- **On Windows:**
  1. Open PowerShell.
  2. Run:
     ```powershell
     powershell -ExecutionPolicy ByPass -c "irm https://raw.githubusercontent.com/xosnrdev/naijascript/master/scripts/install.ps1 | iex"
     ```
- After installation, ensure `$HOME/.local/bin` (Linux/macOS) or `%USERPROFILE%\.naijascript\bin` (Windows) is in your `PATH`. Restart your terminal if needed.

### 3. Install the NaijaScript Interpreter

- To install the latest interpreter, run:
  ```sh
  naijaup install latest
  ```
- To set the latest version as your default, run:
  ```sh
  naijaup default latest
  ```
- To list installed versions, run:
  ```sh
  naijaup list
  ```
- To see all available versions online, run:
  ```sh
  naijaup available
  ```
- To uninstall a version, run:
  ```sh
  naijaup uninstall <version>
  ```
- To update `naijaup` itself, run:
  ```sh
  naijaup self update
  ```
- To uninstall `naijaup` and all NaijaScript data, run:
  ```sh
  naijaup self uninstall --yes
  ```

## How to Use NaijaScript

- To run a script, use:
  ```sh
  naija your_script.ns
  ```
- To start the interactive REPL, use:
  ```sh
  naija --interactive
  ```
- To evaluate a code snippet, use:
  ```sh
  naija --eval "make x get 5 shout ( x add 2 )"
  ```
- To pipe code from stdin, use:
  ```sh
  echo "make x get 5 shout ( x add 2 )" | naija -
  ```

## Troubleshooting

- **If `naijaup` or `naija` is not found:**
  - Make sure your install directory is in your `PATH`.
  - Restart your terminal.
- **To uninstall everything:**
  - Run: `naijaup self uninstall --yes`
- **To manually download binaries:**
  - Visit the [Releases page](https://github.com/xosnrdev/naijascript/releases/latest).

## Next Steps

- Explore the [examples/](./examples) directory for syntax examples.
- Read [docs/DESIGN.md](./docs/DESIGN.md) for technical and contributor details.

## License

This project is licensed under the MIT License.
