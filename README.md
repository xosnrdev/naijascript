# NaijaScript

[![platform](https://img.shields.io/badge/platform-linux%20%7C%20macOS%20%7C%20windows-lightgrey)](#)
[![Docs](https://img.shields.io/badge/docs-blue.svg?style=flat&logo=bookstack&logoColor=white)](https://xosnrdev.github.io/naijascript/)
[![Playground](https://img.shields.io/badge/playground-online-brightgreen?logo=google-chrome&logoColor=white)](https://naijascript-playground.pages.dev)

A scripting language interpreter inspired by the expressive linguistics of Naija (Nigerian Pidgin English).

> [!WARNING]
>
> This project is **experimental** and may be subjected to breaking changes.

## Getting Started

1. **Install the Toolchain Manager (`naijaup`)**

   - **Linux/macOS:**
     ```sh
     curl --proto '=https' --tlsv1.2 -LsSf https://raw.githubusercontent.com/xosnrdev/naijascript/master/scripts/install.sh | sh
     ```
   - **Windows (PowerShell):**
     ```powershell
     powershell -ExecutionPolicy ByPass -c "irm https://raw.githubusercontent.com/xosnrdev/naijascript/master/scripts/install.ps1 | iex"
     ```
   - After installation, ensure `$HOME/.local/bin` (Linux/macOS) or `%USERPROFILE%\.naijascript\bin` (Windows) is in your `PATH`. Restart your terminal if needed.

2. **Install and Set Up the Interpreter (`naija`)**

   - Install the latest version:
     ```sh
     naijaup install latest
     ```
   - Set the installed version as default:
     ```sh
     naijaup default latest
     ```

3. **Explore Documentation**
   - [Toolchain Manager (`naijaup`)](./docs/NAIJAUP.md): Installation, version management, troubleshooting, advanced usage.
   - [Interpreter (`naija`)](./docs/NAIJA.md): Running scripts, evaluating code, piping, CLI options, troubleshooting.
   - [Design Philosophy](./docs/DESIGN.md): Philosophy and architecture behind NaijaScript.
   - [Language Reference Manual](./docs/LRM.md): The official language specification.

## License

This project is licensed under the [MIT License](./LICENSE).
