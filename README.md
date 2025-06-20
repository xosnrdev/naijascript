# NaijaScript

A scripting language for learning, automation, and fun with Naija (Nigerian) lingo.

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

3. **Explore Documentation and Examples**
   - [Toolchain Manager (`naijaup`)](./docs/naijaup.md): Installation, version management, troubleshooting, advanced usage.
   - [Interpreter (`naija`)](./docs/naija.md): Running scripts, REPL, evaluating code, piping, CLI options, troubleshooting.
   - [Language Design](./docs/DESIGN.md): Philosophy and architecture behind NaijaScript.
   - [Examples](./examples): Syntax samples and practical usage.

## License

This project is licensed under the [MIT License](./LICENSE).
