# NaijaScript

A scripting language for learning, automation, and fun with Naija (Nigerian) lingo.

## Quick Start

1. **Install the Toolchain Manager (`naijaup`)**

   - Linux/macOS:

     ```sh
     curl --proto '=https' --tlsv1.2 -LsSf https://raw.githubusercontent.com/xosnrdev/naijascript/master/scripts/install.sh | sh
     ```

   - Windows (PowerShell):

     ```powershell
     powershell -ExecutionPolicy ByPass -c "irm https://raw.githubusercontent.com/xosnrdev/naijascript/master/scripts/install.ps1 | iex"
     ```

- After installation, ensure `$HOME/.local/bin` (Linux/macOS) or `%USERPROFILE%\.naijascript\bin` (Windows) is in your `PATH`. Restart your terminal if needed.

2. **Set Up the Interpreter (`naija`)**

   - Install the latest version of the interpreter:

     ```sh
     naijaup install latest
     ```

   - Set the installed version as the default:
     ```sh
     naijaup default latest
     ```

## Documentation

1. **Set Up and Manage the Toolchain**

   - See [Toolchain Manager (`naijaup`)](./docs/naijaup.md) for installation steps, version management, troubleshooting, and advanced usage.

2. **Run and Explore NaijaScript**

   - Visit [Interpreter (`naija`)](./docs/naija.md) for instructions on running scripts, using the REPL, evaluating code, piping, CLI options, and troubleshooting.

3. **Understand the Language**

   - Read [Language Design](./docs/DESIGN.md) for insights into the philosophy and architecture behind NaijaScript.

4. **Learn by Example**
   - Browse the [examples/](./examples) directory for syntax samples and practical usage.

## License

This project is licensed under the [MIT License](./LICENSE).
