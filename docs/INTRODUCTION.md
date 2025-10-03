# Welcome to NaijaScript

NaijaScript is a scripting language you can use to learn programming, automate everyday tasks, and experiment for fun. This guide walks you through everything you need to know—from the philosophy behind the language to the practical steps for running your first program.

Want to try it immediately? Visit the online [playground](https://naijascript-playground.pages.dev).

## How to Use This Book

We organized this book so you can move from the big picture to hands-on execution:

- **[Introduction](INTRODUCTION.md)**: You are here. Start with the high-level overview and collect the setup steps you need.
- **[Design Philosophy](DESIGN.md)**: Understand the rationale behind each language decision before you dive into the details.
- **[Language Reference Manual (LRM)](LRM.md)**: Consult the formal grammar, value types, and control flow rules as you build programs.
- **[Interpreter (`naija`)](NAIJA.md)**: Learn how to run scripts from the command line using the `naija` CLI.
- **[Toolchain Manager (`naijaup`)](NAIJAUP.md)**: Manage installations, switch between versions, and keep your toolchain up to date.

## Let's Get Started

Begin by installing the toolchain manager and interpreter.

1. **Install `naijaup` (Toolchain Manager)**

   - **Linux or macOS:**
     ```sh
     curl --proto '=https' --tlsv1.2 -LsSf https://raw.githubusercontent.com/xosnrdev/naijascript/master/scripts/install.sh | sh
     ```
   - **Windows (PowerShell):**
     ```powershell
     powershell -ExecutionPolicy ByPass -c "irm https://raw.githubusercontent.com/xosnrdev/naijascript/master/scripts/install.ps1 | iex"
     ```
   - After installation, add `$HOME/.local/bin` (Linux/macOS) or `%USERPROFILE%\.naijascript\bin` (Windows) to your `PATH`, then restart the terminal if your shell does not pick it up automatically.

2. **Install and Configure `naija` (Interpreter)**

   - Install the latest release:
     ```sh
     naijaup install latest
     ```
   - Set that version as the default:
     ```sh
     naijaup default latest
     ```

With the interpreter ready, continue through the remaining chapters and start building NaijaScript programs.
