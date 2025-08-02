# Kedu! Welcome to NaijaScript

NaijaScript na language wey you fit use learn coding, automate things, and catch fun. This book go teach you everything wey you need to know, from how we think am to how you go take use am.

You wan experiment quick? Try the online [playground](https://naijascript-playground.pages.dev).

## How to Use This Book

We don arrange this book so e go carry you from the idea to the action. Na so the sections take arrange:

- **[Introduction](INTRODUCTION.md)**: Na here you dey so! This place go give you the full gist about the book and help you get the tools wey you need.
- **[Our Mind (Design Philosophy)](DESIGN.md)**: Learn the "why" behind NaijaScript. If you wan understand the vision, make you start from here.
- **[Language Manual (LRM)](LRM.md)**: The official grammar book for NaijaScript. E go explain the syntax, data types, and how to control your code.
- **[Interpreter (`naija`)](naija.md)**: The practical guide on how to use the `naija` command to run your scripts.
- **[Toolchain Manager (`naijaup`)](naijaup.md)**: Learn how to manage your NaijaScript installation, change versions, and keep everything fresh.

## Oya, Make We Start

To start to dey use NaijaScript, you need to install the interpreter first.

1. **First, Install `naijaup` (the Toolchain Manager)**

   - **For Linux/macOS:**
     ```sh
     curl --proto '=https' --tlsv1.2 -LsSf https://raw.githubusercontent.com/xosnrdev/naijascript/master/scripts/install.sh | sh
     ```
   - **For Windows (PowerShell):**
     ```powershell
     powershell -ExecutionPolicy ByPass -c "irm https://raw.githubusercontent.com/xosnrdev/naijascript/master/scripts/install.ps1 | iex"
     ```
   - After you don install am, make sure say `$HOME/.local/bin` (for Linux/macOS) or `%USERPROFILE%\.naijascript\bin` (for Windows) dey your `PATH`. If e no work, restart your terminal.

2. **Then, Install and Set Up `naija` (the Interpreter)**

   - To install the latest version:
     ```sh
     naijaup install latest
     ```
   - To make the version you just installed the default one:
     ```sh
     naijaup default latest
     ```

Now, you don ready to explore the rest of the book and start to dey write your own NaijaScript programs. Oya, let's go!
