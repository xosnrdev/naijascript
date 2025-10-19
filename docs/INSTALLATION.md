# Installation

NaijaScript runs on Linux, macOS, and Windows.

## Quick install (recommended)

Linux/macOS

```bash
curl --proto '=https' --tlsv1.2 -LsSf https://raw.githubusercontent.com/xosnrdev/naijascript/master/scripts/install.sh | sh
```

Windows (PowerShell)

```powershell
powershell -ExecutionPolicy ByPass -c "irm https://raw.githubusercontent.com/xosnrdev/naijascript/master/scripts/install.ps1 | iex"
```

These scripts download the latest stable release, install the `naija` interpreter, and add the install directory to your PATH. You may need to restart your terminal after installation.

Verify the install

```bash
naija --version
```

Expected output example:

```text
naijascript 0.11.5
```

You can also confirm the binary location:

- Linux/macOS: `which naija`
- PowerShell: `Get-Command naija`

## First program

Create `hello.ns`:

```naijascript
shout("Hello, World!")
```

Run it:

```bash
naija hello.ns
```

For help:

```bash
naija help
```

## Web playground

Try NaijaScript in your browser: [https://naijascript-playground.pages.dev](https://naijascript-playground.pages.dev)

## Development setup

1. Clone the repository

```bash
git clone https://github.com/xosnrdev/naijascript.git
cd naijascript
```

2. Ensure Rust tooling

Install Rust via rustup if you do not have it. The repository pins the toolchain in rust-toolchain.toml; rustup will select the required nightly toolchain automatically.

3. Build the interpreter

```bash
cargo build --bin naija
```

4. Run tests

```bash
cargo test
```

If you encounter any issues, feel free to open an issue on the [GitHub repository](https://github.com/xosnrdev/naijascript/issues).
