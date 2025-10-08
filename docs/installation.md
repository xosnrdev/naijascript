# Installation

Getting started with NaijaScript is straightforward. The interpreter runs on Linux, macOS, and Windows.

The easiest way to install NaijaScript is using our installation scripts:

## Linux/macOS

```bash
curl --proto '=https' --tlsv1.2 -LsSf https://raw.githubusercontent.com/xosnrdev/naijascript/master/scripts/install.sh | sh
```

## Windows (PowerShell)

```powershell
powershell -ExecutionPolicy ByPass -c "irm https://raw.githubusercontent.com/xosnrdev/naijascript/master/scripts/install.ps1 | iex"
```

These scripts will:

- Download the latest stable release
- Install the `naija` interpreter to your system
- Add the installation directory to your PATH

You may need to restart your terminal after installation.

## Verify Installation

Test your installation by running:

```bash
naija --version
```

You should see output similar to:

```text
naijascript 0.11.5
```

## Try Your First Program

Create a file called `hello.ns`:

```naijascript
shout("Hello, World!")
```

Execute it with:

```bash
naija hello.ns
```

For help with available commands:

```bash
naija help
```

## Web Playground

If you want to try NaijaScript without installing anything locally, use our [online playground](https://naijascript-playground.pages.dev). It runs entirely in your browser using WebAssembly.

## Development Setup

For contributing to NaijaScript development, you'll need to:

1. **Clone the repository**:

```bash
git clone https://github.com/xosnrdev/naijascript.git
cd naijascript
```

2. **Build from source**:

```bash
cargo build
```

3. **Run tests**:

```bash
cargo test
```

## Troubleshooting

If you encounter any issues, feel free to open an issue on the [GitHub repository](https://github.com/xosnrdev/naijascript/issues).
