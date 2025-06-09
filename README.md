# NaijaScript

A scripting language for learning, automation, and fun with Naija (Nigerian) lingo.

## Install

**POSIX (Linux/macOS):**

```sh
curl --proto '=https' --tlsv1.2 -LsSf https://raw.githubusercontent.com/xosnrdev/naijascript/master/scripts/install.sh | sh
```

**Windows (PowerShell):**

```powershell
powershell -ExecutionPolicy ByPass -c "irm https://raw.githubusercontent.com/xosnrdev/naijascript/master/scripts/install.ps1 | iex"
```

## Interpreter Setup

To set up the NaijaScript interpreter:

1. **Install the latest version using `naijaup`:**

   ```sh
   naijaup install latest
   ```

2. **Set the latest version as the default:**

   ```sh
   naijaup default latest
   ```

Alternatively, you can manually download pre-built binaries from the [Releases page](https://github.com/xosnrdev/naijascript/releases/latest).

## Usage

Run a script:

```sh
naija your_script.ns
```

Start the REPL:

```sh
naija --interactive
```

Eval a code snippet:

```sh
naija --eval "make x get 5 shout ( x add 2 )"
```

Pipe from stdin:

```sh
echo "make x get 5 shout ( x add 2 )" | naija -
```

## Examples

See [examples/](./examples) for runnable scripts.

For technical and contributor details, see [docs/DESIGN.md](./docs/DESIGN.md).
