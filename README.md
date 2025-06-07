# NaijaScript

A scripting language for learning, automation, and fun with Naija (Nigerian) lingo.

## Install

**POSIX (Linux/macOS):**

```sh
curl -fsSL https://raw.githubusercontent.com/xosnrdev/naijascript/main/scripts/install.sh | sh
```

**Windows (PowerShell):**

```powershell
irm https://raw.githubusercontent.com/xosnrdev/naijascript/main/scripts/install.ps1 | iex
```

## Interpreter Setup

```sh
naijaup install latest
```

## Usage

Run a script:

```sh
naija example.ns
```

Start the REPL:

```sh
naija --interactive
```

Eval a code snippet:

```sh
naija --eval "make x get 5; shout ( x add 2 )"
```

Pipe from stdin:

```sh
echo "make x get 1" | naija -
```

## Examples

See [examples/](./examples) for runnable scripts.

For technical and contributor details, see [docs/DESIGN.md](./docs/DESIGN.md).
