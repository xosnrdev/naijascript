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

```sh
naijaup install 0.1.0
```

You can also download binaries from [the Releases page](https://github.com/xosnrdev/naijascript/releases/latest).

## Usage

Run a script:

```sh
~/.naijaup/versions/0.1.0/naija your_script.ns
```

Start the REPL:

```sh
~/.naijaup/versions/0.1.0/naija --interactive
```

Eval a code snippet:

```sh
~/.naijaup/versions/0.1.0/naija --eval "make x get 5; shout ( x add 2 )"
```

Pipe from stdin:

```sh
echo "make x get 1" | ~/.naijaup/versions/0.1.0/naija -
```

## Examples

See [examples/](./examples) for runnable scripts.

For technical and contributor details, see [docs/DESIGN.md](./docs/DESIGN.md).
