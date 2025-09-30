# Naija – The Interpreter CLI

`naija` is the command-line interface for executing NaijaScript programs.

## Getting Started

Run `naija --help` to view every available flag and subcommand.

### 1. Execute a Script File

NaijaScript source files must end with `.ns` or `.naija`.

```sh
naija example.ns
```

### 2. Evaluate Inline Code

Run a quick expression or block of code directly from the terminal:

```sh
naija --eval "make x get 5 shout(x add 2)"
```

### 3. Pipe Source from Another Command

Send code over standard input using a pipe. Use `-` to indicate stdin.

```sh
echo "make x get 5 shout(x add 2)" | naija -
```

or

```sh
cat your_script.ns | naija -
```

## Command-Line Options

- `--eval <code>`, `-e <code>`: Execute the provided source string.
- `<script>`: Path to the script file to run.
- `-h`, `--help`: Display usage information.
- `-V`, `--version`: Print the current `naija` version.

## Script Requirements

- Source files must use the `.ns` or `.naija` extension.
- Passing `-` as the script name tells `naija` to read from standard input.

## Examples

### Example Script: `sum.ns`

```naijascript
make x get 10
make y get 20
shout(x add y)
```

Run the script with:

```sh
naija sum.ns
```

### Example: Evaluate a Single Command

```sh
naija --eval "make x get 2 shout(x times 3)"
```

## Troubleshooting

- **The script fails to run.** Confirm the file ends with `.ns` or `.naija`.
- **The `naija` executable is not found.** Add the installation directory to your `PATH`. Refer to [naijaup](./NAIJAUP.md) for installation details. If the problem persists, [open an issue on GitHub](https://github.com/xosnrdev/naijascript/issues).
- **Piping input does not work.** Use `naija -` so the interpreter reads from standard input.
