# NaijaScript Interpreter (`naija`)

The official command-line interpreter for NaijaScript.

## Usage

Run `naija --help` to see all available options and commands.

### 1. Run a Script File

To execute a NaijaScript file (must end with `.ns` or `.naija`):

```sh
naija your_script.ns
```

### 2. Evaluate Code from the Command Line

To evaluate a code snippet directly:

```sh
naija --eval "make x get 5 shout ( x add 2 )"
```

### 3. Start the Interactive REPL

To enter interactive mode:

```sh
naija --interactive
```

- Type code and press Enter to execute.
- Type `exit` or press Ctrl+D to quit.

### 4. Pipe Code from Standard Input

To run code piped from another command or file:

```sh
echo "make x get 5 shout ( x add 2 )" | naija -
```

Or:

```sh
cat your_script.ns | naija -
```

## Command-Line Options

- `--eval <code>`, `-e <code>`: Evaluate the provided code string.
- `--interactive`, `-i`: Start the interactive REPL.
- `<script>`: Path to a script file to execute (must end with `.ns` or `.naija`).
- `-h`, `--help`: Show help information.
- `-V`, `--version`: Show version information.

## Script File Requirements

- Script files must have a `.ns` or `.naija` extension.
- If you pass `-` as the script argument, code will be read from standard input.

## Examples

### Example Script: `sum.ns`

```naijascript
make x get 10
make y get 20
shout ( x add y )
```

Run it:

```sh
naija sum.ns
```

### Example: Evaluate a Snippet

```sh
naija --eval "make x get 2 shout ( x times 3 )"
```

### Example: REPL

```sh
naija --interactive
```

## Troubleshooting

- **Script Not Running:**
  - Ensure your script file ends with `.ns` or `.naija`.
- **Command Not Found:**
  - Make sure the `naija` binary is in your `PATH`. See the [naijaup toolchain manager documentation](./naijaup.md) for setup and troubleshooting.
  - If you continue to have issues, please [open an issue on GitHub](https://github.com/xosnrdev/naijascript/issues).
- **REPL Not Starting:**
  - Use `naija --interactive`.
- **Piping Not Working:**
  - Use `naija -` to read from standard input.
