# Naija - The Interpreter CLI

Na the main command-line tool wey dey run NaijaScript.

## How to Use Am

Run `naija --help` make you see everything wey you fit do.

### 1. How to Run Your Script File

To run your NaijaScript file (e must end with `.ns` or `.naija`):

```sh
naija example.ns
```

### 2. How to Run Code Straight from Command Line

To run small code sharp sharp:

```sh
naija --eval "make x get 5 shout(x add 2)"
```

### 3. How to Send Code from Another Place

To run code wey you send from another command or file:

```sh
echo "make x get 5 shout(x add 2)" | naija -
```

Or:

```sh
cat your_script.ns | naija -
```

## All the Command-Line Options

- `--eval <code>`, `-e <code>`: E go run the code wey you give am inside the quote.
- `<script>`: The path to the script file wey you wan run.
- `-h`, `--help`: E go show you help message.
- `-V`, `--version`: E go show you the version of `naija` wey you get.

## Wetin Your Script File Need

- Your script file must end with `.ns` or `.naija`.
- If you use `-` as the script name, `naija` go read code from standard input.

## Examples

### Example Script: `sum.ns`

```naijascript
make x get 10
make y get 20
shout(x add y)
```

Run am like this:

```sh
naija sum.ns
```

### Example: Run Small Code

```sh
naija --eval "make x get 2 shout(x times 3)"
```

## If Wahala Happen (Troubleshooting)

- **Script No Wan Run:**
  - Check say your script file end with `.ns` or `.naija`.
- **Command No Dey:**
  - Make sure say the `naija` program dey your `PATH`. Check the [naijaup](./NAIJAUP.md) to learn how to set am up.
  - If the wahala still dey, abeg [open issue for us for GitHub](https://github.com/xosnrdev/naijascript/issues).
- **Piping No Dey Work:**
  - Use `naija -` to read from standard input.
