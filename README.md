# NaijaScript

A scripting language inspired by Naija (Nigerian) lingo — for learning, automation, and fun.

## What is NaijaScript?

NaijaScript is an experimental, expressive scripting language designed for Nigerians and anyone who loves Pidgin English. It aims to make programming more relatable and less intimidating, especially for beginners and educators.

> ⚠️ This project is experimental and subject to breaking changes as we learn and grow.

## Features

- Pidgin-English-based syntax and error messages
- REPL, script file, and eval modes
- Stack-based variable scoping
- Contextual error reporting (line/column)
- Designed for easy extension (strings, arrays, booleans coming soon)

## Usage

**Run a script:**

```sh
cargo run -- example.ns
```

**Start the REPL:**

```sh
cargo run -- --interactive
```

**Eval a code snippet:**

```sh
cargo run -- --eval "make x get 5; shout ( x add 2 )"
```

**Pipe from stdin:**

```sh
echo "make x get 1" | cargo run -- -
```

## Example

```naijascript
make x get 5
shout ( x add 2 )
if to say ( x na 5 ) start
    shout ( 100 )
end
```

_For technical details, see [DESIGN.md](./DESIGN.md)._
