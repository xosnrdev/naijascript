# NaijaScript

![platform](https://img.shields.io/badge/platform-linux%20%7C%20macOS%20%7C%20windows-lightgrey)
[![Docs](https://img.shields.io/badge/docs-blue.svg?style=flat&logo=bookstack&logoColor=white)](https://xosnrdev.github.io/naijascript/)
[![Playground](https://img.shields.io/badge/playground-online-brightgreen?logo=google-chrome&logoColor=white)](https://naijascript-playground.pages.dev)

A scripting language interpreter inspired by the expressive linguistics of Naija (Nigerian Pidgin English).

> [!WARNING]
>
> This project is **experimental** and may be subjected to breaking changes.

## Motivation

Programming should not require abandoning your cultural identity. For over 75 million speakers of Nigerian Pidgin English, learning to code has meant translating thoughts between languages before expressing computational logic.

NaijaScript eliminates this cognitive overhead by embracing authentic Pidgin expressions as first-class programming constructs. When you write `make age get 25` instead of `let age = 25`, you are not just writing code but thinking computationally in your natural linguistic patterns.

This is more than syntax translation. It represents a fundamental shift toward inclusive technology that recognizes linguistic diversity as strength. By removing artificial barriers, NaijaScript opens programming education to millions while demonstrating that powerful software can emerge from any cultural context.

The language proves this philosophy through performance. In benchmarks, NaijaScript often outperforms JavaScript and Python in string manipulation, mathematical computation, and algorithmic tasks. Cultural authenticity and technical excellence are not opposing forces.

## Acknowledgements

NaijaScript serves as an educational tool and experimental platform for exploring culturally-rooted programming languages. It is particularly effective for:

- Grasping computational concepts without simultaneous linguistic translation
- Quick automation tasks using familiar language patterns
- Studying the impact of linguistic accessibility on programming comprehension
- Building software that reflects Nigerian linguistic heritage

### Current Limitations

As an experimental language, NaijaScript has intentional constraints:

- No package management or extensive standard library yet
- Optimized for educational and scripting use cases, not large-scale applications
- Lacks IDE support and advanced tooling

## Getting Started

### Installation

The easiest way to install the interpreter is using our installation scripts:

**Linux/macOS:**

```sh
curl --proto '=https' --tlsv1.2 -LsSf https://raw.githubusercontent.com/xosnrdev/naijascript/master/scripts/install.sh | sh
```

**Windows (PowerShell):**

```powershell
powershell -ExecutionPolicy ByPass -c "irm https://raw.githubusercontent.com/xosnrdev/naijascript/master/scripts/install.ps1 | iex"
```

This installs the latest stable release.

The installer adds `naija` to your PATH automatically. You may need to restart your terminal after installation.

### Usage

Try run your first program

Create a file called `hello.ns`:

```naijascript
shout("Hello World!")
```

Execute it with:

```bash
naija hello.ns
```

You can also run code directly:

```bash
naija --eval 'shout("Hello World!")'
```

Or pipe code through standard input:

```bash
echo 'shout("Hello World!")' | naija -
```

For help with available commands:

```bash
naija help
```

### Documentation

- [Language Guide](https://xosnrdev.github.io/naijascript/)
- [Online Playground](https://naijascript-playground.pages.dev)
- [Examples](./examples/)

## Contributing

The language features are intentionally small and focused. I am open to hearing different perspectives. If you have ideas for new features or improvements that you think would be a good addition, feel free to open an issue or discussion.

## License

This project is licensed under the [MIT License](./LICENSE)
