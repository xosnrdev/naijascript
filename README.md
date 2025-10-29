# NaijaScript

![platform](https://img.shields.io/badge/platform-linux%20%7C%20macOS%20%7C%20windows-lightgrey)
[![Docs](https://img.shields.io/badge/docs-blue.svg?style=flat&logo=bookstack&logoColor=white)](https://xosnrdev.github.io/naijascript/)
[![Playground](https://img.shields.io/badge/playground-online-brightgreen?logo=google-chrome&logoColor=white)](https://naijascript-playground.pages.dev)

A scripting language interpreter inspired by the expressive linguistics of Naija (Nigerian Pidgin English).

## Motivation

Programming should not require abandoning your cultural identity.

For over 75 million speakers of Nigerian Pidgin English, learning to code has meant translating thoughts between languages before expressing computational logic.

NaijaScript eliminates this cognitive overhead by embracing authentic Pidgin expressions as first-class programming constructs.

When you write:

```naijascript
make age get 25
if to say (age pass 18) start
    shout("You fit vote!")
end
```

you are not just writing code but thinking computationally in your natural linguistic patterns.

This is more than syntax translation. It represents a fundamental shift toward inclusive technology that recognizes linguistic diversity as strength. By removing artificial barriers, NaijaScript opens programming education to millions while demonstrating that powerful software can emerge from any cultural context.

The language proves this philosophy through performance. In benchmarks, NaijaScript often outperforms JavaScript and Python in string manipulation, mathematical computation, and algorithmic tasks making cultural authenticity and technical excellence complementary goals.

> This is a merge sort performance comparison between NaijaScript and Python

![merge sort benchmark gif](./docs/merge_sort_benchmark.gif)

Read more in [this article](https://hackmd.io/sIhWJ4QeSAGiaE3D-xiieg).

## Limitations

While NaijaScript aims to be a practical scripting language, it currently has some limitations:

- No package management or extensive standard library yet
- Designed for educational and scripting use cases, not large-scale applications
- Lacks IDE support and advanced tooling

## Getting Started

### Installation

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

See the [Installation Guide](./INSTALL.md) for more.

### First program

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

### Documentation

- [Official Documentation](https://xosnrdev.github.io/naijascript/)
- [Web Playground](https://naijascript-playground.pages.dev)
- [Examples](./examples/)

## Contributing

The language features are intentionally small and focused. I am open to hearing different perspectives. If you have ideas for new features or improvements that you think would be a good addition, feel free to open an issue or discussion.

## License

This project is licensed under the [MIT License](./LICENSE)
