# NaijaScript Performance Benchmarks

This directory contains a comprehensive performance evaluation of NaijaScript against JavaScript (Node.js) and Python using academic benchmarking methodology.

## Quick Start

```bash
# Run all benchmarks (15-20 minutes)
./run_benchmarks.sh

# Generate charts and analysis
python3 analyze_results.py

# View results
open results/summary.md
```

## What You'll Learn

- **Relative performance** - How much slower/faster NaijaScript is for different task types
- **Performance patterns** - Which operations are competitive vs. slower
- **Data confidence** - Statistical confidence intervals for all measurements

## Benchmark Categories

| Category               | What it measures              | Example tasks                |
| ---------------------- | ----------------------------- | ---------------------------- |
| **Mathematical**       | Algorithm execution speed     | Fibonacci, prime counting    |
| **String Processing**  | Text manipulation performance | Concatenation, interpolation |
| **Built-in Functions** | Standard library efficiency   | Math operations              |

## Environment Requirements

- NaijaScript interpreter (`naija` in PATH)
- Node.js v16+ and Python 3.8+
- [hyperfine](https://github.com/sharkdp/hyperfine) benchmarking tool

## Results Documentation

| Document                                     | Purpose                               |
| -------------------------------------------- | ------------------------------------- |
| **[METHODOLOGY.md](METHODOLOGY.md)**         | How benchmarks ensure fair comparison |
| **[results/summary.md](results/summary.md)** | Complete timing data and analysis     |
| **[results/charts/](results/charts/)**       | Visual performance comparisons        |
