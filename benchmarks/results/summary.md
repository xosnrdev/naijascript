# NaijaScript Performance Results

## What This Means

**NaijaScript is surprisingly fast** - it's the fastest language in 5 out of 7 tests

**Standout performance**: Notably faster at prime count, math functions, fibonacci iterative, string manipulation

**Areas for improvement**: Significantly slower at string concat, fibonacci recursive

**Recommendation**: NaijaScript shows competitive performance for most tasks

## Performance Numbers

_Times are from hyperfine with 12 measurements each. Lower is better._

### Mathematical Operations

**Fibonacci Recursive**

| Language    | Time    | vs Fastest  |
| ----------- | ------- | ----------- |
| JavaScript  | 34.1ms  | **fastest** |
| Python      | 125.3ms | 3.7x slower |
| NaijaScript | 290.8ms | 8.5x slower |

**Fibonacci Iterative**

| Language    | Time          | vs Fastest   |
| ----------- | ------------- | ------------ |
| NaijaScript | 1.5ms ± 0.3ms | **fastest**  |
| Python      | 16.8ms        | 11.5x slower |
| JavaScript  | 27.6ms        | 18.8x slower |

**Prime Count**

| Language    | Time           | vs Fastest  |
| ----------- | -------------- | ----------- |
| NaijaScript | 12.0ms         | **fastest** |
| Python      | 22.4ms ± 2.7ms | 1.9x slower |
| JavaScript  | 28.4ms         | 2.4x slower |

### String Processing

**String Interpolation**

| Language    | Time           | vs Fastest  |
| ----------- | -------------- | ----------- |
| NaijaScript | 23.5ms ± 9.8ms | **fastest** |
| JavaScript  | 31.4ms         | 1.3x slower |
| Python      | 32.7ms         | 1.4x slower |

**String Concat**

| Language    | Time    | vs Fastest  |
| ----------- | ------- | ----------- |
| Python      | 28.0ms  | **fastest** |
| JavaScript  | 30.0ms  | 1.1x slower |
| NaijaScript | 215.2ms | 7.7x slower |

**String Manipulation**

| Language    | Time   | vs Fastest  |
| ----------- | ------ | ----------- |
| NaijaScript | 6.6ms  | **fastest** |
| Python      | 20.2ms | 3.1x slower |
| JavaScript  | 29.4ms | 4.5x slower |

### Built-in Functions

**Math Functions**

| Language    | Time            | vs Fastest  |
| ----------- | --------------- | ----------- |
| NaijaScript | 4.4ms           | **fastest** |
| Python      | 23.9ms ± 10.8ms | 5.4x slower |
| JavaScript  | 28.6ms          | 6.5x slower |

## Environment

**Hardware**: Apple M2 processor

**Software**: NaijaScript 0.11.3, Node.js v24.4.1, Python 3.13.3

**Methodology**: See [METHODOLOGY.md](../METHODOLOGY.md) for details.
