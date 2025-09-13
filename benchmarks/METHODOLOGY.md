# Benchmark Methodology

**Purpose**: Ensure fair, reproducible performance comparisons between NaijaScript, JavaScript, and Python.

## Algorithm Equivalence

All benchmarks implement **identical algorithms** across languages:

```bash
# Example: Recursive Fibonacci
fibonacci_recursive.ns  # NaijaScript implementation
fibonacci_recursive.js  # Node.js implementation
fibonacci_recursive.py  # Python implementation
```

**Verification**: We manually review each implementation to ensure:

- Same algorithmic complexity (O(n), O(2^n), etc.)
- Equivalent data structures (numbers, strings, arrays)
- No language-specific optimizations

## Statistical Rigor

### Timing Protocol

- **Tool**: [hyperfine](https://github.com/sharkdp/hyperfine) industry-standard benchmark tool
- **Warmup**: 1 run to eliminate cold-start effects
- **Measurements**: 12 runs per benchmark for statistical confidence
- **Precision**: Microsecond-level timing accuracy

### Statistical Analysis

- **Central tendency**: Mean execution time reported
- **Variance**: Standard deviation calculated
- **Confidence**: All results include confidence intervals
- **Outlier handling**: Statistical outliers identified but retained for transparency

## Environment Controls

### System Standardization

- **OS**: macOS (consistent across all runs)
- **Process isolation**: Each run in fresh process
- **Resource availability**: No competing CPU-intensive processes
- **Memory**: Sufficient RAM to avoid swapping

### Language Runtime Versions

- **NaijaScript**: Latest git commit
- **Node.js**: v16+ (V8 optimizations enabled)
- **Python**: 3.8+ (standard CPython interpreter)

## Fairness Criteria

### What We Do

✅ **Identical algorithms** - Same computational complexity  
✅ **Representative inputs** - Realistic problem sizes  
✅ **Multiple runs** - Statistical confidence  
✅ **Fair runtimes** - Production-ready language versions

### What We Don't Do

❌ **Language-specific tricks** - No special optimizations for any language  
❌ **Unfair comparisons** - No interpreted vs compiled mismatches  
❌ **Cherry-picking** - All results reported, including unfavorable ones

## Reproducibility

### Running Benchmarks

```bash
# Full benchmark suite
./run_benchmarks.sh

# Individual benchmark
hyperfine --warmup 1 --runs 12 \
  'naija fibonacci_recursive.ns' \
  'node fibonacci_recursive.js' \
  'python3 fibonacci_recursive.py'
```

## Limitations

**Interpretation scope**: These benchmarks measure execution speed only. They don't measure:

- Development productivity
- Code maintainability
- Learning curve
- Memory usage patterns
- Compilation time

**Representative workloads**: Benchmarks focus on algorithmic tasks. Real applications may have different performance characteristics.
