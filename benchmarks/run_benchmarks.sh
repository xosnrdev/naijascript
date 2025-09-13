#!/bin/bash

# NaijaScript Benchmark Runner

set -euo pipefail

BENCHMARK_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
RESULTS_DIR="${BENCHMARK_DIR}/results"
TEMP_DIR="${BENCHMARK_DIR}/temp"

# Create directories
mkdir -p "${RESULTS_DIR}" "${TEMP_DIR}"

# Clear filesystem cache for consistent measurements (if possible)
echo "Clearing filesystem cache..."
if command -v purge >/dev/null 2>&1; then
    if purge 2>/dev/null; then
        echo "✓ Filesystem cache cleared"
    else
        echo "⚠ Could not clear cache (no sudo), continuing anyway..."
    fi
else
    echo "⚠ Cache clearing not available on this system, continuing anyway..."
fi

# Function to run benchmark with hyperfine
run_benchmark() {
    local name="$1"
    local naija_cmd="$2" 
    local js_cmd="$3"
    local py_cmd="$4"
    
    echo "Running benchmark: $name"
    
    # Hyperfine configuration based on academic standards:
    # - 12 runs with first run discarded (warmup)
    # - Export to JSON for analysis
    # - Show output to verify correctness
    hyperfine \
        --warmup 1 \
        --runs 12 \
        --show-output \
        --export-json "${RESULTS_DIR}/${name}.json" \
        --export-markdown "${RESULTS_DIR}/${name}.md" \
        --command-name "NaijaScript" "${naija_cmd}" \
        --command-name "JavaScript" "${js_cmd}" \
        --command-name "Python" "${py_cmd}"
        
    echo "✓ Completed: $name"
    echo ""
}

# Verify all programs produce same output
verify_correctness() {
    echo "Verifying algorithmic correctness..."
    
    local failed=0
    
    for category in math strings builtins; do
        for benchmark in "${BENCHMARK_DIR}/${category}"/*.ns; do
            local basename=$(basename "$benchmark" .ns)
            
            # Skip if other language files don't exist
            [[ -f "${BENCHMARK_DIR}/${category}/${basename}.js" ]] || continue
            [[ -f "${BENCHMARK_DIR}/${category}/${basename}.py" ]] || continue
            
            local ns_output=$(naija "$benchmark" 2>/dev/null || echo "ERROR")
            local js_output=$(node "${BENCHMARK_DIR}/${category}/${basename}.js" 2>/dev/null || echo "ERROR")
            local py_output=$(python3 "${BENCHMARK_DIR}/${category}/${basename}.py" 2>/dev/null || echo "ERROR")
            
            if [[ "$ns_output" == "$js_output" && "$js_output" == "$py_output" && "$ns_output" != "ERROR" ]]; then
                echo "✓ ${basename}: All outputs match ($ns_output)"
            else
                echo "✗ ${basename}: Outputs differ!"
                echo "  NaijaScript: $ns_output"
                echo "  JavaScript:  $js_output" 
                echo "  Python:      $py_output"
                failed=1
            fi
        done
    done
    
    if [[ $failed -eq 1 ]]; then
        echo "❌ Correctness verification failed! Fix implementations before benchmarking."
        exit 1
    fi
    
    echo "✅ All implementations produce identical outputs"
    echo ""
}

# Main execution
echo "NaijaScript Performance Benchmark Suite"
echo "======================================="
echo ""

# System information
echo "Environment Information:"
echo "- Date: $(date)"
echo "- OS: $(uname -s) $(uname -r)"
echo "- Hardware: $(sysctl -n machdep.cpu.brand_string 2>/dev/null || echo 'Unknown')"
echo "- NaijaScript: $(naija --version 2>/dev/null || echo 'Not found')"
echo "- Node.js: $(node --version 2>/dev/null || echo 'Not found')"
echo "- Python: $(python3 --version 2>/dev/null || echo 'Not found')"
echo "- Hyperfine: $(hyperfine --version 2>/dev/null || echo 'Not found')"
echo ""

# Verify correctness first
verify_correctness

# Run benchmarks
echo "Starting benchmarks..."
echo ""

# Mathematical benchmarks
run_benchmark "fibonacci_recursive" \
    "naija ${BENCHMARK_DIR}/math/fibonacci_recursive.ns" \
    "node ${BENCHMARK_DIR}/math/fibonacci_recursive.js" \
    "python3 ${BENCHMARK_DIR}/math/fibonacci_recursive.py"

run_benchmark "fibonacci_iterative" \
    "naija ${BENCHMARK_DIR}/math/fibonacci_iterative.ns" \
    "node ${BENCHMARK_DIR}/math/fibonacci_iterative.js" \
    "python3 ${BENCHMARK_DIR}/math/fibonacci_iterative.py"

run_benchmark "prime_count" \
    "naija ${BENCHMARK_DIR}/math/prime_count.ns" \
    "node ${BENCHMARK_DIR}/math/prime_count.js" \
    "python3 ${BENCHMARK_DIR}/math/prime_count.py"

# String benchmarks
run_benchmark "string_interpolation" \
    "naija ${BENCHMARK_DIR}/strings/string_interpolation.ns" \
    "node ${BENCHMARK_DIR}/strings/string_interpolation.js" \
    "python3 ${BENCHMARK_DIR}/strings/string_interpolation.py"

run_benchmark "string_concat" \
    "naija ${BENCHMARK_DIR}/strings/string_concat.ns" \
    "node ${BENCHMARK_DIR}/strings/string_concat.js" \
    "python3 ${BENCHMARK_DIR}/strings/string_concat.py"

run_benchmark "string_manipulation" \
    "naija ${BENCHMARK_DIR}/strings/string_manipulation.ns" \
    "node ${BENCHMARK_DIR}/strings/string_manipulation.js" \
    "python3 ${BENCHMARK_DIR}/strings/string_manipulation.py"

# Built-in function benchmarks
run_benchmark "math_functions" \
    "naija ${BENCHMARK_DIR}/builtins/math_functions.ns" \
    "node ${BENCHMARK_DIR}/builtins/math_functions.js" \
    "python3 ${BENCHMARK_DIR}/builtins/math_functions.py"

echo "🎉 All benchmarks completed!"
echo ""
echo "Results saved to: ${RESULTS_DIR}/"
echo "Next steps:"
echo "1. Run: ./analyze_results.py"
echo "2. Review: ${RESULTS_DIR}/summary.md"