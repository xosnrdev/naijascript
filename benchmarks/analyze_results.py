#!/usr/bin/env python3

"""
NaijaScript Benchmark Results Analyzer

Processes hyperfine JSON output to create performance comparisons and visualizations.
"""

import json
import sys
from pathlib import Path
import matplotlib.pyplot as plt
import pandas as pd
from typing import Dict

# Configuration
BENCHMARK_DIR = Path(__file__).parent
RESULTS_DIR = BENCHMARK_DIR / "results"
CHARTS_DIR = RESULTS_DIR / "charts"


def load_benchmark_results() -> Dict[str, Dict]:
    """Load all benchmark JSON results."""
    results = {}

    for json_file in RESULTS_DIR.glob("*.json"):
        benchmark_name = json_file.stem
        try:
            with open(json_file) as f:
                data = json.load(f)
                results[benchmark_name] = data
        except Exception as e:
            print(f"Warning: Could not load {json_file}: {e}")

    return results


def extract_performance_data(results: Dict[str, Dict]) -> pd.DataFrame:
    """Extract performance data into a pandas DataFrame."""
    data = []

    for benchmark_name, benchmark_data in results.items():
        for result in benchmark_data.get("results", []):
            command_name = result.get("command", "Unknown")
            mean_time = result.get("mean", 0)
            std_time = result.get("stddev", 0)

            data.append(
                {
                    "benchmark": benchmark_name,
                    "language": command_name,
                    "mean_time": mean_time,
                    "std_time": std_time,
                }
            )

    return pd.DataFrame(data)


def create_performance_comparison_chart(df: pd.DataFrame):
    """Create a comprehensive performance comparison chart."""

    # Set up the plot style
    plt.style.use("seaborn-v0_8-whitegrid")
    fig, axes = plt.subplots(2, 2, figsize=(16, 12))
    fig.suptitle(
        "NaijaScript Performance Benchmarks\nvs JavaScript (Node.js) and Python",
        fontsize=16,
        fontweight="bold",
    )

    # Color scheme
    colors = {
        "NaijaScript": "#27AE60",  # Green
        "JavaScript": "#F1C40F",  # Yellow
        "Python": "#3498DB",  # Blue
    }

    # Group benchmarks by category
    categories = {
        "Mathematical": ["fibonacci_recursive", "fibonacci_iterative", "prime_count"],
        "String Processing": [
            "string_interpolation",
            "string_concat",
            "string_manipulation",
        ],
        "Built-ins": ["math_functions"],
        "All Benchmarks": df["benchmark"].unique(),
    }

    plot_idx = 0
    for category, benchmarks in list(categories.items())[
        :4
    ]:  # Only first 4 for 2x2 grid
        if plot_idx >= 4:
            break

        ax = axes[plot_idx // 2, plot_idx % 2]

        # Filter data for this category
        if category == "All Benchmarks":
            category_df = df
        else:
            category_df = df[df["benchmark"].isin(benchmarks)]

        if category_df.empty:
            continue

        # Create grouped bar chart
        benchmark_names = category_df["benchmark"].unique()
        x = range(len(benchmark_names))
        width = 0.25

        for i, lang in enumerate(["NaijaScript", "JavaScript", "Python"]):
            lang_data = category_df[category_df["language"] == lang]
            times = []
            errors = []

            for bench in benchmark_names:
                bench_data = lang_data[lang_data["benchmark"] == bench]
                if not bench_data.empty:
                    times.append(bench_data["mean_time"].iloc[0])
                    errors.append(bench_data["std_time"].iloc[0])
                else:
                    times.append(0)
                    errors.append(0)

            x_pos = [pos + width * i for pos in x]
            bars = ax.bar(
                x_pos,
                times,
                width,
                label=lang,
                color=colors.get(lang, "gray"),
                alpha=0.8,
                yerr=errors,
                capsize=3,
            )

            # Add value labels on bars
            for bar, time in zip(bars, times):
                if time > 0:
                    height = bar.get_height()
                    ax.text(
                        bar.get_x() + bar.get_width() / 2.0,
                        height + max(errors) * 0.1,
                        f"{time:.2f}s",
                        ha="center",
                        va="bottom",
                        fontsize=8,
                    )

        ax.set_xlabel("Benchmark")
        ax.set_ylabel("Time (seconds)")
        ax.set_title(f"{category}")
        ax.set_xticks([pos + width for pos in x])
        ax.set_xticklabels(
            [bench.replace("_", "\n") for bench in benchmark_names], rotation=45
        )
        ax.legend()
        ax.set_yscale("log")  # Log scale to handle large differences

        plot_idx += 1

    plt.tight_layout()
    plt.savefig(CHARTS_DIR / "performance_comparison.png", dpi=300, bbox_inches="tight")
    print(f"✓ Saved performance comparison chart to {CHARTS_DIR}")


def create_relative_performance_chart(df: pd.DataFrame):
    """Create a chart showing relative performance (normalized to fastest)."""

    plt.figure(figsize=(14, 8))

    # Calculate relative performance (normalized to fastest for each benchmark)
    relative_data = []

    for benchmark in df["benchmark"].unique():
        bench_df = df[df["benchmark"] == benchmark]
        fastest_time = bench_df["mean_time"].min()

        for _, row in bench_df.iterrows():
            relative_time = row["mean_time"] / fastest_time
            relative_data.append(
                {
                    "benchmark": benchmark,
                    "language": row["language"],
                    "relative_time": relative_time,
                    "absolute_time": row["mean_time"],
                }
            )

    rel_df = pd.DataFrame(relative_data)

    # Create bar chart for better readability
    fig, ax = plt.subplots(figsize=(14, 8))

    benchmarks = rel_df["benchmark"].unique()
    languages = ["NaijaScript", "JavaScript", "Python"]
    colors = ["#27AE60", "#F1C40F", "#3498DB"]  # Green, Yellow, Blue

    x = range(len(benchmarks))
    width = 0.25

    for i, lang in enumerate(languages):
        lang_data = rel_df[rel_df["language"] == lang]
        relative_times = []

        for bench in benchmarks:
            bench_data = lang_data[lang_data["benchmark"] == bench]
            if not bench_data.empty:
                relative_times.append(bench_data["relative_time"].iloc[0])
            else:
                relative_times.append(float("inf"))

        x_pos = [pos + width * i for pos in x]
        bars = ax.bar(
            x_pos, relative_times, width, label=lang, color=colors[i], alpha=0.8
        )

        # Add value labels
        for bar, rel_time in zip(bars, relative_times):
            if rel_time != float("inf"):
                height = bar.get_height()
                ax.text(
                    bar.get_x() + bar.get_width() / 2.0,
                    height + 0.1,
                    f"{rel_time:.1f}x",
                    ha="center",
                    va="bottom",
                    fontweight="bold",
                )

    ax.set_xlabel("Benchmark")
    ax.set_ylabel("Relative Performance (1x = fastest)")
    ax.set_title(
        "Relative Performance Comparison\n(Lower is Better, 1x = Fastest Implementation)"
    )
    ax.set_xticks([pos + width for pos in x])
    ax.set_xticklabels([bench.replace("_", "\n") for bench in benchmarks])
    ax.legend()
    ax.set_yscale("log")
    ax.grid(True, alpha=0.3)

    # Add horizontal line at 1x for reference
    ax.axhline(y=1, color="black", linestyle="--", alpha=0.5, label="Fastest (1x)")

    plt.tight_layout()
    plt.savefig(CHARTS_DIR / "relative_performance.png", dpi=300, bbox_inches="tight")
    print(f"✓ Saved relative performance chart to {CHARTS_DIR}")


def generate_summary_report(df: pd.DataFrame) -> str:
    """Generate performance insights using hyperfine data directly."""

    if df.empty:
        return "# Error: No benchmark data available\n\nRun benchmarks first: `./run_benchmarks.sh`"

    report = []

    report.append("# NaijaScript Performance Results")
    report.append("")

    # Calculate clear, actionable insights from the actual data
    naija_data = df[df["language"] == "NaijaScript"]
    if naija_data.empty:
        return "# Error: No NaijaScript benchmark data found"

    # Direct performance comparison using hyperfine numbers
    performance_summary = []
    for benchmark in df["benchmark"].unique():
        bench_df = df[df["benchmark"] == benchmark]

        naija_time = bench_df[bench_df["language"] == "NaijaScript"]["mean_time"]
        js_time = bench_df[bench_df["language"] == "JavaScript"]["mean_time"]
        py_time = bench_df[bench_df["language"] == "Python"]["mean_time"]

        if not naija_time.empty:
            result = {
                "benchmark": benchmark,
                "naija_ms": naija_time.iloc[0] * 1000,
                "js_ratio": naija_time.iloc[0] / js_time.iloc[0]
                if not js_time.empty
                else None,
                "py_ratio": naija_time.iloc[0] / py_time.iloc[0]
                if not py_time.empty
                else None,
                "fastest": bench_df.loc[bench_df["mean_time"].idxmin(), "language"],
            }
            performance_summary.append(result)

    # Key insights - what actually matters for users
    if performance_summary:
        naija_wins = sum(
            1 for r in performance_summary if r["fastest"] == "NaijaScript"
        )
        total_benchmarks = len(performance_summary)

        report.append("## What This Means")
        report.append("")

        if naija_wins >= total_benchmarks * 0.6:  # 60% threshold
            report.append(
                f"**NaijaScript is surprisingly fast** - it's the fastest language in {naija_wins} out of {total_benchmarks} tests"
            )
        elif naija_wins >= total_benchmarks * 0.3:  # 30% threshold
            report.append(
                f"**Mixed but promising results** - NaijaScript is fastest in {naija_wins} out of {total_benchmarks} tests"
            )
        else:
            report.append(
                f"**Performance varies by task** - NaijaScript is fastest in {naija_wins} out of {total_benchmarks} tests"
            )

        report.append("")

        # Find the most dramatic differences (both good and bad)
        dramatic_wins = [
            r
            for r in performance_summary
            if r["fastest"] == "NaijaScript"
            and (r["js_ratio"] if r["js_ratio"] else 1) < 0.5
        ]
        dramatic_losses = [
            r
            for r in performance_summary
            if r["fastest"] != "NaijaScript"
            and (
                (r["js_ratio"] if r["js_ratio"] else 1) > 5
                or (r["py_ratio"] if r["py_ratio"] else 1) > 5
            )
        ]

        if dramatic_wins:
            report.append(
                f"**Standout performance**: Notably faster at {', '.join([r['benchmark'].replace('_', ' ') for r in dramatic_wins])}"
            )
            report.append("")

        if dramatic_losses:
            report.append(
                f"**Areas for improvement**: Significantly slower at {', '.join([r['benchmark'].replace('_', ' ') for r in dramatic_losses])}"
            )
            report.append("")

        # Simple recommendation based on the data
        if naija_wins >= total_benchmarks * 0.5:
            report.append(
                "**Recommendation**: NaijaScript shows competitive performance for most tasks"
            )
        else:
            report.append(
                "**Recommendation**: Consider NaijaScript's performance characteristics for your specific use case"
            )

        report.append("")

    report.append("## Performance Numbers")
    report.append("")
    report.append(
        "*Times are from hyperfine with 12 measurements each. Lower is better.*"
    )
    report.append("")

    # Organize by category for clarity
    categories = {
        "Mathematical Operations": [
            "fibonacci_recursive",
            "fibonacci_iterative",
            "prime_count",
        ],
        "String Processing": [
            "string_interpolation",
            "string_concat",
            "string_manipulation",
        ],
        "Built-in Functions": ["math_functions"],
    }

    for category, benchmarks in categories.items():
        category_data = df[df["benchmark"].isin(benchmarks)]
        if category_data.empty:
            continue

        report.append(f"### {category}")
        report.append("")

        for benchmark in benchmarks:
            bench_df = category_data[category_data["benchmark"] == benchmark]
            if bench_df.empty:
                continue

            report.append(f"**{benchmark.replace('_', ' ').title()}**")
            report.append("")

            # Simple, clear table with the actual numbers
            report.append("| Language | Time | vs Fastest |")
            report.append("|----------|------|------------|")

            sorted_bench = bench_df.sort_values("mean_time")
            fastest_time = sorted_bench["mean_time"].iloc[0]

            for _, row in sorted_bench.iterrows():
                lang = row["language"]
                mean_ms = row["mean_time"] * 1000
                std_ms = row["std_time"] * 1000
                relative = row["mean_time"] / fastest_time

                # Format time clearly
                if mean_ms < 1:
                    time_str = f"{mean_ms:.2f}ms"
                else:
                    time_str = f"{mean_ms:.1f}ms"

                # Add uncertainty only if significant
                if std_ms > mean_ms * 0.1:  # Only show if stddev > 10% of mean
                    time_str += f" ± {std_ms:.1f}ms"

                if relative == 1.0:
                    vs_fastest = "**fastest**"
                else:
                    vs_fastest = f"{relative:.1f}x slower"

                report.append(f"| {lang} | {time_str} | {vs_fastest} |")

            report.append("")

    report.append("## Environment")
    report.append("")
    report.append("**Hardware**: Apple M2 processor")
    report.append("")
    report.append("**Software**: NaijaScript 0.11.4, Node.js v24.4.1, Python 3.13.3")
    report.append("")
    report.append(
        "**Methodology**: See [METHODOLOGY.md](../METHODOLOGY.md) for details."
    )

    return "\n".join(report)


def main():
    """Main execution function."""

    # Ensure required directories exist
    CHARTS_DIR.mkdir(exist_ok=True)

    print("Analyzing benchmark results...")

    # Load results
    results = load_benchmark_results()
    if not results:
        print("No benchmark results found. Run ./run_benchmarks.sh first.")
        sys.exit(1)

    print(f"Found {len(results)} benchmark results")

    # Process data
    df = extract_performance_data(results)
    if df.empty:
        print("No performance data could be extracted.")
        sys.exit(1)

    print(f"Processed {len(df)} data points")

    # Create visualizations
    print("Creating performance comparison chart...")
    create_performance_comparison_chart(df)

    print("Creating relative performance chart...")
    create_relative_performance_chart(df)

    # Generate summary report
    print("Generating summary report...")
    report = generate_summary_report(df)

    with open(RESULTS_DIR / "summary.md", "w") as f:
        f.write(report)

    print("✓ Analysis complete!")
    print(f"✓ Charts saved to: {CHARTS_DIR}")
    print(f"✓ Summary report: {RESULTS_DIR}/summary.md")


if __name__ == "__main__":
    main()
