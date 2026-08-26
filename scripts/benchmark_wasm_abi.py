#!/usr/bin/env python3
"""Repeatable local timing harness for the host-neutral NaijaScript ABI."""

from __future__ import annotations

import argparse
import json
import statistics
import time
from pathlib import Path

import wasmtime


def percentile(values: list[int], fraction: float) -> int:
    ordered = sorted(values)
    return ordered[min(len(ordered) - 1, int((len(ordered) - 1) * fraction))]


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("wasm", type=Path)
    parser.add_argument("source", type=Path)
    parser.add_argument("--iterations", type=int, default=100)
    parser.add_argument("--warmup", type=int, default=10)
    parser.add_argument(
        "--target",
        help="Wasmtime target, for example pulley64 (support is version-dependent)",
    )
    args = parser.parse_args()
    if args.iterations < 1 or args.warmup < 0:
        parser.error("iterations must be positive and warmup cannot be negative")

    source = args.source.read_bytes()
    filename = args.source.name.encode()
    if not source:
        parser.error("source must not be empty")

    config = wasmtime.Config()
    if args.target:
        config.target = args.target

    started = time.perf_counter_ns()
    engine = wasmtime.Engine(config)
    module = wasmtime.Module.from_file(engine, str(args.wasm))
    store = wasmtime.Store(engine)
    instance = wasmtime.Instance(store, module, [])
    setup_ns = time.perf_counter_ns() - started

    exports = instance.exports(store)
    memory = exports["memory"]
    alloc = exports["ns_alloc"]
    free = exports["ns_free"]
    evaluate = exports["ns_eval"]
    result_ptr = exports["ns_result_ptr"]
    result_len = exports["ns_result_len"]
    result_free = exports["ns_result_free"]
    version = exports["ns_abi_version"](store)
    if version != 1:
        raise RuntimeError(f"unsupported ABI version {version}")

    def run_once() -> dict[str, object]:
        source_ptr = alloc(store, len(source))
        filename_ptr = alloc(store, len(filename))
        if source_ptr == 0 or filename_ptr == 0:
            raise MemoryError("module input allocation failed")
        handle = 0
        try:
            memory.write(store, source, source_ptr)
            memory.write(store, filename, filename_ptr)
            handle = evaluate(store, source_ptr, len(source), filename_ptr, len(filename))
            ptr = result_ptr(store, handle)
            length = result_len(store, handle)
            if ptr == 0 or length == 0:
                raise RuntimeError("module returned an invalid result")
            document = json.loads(bytes(memory.read(store, ptr, ptr + length)))
            if document["status"] not in ("ok", "diagnostic_error"):
                raise RuntimeError(f"evaluation failed: {document}")
            return document
        finally:
            if handle:
                result_free(store, handle)
            free(store, source_ptr, len(source))
            free(store, filename_ptr, len(filename))

    for _ in range(args.warmup):
        run_once()

    samples: list[int] = []
    last_document: dict[str, object] = {}
    for _ in range(args.iterations):
        started = time.perf_counter_ns()
        last_document = run_once()
        samples.append(time.perf_counter_ns() - started)

    report = {
        "wasm": str(args.wasm),
        "wasm_bytes": args.wasm.stat().st_size,
        "source": str(args.source),
        "source_bytes": len(source),
        "target": args.target or "wasmtime-default",
        "warmup_iterations": args.warmup,
        "measured_iterations": args.iterations,
        "setup_ns": setup_ns,
        "evaluation_ns": {
            "min": min(samples),
            "median": int(statistics.median(samples)),
            "p95": percentile(samples, 0.95),
            "max": max(samples),
        },
        "last_status": last_document["status"],
        "last_output_items": len(last_document["output"]),
        "last_diagnostic_items": len(last_document["diagnostics"]),
    }
    print(json.dumps(report, indent=2))


if __name__ == "__main__":
    main()
