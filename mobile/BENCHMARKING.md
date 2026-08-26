# Mobile ABI benchmark protocol

The repository does not include benchmark results. Generate and retain raw JSON
for every tested device/runtime combination.

## Desktop smoke baseline

Build the module, install the matching Python binding (`python -m pip install
wasmtime`), then run:

```sh
python scripts/benchmark_wasm_abi.py \
  target/wasm32-unknown-unknown/release/naijascript_wasm_abi.wasm \
  examples/fibonacci.ns \
  --target pulley64 --warmup 20 --iterations 200
```

If the installed Wasmtime build does not recognize `pulley64`, use a release
with Pulley enabled or omit `--target` only for a clearly labelled
non-Pulley comparison. The harness measures the complete warm-instance ABI
round trip: input allocation/copy, pipeline evaluation, JSON copy/decode, and
free. `setup_ns` separately records engine/module/instance setup.

## Device protocol

For each physical Android/iOS device:

1. Record device model, OS, runtime name/version/build options, wasm SHA-256,
   app optimization mode, thermal state, and power mode.
2. Use one release-mode app and identical source fixtures copied into the app.
3. Record cold engine creation/module load/instantiation separately.
4. Warm one instance with 20 unrecorded evaluations, then record at least 200
   serialized evaluations. Also test fresh-instance latency separately.
5. Consume the JSON result each time so work cannot be elided. Verify status,
   output count, and diagnostic count before accepting a sample.
6. Report raw nanoseconds plus median, p95, min, and max. Record wasm size and
   added compressed/uncompressed application size.
7. Repeat at least five process launches, randomize runtime order, and pause
   when thermal throttling is observed.
8. Apply the same memory/fuel/time limits to every runtime where supported.

Use representative small, medium, error-producing, allocation-heavy, and
recursion fixtures. Do not compare a Pulley interpreter against a JIT without
labeling execution mode. Do not publish desktop numbers as mobile results.
