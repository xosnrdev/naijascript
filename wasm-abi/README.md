# NaijaScript host-neutral WebAssembly ABI

This crate builds a plain `wasm32-unknown-unknown` module. It is separate from
the `wasm/` browser crate and imports neither `wasm-bindgen` nor WASI. A host
only needs WebAssembly functions and exported linear memory.

Build:

```sh
cargo build -p naijascript-wasm-abi --release --target wasm32-unknown-unknown
```

The artifact is
`target/wasm32-unknown-unknown/release/naijascript_wasm_abi.wasm`.

## ABI v1

All parameters and return values are WebAssembly `i32`; the C declarations use
`uint32_t` to make signedness explicit.

```c
uint32_t ns_abi_version(void);
uint32_t ns_alloc(uint32_t len);
uint32_t ns_free(uint32_t ptr, uint32_t len);
uint32_t ns_eval(uint32_t source_ptr, uint32_t source_len,
                 uint32_t filename_ptr, uint32_t filename_len);
uint32_t ns_result_ptr(uint32_t handle);
uint32_t ns_result_len(uint32_t handle);
uint32_t ns_result_free(uint32_t handle);
```

Host call sequence:

1. Require `ns_abi_version() == 1`.
2. Allocate source and optional filename with `ns_alloc`, then copy bytes into
   exported `memory`. Allocation failure is returned as zero.
3. Call `ns_eval`. `(0, 0)` means no filename. Source must be a non-empty live
   allocation. The returned nonzero value is an opaque result handle.
4. Read exactly `ns_result_len(handle)` bytes beginning at
   `ns_result_ptr(handle)` and decode them as UTF-8 JSON.
5. Call `ns_result_free(handle)`. Separately call `ns_free` for every input.

Input allocations remain host-owned across `ns_eval`. `ns_free` succeeds only
when pointer and length exactly match a live allocation. Evaluation copies
registered input bytes before parsing and never constructs a Rust slice from
an arbitrary host address. Result bytes are module-owned, immutable, and valid
only until their handle is freed. A stale/unknown handle returns pointer and
length zero. Status-returning free functions return zero on success and one on
invalid ownership metadata.

The JSON shape is:

```json
{
  "abi_version": 1,
  "status": "ok",
  "filename": "example.ns",
  "output": ["one printed value", "another"],
  "diagnostics": [
    {
      "severity": "warning",
      "code": "example",
      "message": "description",
      "span": {"start": 0, "end": 4},
      "labels": [
        {"message": "detail", "span": {"start": 0, "end": 4}}
      ]
    }
  ]
}
```

`status` is one of `ok`, `diagnostic_error`, `invalid_request`, or
`engine_error`. Spans are UTF-8 byte offsets into source. `filename` is present
for evaluations that enter the engine. Output values use the interpreter's
existing display representation.

## Limits and safety

- A single allocation is capped at 16 MiB, source at 8 MiB, and filename at
  4 KiB.
- WebAssembly still traps on exhausted linear memory, Rust panics, stack
  exhaustion beyond what the engine catches, or a hostile host writing outside
  its allocation. No in-module ABI can protect its instance from a host that
  directly mutates arbitrary exported memory.
- The current interpreter scratch arenas are process-global. Calls into one
  instance must be serialized and must not re-enter `ns_eval`.
- Result handles are instance-local. Do not move pointers or handles between
  instances.
- Hosts should cap fuel/epochs, wall time, and module memory using their runtime
  APIs. ABI v1 itself does not expose cancellation.
- A future incompatible contract increments `ns_abi_version`; hosts must reject
  unknown versions rather than guessing compatibility.
