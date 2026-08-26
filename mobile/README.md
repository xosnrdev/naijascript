# Mobile embedding spike

Both mobile sketches embed the exact
`naijascript_wasm_abi.wasm` artifact produced by `wasm-abi/`; there is no
platform-specific language build. The host runtime adapter must provide:

- instantiate bytes with no imports and expose the module's `memory`;
- invoke exported functions with `i32` arguments/results;
- bounds-checked memory read/write;
- instance-level serialization and execution limits.

The documented baseline is **Wasmtime using its Pulley interpreter target**.
Pulley avoids executable-memory/JIT requirements and keeps semantics aligned
with Wasmtime. Build Wasmtime's C API for each mobile architecture with Pulley
enabled, configure the engine for the Pulley target (currently `pulley64` for
64-bit hosts), disable unused compiler backends, and expose a narrow JNI or
Swift/Objective-C bridge. Pin a tested Wasmtime release: target/configuration
spellings are runtime-version-specific.

Smaller alternatives worth measuring are WAMR in interpreter mode and wasm3.
On Apple platforms, WasmKit is another interpreter-oriented option. They may
reduce binary size but need separate checks for spec coverage, maintenance,
fuel/interrupt support, memory limits, and C/Swift/Kotlin integration. The ABI
does not depend on any of them.

Recommended packaging:

- Android: place the wasm at
  `app/src/main/assets/naijascript_wasm_abi.wasm`.
- iOS: add the same file to the application target's Copy Bundle Resources
  phase.

Do not enable NaijaScript process execution in a mobile adapter. The
`wasm32-unknown-unknown` engine uses the repository's wasm host policy, and the
mobile runtime should instantiate with no imports.

See the platform directories for host-facing skeletons and
`../wasm-abi/README.md` for ownership details.
