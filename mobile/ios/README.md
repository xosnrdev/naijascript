# iOS proof of concept

Add `NaijaScriptEngine.swift` and the wasm artifact to an iOS target. Supply a
`WasmRuntimeAdapter` backed by a Pulley-enabled Wasmtime C API build, WasmKit,
WAMR, or wasm3. The actor serializes evaluation on one instance.

`evaluateABI` implements the allocation/evaluation/result sequence in
`../../wasm-abi/README.md`. It must validate every memory offset plus length
against the runtime's current memory size before reading or writing. Copy result
bytes before `ns_result_free`; free inputs on every non-trapping path. Convert a
trap to `NaijaScriptEngine.EngineError.runtime` and replace the trapped instance
before reuse.

For Wasmtime, configure the engine for its Pulley target and apply store
resource/fuel/epoch limits in the C/Objective-C++ adapter. Pulley is the
baseline because iOS generally prohibits application JIT code, but confirm the
selected Wasmtime version and App Store constraints. Do not ship Cranelift or
Winch unless the application has an applicable executable-memory entitlement.

This intentionally stops at a runtime protocol: XCFramework creation, C module
maps, and runtime lifecycle APIs differ substantially across runtime releases.
The same Swift facade and wasm resource work with each candidate.
