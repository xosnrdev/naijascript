import Foundation

/// Runtime-neutral application facade for the bundled host-neutral wasm.
public actor NaijaScriptEngine {
    public enum EngineError: Error {
        case missingModule
        case unsupportedABI(UInt32)
        case runtime(String)
        case invalidJSON
    }

    private let runtime: WasmRuntimeAdapter

    public init(
        bundle: Bundle = .main,
        runtimeFactory: (Data) throws -> WasmRuntimeAdapter
    ) throws {
        guard let url = bundle.url(
            forResource: "naijascript_wasm_abi",
            withExtension: "wasm"
        ) else {
            throw EngineError.missingModule
        }
        runtime = try runtimeFactory(Data(contentsOf: url))
        let version = try runtime.call("ns_abi_version", [])
        guard version == 1 else { throw EngineError.unsupportedABI(version) }
    }

    public func evaluate(
        _ source: String,
        filename: String = "<ios>"
    ) throws -> [String: Any] {
        let json = try runtime.evaluateABI(
            source: Data(source.utf8),
            filename: Data(filename.utf8)
        )
        guard let value = try JSONSerialization.jsonObject(with: json) as? [String: Any] else {
            throw EngineError.invalidJSON
        }
        return value
    }
}

/// Implement this protocol around Wasmtime/Pulley, WAMR, wasm3, or WasmKit.
/// One adapter owns exactly one instance and performs bounds-checked memory IO.
public protocol WasmRuntimeAdapter: AnyObject, Sendable {
    func call(_ export: String, _ arguments: [UInt32]) throws -> UInt32
    func evaluateABI(source: Data, filename: Data) throws -> Data
}
