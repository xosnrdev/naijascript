package dev.naijascript.mobile

import android.content.Context
import org.json.JSONObject

/**
 * Proof-of-concept Android facade.
 *
 * NativeWasmAbi is intentionally tiny: its JNI implementation can use
 * Wasmtime/Pulley, WAMR, or wasm3 without changing application code.
 */
class NaijaScriptEngine private constructor(
    private val runtime: NativeWasmAbi,
) : AutoCloseable {
    companion object {
        fun fromAssets(context: Context): NaijaScriptEngine {
            val wasm = context.assets.open("naijascript_wasm_abi.wasm").use { it.readBytes() }
            return NaijaScriptEngine(NativeWasmAbi(wasm))
        }
    }

    @Synchronized
    fun evaluate(source: String, filename: String = "<android>"): JSONObject {
        return JSONObject(runtime.evaluate(source.encodeToByteArray(), filename.encodeToByteArray()))
    }

    override fun close() = runtime.close()
}

/**
 * JNI owns one runtime store and one module instance. The native implementation
 * must perform the ns_alloc/write/ns_eval/read/free sequence from wasm-abi.
 */
private class NativeWasmAbi(wasm: ByteArray) : AutoCloseable {
    private var handle: Long = nativeCreate(wasm)

    init {
        check(handle != 0L) { "Unable to instantiate NaijaScript WebAssembly module" }
        check(nativeAbiVersion(handle) == 1) { "Unsupported NaijaScript ABI" }
    }

    fun evaluate(source: ByteArray, filename: ByteArray): String {
        check(handle != 0L) { "Engine is closed" }
        return nativeEvaluate(handle, source, filename)
    }

    override fun close() {
        val current = handle
        handle = 0
        if (current != 0L) nativeDestroy(current)
    }

    private external fun nativeCreate(wasm: ByteArray): Long
    private external fun nativeAbiVersion(handle: Long): Int
    private external fun nativeEvaluate(handle: Long, source: ByteArray, filename: ByteArray): String
    private external fun nativeDestroy(handle: Long)
}
