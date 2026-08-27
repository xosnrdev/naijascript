/**
 * Low-level bindings for the NaijaScript host-neutral WASM ABI.
 *
 * This module honors the ownership/memory contract exactly:
 *
 *   1. The caller allocates an input buffer with `ns_alloc(len)`, copies bytes
 *      into WASM linear memory at the returned pointer, calls the ABI function,
 *      then MUST call `ns_free(ptr, len)` for every input buffer.
 *   2. The function returns a result handle (u32). The JSON is read via
 *      `ns_result_ptr(handle)` + `ns_result_len(handle)`, decoded as UTF-8,
 *      then released with `ns_result_free(handle)`.
 *
 * Ids in the JSON protocol are decimal strings (for u64 safety); revisions and
 * generations are plain numbers.
 */

/** The exact set of exports the ABI provides. */
export interface NaijaExports {
  readonly memory: WebAssembly.Memory;
  ns_abi_version(): number;
  ns_alloc(len: number): number;
  ns_free(ptr: number, len: number): number;
  ns_eval(
    sourcePtr: number,
    sourceLen: number,
    filenamePtr: number,
    filenameLen: number,
  ): number;
  ns_result_ptr(handle: number): number;
  ns_result_len(handle: number): number;
  ns_result_free(handle: number): number;
  ns_ui_mount(
    sourcePtr: number,
    sourceLen: number,
    paramsPtr: number,
    paramsLen: number,
  ): number;
  ns_ui_dispatch(requestPtr: number, requestLen: number): number;
  ns_ui_close(sessionId: number): number;
}

/** A live input allocation. `{ ptr: 0, len: 0 }` denotes "omitted/empty". */
export interface InputBuffer {
  readonly ptr: number;
  readonly len: number;
}

/** Raised when the ABI reports a contract violation (bad alloc/free/handle). */
export class NaijaAbiError extends Error {
  constructor(message: string) {
    super(message);
    this.name = "NaijaAbiError";
  }
}

const EMPTY: InputBuffer = { ptr: 0, len: 0 };

/**
 * A thin, safe wrapper over the raw exports. It never retains views into
 * linear memory across calls, because `memory.grow` (or any allocation that
 * grows memory) detaches and replaces the underlying `ArrayBuffer`.
 */
export class Abi {
  readonly exports: NaijaExports;
  private readonly encoder = new TextEncoder();
  private readonly decoder = new TextDecoder("utf-8", { fatal: false });

  constructor(exports: NaijaExports) {
    this.exports = exports;
  }

  /** The ABI major version the loaded module implements. */
  abiVersion(): number {
    return this.exports.ns_abi_version() >>> 0;
  }

  /** UTF-8 encodes a string. */
  encodeUtf8(text: string): Uint8Array {
    return this.encoder.encode(text);
  }

  /** UTF-8 decodes bytes. */
  decodeUtf8(bytes: Uint8Array): string {
    return this.decoder.decode(bytes);
  }

  /**
   * Allocates `len` writable bytes in linear memory and returns the pointer.
   * Throws for a nonzero request the engine refused (0, oversized, or OOM).
   */
  alloc(len: number): number {
    if (len === 0) {
      return 0;
    }
    const ptr = this.exports.ns_alloc(len >>> 0) >>> 0;
    if (ptr === 0) {
      throw new NaijaAbiError(`ns_alloc(${len}) failed (zero, oversized, or out of memory)`);
    }
    return ptr;
  }

  /**
   * Frees an input buffer. The `ptr`/`len` MUST exactly match a prior `alloc`.
   * A `{ 0, 0 }` buffer is a no-op. Throws on a mismatch (ABI returns 1).
   */
  free(ptr: number, len: number): void {
    if (ptr === 0 && len === 0) {
      return;
    }
    if (this.exports.ns_free(ptr >>> 0, len >>> 0) !== 0) {
      throw new NaijaAbiError(`ns_free(${ptr}, ${len}) did not match a live allocation`);
    }
  }

  /**
   * Allocates and copies `bytes` into linear memory, returning the live buffer.
   * An empty input yields the sentinel `{ ptr: 0, len: 0 }`, which the ABI
   * accepts for omitted optional inputs (e.g. a missing filename or params).
   */
  writeBytes(bytes: Uint8Array): InputBuffer {
    if (bytes.length === 0) {
      return EMPTY;
    }
    const ptr = this.alloc(bytes.length);
    // Re-read the buffer AFTER alloc: growing memory detaches the old view.
    new Uint8Array(this.exports.memory.buffer, ptr, bytes.length).set(bytes);
    return { ptr, len: bytes.length };
  }

  /** UTF-8 encodes and writes a string as an input buffer. */
  writeString(text: string): InputBuffer {
    return this.writeBytes(this.encodeUtf8(text));
  }

  /** Frees an input buffer produced by {@link writeBytes}/{@link writeString}. */
  freeInput(buffer: InputBuffer): void {
    this.free(buffer.ptr, buffer.len);
  }

  /**
   * Reads a result handle's immutable JSON bytes, decodes them as UTF-8, and
   * releases the handle. Always frees, even if decoding throws.
   */
  readResult(handle: number): string {
    handle = handle >>> 0;
    if (handle === 0) {
      throw new NaijaAbiError("ABI returned a null result handle");
    }
    try {
      const ptr = this.exports.ns_result_ptr(handle) >>> 0;
      const len = this.exports.ns_result_len(handle) >>> 0;
      if (ptr === 0 && len === 0) {
        throw new NaijaAbiError(`result handle ${handle} is unknown or empty`);
      }
      // Copy out before freeing so the returned data cannot be invalidated.
      const view = new Uint8Array(this.exports.memory.buffer, ptr, len);
      const copy = view.slice();
      return this.decodeUtf8(copy);
    } finally {
      this.exports.ns_result_free(handle);
    }
  }

  /**
   * Reads and JSON-parses a result handle. See {@link readResult}.
   */
  readResultJson<T>(handle: number): T {
    return JSON.parse(this.readResult(handle)) as T;
  }
}
