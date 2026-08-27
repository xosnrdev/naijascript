/**
 * High-level, dependency-free TypeScript SDK for the NaijaScript engine.
 *
 * It wraps a `WebAssembly.Instance` of the host-neutral ABI (`wasm-abi`) and
 * exposes the plain-value evaluation surface plus the UI engine's
 * mount -> event -> patch lifecycle. All memory management follows the ABI
 * ownership contract; see `./abi.ts`.
 *
 * The protocol types below mirror the JSON shapes emitted by `wasm-abi/src/ui.rs`
 * and documented in `docs/spec/UI_IR_PROTOCOL.md`. Node/handler ids are decimal
 * strings so 64-bit ids survive JSON; revisions and generations are numbers.
 */

import { Abi, NaijaAbiError } from "./abi.js";
import type { NaijaExports } from "./abi.js";

/** The ABI major version this SDK is written against. */
export const SUPPORTED_ABI_VERSION = 1;

// ----- shared protocol types -----

/** A structured UI error body (see `encode_error_body` in the ABI). */
export interface UiErrorBody {
  category: string;
  code: string;
  message: string;
  recoverability: string;
  span: { start: number; end: number } | null;
}

/** A typed property value as encoded on the wire. */
export type PropValue =
  | { type: "text"; value: string }
  | { type: "number"; value: number }
  | { type: "boolean"; value: boolean }
  | { type: "key"; value: string }
  | { type: "color"; r: number; g: number; b: number; a: number }
  | { type: "length"; px: number }
  | { type: "image"; asset: string }
  | { type: "enum"; enum: string; case: string };

/** An active handler binding: an opaque routing id plus a generation counter. */
export interface HandlerBinding {
  id: string;
  generation: number;
}

/** A UI IR node: either a text leaf or an element with props/handlers/children. */
export type Node = TextNode | ElementNode;

export interface TextNode {
  kind: "text";
  id: string;
  text: string;
}

export interface ElementNode {
  kind: "element";
  id: string;
  primitive: string;
  props: Record<string, PropValue>;
  handlers: Record<string, HandlerBinding>;
  children: Node[];
}

/** A single patch operation. Ids are decimal strings; indexes are numbers. */
export type Op =
  | { op: "insert"; parent: string; index: number; node: Node }
  | { op: "remove"; node: string }
  | { op: "move"; node: string; parent: string; index: number }
  | { op: "replace"; node: string; replacement: Node }
  | { op: "set_property"; node: string; property: string; value: PropValue }
  | { op: "remove_property"; node: string; property: string }
  | { op: "set_text"; node: string; text: string }
  | { op: "bind_handler"; node: string; event_kind: EventKind; binding: HandlerBinding }
  | { op: "unbind_handler"; node: string; event_kind: EventKind };

// ----- eval surface -----

export interface DiagnosticLabel {
  message: string;
  span: { start: number; end: number };
}

export interface Diagnostic {
  severity: "error" | "warning" | "note";
  code: string;
  message: string;
  span: { start: number; end: number } | null;
  labels: DiagnosticLabel[];
}

/** The document returned by {@link NaijaEngine.eval}. */
export interface EvalResult {
  abi_version: number;
  status: "ok" | "diagnostic_error" | "invalid_request" | "engine_error";
  output: string[];
  diagnostics: Diagnostic[];
  filename?: string;
}

// ----- UI surface -----

export type EventKind = "tap" | "change" | "submit" | "focus";

/** An event payload, discriminated by `kind`, matching the ABI decoder. */
export type EventPayload =
  | { kind: "tap" }
  | { kind: "change"; value: string }
  | { kind: "submit"; value?: string }
  | { kind: "focus"; focused: boolean };

/**
 * A dispatch request. `node_id`/`handler_id` may be decimal strings (as emitted
 * in mount/patch docs) or numbers; the ABI accepts both.
 */
export interface DispatchRequest {
  session_id: number;
  node_id: string | number;
  handler_id: string | number;
  handler_generation: number;
  event: EventPayload;
}

/** A successful mount snapshot document. */
export interface MountDoc {
  abi_version: number;
  type: "mount";
  status: "ok";
  session_id: number;
  tree_revision: number;
  tree: Node;
}

/** A successful patch transaction document. */
export interface PatchDoc {
  abi_version: number;
  type: "patch";
  status: "ok";
  session_id: number;
  base_revision: number;
  target_revision: number;
  operations: Op[];
}

/** A stale-handler patch document; the event was safely rejected. */
export interface StalePatchDoc {
  abi_version: number;
  type: "patch";
  status: "stale";
  session_id: number;
  error: UiErrorBody;
}

/** An error document for the mount or patch surface. */
export interface ErrorDoc {
  abi_version: number;
  type: "mount" | "patch";
  status: "error";
  error: UiErrorBody;
}

export type MountResult = MountDoc | ErrorDoc;
export type PatchResult = PatchDoc | StalePatchDoc | ErrorDoc;

// ----- narrowing helpers -----

/** True when a mount succeeded and a tree is available. */
export function isMountOk(result: MountResult): result is MountDoc {
  return result.status === "ok";
}

/** True when a dispatch produced an applicable patch. */
export function isPatchOk(result: PatchResult): result is PatchDoc {
  return result.status === "ok";
}

/** True when a dispatch was rejected as stale (drop the event, keep the tree). */
export function isPatchStale(result: PatchResult): result is StalePatchDoc {
  return result.status === "stale";
}

// ----- engine -----

/**
 * A NaijaScript engine bound to a single WASM instance.
 *
 * The instance is single-threaded and stateful: it retains UI sessions until
 * {@link NaijaEngine.uiClose} is called, so do not share one engine across
 * concurrent async flows that must be isolated.
 */
export class NaijaEngine {
  private readonly abi: Abi;

  private constructor(abi: Abi) {
    this.abi = abi;
  }

  /**
   * Compiles and instantiates the ABI module, then verifies the ABI version.
   *
   * Per the compatibility rule, the host requires an EXACT `abi_version` match.
   */
  static async instantiate(
    wasmBytes: BufferSource,
    imports: WebAssembly.Imports = {},
  ): Promise<NaijaEngine> {
    const module = await WebAssembly.compile(wasmBytes);
    const instance = await WebAssembly.instantiate(module, imports);
    const abi = new Abi(instance.exports as unknown as NaijaExports);
    const version = abi.abiVersion();
    if (version !== SUPPORTED_ABI_VERSION) {
      throw new NaijaAbiError(
        `ABI version mismatch: module is ${version}, SDK requires exactly ${SUPPORTED_ABI_VERSION}`,
      );
    }
    return new NaijaEngine(abi);
  }

  /** The ABI major version reported by the module. */
  abiVersion(): number {
    return this.abi.abiVersion();
  }

  /** Evaluates NaijaScript source and returns its output and diagnostics. */
  eval(source: string, filename?: string): EvalResult {
    const src = this.abi.writeString(source);
    const file = filename === undefined ? { ptr: 0, len: 0 } : this.abi.writeString(filename);
    try {
      const handle = this.abi.exports.ns_eval(src.ptr, src.len, file.ptr, file.len);
      return this.abi.readResultJson<EvalResult>(handle);
    } finally {
      this.abi.freeInput(src);
      this.abi.freeInput(file);
    }
  }

  /**
   * Compiles `.nui` source and mounts a screen, returning a mount snapshot or
   * an error document. `params` is serialized to a JSON object.
   */
  uiMount(source: string, params?: Record<string, unknown>): MountResult {
    const src = this.abi.writeString(source);
    const paramsBuffer =
      params === undefined ? { ptr: 0, len: 0 } : this.abi.writeString(JSON.stringify(params));
    try {
      const handle = this.abi.exports.ns_ui_mount(
        src.ptr,
        src.len,
        paramsBuffer.ptr,
        paramsBuffer.len,
      );
      return this.abi.readResultJson<MountResult>(handle);
    } finally {
      this.abi.freeInput(src);
      this.abi.freeInput(paramsBuffer);
    }
  }

  /** Dispatches an event to a live session, returning a patch/stale/error doc. */
  uiDispatch(request: DispatchRequest): PatchResult {
    const buffer = this.abi.writeString(JSON.stringify(request));
    try {
      const handle = this.abi.exports.ns_ui_dispatch(buffer.ptr, buffer.len);
      return this.abi.readResultJson<PatchResult>(handle);
    } finally {
      this.abi.freeInput(buffer);
    }
  }

  /** Frees a session's retained state. Returns true when a session was closed. */
  uiClose(sessionId: number): boolean {
    return this.abi.exports.ns_ui_close(sessionId >>> 0) === 0;
  }
}

export { Abi, NaijaAbiError } from "./abi.js";
export type { NaijaExports, InputBuffer } from "./abi.js";
