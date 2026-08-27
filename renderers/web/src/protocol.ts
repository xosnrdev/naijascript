/**
 * TypeScript types for the NaijaScript renderer-neutral UI protocol.
 *
 * These mirror the JSON wire form emitted by the Rust engine (see
 * `docs/spec/UI_IR_PROTOCOL.md`). The protocol is intentionally renderer
 * neutral: it carries logical values, not DOM/CSS/native constructs.
 *
 * Node ids and handler ids are u64 values that JSON encodes as decimal
 * strings so precision is never lost. We therefore model them as `string`
 * and never coerce them to `number`.
 */

/** Decimal-string encoding of a u64 node id. */
export type NodeId = string;

/** Decimal-string encoding of a u64 handler id. */
export type HandlerId = string;

/** The set of primitive element kinds understood by renderers. */
export type Primitive =
  | "stack"
  | "row"
  | "scroll"
  | "label"
  | "button"
  | "input"
  | "image"
  | "spacer";

/** Event kinds a handler may be bound to. */
export type EventKind = "tap" | "change" | "submit" | "focus";

/**
 * A tagged property value. The `type` discriminant selects the shape.
 */
export type PropValue =
  | { type: "text"; value: string }
  | { type: "number"; value: number }
  | { type: "boolean"; value: boolean }
  | { type: "key"; value: string }
  | { type: "color"; r: number; g: number; b: number; a: number }
  | { type: "length"; px: number }
  | { type: "image"; asset: string }
  | { type: "enum"; enum: string; case: string };

/** A resolved handler binding: which handler and which generation. */
export interface HandlerBinding {
  id: HandlerId;
  generation: number;
}

/** Map of property name -> value on an element node. */
export type PropMap = Record<string, PropValue>;

/** Map of event kind -> handler binding on an element node. */
export type HandlerMap = Partial<Record<EventKind, HandlerBinding>>;

/** An element node in the UI tree. */
export interface ElementNode {
  kind: "element";
  id: NodeId;
  primitive: Primitive;
  props: PropMap;
  handlers: HandlerMap;
  children: Node[];
}

/** A text leaf node in the UI tree. */
export interface TextNode {
  kind: "text";
  id: NodeId;
  text: string;
}

/** Either an element or a text node. */
export type Node = ElementNode | TextNode;

/** Initial mount document carrying the full tree at a revision. */
export interface MountDoc {
  abi_version: number;
  type: "mount";
  status: "ok";
  session_id: number;
  tree_revision: number;
  tree: Node;
}

/** A single patch operation, tagged by `op`. */
export type Op =
  | { op: "insert"; parent: NodeId; index: number; node: Node }
  | { op: "remove"; node: NodeId }
  | { op: "move"; node: NodeId; parent: NodeId; index: number }
  | { op: "replace"; node: NodeId; replacement: Node }
  | { op: "set_property"; node: NodeId; property: string; value: PropValue }
  | { op: "remove_property"; node: NodeId; property: string }
  | { op: "set_text"; node: NodeId; text: string }
  | {
      op: "bind_handler";
      node: NodeId;
      event_kind: EventKind;
      binding: HandlerBinding;
    }
  | { op: "unbind_handler"; node: NodeId; event_kind: EventKind };

/** A patch document: an ordered, atomic transaction of operations. */
export interface PatchDoc {
  abi_version: number;
  type: "patch";
  status: "ok" | "stale";
  session_id: number;
  base_revision: number;
  target_revision: number;
  operations: Op[];
}

/** One scripted step in a conformance fixture. */
export interface FixtureStep {
  action: unknown;
  status: string;
  patch?: PatchDoc;
}

/** A full golden conformance fixture. */
export interface GoldenFixture {
  protocol_version: number;
  status: "ok";
  mount: MountDoc;
  steps: FixtureStep[];
  final_tree: Node;
}

/**
 * A plain-data UI event as emitted by a renderer across the boundary.
 * Renderers emit data, never callbacks or executable code.
 */
export interface UiEvent {
  session_id: number;
  node_id: NodeId;
  handler_id: HandlerId;
  handler_generation: number;
  event: UiEventPayload;
}

/** The closed set of event payloads, discriminated by `kind`. */
export type UiEventPayload =
  | { kind: "tap" }
  | { kind: "change"; value: string }
  | { kind: "submit"; value?: string }
  | { kind: "focus"; focused: boolean };
