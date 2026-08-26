# Renderer-Neutral UI IR and Protocol (Stages 2 and 6)

Status: MVP specification draft

IR version: `ui-ir/1`

Patch protocol version: `ui-patch/1`

Event protocol version: `ui-event/1`

`name/major` denotes minor version zero (for example, `ui-patch/1` is `ui-patch/1.0`). Messages carry the selected major and minor even when documents use the short form. Patch-level specification/fixture corrections do not change the wire/logical version because they cannot change accepted values or outcomes.

## Boundary and ownership

The NaijaScript UI compiler and runtime own typed tree construction, reconciliation, state, handler registration, identity, validation, and protocol semantics. Platform SDKs own rendering and event transport. Neither side exposes DOM nodes, Android `View` objects, Compose functions, nor SwiftUI views across the boundary.

The core library produces and consumes the logical values defined here. It MUST NOT depend on JSON, CBOR, protobuf, `serde`, browser APIs, or mobile UI frameworks. A transport adapter outside core encodes these values and MUST prove round-trip equivalence with the logical model.

## Capability negotiation and limits

Before mounting, the host sends a capability document:

```text
Capabilities {
  protocol_versions: Set<ProtocolVersion>
  ir_versions: Set<IrVersion>
  primitives: Map<PrimitiveKind, PrimitiveCapability>
  event_kinds: Set<EventKind>
  limits: Limits
  extensions: Set<ExtensionId>
}

Limits {
  max_nodes: PositiveInteger
  max_depth: PositiveInteger
  max_children_per_node: PositiveInteger
  max_props_per_node: PositiveInteger
  max_text_bytes: PositiveInteger
  max_patch_ops: PositiveInteger
  max_event_bytes: PositiveInteger
  max_queued_events: PositiveInteger
}
```

The runtime selects exactly one mutually supported major/minor version and a capability subset before the first snapshot. There is no implicit default. An unsupported major version, primitive, property, event kind, or required extension fails before mount. Unknown optional extensions MAY be ignored only when explicitly marked optional in the document.

Every limit is mandatory, positive, and host-configured. Builds MUST publish their chosen values with conformance results. The runtime checks limits while constructing IR; the SDK checks them again before allocating platform objects. Limit violations reject the whole snapshot or patch transaction.

## Typed UI IR

The IR is an immutable ordered tree:

```text
UiTree {
  ir_version: IrVersion
  root: Node
}

Node =
  ElementNode {
    id: NodeId
    kind: PrimitiveKind
    props: Map<PropertyId, PropertyValue>
    handlers: Map<EventKind, HandlerBinding>
    children: List<Node>
  }
| TextNode {
    id: NodeId
    text: Text
  }

PropertyValue =
  Text(Text)
| Number(FiniteNumber)
| Boolean(Boolean)
| Color(Color)
| Length(Length)
| ImageSource(ImageSource)
| Key(Key)
| Enum(EnumType, EnumCase)
| Optional(PropertyValue)

HandlerBinding {
  id: HandlerId
  generation: HandlerGeneration
  event_kind: EventKind
}
```

`PrimitiveKind`, `PropertyId`, event compatibility, allowed children, defaults, and enum cases are defined in the negotiated schema. Names such as `stack`, `button`, and `a11yLabel` are schema identifiers, not web element or CSS names.

`FiniteNumber` excludes NaN and infinities. `Color` is an explicit color-space-qualified value. `Length` is a tagged value such as logical pixels, percent, or intrinsic; a host MUST reject unsupported units during negotiation. `ImageSource` is a validated application asset identifier or policy-approved URI, never executable platform code.

Conditions and list constructs do not appear in IR: the runtime evaluates them and emits the selected, ordered nodes. Component boundaries MAY be retained in debug metadata, but metadata cannot affect rendering and is not part of identity.

## Deterministic identity

A `NodeId` is an opaque, stable value derived by core from:

1. the mounted screen instance ID;
2. the static compiler-assigned child slot at each component/element boundary; and
3. each explicit list or element key on the path.

The derivation algorithm and canonical encoding are part of `ui-ir/1` conformance data. It MUST be deterministic across process runs, operating systems, architectures, hash randomization, and map iteration order. It MUST NOT contain a memory address, renderer object ID, source path, user text, array index for a dynamic keyed item, or a secret.

Static unkeyed siblings receive compiler slots and remain stable while their source positions remain semantically the same. Dynamic lists require unique explicit keys. Reordering keyed items preserves IDs and produces moves; changing a key creates a different identity. Duplicate sibling IDs or keys invalidate the complete render transaction.

## Versioned snapshot and patch protocol

An initial mount or explicit resynchronization sends:

```text
Snapshot {
  protocol_version: "ui-patch/1"
  ir_version: "ui-ir/1"
  session_id: SessionId
  tree_revision: Revision
  tree: UiTree
}
```

Subsequent updates are atomic ordered transactions:

```text
Patch {
  protocol_version: "ui-patch/1"
  session_id: SessionId
  base_revision: Revision
  target_revision: Revision
  operations: List<PatchOp>
}

PatchOp =
  Insert { parent: NodeId, index: Index, node: Node }
| Remove { node: NodeId }
| Move { node: NodeId, parent: NodeId, index: Index }
| Replace { node: NodeId, replacement: Node }
| SetProperty { node: NodeId, property: PropertyId, value: PropertyValue }
| RemoveProperty { node: NodeId, property: PropertyId }
| SetText { node: NodeId, text: Text }
| BindHandler { node: NodeId, event_kind: EventKind, binding: HandlerBinding }
| UnbindHandler { node: NodeId, event_kind: EventKind }
```

Rules:

- Revisions are monotonically increasing unsigned logical counters scoped to `session_id`; `target_revision` MUST be greater than `base_revision`.
- The receiver applies operations in list order to a staging tree, validates the result, and publishes it only if every operation succeeds.
- An operation references the tree produced by all preceding operations in that transaction.
- Invalid indexes, missing IDs, duplicate IDs, schema/type errors, limit violations, base-revision mismatch, and arithmetic overflow reject the entire patch without visible partial application.
- A patch MUST NOT be retried against a different base revision. On rejection the sender issues a complete snapshot with a new revision, or terminates the session according to host policy.
- Duplicate delivery of an already accepted `(session_id, target_revision)` is acknowledged without reapplying. A conflicting payload for the same tuple is a protocol violation.
- Property maps have canonical ordering for hashing/encoding even if a language implementation stores them unordered.
- Major versions are incompatible. A minor additive feature requires capability negotiation and MUST preserve old semantics. Removing or changing a field/type requires a new major version.

## Typed event envelope

Renderers emit data, never callbacks or executable code:

```text
UiEvent {
  protocol_version: "ui-event/1"
  session_id: SessionId
  observed_revision: Revision
  sequence: EventSequence
  timestamp: MonotonicInstant?
  node_id: NodeId
  handler_id: HandlerId
  handler_generation: HandlerGeneration
  kind: EventKind
  payload: EventPayload
}

EventPayload =
  Tap { pointer: PointerKind, modifiers: ModifierSet }
| Change { value: Text, composing: Boolean }
| Submit { value: Text? }
| Focus { focused: Boolean }
```

Payload variants and fields are closed for a protocol major version and type-checked against `kind`. Coordinates, raw key codes, platform objects, and arbitrary maps are absent from MVP. `timestamp` is advisory and MUST NOT determine ordering; `sequence` is a monotonically increasing host counter scoped to the session.

The runtime validates, in order: envelope size, protocol/session, sequence policy, node existence, current binding for `kind`, exact handler ID and generation, payload shape, and payload limits. Invalid events do not execute user code.

## Handler lifecycle and stale-event rejection

Core allocates a `HandlerId` for a logical named handler binding and a monotonically increasing `HandlerGeneration`. A binding is active only after the snapshot/patch that contains it is accepted.

- Re-rendering the same named handler at the same node with unchanged capture identity MAY preserve both values.
- Replacing a handler, changing captures, remounting its component, unbinding it, or removing its node retires the current generation.
- IDs MAY be reused only with a strictly newer generation in the same session. Generation overflow terminates the session; wrapping is forbidden.
- Core keeps no renderer callback. Its registry maps the active `(session, node, event kind, handler ID, generation)` tuple to a runtime handler.
- An event whose node/binding is absent, retired, newer than core knows, or older than the active generation is stale and MUST be rejected.
- Events observed against an older tree revision MAY execute only if that exact binding tuple is still active and host ordering policy accepts the sequence. Revision equality alone is not a substitute for generation validation.
- Session termination retires every binding. Events from another or a previous session are rejected even if all other fields match.

Stale rejection returns a structured `StaleHandler` result for diagnostics/telemetry and MUST NOT be surfaced as an application exception by default.

## Error model

Every failure has a stable category and machine-readable code:

```text
UiError {
  category: Compile | Negotiation | Validation | Protocol | Limit | Renderer | Security
  code: StableErrorCode
  message: Text
  location: SourceRange?
  node_id: NodeId?
  revision: Revision?
  recoverability: Fatal | Resync | DropEvent
}
```

Compile and negotiation errors prevent mount. Snapshot validation and fatal security errors terminate the session without publishing UI. Patch validation errors preserve the last accepted tree and request resynchronization. Invalid/stale events are dropped and recorded. A platform rendering failure MUST preserve or restore the last committed platform tree when the SDK can do so; otherwise it marks the mount failed and requests a fresh snapshot. User-visible fallback UI is an application/host policy, not synthesized by core.

Diagnostics MUST redact secret property values and untrusted event payloads. Logging the full tree or envelope is opt-in development behavior.

## Security constraints

- Treat source-provided text, image references, transport bytes, event payloads, and SDK messages as untrusted.
- Validate schema, types, lengths, nesting, counts, integer bounds, enum cases, UTF encoding, and canonical form before allocation or dispatch.
- No IR field may contain code, selector strings, HTML, CSS, JavaScript, native class names, reflection targets, filesystem paths, or serialized native objects.
- Text is rendered as text. Web adapters MUST use text-node/property APIs, never `innerHTML`. URI/image loading uses a host allowlist and platform-safe decoders.
- Event payloads expose the minimum semantic data. Password/secret inputs require a future capability with redaction rules and are not MVP inputs.
- Handler IDs and Node IDs are routing identifiers, not authorization tokens. Host actions called by handlers must perform their own authorization.
- Renderers MUST confine all UI mutation to the platform UI thread and MUST prevent re-entrant application of transactions.
- Transport adapters MUST authenticate local/remote peers when crossing a trust boundary and SHOULD provide integrity and replay protection. The logical protocol does not prescribe a transport.
- Resource limits apply before recursive descent or bulk allocation. Failure is deterministic and atomic.

## Platform mapping

Mapping is semantic. A platform SDK MAY choose a different concrete widget when required for accessibility or platform conventions, but it MUST preserve the negotiated behavior and conformance output.

| IR primitive/semantic | Web | Jetpack Compose | SwiftUI |
| --- | --- | --- | --- |
| `stack` | styled neutral container with column layout | `Column` | `VStack` |
| `row` | styled neutral container with row layout | `Row` | `HStack` |
| `scroll` | overflow container | scrolling container appropriate to axis | `ScrollView` |
| `label` / text child | text node in semantic element | `Text` | `Text` |
| `button` / tap | native `<button>` plus listener | `Button` | `Button` |
| `input` / change, submit, focus | controlled `<input>` plus semantic listeners | controlled `TextField` | controlled `TextField` |
| `image` | `<img>` with policy-approved source and alt semantics | `Image`/approved loader | `Image`/approved loader |
| `spacer` | non-semantic layout box | `Spacer` | `Spacer` |
| `a11yLabel` | ARIA/native accessible name | semantics/content description | accessibility label |
| `enabled` | native disabled semantics | enabled state | disabled state |

SDKs MUST use logical `Length`, `Color`, alignment, focus, text-input composition, and accessibility adapters rather than exposing CSS or framework modifiers in IR. Platform defaults that affect observable behavior MUST be normalized in the capability schema or recorded as an allowed platform variance in conformance fixtures.

## Serialization adapter requirements

A serializer package lives beside each transport/SDK, not in the interpreter core. It must:

1. declare the logical protocol versions it encodes;
2. reject unknown required fields and non-canonical values;
3. preserve opaque IDs and integer precision;
4. enforce limits before decoding nested values;
5. pass canonical fixture round trips and malformed-input fixtures; and
6. never add transport-specific values to UI IR.

JSON MAY be used for debugging and fixtures; a binary encoding MAY be used in production. Encoding choice cannot alter revisions, identity, ordering, validation, or error behavior.
