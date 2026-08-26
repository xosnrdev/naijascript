# ADR 0001: Replace the DOM-shaped VDOM Prototype with a Renderer-Neutral UI Boundary

- Status: Accepted for Stages 2 and 6
- Date: 2026-08-26
- Decision owners: NaijaScript maintainers
- Related specifications: `../spec/UI_DSL.md`, `../spec/UI_IR_PROTOCOL.md`, `../spec/CROSS_PLATFORM_CONFORMANCE.md`

## Context

The initial UI experiment in `src/vdom.rs` modeled a small DOM-shaped virtual tree, while the mock `NaijaEngine` in `src/lib.rs` returned one hard-coded tree. It did not implement reconciliation or renderer validation. The sketch was useful for identifying the desired render/dispatch boundary, but it put web concepts at the architecture boundary and bypassed the interpreter pipeline.

NaijaScript UI must target Web, Jetpack Compose, and SwiftUI. Those systems differ in retained/declarative lifecycle, text input, focus, accessibility, layout, event delivery, and thread rules. Treating one platform's element/tag/attribute vocabulary as the canonical model would force other SDKs to emulate DOM behavior or accumulate platform branches in core. A mock mutation log can test operation ordering, but it cannot by itself define native semantic equivalence, handler lifetime, stale event behavior, protocol compatibility, or accessibility.

Cross-boundary serialization is also required eventually, but the interpreter core currently has no reason to know whether a host is in-process, WebAssembly, FFI, or remote. Selecting a wire format in core would couple language/runtime semantics to transport dependencies and their versioning.

## Decision

We will replace, rather than evolve as the public contract, the DOM-specific `src/vdom.rs`/mock approach.

The stable boundary consists of:

1. a dedicated typed declarative `.nui` source DSL;
2. a renderer-neutral typed UI IR based on semantic primitives and properties;
3. deterministic core-owned node identity and reconciliation;
4. an atomic versioned snapshot/patch protocol;
5. typed event envelopes and a core-owned named-handler registry with generation-based stale rejection;
6. negotiated capabilities and mandatory resource limits; and
7. one cross-platform conformance corpus, with native semantic adapters for Web, Compose, and SwiftUI.

Core owns the logical IR/protocol value types and their validation. Platform SDKs map those semantics to native controls and return typed event data. Platform framework objects and callback closures never cross the boundary.

Serialization stays outside core. Transport/SDK packages encode and decode the logical values using JSON, CBOR, protobuf, an FFI representation, or another reviewed format. All encodings must pass the same canonical logical round-trip, malformed-input, limit, and compatibility fixtures. No encoding is normative over the logical protocol.

The existing prototype may remain temporarily as non-normative migration input or be removed when replacement implementation starts. It must not define names, identity, diff behavior, or compatibility for the new API.

## Rationale

- **Portability:** semantic primitives can map naturally to each platform without making Compose/SwiftUI emulate a DOM.
- **Correctness:** typed properties/events, deterministic IDs, atomic patches, and explicit lifecycle rules create testable behavior beyond mock operation logs.
- **Security:** the boundary carries validated data, not HTML, selectors, source snippets, reflection names, native objects, or callbacks.
- **Accessibility:** native roles, names, state, focus, and input behavior are conformance requirements rather than renderer-specific afterthoughts.
- **Versioning:** logical protocol compatibility can evolve independently from public SDK APIs and transport encodings.
- **Core size and build health:** avoiding serializer/framework dependencies keeps the interpreter usable in native, WASM, embedded, and test configurations.
- **Testing:** one logical fixture corpus catches cross-platform drift while platform integration tests verify native semantics.

## Consequences

Positive:

- Web, Compose, and SwiftUI are peers at the architecture boundary.
- A headless logical renderer can test reconciliation without claiming to represent a platform.
- Hosts can choose in-process or serialized transport without changing language semantics.
- Protocol negotiation, limits, errors, and stale events have deterministic behavior.

Costs:

- The project must maintain a primitive/property capability schema and native mappings.
- SDKs need atomic staging/rollback behavior and semantic inspection adapters for conformance.
- Text input, focus, accessibility, and platform variance require real integration fixtures, not only unit tests.
- Generated or hand-written protocol models may exist in several ecosystems.

## Alternatives considered

### Keep the DOM-shaped VDOM as canonical

Rejected. It favors Web vocabulary, leaks attributes/tags into the contract, and makes native SDK behavior an emulation problem. It also does not solve event generation, negotiated capabilities, or protocol versioning.

### Add platform-specific nodes to one union

Rejected for the MVP. `WebElement | ComposeNode | SwiftUINode` would move platform conditionals into core, prevent one portable fixture expectation, and make applications accidentally platform-dependent.

### Put JSON serialization in core

Rejected. JSON is useful for debug fixtures but does not need to be a runtime semantic dependency. Core ownership of JSON would blur validation and transport, increase dependency/version surface, and constrain FFI or compact mobile transports.

### Standardize a binary format in core

Rejected for the same layering reason. A future standardized encoding can be a separately versioned transport artifact if interoperability requires it; it must still implement the logical protocol.

### Expose renderer callbacks directly

Rejected. Closures/native callbacks are not portable or serializable, obscure lifecycle, and make stale delivery and trust-boundary validation difficult. Named handlers plus opaque IDs and generations provide explicit routing.

## Migration constraints

Implementation work following this ADR must:

- introduce the logical types without importing web or mobile framework types into core;
- keep prototype tests separate from protocol conformance tests;
- establish canonical identity vectors before SDK implementations depend on IDs;
- implement snapshot validation before incremental patches;
- implement handler retirement/stale rejection before enabling events;
- ship capability negotiation and limits with the first SDK, not as later hardening; and
- avoid compatibility promises based on the current prototype.

This ADR decides architecture, not implementation completion. The specifications remain draft until executable fixtures validate all normative behavior.
