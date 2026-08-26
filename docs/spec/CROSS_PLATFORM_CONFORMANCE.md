# Cross-Platform UI Conformance and SDK Delivery (Stage 6)

Status: release-gate specification draft

Applies to: Web, Jetpack Compose, and SwiftUI SDKs implementing `ui-ir/1`, `ui-patch/1`, and `ui-event/1`

## Conformance fixture model

One renderer-neutral fixture corpus is the normative source for every SDK. Fixtures test logical outcomes and semantic platform mappings; they do not make framework object graphs or screenshots the cross-platform source of truth.

Proposed repository layout:

```text
ui-conformance/
  manifest.json
  schema/
    capabilities-v1.json
    fixture-v1.json
  fixtures/
    compile/
    snapshot/
    patch/
    event/
    identity/
    errors/
    limits/
    security/
    accessibility/
  platform-expectations/
    web/
    compose/
    swiftui/
```

Each fixture directory contains:

```text
case.nui                 # when source compilation is under test
capabilities.logical     # negotiated capability/limit values
input.logical            # snapshot, patch, event, or malformed logical value
expected.logical         # canonical IR/result/error
expected.semantics       # roles, labels, state, order, and event bindings
case.json                # ID, fixture schema version, protocols, tags, rationale
```

`.logical` denotes a canonical human-reviewable fixture notation maintained by the conformance project. It is not a required runtime serialization. Binary and JSON transport packages generate their own encoded vectors from the same logical fixture.

Platform expectation files contain only justified native mappings or declared variances. They MUST reference a fixture ID and capability. A platform cannot silently skip or rewrite a shared expectation.

## Required fixture suites

All SDK release candidates MUST run the same applicable fixture IDs:

1. **Compilation:** screens/components, typed properties, nested elements, text, both conditional branches, keyed lists, state transitions, named handlers, and every required diagnostic.
2. **Snapshots:** every primitive/property/event; empty and maximum-allowed trees; canonical property ordering; Unicode text; accessibility semantics.
3. **Patches:** each operation alone and in combination; operation ordering; keyed insertion/removal/reorder; property/handler replacement; atomic rollback; duplicate delivery; resync.
4. **Events:** each payload variant; controlled input and composition; ordering; exact dispatch; retired/replaced handlers; old revisions with active and stale generations; old sessions.
5. **Identity:** repeated compilation and rendering produce canonical IDs; keyed reorder preserves identity; changed keys replace identity; static sibling slots; duplicate-key rejection.
6. **Errors and limits:** unsupported versions/capabilities, wrong property types, invalid children, missing IDs, revision gaps, all limit boundaries, overflow, malformed Unicode, and deterministic error codes.
7. **Security:** text injection strings remain text; forbidden URI schemes; oversized/deep payloads; arbitrary/unknown fields; replayed events; cross-session IDs; secret redaction.
8. **Platform semantics:** native role/control, accessible name, disabled/focus state, child order, controlled value, and semantic event equivalence for Web, Compose, and SwiftUI.

Every limit fixture is generated from the explicit `Limits` in that case: one below, exactly at, and one above the configured bound where meaningful. This tests boundary behavior without defining product limits in the fixture standard.

## Concrete fixture example

```text
id: patch.keyed-reorder.preserves-identity
fixture_schema: 1
requires: [ui-ir/1, ui-patch/1]

initial children:
  - { id: node-for-key-a, key: "a", text: "Ada" }
  - { id: node-for-key-b, key: "b", text: "Bola" }

patch:
  base_revision: 4
  target_revision: 5
  operations:
    - Move { node: node-for-key-b, parent: list, index: 0 }

expected children:
  - { id: node-for-key-b, key: "b", text: "Bola" }
  - { id: node-for-key-a, key: "a", text: "Ada" }

expected semantics:
  node-for-key-b retains focus, local native control state, and handler generation
```

Actual fixture IDs use canonical opaque `NodeId` test vectors emitted by the identity algorithm; readable names above explain the case only.

## Harness contract

Each SDK supplies a headless conformance adapter with these operations:

```text
negotiate(capabilities) -> SelectedCapabilities | UiError
mount(snapshot) -> Observation
apply(patch) -> Observation
emit(event) -> DispatchObservation
inspect() -> SemanticTree
reset() -> EmptyObservation
```

`Observation` includes acceptance/rejection, current revision, canonical semantic tree, active handler tuples, emitted errors, and resource counters exposed for tests. It MUST NOT depend on private framework type names.

The harness:

- validates fixture metadata and protocol applicability before execution;
- executes fixtures in isolation and in a deterministic order when sequence matters;
- compares stable error codes and structured fields, not localized messages;
- supports deterministic event-loop draining without arbitrary sleeps;
- reports pass, fail, unsupported-optional, or invalid-fixture; and
- treats a missing required capability or skipped required fixture as failure.

Platform integration tests additionally verify actual native accessibility trees, focus/input behavior, and UI-thread application. Visual screenshot tests MAY supplement semantics but MUST NOT replace shared fixtures.

## Artifact strategy

The release pipeline produces independently consumable artifacts from one protocol-compatible source revision:

| Artifact role | Suggested artifact ID | Contents |
| --- | --- | --- |
| Compiler/runtime core | `naijascript-ui-core` | typed IR construction, reconciliation, validation, handler registry; no serialization/framework |
| Logical protocol model | `naijascript-ui-protocol` | generated/native value types, schemas, stable error codes |
| Conformance corpus | `naijascript-ui-conformance` | fixture schemas, vectors, harness contract |
| Web SDK | `naijascript-ui-web` | web renderer plus chosen transport serializer |
| Compose SDK | `naijascript-ui-compose` | Android/Compose renderer plus serializer |
| SwiftUI SDK | `NaijaScriptUI` | Apple/SwiftUI renderer plus serializer |

These are artifact identifiers, not commitments to a package registry or language-specific naming convention. Each ecosystem may adapt casing/grouping while publishing the canonical artifact ID in metadata.

Generated protocol types are built from the versioned logical schema and checked in only when an ecosystem requires it. Hand-written platform types MUST pass the same schema vectors. Core does not import generated serializer code.

Every artifact includes:

- artifact semantic version and immutable source revision;
- supported protocol/IR version ranges;
- fixture schema version and conformance corpus digest;
- capabilities and build-time feature flags;
- toolchain/minimum platform declarations;
- software bill of materials and provenance supported by the release system; and
- a conformance report identifying platform, device/runtime class, fixture digest, pass/fail/skip counts, and failures.

## Versioning and compatibility

Artifact versions use semantic versioning but are distinct from protocol versions:

- A protocol major changes only for incompatible logical semantics or shape.
- A protocol minor is additive and gated by capabilities. Existing negotiated behavior remains unchanged.
- A protocol patch clarifies text or fixes fixtures without changing accepted logical values or outcomes.
- An SDK major may change its public host API even while protocol support is unchanged.
- An SDK minor may add capabilities or support a newer compatible protocol minor.
- An SDK patch may fix rendering/performance while preserving protocol and public behavior.

The release manifest pins an exact conformance corpus digest and records a compatibility matrix rather than assuming equal artifact versions are compatible. SDKs MUST negotiate at runtime when versions can differ. A lockstep release MAY pin exact versions for an application, but is not a protocol rule.

For each supported protocol major, the project maintains:

1. canonical schema and identity vectors;
2. forward/backward negotiation fixtures;
3. a documented support window;
4. migration notes for capability or behavior changes; and
5. explicit end-of-support status.

No artifact is published as stable until all required shared fixtures pass for every claimed platform.

## Mobile performance gates

Performance gates are defined here without claiming unmeasured latency, memory, frame-rate, or throughput values. Numeric budgets belong in a versioned release budget file populated from reproducible measurements on declared reference environments.

Required scenarios:

- cold SDK initialization and first mount;
- warm mount;
- no-op re-render;
- single text/property update;
- controlled text-input update and event round trip;
- keyed list append, remove, and reorder;
- large-tree snapshot and patch at configured limits;
- rapid state updates with batching;
- mount/unmount cycles and handler retirement;
- malformed/over-limit input rejection.

Required metrics:

- end-to-end mount and patch latency distributions;
- UI-thread blocked time and longest contiguous block;
- frame deadline misses/jank metric native to the platform;
- allocations and peak resident/native heap growth;
- retained objects after repeated mount/unmount;
- patch operations and encoded bytes per logical change;
- event-to-handler and handler-to-visible-update latency;
- startup/package size contribution; and
- energy/CPU indicators where stable tooling exists.

The versioned budget file MUST define, per scenario and platform:

```text
reference_environment
build_mode
warmup_and_iteration_method
metric
absolute_budget_or_not_applicable
regression_budget_against_baseline
measurement_tool
variance_policy
```

There are intentionally no default numeric values. Release owners establish the first budgets only after collecting and reviewing repeatable baseline data from physical reference devices. A budget change requires rationale and review; the candidate run cannot rewrite its own baseline.

A mobile SDK release passes performance only when:

- conformance is already passing;
- the benchmark uses the declared release build and reference environment;
- every applicable metric has an approved budget or an explicit reviewed `not_applicable`;
- no scenario exceeds its absolute or regression budget under the declared variance policy;
- repeated mount/unmount shows no statistically/operationally significant retained-growth trend under that policy; and
- benchmark artifacts, raw samples, tool versions, budget file, and comparison report are retained.

An unavailable reference device, missing baseline, flaky harness, or absent budget is `not evaluated`, never a pass. Emulator/simulator results MAY provide early feedback but cannot satisfy physical-device gates unless the approved budget explicitly names that environment.

## Release gate

A platform SDK is publishable only when protocol schemas validate, required fixtures pass, platform semantic integration tests pass, security fixtures pass, the conformance report matches the packaged corpus digest, and applicable performance gates pass. Waivers must be explicit, time-bound, linked to exact artifact versions, and visible in release metadata.
