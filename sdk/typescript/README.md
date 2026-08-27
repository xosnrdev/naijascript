# @naijascript/sdk

A thin, **dependency-free** TypeScript wrapper around the NaijaScript
host-neutral WebAssembly ABI (the `wasm-abi` crate). It exposes:

- the plain-value evaluation surface (`eval`), and
- the UI engine's **mount → event → patch** lifecycle (`uiMount`, `uiDispatch`,
  `uiClose`).

The wrapper honors the ABI's ownership/memory contract for you: every input
buffer is allocated with `ns_alloc`, copied into linear memory, and freed with
`ns_free`; every result handle is read via `ns_result_ptr`/`ns_result_len` and
released with `ns_result_free`.

> Compatibility rule: the SDK requires an **exact** `abi_version` match
> (currently `1`). `instantiate` throws if the loaded module reports a
> different version.

## Install / build

```bash
npm install
npm run typecheck   # tsc --noEmit
npm run build       # tsc -> dist/
```

There are no runtime dependencies. TypeScript is the only dev dependency.

## Usage: mount a counter, tap it, apply the patch

The example below mounts the `counter` conformance fixture, taps the
`Increment` button by reading its id/binding straight out of the mount tree,
then applies the returned `set_text` patch.

```ts
import { readFile } from "node:fs/promises";
import {
  NaijaEngine,
  isMountOk,
  isPatchOk,
  type ElementNode,
  type Node,
} from "@naijascript/sdk";

// 1. Instantiate the engine from the compiled ABI module.
const wasm = await readFile("naijascript_abi.wasm");
const engine = await NaijaEngine.instantiate(wasm);

const source = `
screen Counter start
  state count : Number get 0
  when increment(evt: TapEvent) start
    count get count add 1
  end
  view start
    stack(axis: Axis.vertical, spacing: 8px) start
      label start
        text "Count: {count}"
      end
      button(label: "Increment", onTap: @increment) /
    end
  end
end
`;

// 2. Mount the screen.
const mount = engine.uiMount(source);
if (!isMountOk(mount)) {
  throw new Error(`mount failed: ${mount.error.code} ${mount.error.message}`);
}

// 3. Find the button element and its tap binding in the mount tree.
function findButton(node: Node): ElementNode | undefined {
  if (node.kind !== "element") return undefined;
  if (node.primitive === "button") return node;
  for (const child of node.children) {
    const found = findButton(child);
    if (found) return found;
  }
  return undefined;
}

const button = findButton(mount.tree);
if (!button) throw new Error("no button in tree");
const tap = button.handlers["tap"];
if (!tap) throw new Error("button has no tap handler");

// 4. Dispatch a tap event using the node id + handler binding.
const patch = engine.uiDispatch({
  session_id: mount.session_id,
  node_id: button.id,
  handler_id: tap.id,
  handler_generation: tap.generation,
  event: { kind: "tap" },
});

// 5. Apply the patch (here we just read the resulting set_text op).
if (isPatchOk(patch)) {
  for (const op of patch.operations) {
    if (op.op === "set_text") {
      console.log(`node ${op.node} -> ${op.text}`); // "Count: 1"
    }
  }
}

// 6. Close the session to release engine-retained state.
engine.uiClose(mount.session_id);
```

## API

- `NaijaEngine.instantiate(wasmBytes, imports?)` — compile + instantiate; verifies ABI version.
- `engine.eval(source, filename?)` — evaluate `.ns` source; returns output + diagnostics.
- `engine.uiMount(source, params?)` — compile `.nui` and mount; returns a `MountDoc` or `ErrorDoc`.
- `engine.uiDispatch(request)` — dispatch an event; returns a `PatchDoc`, `StalePatchDoc`, or `ErrorDoc`.
- `engine.uiClose(sessionId)` — free a session; returns `true` if a session was closed.

Narrowing helpers: `isMountOk`, `isPatchOk`, `isPatchStale`.

Low-level access: the `Abi` class (and `NaijaExports`/`InputBuffer` types) are
exported for callers that need to drive the raw ABI directly.

## Notes

- Node and handler **ids are decimal strings** on the wire so 64-bit ids survive
  JSON. Revisions and generations are numbers.
- A `stale` dispatch result means the event referenced a retired handler
  generation; drop the event and keep the last accepted tree (do not resync).
- The instance is single-threaded and stateful. Sessions persist until closed.

See the protocol reference in
[`docs/spec/UI_IR_PROTOCOL.md`](../../docs/spec/UI_IR_PROTOCOL.md), the language
reference in [`docs/spec/UI_DSL.md`](../../docs/spec/UI_DSL.md), the practical
guide in [`docs/UI_ENGINE.md`](../../docs/UI_ENGINE.md), and the golden shapes
in [`conformance/fixtures/`](../../conformance/fixtures/).
