# naijascript-sdk (Python)

A thin Python wrapper around the NaijaScript host-neutral WebAssembly ABI (the
`wasm-abi` crate), hosted with [`wasmtime`](https://pypi.org/project/wasmtime/).
It exposes:

- the plain-value evaluation surface (`eval`), and
- the UI engine's **mount → event → patch** lifecycle (`ui_mount`,
  `ui_dispatch`, `ui_close`).

The wrapper honors the ABI ownership/memory contract for you: every input
buffer is allocated with `ns_alloc`, copied into linear memory, and freed with
`ns_free`; every result handle is read via `ns_result_ptr`/`ns_result_len` and
released with `ns_result_free`. Results are parsed into plain Python `dict`s.

> Compatibility rule: the SDK requires an **exact** `abi_version` match
> (currently `1`). Construction raises `NaijaAbiError` on a mismatch.

## Install

The only runtime dependency is `wasmtime`, declared in `pyproject.toml`:

```bash
pip install .          # installs wasmtime as a dependency
# or, for development:
pip install -e .
```

Requires Python >= 3.10.

## Usage: mount a counter, tap it, apply the patch

```python
from naijascript_sdk import NaijaEngine

engine = NaijaEngine.from_file("naijascript_abi.wasm")

source = """
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
"""

# 1. Mount the screen.
mount = engine.ui_mount(source)
assert mount["status"] == "ok", mount

# 2. Find the button element and its tap binding in the mount tree.
def find_button(node):
    if node.get("kind") != "element":
        return None
    if node.get("primitive") == "button":
        return node
    for child in node.get("children", []):
        found = find_button(child)
        if found:
            return found
    return None

button = find_button(mount["tree"])
tap = button["handlers"]["tap"]

# 3. Dispatch a tap using the node id + handler binding.
patch = engine.ui_dispatch(
    {
        "session_id": mount["session_id"],
        "node_id": button["id"],           # decimal string id
        "handler_id": tap["id"],
        "handler_generation": tap["generation"],
        "event": {"kind": "tap"},
    }
)

# 4. Apply the patch (here we just read the resulting set_text op).
if patch["status"] == "ok":
    for op in patch["operations"]:
        if op["op"] == "set_text":
            print(op["node"], "->", op["text"])  # "Count: 1"

# 5. Close the session to release engine-retained state.
engine.ui_close(mount["session_id"])
```

## API

- `NaijaEngine.from_file(path)` / `NaijaEngine.from_bytes(data)` — instantiate; verifies ABI version.
- `engine.eval(source, filename=None)` — evaluate `.ns` source; returns output + diagnostics.
- `engine.ui_mount(source, params=None)` — compile `.nui` and mount; returns a mount or error doc.
- `engine.ui_dispatch(request)` — dispatch an event; returns a patch (possibly `status == "stale"`) or error doc.
- `engine.ui_close(session_id)` — free a session; returns `True` if one was closed.

## Notes

- Node and handler **ids are decimal strings** on the wire so 64-bit ids survive
  JSON; the ABI also accepts them as numbers on dispatch.
- A `stale` dispatch result means the event referenced a retired handler
  generation; drop the event and keep the last accepted tree (do not resync).
- The engine is single-threaded and stateful. Sessions persist until closed.

See [`docs/spec/UI_IR_PROTOCOL.md`](../../docs/spec/UI_IR_PROTOCOL.md),
[`docs/spec/UI_DSL.md`](../../docs/spec/UI_DSL.md), the practical guide in
[`docs/UI_ENGINE.md`](../../docs/UI_ENGINE.md), and the golden shapes in
[`conformance/fixtures/`](../../conformance/fixtures/).
