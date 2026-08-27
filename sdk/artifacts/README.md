# Versioned artifact metadata

Every released NaijaScript engine WASM module ships with a **manifest** that
pins the ABI/protocol versions, records the exact wasm bytes (by SHA-256), and
declares the negotiated capability set. Hosts read the manifest before loading
the module and refuse to instantiate anything that does not match.

- [`manifest.schema.json`](./manifest.schema.json) — JSON Schema (draft 2020-12) for a manifest.
- [`manifest.example.json`](./manifest.example.json) — a filled example (`abi_version` 1, `protocol_version` 1).

## Fields

| Field | Type | Meaning |
| --- | --- | --- |
| `abi_version` | int | C-ABI major version exported by `ns_abi_version()`. Exact-match. |
| `engine_version` | semver string | Human-facing engine build version. |
| `protocol_version` | int | UI patch/event/IR protocol major (`ui-patch/N`). Exact-match. |
| `wasm.path` | string | Path to the module, relative to the manifest. |
| `wasm.sha256` | hex string | Lowercase SHA-256 of the exact wasm bytes. |
| `wasm.bytes` | int | Module size in bytes. |
| `targets` | array | Build target triples (e.g. `wasm32-unknown-unknown`). |
| `capabilities` | object | Supported primitives, event kinds, extensions, and `limits`. |
| `built_at` | RFC 3339 | Build timestamp. |
| `git_commit` | string | Commit the artifact was built from. |

## How a release pipeline produces a manifest

1. **Build** the ABI crate for each target, e.g.:

   ```bash
   cargo build -p naijascript-wasm-abi --release --target wasm32-unknown-unknown
   ```

2. **Compute the digest** over the exact bytes that will ship:

   ```bash
   # macOS
   shasum -a 256 naijascript_abi.wasm
   # Linux
   sha256sum naijascript_abi.wasm
   ```

   Put the lowercase hex digest in `wasm.sha256` and the file size in
   `wasm.bytes`. Any post-build transform (e.g. `wasm-opt`) MUST run **before**
   the digest is computed so the manifest describes the shipped bytes.

3. **Pin versions.** `abi_version` MUST equal the value the module returns from
   `ns_abi_version()` (currently `1`). `protocol_version` MUST equal the UI
   protocol major the build implements (currently `1`). These are recorded, not
   guessed — a mismatch between the manifest and the module is a release bug.

4. **Record provenance.** Set `built_at` to the build time (RFC 3339, UTC) and
   `git_commit` to the full commit SHA.

5. **Publish** the manifest alongside conformance results, as required by
   `docs/spec/UI_IR_PROTOCOL.md` ("Builds MUST publish their chosen values with
   conformance results").

## Compatibility rule

**A host MUST require an exact `abi_version` AND `protocol_version` match.**

Major versions are intentionally incompatible: the memory/ownership contract and
the wire semantics may change between majors, and there is no implicit default
or best-effort fallback. On load a host should:

1. verify `wasm.sha256` against the module bytes it is about to instantiate;
2. instantiate and read `ns_abi_version()`, then check it equals both the
   manifest's `abi_version` and the version the host was built for;
3. confirm `protocol_version` equals the protocol the host speaks;
4. confirm the requested primitives, event kinds, and limits are within
   `capabilities`.

If any check fails, the host MUST refuse to mount rather than degrade. The
bundled TypeScript and Python SDKs enforce step 2 at instantiation time (they
throw / raise on an `abi_version` mismatch).
