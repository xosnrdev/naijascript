"""Thin Python wrapper around the NaijaScript host-neutral WASM ABI.

The engine hosts the ABI module with `wasmtime` and honors the ownership /
memory contract exactly:

1. The caller allocates an input buffer with ``ns_alloc(len)``, copies bytes
   into WASM linear memory at the returned pointer, calls the ABI function, then
   MUST call ``ns_free(ptr, len)`` for every input buffer.
2. The function returns a result handle (u32). The JSON is read via
   ``ns_result_ptr(handle)`` + ``ns_result_len(handle)``, decoded as UTF-8, then
   released with ``ns_result_free(handle)``.

Ids in the JSON protocol are decimal strings (for u64 safety); revisions and
generations are numbers. Results are parsed into plain Python dicts.
"""

from __future__ import annotations

import json
import os
from typing import Any, Mapping, Optional, Union

import wasmtime

#: The ABI major version this SDK is written against.
SUPPORTED_ABI_VERSION = 1

# Type aliases for the parsed JSON documents (plain dicts on the wire).
Json = dict[str, Any]
PathLike = Union[str, bytes, "os.PathLike[str]", "os.PathLike[bytes]"]


class NaijaAbiError(RuntimeError):
    """Raised when the ABI reports a contract violation (alloc/free/handle)."""


class _Input:
    """A live input allocation. ``(0, 0)`` denotes an omitted/empty buffer."""

    __slots__ = ("ptr", "len")

    def __init__(self, ptr: int, length: int) -> None:
        self.ptr = ptr
        self.len = length


class NaijaEngine:
    """A NaijaScript engine bound to a single ``wasmtime`` instance.

    The instance is single-threaded and stateful: it retains UI sessions until
    :meth:`ui_close` is called. Do not share one engine across threads.
    """

    def __init__(self, wasm_bytes: bytes) -> None:
        self._engine = wasmtime.Engine()
        self._store = wasmtime.Store(self._engine)
        module = wasmtime.Module(self._engine, wasm_bytes)
        self._instance = wasmtime.Instance(self._store, module, [])
        exports = self._instance.exports(self._store)

        def export(name: str) -> Any:
            item = exports.get(name)
            if item is None:
                raise NaijaAbiError(f"module is missing required export: {name}")
            return item

        memory = export("memory")
        if not isinstance(memory, wasmtime.Memory):
            raise NaijaAbiError("module export 'memory' is not a linear memory")
        self._memory: wasmtime.Memory = memory

        self._ns_abi_version = export("ns_abi_version")
        self._ns_alloc = export("ns_alloc")
        self._ns_free = export("ns_free")
        self._ns_eval = export("ns_eval")
        self._ns_result_ptr = export("ns_result_ptr")
        self._ns_result_len = export("ns_result_len")
        self._ns_result_free = export("ns_result_free")
        self._ns_ui_mount = export("ns_ui_mount")
        self._ns_ui_dispatch = export("ns_ui_dispatch")
        self._ns_ui_close = export("ns_ui_close")

        version = self.abi_version()
        if version != SUPPORTED_ABI_VERSION:
            raise NaijaAbiError(
                f"ABI version mismatch: module is {version}, "
                f"SDK requires exactly {SUPPORTED_ABI_VERSION}"
            )

    # ----- constructors -----

    @classmethod
    def from_bytes(cls, data: bytes) -> "NaijaEngine":
        """Instantiate from in-memory WASM bytes."""
        return cls(bytes(data))

    @classmethod
    def from_file(cls, path: PathLike) -> "NaijaEngine":
        """Instantiate from a compiled ABI module on disk."""
        with open(path, "rb") as handle:
            return cls(handle.read())

    # ----- public API -----

    def abi_version(self) -> int:
        """The ABI major version reported by the module."""
        return int(self._ns_abi_version(self._store)) & 0xFFFFFFFF

    def eval(self, source: str, filename: Optional[str] = None) -> Json:
        """Evaluate NaijaScript source; returns output and diagnostics."""
        src = self._write_str(source)
        file = self._write_str(filename) if filename is not None else _Input(0, 0)
        try:
            handle = self._ns_eval(self._store, src.ptr, src.len, file.ptr, file.len)
            return self._read_result_json(handle)
        finally:
            self._free(file)
            self._free(src)

    def ui_mount(self, source: str, params: Optional[Mapping[str, Any]] = None) -> Json:
        """Compile ``.nui`` source and mount a screen.

        Returns a mount snapshot document or an error document.
        """
        src = self._write_str(source)
        params_buf = _Input(0, 0) if params is None else self._write_json(params)
        try:
            handle = self._ns_ui_mount(
                self._store, src.ptr, src.len, params_buf.ptr, params_buf.len
            )
            return self._read_result_json(handle)
        finally:
            self._free(params_buf)
            self._free(src)

    def ui_dispatch(self, request: Mapping[str, Any]) -> Json:
        """Dispatch an event to a live session.

        Returns a patch document (possibly ``status == "stale"``) or an error.
        """
        buf = self._write_json(request)
        try:
            handle = self._ns_ui_dispatch(self._store, buf.ptr, buf.len)
            return self._read_result_json(handle)
        finally:
            self._free(buf)

    def ui_close(self, session_id: int) -> bool:
        """Free a session's retained state. Returns ``True`` if one was closed."""
        result = self._ns_ui_close(self._store, int(session_id) & 0xFFFFFFFF)
        return int(result) == 0

    # ----- low-level ABI helpers (honor the ownership contract) -----

    def _alloc(self, length: int) -> int:
        if length == 0:
            return 0
        ptr = int(self._ns_alloc(self._store, length & 0xFFFFFFFF)) & 0xFFFFFFFF
        if ptr == 0:
            raise NaijaAbiError(
                f"ns_alloc({length}) failed (zero, oversized, or out of memory)"
            )
        return ptr

    def _free(self, buffer: _Input) -> None:
        if buffer.ptr == 0 and buffer.len == 0:
            return
        result = self._ns_free(self._store, buffer.ptr, buffer.len)
        if int(result) != 0:
            raise NaijaAbiError(
                f"ns_free({buffer.ptr}, {buffer.len}) did not match a live allocation"
            )

    def _write_bytes(self, data: bytes) -> _Input:
        if len(data) == 0:
            return _Input(0, 0)
        ptr = self._alloc(len(data))
        # Re-fetch memory each call: growth may replace the backing buffer.
        self._memory.write(self._store, data, ptr)
        return _Input(ptr, len(data))

    def _write_str(self, text: str) -> _Input:
        return self._write_bytes(text.encode("utf-8"))

    def _write_json(self, value: Any) -> _Input:
        return self._write_str(json.dumps(value, separators=(",", ":")))

    def _read_result_json(self, handle: Any) -> Json:
        return json.loads(self._read_result(handle))

    def _read_result(self, handle: Any) -> str:
        handle = int(handle) & 0xFFFFFFFF
        if handle == 0:
            raise NaijaAbiError("ABI returned a null result handle")
        try:
            ptr = int(self._ns_result_ptr(self._store, handle)) & 0xFFFFFFFF
            length = int(self._ns_result_len(self._store, handle)) & 0xFFFFFFFF
            if ptr == 0 and length == 0:
                raise NaijaAbiError(f"result handle {handle} is unknown or empty")
            data = self._memory.read(self._store, ptr, ptr + length)
            return bytes(data).decode("utf-8")
        finally:
            self._ns_result_free(self._store, handle)
