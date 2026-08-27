"""NaijaScript Python SDK.

A thin wrapper around the host-neutral WASM ABI (the ``wasm-abi`` crate),
exposing the plain-value evaluation surface plus the UI engine's
mount -> event -> patch lifecycle. See :mod:`naijascript_sdk.engine`.
"""

from .engine import NaijaAbiError, NaijaEngine, SUPPORTED_ABI_VERSION

__all__ = ["NaijaEngine", "NaijaAbiError", "SUPPORTED_ABI_VERSION"]

__version__ = "0.1.0"
