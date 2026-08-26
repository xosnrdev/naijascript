//! Host-neutral `NaijaScript` WebAssembly ABI.
//!
//! This crate deliberately has no `wasm-bindgen`, WASI, browser, JNI, or Apple
//! framework dependency. See `README.md` for the complete ownership contract.

use std::cell::RefCell;
use std::collections::BTreeMap;

use naijascript::arena::{self, scratch_arena};
use naijascript::diagnostics::{Diagnostics, Severity};
use naijascript::helpers::MEBI;
use naijascript::resolver::Resolver;
use naijascript::runtime::Runtime;
use naijascript::syntax::parser::Parser;
use naijascript::syntax::scanner::Lexer;
use serde_json::{Value, json};

const ABI_VERSION: u32 = 1;
const ARENA_BYTES: usize = 16 * MEBI;
const MAX_ALLOCATION_BYTES: usize = 16 * MEBI;
const MAX_SOURCE_BYTES: usize = 8 * MEBI;
const MAX_FILENAME_BYTES: usize = 4096;

struct State {
    allocations: BTreeMap<u32, Box<[u8]>>,
    results: BTreeMap<u32, Box<[u8]>>,
    next_result: u32,
}

impl State {
    fn new() -> Self {
        Self { allocations: BTreeMap::new(), results: BTreeMap::new(), next_result: 1 }
    }

    fn insert_result(&mut self, bytes: Box<[u8]>) -> u32 {
        loop {
            let handle = self.next_result;
            self.next_result = self.next_result.wrapping_add(1).max(1);
            if handle != 0 && !self.results.contains_key(&handle) {
                self.results.insert(handle, bytes);
                return handle;
            }
        }
    }
}

thread_local! {
    static STATE: RefCell<State> = RefCell::new(State::new());
}

/// Returns the ABI major version. A host must require exactly the version it
/// was written for.
#[unsafe(no_mangle)]
pub extern "C" fn ns_abi_version() -> u32 {
    ABI_VERSION
}

/// Allocates a host-writable byte buffer and returns its linear-memory offset.
///
/// Returns zero for a zero-length request, an oversized request, allocation
/// failure, or when compiled for a non-wasm target whose pointer cannot fit in
/// the ABI's 32-bit address field.
#[unsafe(no_mangle)]
pub extern "C" fn ns_alloc(len: u32) -> u32 {
    let len = len as usize;
    if len == 0 || len > MAX_ALLOCATION_BYTES {
        return 0;
    }

    let mut bytes = Vec::new();
    if bytes.try_reserve_exact(len).is_err() {
        return 0;
    }
    bytes.resize(len, 0);
    let mut bytes = bytes.into_boxed_slice();
    let Ok(ptr) = u32::try_from(bytes.as_mut_ptr() as usize) else {
        return 0;
    };
    if ptr == 0 {
        return 0;
    }

    STATE.with(|state| {
        let replaced = state.borrow_mut().allocations.insert(ptr, bytes);
        debug_assert!(replaced.is_none());
    });
    ptr
}

/// Frees an input buffer. The pointer and length must exactly match a
/// successful `ns_alloc` call. Returns 0 on success and 1 on mismatch.
#[unsafe(no_mangle)]
pub extern "C" fn ns_free(ptr: u32, len: u32) -> u32 {
    STATE.with(|state| {
        let mut state = state.borrow_mut();
        match state.allocations.get(&ptr) {
            Some(bytes) if bytes.len() == len as usize => {
                state.allocations.remove(&ptr);
                0
            }
            _ => 1,
        }
    })
}

/// Evaluates UTF-8 source and returns an opaque result handle.
///
/// Source and filename must be exact live allocations from `ns_alloc`.
/// `(0, 0)` is accepted for an omitted filename. Inputs remain owned by the
/// host and must still be passed to `ns_free`.
#[unsafe(no_mangle)]
pub extern "C" fn ns_eval(
    source_ptr: u32,
    source_len: u32,
    filename_ptr: u32,
    filename_len: u32,
) -> u32 {
    let source = match copy_allocation(source_ptr, source_len, false, MAX_SOURCE_BYTES) {
        Ok(bytes) => bytes,
        Err(message) => return store_result(&error_document("invalid_request", message)),
    };
    let filename = match copy_allocation(filename_ptr, filename_len, true, MAX_FILENAME_BYTES) {
        Ok(bytes) => bytes,
        Err(message) => return store_result(&error_document("invalid_request", message)),
    };
    let Ok(source) = String::from_utf8(source) else {
        return store_result(&error_document("invalid_request", "source is not valid UTF-8"));
    };
    let filename = if filename.is_empty() {
        "<host>".to_owned()
    } else {
        match String::from_utf8(filename) {
            Ok(filename) => filename,
            Err(_) => {
                return store_result(&error_document(
                    "invalid_request",
                    "filename is not valid UTF-8",
                ));
            }
        }
    };

    store_result(&evaluate(&source, &filename))
}

/// Returns the linear-memory offset of a result's immutable JSON bytes, or
/// zero for an unknown handle. The pointer remains valid until
/// `ns_result_free(handle)`.
#[unsafe(no_mangle)]
pub extern "C" fn ns_result_ptr(handle: u32) -> u32 {
    STATE.with(|state| {
        state
            .borrow()
            .results
            .get(&handle)
            .and_then(|bytes| u32::try_from(bytes.as_ptr() as usize).ok())
            .unwrap_or(0)
    })
}

/// Returns a result's JSON byte length, or zero for an unknown handle.
#[unsafe(no_mangle)]
pub extern "C" fn ns_result_len(handle: u32) -> u32 {
    STATE.with(|state| {
        state
            .borrow()
            .results
            .get(&handle)
            .and_then(|bytes| u32::try_from(bytes.len()).ok())
            .unwrap_or(0)
    })
}

/// Invalidates a result handle and its pointer. Returns 0 on success and 1 for
/// an unknown handle.
#[unsafe(no_mangle)]
pub extern "C" fn ns_result_free(handle: u32) -> u32 {
    STATE.with(|state| u32::from(state.borrow_mut().results.remove(&handle).is_none()))
}

fn copy_allocation(
    ptr: u32,
    len: u32,
    allow_empty: bool,
    maximum: usize,
) -> Result<Vec<u8>, &'static str> {
    if ptr == 0 && len == 0 && allow_empty {
        return Ok(Vec::new());
    }
    if len as usize > maximum {
        return Err("input exceeds the ABI size limit");
    }
    STATE.with(|state| {
        let state = state.borrow();
        match state.allocations.get(&ptr) {
            Some(bytes) if bytes.len() == len as usize => Ok(bytes.to_vec()),
            _ => Err("pointer and length do not identify a live ABI allocation"),
        }
    })
}

fn evaluate(source: &str, filename: &str) -> Value {
    if let Err(code) = arena::init(ARENA_BYTES) {
        return error_document("engine_error", &format!("arena initialization failed ({code})"));
    }
    let arena = scratch_arena(None);
    let lexer = Lexer::new(source, &arena);
    let mut parser = Parser::new(lexer, &arena);
    let (root, parse_diagnostics) = parser.parse_program();
    let mut diagnostics = diagnostics_json(parse_diagnostics);
    if parse_diagnostics.has_errors() {
        return evaluation_document("diagnostic_error", &[], &diagnostics);
    }

    let resolver_arena = scratch_arena(Some(&arena));
    let mut resolver = Resolver::with_facts_arena(&resolver_arena, &arena);
    resolver.resolve(root);
    diagnostics.extend(diagnostics_json(&resolver.errors));
    if resolver.errors.has_errors() {
        return evaluation_document("diagnostic_error", &[], &diagnostics);
    }
    let (facts, optimization_plan) = resolver.into_artifacts();
    drop(resolver_arena);

    let frame = scratch_arena(Some(&arena));
    let mut runtime = Runtime::new(&arena, Some(&frame));
    runtime.run_with_analysis(root, &facts, optimization_plan.as_ref());
    diagnostics.extend(diagnostics_json(&runtime.errors));
    let output: Vec<String> = runtime.output.iter().map(ToString::to_string).collect();
    let status = if runtime.errors.has_errors() { "diagnostic_error" } else { "ok" };

    let mut document = evaluation_document(status, &output, &diagnostics);
    document["filename"] = json!(filename);
    document
}

fn diagnostics_json(diagnostics: &Diagnostics<'_>) -> Vec<Value> {
    diagnostics
        .diagnostics
        .iter()
        .map(|diagnostic| {
            let severity = match diagnostic.severity {
                Severity::Error => "error",
                Severity::Warning => "warning",
                Severity::Note => "note",
            };
            let labels: Vec<_> = diagnostic
                .labels
                .iter()
                .map(|label| {
                    json!({
                        "message": label.message.as_ref(),
                        "span": {"start": label.span.start, "end": label.span.end}
                    })
                })
                .collect();
            json!({
                "severity": severity,
                "code": diagnostic.code,
                "message": diagnostic.message,
                "span": {"start": diagnostic.span.start, "end": diagnostic.span.end},
                "labels": labels
            })
        })
        .collect()
}

fn evaluation_document(status: &str, output: &[String], diagnostics: &[Value]) -> Value {
    json!({
        "abi_version": ABI_VERSION,
        "status": status,
        "output": output,
        "diagnostics": diagnostics
    })
}

fn error_document(status: &str, message: &str) -> Value {
    json!({
        "abi_version": ABI_VERSION,
        "status": status,
        "output": [],
        "diagnostics": [{
            "severity": "error",
            "code": "ABI0001",
            "message": message,
            "span": Value::Null,
            "labels": []
        }]
    })
}

fn store_result(value: &Value) -> u32 {
    let bytes = serde_json::to_vec(&value).unwrap_or_else(|_| {
        br#"{"abi_version":1,"status":"engine_error","output":[],"diagnostics":[]}"#.to_vec()
    });
    STATE.with(|state| state.borrow_mut().insert_result(bytes.into_boxed_slice()))
}
