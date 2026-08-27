//! UI engine surface for the host-neutral ABI.
//!
//! This is the transport adapter the core engine deliberately omits: it maps
//! the renderer-neutral IR, snapshots, patches, and events to a stable JSON
//! protocol. The core `naijascript::ui` module never depends on JSON; all
//! encoding and decoding lives here so a production host may swap in a binary
//! codec without touching engine semantics. See `docs/spec/UI_IR_PROTOCOL.md`.

use std::cell::RefCell;
use std::collections::BTreeMap;

use naijascript::ui::engine::{UiSession, UiVal};
use naijascript::ui::error::UiError;
use naijascript::ui::event::{EventKind, EventPayload, HandlerBinding, HandlerId, UiEvent};
use naijascript::ui::identity::NodeId;
use naijascript::ui::ir::{ElementNode, Node, PrimitiveKind, UiTree};
use naijascript::ui::limits::Limits;
use naijascript::ui::patch::{Patch, PatchOp, Snapshot};
use naijascript::ui::value::{Length, PropValue};
use serde_json::{Map, Value, json};

use crate::{ABI_VERSION, MAX_SOURCE_BYTES, copy_allocation, store_result};

const MAX_REQUEST_BYTES: usize = 8 * naijascript::helpers::MEBI;

struct Sessions {
    live: BTreeMap<u32, UiSession>,
    next: u32,
}

impl Sessions {
    const fn new() -> Self {
        Self { live: BTreeMap::new(), next: 1 }
    }

    fn allocate(&mut self) -> u32 {
        loop {
            let id = self.next;
            self.next = self.next.wrapping_add(1).max(1);
            if id != 0 && !self.live.contains_key(&id) {
                return id;
            }
        }
    }
}

thread_local! {
    static SESSIONS: RefCell<Sessions> = const { RefCell::new(Sessions::new()) };
}

/// Compiles `.nui` source and mounts a screen. Returns a result handle whose
/// JSON is either a `mount` document or an `error` document. Inputs remain
/// host-owned and must still be freed with `ns_free`.
#[unsafe(no_mangle)]
pub extern "C" fn ns_ui_mount(
    source_ptr: u32,
    source_len: u32,
    params_ptr: u32,
    params_len: u32,
) -> u32 {
    let source = match copy_allocation(source_ptr, source_len, false, MAX_SOURCE_BYTES) {
        Ok(bytes) => bytes,
        Err(message) => return store_result(&error_doc("mount", "invalid_request", message)),
    };
    let params_bytes = match copy_allocation(params_ptr, params_len, true, MAX_REQUEST_BYTES) {
        Ok(bytes) => bytes,
        Err(message) => return store_result(&error_doc("mount", "invalid_request", message)),
    };
    let Ok(source) = String::from_utf8(source) else {
        return store_result(&error_doc("mount", "invalid_request", "source is not valid UTF-8"));
    };

    let params = if params_bytes.is_empty() {
        BTreeMap::new()
    } else {
        match serde_json::from_slice::<Value>(&params_bytes) {
            Ok(Value::Object(map)) => object_to_params(&map),
            Ok(_) => {
                return store_result(&error_doc(
                    "mount",
                    "invalid_request",
                    "params must be a JSON object",
                ));
            }
            Err(_) => {
                return store_result(&error_doc("mount", "invalid_request", "params is not JSON"));
            }
        }
    };

    let session_id = SESSIONS.with(|sessions| sessions.borrow_mut().allocate());
    match UiSession::mount(&source, None, params, u64::from(session_id), Limits::defaults()) {
        Ok((session, output)) => {
            SESSIONS.with(|sessions| {
                sessions.borrow_mut().live.insert(session_id, session);
            });
            let mut document = encode_snapshot(&output.snapshot);
            document["session_id"] = json!(session_id);
            store_result(&document)
        }
        Err(error) => store_result(&encode_error("mount", &error)),
    }
}

/// Dispatches a JSON-encoded event to a live session and returns a `patch`
/// document (possibly with `status: "stale"`) or an `error` document.
#[unsafe(no_mangle)]
pub extern "C" fn ns_ui_dispatch(request_ptr: u32, request_len: u32) -> u32 {
    let bytes = match copy_allocation(request_ptr, request_len, false, MAX_REQUEST_BYTES) {
        Ok(bytes) => bytes,
        Err(message) => return store_result(&error_doc("patch", "invalid_request", message)),
    };
    let request: Value = match serde_json::from_slice(&bytes) {
        Ok(value) => value,
        Err(_) => {
            return store_result(&error_doc("patch", "invalid_request", "request is not JSON"));
        }
    };

    let Some(session_id) = request.get("session_id").and_then(Value::as_u64) else {
        return store_result(&error_doc("patch", "invalid_request", "missing session_id"));
    };
    let session_id = session_id as u32;

    let event = match decode_event(&request) {
        Ok(event) => event,
        Err(message) => return store_result(&error_doc("patch", "invalid_request", message)),
    };

    SESSIONS.with(|sessions| {
        let mut sessions = sessions.borrow_mut();
        let Some(session) = sessions.live.get_mut(&session_id) else {
            return store_result(&error_doc("patch", "unknown_session", "no such session"));
        };
        match session.dispatch(&event) {
            Ok(patch) => store_result(&encode_patch(&patch)),
            Err(error) if error.code == "UI_STALE_HANDLER" => {
                store_result(&encode_stale(session_id, &error))
            }
            Err(error) => store_result(&encode_error("patch", &error)),
        }
    })
}

/// Frees a session's retained state. Returns 0 on success and 1 for an unknown
/// session id.
#[unsafe(no_mangle)]
pub extern "C" fn ns_ui_close(session_id: u32) -> u32 {
    SESSIONS.with(|sessions| u32::from(sessions.borrow_mut().live.remove(&session_id).is_none()))
}

// ----- conformance harness -----

/// Runs a fixture (source plus a selector-based event script) against the
/// engine and produces a golden document every renderer must reproduce.
///
/// Events select their target element by primitive and property equality rather
/// than by raw node id, so fixtures stay stable across identity-hash changes.
/// The golden captures the mount snapshot, every emitted patch, and the final
/// tree so a renderer conformance runner can apply mount + patches and compare.
#[must_use]
pub fn conformance_run(source: &str, params: &Value, events: &Value) -> Value {
    const SESSION_ID: u64 = 1;
    let params = match params {
        Value::Object(map) => object_to_params(map),
        _ => BTreeMap::new(),
    };
    let (mut session, output) =
        match UiSession::mount(source, None, params, SESSION_ID, Limits::defaults()) {
            Ok(mounted) => mounted,
            Err(error) => {
                return json!({
                    "protocol_version": ABI_VERSION,
                    "status": "compile_error",
                    "error": encode_error_body(&error),
                });
            }
        };

    let mut mount_doc = encode_snapshot(&output.snapshot);
    mount_doc["session_id"] = json!(SESSION_ID);

    let mut steps = Vec::new();
    if let Some(actions) = events.as_array() {
        for action in actions {
            steps.push(run_action(&mut session, action));
        }
    }

    json!({
        "protocol_version": ABI_VERSION,
        "status": "ok",
        "mount": mount_doc,
        "steps": steps,
        "final_tree": encode_tree(session.tree()),
    })
}

fn run_action(session: &mut UiSession, action: &Value) -> Value {
    let kind = action.get("action").and_then(Value::as_str).unwrap_or("tap");
    let event_kind = match kind {
        "tap" => EventKind::Tap,
        "change" => EventKind::Change,
        "submit" => EventKind::Submit,
        "focus" => EventKind::Focus,
        _ => {
            return json!({ "action": action, "status": "error", "message": "unknown action" });
        }
    };

    let matcher = action.get("match");
    let Some(element) = find_target(&session.tree().root, matcher, event_kind) else {
        return json!({ "action": action, "status": "no_match" });
    };
    let Some(binding) = element.handlers.get(&event_kind) else {
        return json!({ "action": action, "status": "no_handler" });
    };

    let payload = match event_kind {
        EventKind::Tap => EventPayload::Tap,
        EventKind::Change => EventPayload::Change {
            value: action.get("value").and_then(Value::as_str).unwrap_or("").to_owned(),
        },
        EventKind::Submit => EventPayload::Submit {
            value: action.get("value").and_then(Value::as_str).map(str::to_owned),
        },
        EventKind::Focus => EventPayload::Focus {
            focused: action.get("focused").and_then(Value::as_bool).unwrap_or(true),
        },
    };

    let event = UiEvent {
        node_id: element.id,
        handler_id: binding.id,
        handler_generation: binding.generation,
        payload,
    };

    match session.dispatch(&event) {
        Ok(patch) => json!({ "action": action, "status": "ok", "patch": encode_patch(&patch) }),
        Err(error) => {
            json!({ "action": action, "status": "rejected", "error": encode_error_body(&error) })
        }
    }
}

/// Finds the first pre-order element matching a selector that also carries a
/// handler for `event_kind`.
fn find_target<'a>(
    node: &'a Node,
    matcher: Option<&Value>,
    event_kind: EventKind,
) -> Option<&'a ElementNode> {
    if let Node::Element(element) = node {
        if element_matches(element, matcher) && element.handlers.contains_key(&event_kind) {
            return Some(element);
        }
        for child in &element.children {
            if let Some(found) = find_target(child, matcher, event_kind) {
                return Some(found);
            }
        }
    }
    None
}

fn element_matches(element: &ElementNode, matcher: Option<&Value>) -> bool {
    let Some(matcher) = matcher else {
        return true;
    };
    if let Some(primitive) = matcher.get("primitive").and_then(Value::as_str)
        && PrimitiveKind::from_str(primitive) != Some(element.kind)
    {
        return false;
    }
    if let Some(Value::Object(props)) = matcher.get("props") {
        for (name, expected) in props {
            let Some(actual) = element.props.get(name) else {
                return false;
            };
            if !prop_equals(actual, expected) {
                return false;
            }
        }
    }
    true
}

fn prop_equals(actual: &PropValue, expected: &Value) -> bool {
    match (actual, expected) {
        (PropValue::Text(text) | PropValue::Key(text), Value::String(other)) => text == other,
        (PropValue::Number(number), Value::Number(other)) => {
            other.as_f64().is_some_and(|value| (value - number).abs() < f64::EPSILON)
        }
        (PropValue::Boolean(flag), Value::Bool(other)) => flag == other,
        (PropValue::Enum { case, .. }, Value::String(other)) => case == other,
        _ => false,
    }
}

// ----- decoding -----

fn object_to_params(map: &Map<String, Value>) -> BTreeMap<String, UiVal> {
    map.iter().map(|(key, value)| (key.clone(), value_to_uival(value))).collect()
}

fn value_to_uival(value: &Value) -> UiVal {
    match value {
        Value::Null => UiVal::Null,
        Value::Bool(flag) => UiVal::Bool(*flag),
        Value::Number(number) => UiVal::Number(number.as_f64().unwrap_or(0.0)),
        Value::String(text) => UiVal::Text(text.clone()),
        Value::Array(items) => UiVal::List(items.iter().map(value_to_uival).collect()),
        Value::Object(map) => UiVal::Record(
            map.iter().map(|(key, value)| (key.clone(), value_to_uival(value))).collect(),
        ),
    }
}

fn decode_event(request: &Value) -> Result<UiEvent, &'static str> {
    let node_id = decode_u64(request.get("node_id")).ok_or("missing or invalid node_id")?;
    let handler_id =
        decode_u64(request.get("handler_id")).ok_or("missing or invalid handler_id")?;
    let generation = request
        .get("handler_generation")
        .and_then(Value::as_u64)
        .ok_or("missing handler_generation")?;
    let event = request.get("event").ok_or("missing event")?;
    let kind = event.get("kind").and_then(Value::as_str).ok_or("missing event kind")?;
    let payload = match kind {
        "tap" => EventPayload::Tap,
        "change" => EventPayload::Change {
            value: event.get("value").and_then(Value::as_str).unwrap_or("").to_owned(),
        },
        "submit" => EventPayload::Submit {
            value: event.get("value").and_then(Value::as_str).map(str::to_owned),
        },
        "focus" => EventPayload::Focus {
            focused: event.get("focused").and_then(Value::as_bool).unwrap_or(false),
        },
        _ => return Err("unknown event kind"),
    };
    Ok(UiEvent {
        node_id: NodeId(node_id),
        handler_id: HandlerId(handler_id),
        handler_generation: generation,
        payload,
    })
}

fn decode_u64(value: Option<&Value>) -> Option<u64> {
    match value {
        Some(Value::String(text)) => text.parse().ok(),
        Some(Value::Number(number)) => number.as_u64(),
        _ => None,
    }
}

// ----- encoding -----

fn encode_snapshot(snapshot: &Snapshot) -> Value {
    json!({
        "abi_version": ABI_VERSION,
        "type": "mount",
        "status": "ok",
        "session_id": snapshot.session_id,
        "tree_revision": snapshot.tree_revision,
        "tree": encode_tree(&snapshot.tree),
    })
}

fn encode_patch(patch: &Patch) -> Value {
    let operations: Vec<Value> = patch.operations.iter().map(encode_op).collect();
    json!({
        "abi_version": ABI_VERSION,
        "type": "patch",
        "status": "ok",
        "session_id": patch.session_id,
        "base_revision": patch.base_revision,
        "target_revision": patch.target_revision,
        "operations": operations,
    })
}

fn encode_stale(session_id: u32, error: &UiError) -> Value {
    json!({
        "abi_version": ABI_VERSION,
        "type": "patch",
        "status": "stale",
        "session_id": session_id,
        "error": encode_error_body(error),
    })
}

fn encode_tree(tree: &UiTree) -> Value {
    encode_node(&tree.root)
}

fn encode_node(node: &Node) -> Value {
    match node {
        Node::Text(text) => json!({
            "kind": "text",
            "id": id_string(text.id),
            "text": text.text,
        }),
        Node::Element(element) => {
            let props: Map<String, Value> = element
                .props
                .iter()
                .map(|(name, value)| (name.clone(), encode_prop(value)))
                .collect();
            let handlers: Map<String, Value> = element
                .handlers
                .iter()
                .map(|(kind, binding)| (kind.as_str().to_owned(), encode_binding(binding)))
                .collect();
            let children: Vec<Value> = element.children.iter().map(encode_node).collect();
            json!({
                "kind": "element",
                "id": id_string(element.id),
                "primitive": element.kind.as_str(),
                "props": props,
                "handlers": handlers,
                "children": children,
            })
        }
    }
}

fn encode_binding(binding: &HandlerBinding) -> Value {
    json!({
        "id": id_string(binding.id.0),
        "generation": binding.generation,
    })
}

fn encode_prop(value: &PropValue) -> Value {
    match value {
        PropValue::Text(text) => json!({ "type": "text", "value": text }),
        PropValue::Number(number) => json!({ "type": "number", "value": number }),
        PropValue::Boolean(flag) => json!({ "type": "boolean", "value": flag }),
        PropValue::Key(key) => json!({ "type": "key", "value": key }),
        PropValue::Color(color) => json!({
            "type": "color",
            "r": color.r,
            "g": color.g,
            "b": color.b,
            "a": color.a,
        }),
        PropValue::Length(Length::LogicalPixels(px)) => json!({ "type": "length", "px": px }),
        PropValue::ImageSource(source) => {
            let naijascript::ui::value::ImageSource::Asset(name) = source;
            json!({ "type": "image", "asset": name })
        }
        PropValue::Enum { ty, case } => {
            json!({ "type": "enum", "enum": ty.as_str(), "case": case })
        }
    }
}

fn encode_op(op: &PatchOp) -> Value {
    match op {
        PatchOp::Insert { parent, index, node } => json!({
            "op": "insert",
            "parent": id_string(*parent),
            "index": index,
            "node": encode_node(node),
        }),
        PatchOp::Remove { node } => json!({ "op": "remove", "node": id_string(*node) }),
        PatchOp::Move { node, parent, index } => json!({
            "op": "move",
            "node": id_string(*node),
            "parent": id_string(*parent),
            "index": index,
        }),
        PatchOp::Replace { node, replacement } => json!({
            "op": "replace",
            "node": id_string(*node),
            "replacement": encode_node(replacement),
        }),
        PatchOp::SetProperty { node, property, value } => json!({
            "op": "set_property",
            "node": id_string(*node),
            "property": property,
            "value": encode_prop(value),
        }),
        PatchOp::RemoveProperty { node, property } => json!({
            "op": "remove_property",
            "node": id_string(*node),
            "property": property,
        }),
        PatchOp::SetText { node, text } => json!({
            "op": "set_text",
            "node": id_string(*node),
            "text": text,
        }),
        PatchOp::BindHandler { node, event_kind, binding } => json!({
            "op": "bind_handler",
            "node": id_string(*node),
            "event_kind": event_kind.as_str(),
            "binding": encode_binding(binding),
        }),
        PatchOp::UnbindHandler { node, event_kind } => json!({
            "op": "unbind_handler",
            "node": id_string(*node),
            "event_kind": event_kind.as_str(),
        }),
    }
}

fn encode_error(doc_type: &str, error: &UiError) -> Value {
    json!({
        "abi_version": ABI_VERSION,
        "type": doc_type,
        "status": "error",
        "error": encode_error_body(error),
    })
}

fn encode_error_body(error: &UiError) -> Value {
    json!({
        "category": error.category.as_str(),
        "code": error.code,
        "message": error.message,
        "recoverability": error.recoverability.as_str(),
        "span": error.span.map(|span| json!({ "start": span.start, "end": span.end })),
    })
}

fn error_doc(doc_type: &str, code: &str, message: &str) -> Value {
    json!({
        "abi_version": ABI_VERSION,
        "type": doc_type,
        "status": "error",
        "error": {
            "category": "protocol",
            "code": code,
            "message": message,
            "recoverability": "resync",
            "span": Value::Null,
        },
    })
}

fn id_string(id: impl Into<IdLike>) -> String {
    id.into().0.to_string()
}

/// A tiny newtype so both `NodeId` and raw `u64` can feed `id_string`.
struct IdLike(u64);

impl From<NodeId> for IdLike {
    fn from(value: NodeId) -> Self {
        IdLike(value.0)
    }
}

impl From<u64> for IdLike {
    fn from(value: u64) -> Self {
        IdLike(value)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    const COUNTER: &str = r#"
screen Counter start
  state count : Number get 0
  when increment(evt: TapEvent) start
    count get count add 1
  end
  view start
    stack start
      label start
        text "Count: {count}"
      end
      button(label: "Increment", onTap: @increment) /
    end
  end
end
"#;

    #[test]
    fn mount_document_shape_is_stable() {
        let (_, output) =
            UiSession::mount(COUNTER, None, BTreeMap::new(), 3, Limits::defaults()).unwrap();
        let mut document = encode_snapshot(&output.snapshot);
        document["session_id"] = json!(3);
        assert_eq!(document["type"], "mount");
        assert_eq!(document["status"], "ok");
        assert_eq!(document["tree_revision"], 1);
        assert_eq!(document["tree"]["kind"], "element");
        assert_eq!(document["tree"]["primitive"], "stack");
        // Node ids are encoded as decimal strings so 64-bit ids survive JSON.
        assert!(document["tree"]["id"].as_str().unwrap().parse::<u64>().is_ok());
    }

    #[test]
    fn dispatch_round_trips_through_json() {
        let (mut session, output) =
            UiSession::mount(COUNTER, None, BTreeMap::new(), 9, Limits::defaults()).unwrap();

        // Locate the button and its binding from the encoded tree.
        let tree = encode_tree(&output.snapshot.tree);
        let button = find_button(&tree).expect("button in encoded tree");
        let node_id = button["id"].clone();
        let binding = &button["handlers"]["tap"];

        let request = json!({
            "session_id": 9,
            "node_id": node_id,
            "handler_id": binding["id"],
            "handler_generation": binding["generation"],
            "event": { "kind": "tap" },
        });
        let event = decode_event(&request).expect("event decodes");
        let patch = session.dispatch(&event).expect("dispatch ok");
        let encoded = encode_patch(&patch);
        assert_eq!(encoded["type"], "patch");
        assert_eq!(encoded["status"], "ok");
        assert_eq!(encoded["target_revision"], 2);
        let ops = encoded["operations"].as_array().unwrap();
        assert!(ops.iter().any(|op| op["op"] == "set_text"));
    }

    #[test]
    fn stale_generation_is_reported_as_stale() {
        let (mut session, output) =
            UiSession::mount(COUNTER, None, BTreeMap::new(), 11, Limits::defaults()).unwrap();
        let tree = encode_tree(&output.snapshot.tree);
        let button = find_button(&tree).expect("button");
        let request = json!({
            "session_id": 11,
            "node_id": button["id"],
            "handler_id": button["handlers"]["tap"]["id"],
            "handler_generation": 999,
            "event": { "kind": "tap" },
        });
        let event = decode_event(&request).unwrap();
        let error = session.dispatch(&event).unwrap_err();
        let encoded = encode_stale(11, &error);
        assert_eq!(encoded["status"], "stale");
        assert_eq!(encoded["error"]["code"], "UI_STALE_HANDLER");
    }

    fn find_button(node: &Value) -> Option<&Value> {
        if node["kind"] == "element" && node["primitive"] == "button" {
            return Some(node);
        }
        node["children"].as_array()?.iter().find_map(find_button)
    }
}
