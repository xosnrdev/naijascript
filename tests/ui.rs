//! Engine-level tests for the renderer-neutral UI subsystem: identity
//! determinism, keyed diffing, the counter demo, stale-event rejection, and
//! limit enforcement.

use std::collections::BTreeMap;

use naijascript::ui::engine::{UiSession, UiVal};
use naijascript::ui::event::{EventKind, EventPayload, UiEvent};
use naijascript::ui::identity::IdPath;
use naijascript::ui::ir::{ElementNode, Node, PrimitiveKind, TextNode};
use naijascript::ui::limits::Limits;
use naijascript::ui::patch::{PatchOp, diff};
use naijascript::ui::value::PropValue;
use naijascript::ui::{Snapshot, UiTree};

const COUNTER: &str = r#"
screen Counter start
  state count : Number get 0
  when increment(evt: TapEvent) start
    count get count add 1
  end
  when reset(evt: TapEvent) start
    count get 0
  end
  view start
    stack(axis: Axis.vertical, spacing: 8px) start
      label start
        text "Count: {count}"
      end
      button(label: "Increment", onTap: @increment) /
      show count pass 0 start
        button(label: "Reset", onTap: @reset) /
      end
    end
  end
end
"#;

fn mount_counter() -> (UiSession, Snapshot) {
    let (session, output) =
        UiSession::mount(COUNTER, None, BTreeMap::new(), 42, Limits::defaults())
            .expect("counter compiles and mounts");
    (session, output.snapshot)
}

fn find_tap_button(node: &Node) -> Option<&ElementNode> {
    match node {
        Node::Text(_) => None,
        Node::Element(element) => {
            if element.kind == PrimitiveKind::Button
                && element.handlers.contains_key(&EventKind::Tap)
            {
                return Some(element);
            }
            element.children.iter().find_map(find_tap_button)
        }
    }
}

fn collect_texts(node: &Node, out: &mut Vec<String>) {
    match node {
        Node::Text(text) => out.push(text.text.clone()),
        Node::Element(element) => {
            for child in &element.children {
                collect_texts(child, out);
            }
        }
    }
}

#[test]
fn identity_is_deterministic_and_key_sensitive() {
    let root = IdPath::root(7);
    assert_eq!(root.slot(0).node_id(), root.slot(0).node_id());
    assert_ne!(root.slot(0).node_id(), root.slot(1).node_id());
    assert_ne!(root.slot(0).key("a").node_id(), root.slot(0).key("b").node_id());
    // A different instance yields different ids for the same structural path.
    assert_ne!(IdPath::root(7).slot(0).node_id(), IdPath::root(8).slot(0).node_id());
}

#[test]
fn mount_produces_stable_ids_across_runs() {
    let (_, first) = mount_counter();
    let (_, second) = mount_counter();
    assert_eq!(first.tree, second.tree, "mount output must be deterministic");
    assert_eq!(first.tree_revision, 1);
}

#[test]
fn counter_snapshot_has_expected_shape() {
    let (_, snapshot) = mount_counter();
    let mut texts = Vec::new();
    collect_texts(&snapshot.tree.root, &mut texts);
    assert!(texts.contains(&"Count: 0".to_owned()), "got texts {texts:?}");
    // The reset button is hidden while count == 0.
    let mut buttons = 0;
    fn count_buttons(node: &Node, buttons: &mut usize) {
        if let Node::Element(element) = node {
            if element.kind == PrimitiveKind::Button {
                *buttons += 1;
            }
            for child in &element.children {
                count_buttons(child, buttons);
            }
        }
    }
    count_buttons(&snapshot.tree.root, &mut buttons);
    assert_eq!(buttons, 1, "only the increment button should be visible");
}

#[test]
fn counter_increment_emits_text_and_insert_patch() {
    let (mut session, snapshot) = mount_counter();
    let button = find_tap_button(&snapshot.tree.root).expect("increment button exists");
    let binding = &button.handlers[&EventKind::Tap];
    let event = UiEvent {
        node_id: button.id,
        handler_id: binding.id,
        handler_generation: binding.generation,
        payload: EventPayload::Tap,
    };

    let patch = session.dispatch(&event).expect("increment dispatches");
    assert_eq!(patch.base_revision, 1);
    assert_eq!(patch.target_revision, 2);

    let has_set_text = patch
        .operations
        .iter()
        .any(|op| matches!(op, PatchOp::SetText { text, .. } if text == "Count: 1"));
    assert!(has_set_text, "expected label text update, got {:?}", patch.operations);

    let has_insert = patch.operations.iter().any(|op| matches!(op, PatchOp::Insert { .. }));
    assert!(has_insert, "expected reset button insert, got {:?}", patch.operations);
}

#[test]
fn stale_event_is_rejected_without_mutation() {
    let (mut session, snapshot) = mount_counter();
    let button = find_tap_button(&snapshot.tree.root).expect("increment button exists");
    let binding = &button.handlers[&EventKind::Tap];
    let stale = UiEvent {
        node_id: button.id,
        handler_id: binding.id,
        handler_generation: binding.generation + 99,
        payload: EventPayload::Tap,
    };
    let error = session.dispatch(&stale).expect_err("stale event rejected");
    assert_eq!(error.code, "UI_STALE_HANDLER");
    assert_eq!(session.revision(), 1, "state must be unchanged after a stale event");
}

#[test]
fn unknown_node_event_is_dropped() {
    let (mut session, _) = mount_counter();
    let bogus = UiEvent {
        node_id: naijascript::ui::identity::NodeId(999_999),
        handler_id: naijascript::ui::event::HandlerId(1),
        handler_generation: 1,
        payload: EventPayload::Tap,
    };
    let error = session.dispatch(&bogus).expect_err("unknown node dropped");
    assert_eq!(error.code, "UI_STALE_HANDLER");
}

// ----- direct IR diff tests -----

fn text_node(path: &IdPath, text: &str) -> Node {
    Node::Text(TextNode { id: path.node_id(), text: text.to_owned() })
}

fn row(path: &IdPath, children: Vec<Node>) -> Node {
    Node::Element(ElementNode {
        id: path.node_id(),
        kind: PrimitiveKind::Stack,
        props: BTreeMap::new(),
        handlers: BTreeMap::new(),
        children,
    })
}

#[test]
fn keyed_reorder_emits_moves_not_replaces() {
    let root = IdPath::root(1);
    let slot = root.slot(0);
    let make = |keys: &[&str]| {
        let children = keys.iter().map(|k| text_node(&slot.key(*k), k)).collect::<Vec<_>>();
        UiTree::new(row(&slot, children))
    };
    let old = make(&["a", "b", "c"]);
    let new = make(&["c", "a", "b"]);
    let ops = diff(&old, &new);
    assert!(ops.iter().any(|op| matches!(op, PatchOp::Move { .. })), "ops {ops:?}");
    assert!(!ops.iter().any(|op| matches!(op, PatchOp::Replace { .. })));
    assert!(!ops.iter().any(|op| matches!(op, PatchOp::SetText { .. })));
}

#[test]
fn keyed_insert_and_remove() {
    let root = IdPath::root(1);
    let slot = root.slot(0);
    let make = |keys: &[&str]| {
        let children = keys.iter().map(|k| text_node(&slot.key(*k), k)).collect::<Vec<_>>();
        UiTree::new(row(&slot, children))
    };
    let old = make(&["a", "b"]);
    let new = make(&["a", "b", "c"]);
    let ops = diff(&old, &new);
    assert_eq!(ops.iter().filter(|op| matches!(op, PatchOp::Insert { .. })).count(), 1);

    let ops = diff(&new, &old);
    assert_eq!(ops.iter().filter(|op| matches!(op, PatchOp::Remove { .. })).count(), 1);
}

#[test]
fn property_change_emits_set_property() {
    let root = IdPath::root(1);
    let slot = root.slot(0);
    let mut props_a = BTreeMap::new();
    props_a.insert("label".to_owned(), PropValue::Text("A".to_owned()));
    let mut props_b = BTreeMap::new();
    props_b.insert("label".to_owned(), PropValue::Text("B".to_owned()));
    let button = |props: BTreeMap<String, PropValue>| {
        UiTree::new(Node::Element(ElementNode {
            id: slot.node_id(),
            kind: PrimitiveKind::Button,
            props,
            handlers: BTreeMap::new(),
            children: Vec::new(),
        }))
    };
    let ops = diff(&button(props_a), &button(props_b));
    assert!(matches!(
        ops.as_slice(),
        [PatchOp::SetProperty { property, value: PropValue::Text(text), .. }]
            if property == "label" && text == "B"
    ));
}

#[test]
fn node_limit_is_enforced_at_mount() {
    let tight = Limits { max_nodes: 2, ..Limits::defaults() };
    let error = UiSession::mount(COUNTER, None, BTreeMap::new(), 1, tight)
        .map(|_| ())
        .expect_err("counter exceeds a 2-node budget");
    assert_eq!(error.code, "UI_LIMIT_NODES");
}

// ----- security, limits, and robustness -----

fn nested_stacks(depth: u32) -> UiTree {
    let root = IdPath::root(1);
    let mut path = root.clone();
    let mut node = Node::Element(ElementNode {
        id: path.node_id(),
        kind: PrimitiveKind::Stack,
        props: BTreeMap::new(),
        handlers: BTreeMap::new(),
        children: Vec::new(),
    });
    for _ in 1..depth {
        path = path.slot(0);
        node = Node::Element(ElementNode {
            id: path.node_id(),
            kind: PrimitiveKind::Stack,
            props: BTreeMap::new(),
            handlers: BTreeMap::new(),
            children: vec![node],
        });
    }
    UiTree::new(node)
}

#[test]
fn depth_limit_is_enforced() {
    let limits = Limits { max_depth: 4, ..Limits::defaults() };
    assert!(limits.check_tree(&nested_stacks(4)).is_ok());
    let error = limits.check_tree(&nested_stacks(6)).expect_err("too deep");
    assert_eq!(error.code, "UI_LIMIT_DEPTH");
}

#[test]
fn children_limit_is_enforced() {
    let limits = Limits { max_children_per_node: 3, ..Limits::defaults() };
    let root = IdPath::root(1);
    let children: Vec<Node> = (0..5u32)
        .map(|i| Node::Text(TextNode { id: root.slot(i).node_id(), text: format!("{i}") }))
        .collect();
    let tree = UiTree::new(Node::Element(ElementNode {
        id: root.node_id(),
        kind: PrimitiveKind::Stack,
        props: BTreeMap::new(),
        handlers: BTreeMap::new(),
        children,
    }));
    let error = limits.check_tree(&tree).expect_err("too many children");
    assert_eq!(error.code, "UI_LIMIT_CHILDREN");
}

#[test]
fn oversized_event_is_dropped() {
    let limits = Limits { max_event_bytes: 8, ..Limits::defaults() };
    let event = UiEvent {
        node_id: naijascript::ui::identity::NodeId(1),
        handler_id: naijascript::ui::event::HandlerId(1),
        handler_generation: 1,
        payload: EventPayload::Change { value: "way too long a value".to_owned() },
    };
    let error = limits.check_event(&event).expect_err("payload too big");
    assert_eq!(error.code, "UI_LIMIT_EVENT");
    assert_eq!(error.recoverability.as_str(), "drop_event");
}

#[test]
fn patch_op_cap_is_enforced() {
    let limits = Limits { max_patch_ops: 2, ..Limits::defaults() };
    assert!(limits.check_patch_ops(2).is_ok());
    let error = limits.check_patch_ops(3).expect_err("too many ops");
    assert_eq!(error.code, "UI_LIMIT_PATCH_OPS");
}

#[test]
fn text_props_never_carry_executable_meaning() {
    // A "script-like" string is still just inert text at the boundary: it is
    // stored verbatim in a Text prop with no parsing or execution.
    const SRC: &str = r#"
screen S start
  view start
    button(label: "<script>alert(1)</script>") /
  end
end
"#;
    let (_, output) =
        UiSession::mount(SRC, None, BTreeMap::new(), 1, Limits::defaults()).expect("mounts");
    fn find_label(node: &Node) -> Option<String> {
        if let Node::Element(element) = node {
            if let Some(PropValue::Text(text)) = element.props.get("label") {
                return Some(text.clone());
            }
            return element.children.iter().find_map(find_label);
        }
        None
    }
    assert_eq!(
        find_label(&output.snapshot.tree.root).as_deref(),
        Some("<script>alert(1)</script>")
    );
}

#[test]
fn malformed_sources_return_errors_not_panics() {
    let cases = [
        "screen",                                      // truncated
        "screen S start view start button( / end end", // unterminated proplist
        "screen S start view start wat / end end",     // unknown element
        "screen S start view start text \"unterminated",
    ];
    for src in cases {
        let result = UiSession::mount(src, None, BTreeMap::new(), 1, Limits::defaults());
        assert!(result.is_err(), "expected compile error for {src:?}");
    }
}

#[test]
fn large_keyed_list_mounts_and_respects_node_budget() {
    const SRC: &str = r#"
screen Big(items: List(Text)) start
  view start
    stack start
      each item in items key item start
        label start
          text item
        end
      end
    end
  end
end
"#;
    let items: Vec<UiVal> = (0..500).map(|i| UiVal::Text(format!("item-{i}"))).collect();
    let mut params = BTreeMap::new();
    params.insert("items".to_owned(), UiVal::List(items));

    let (_, output) = UiSession::mount(SRC, None, params.clone(), 1, Limits::defaults())
        .expect("mounts at scale");
    // root stack + inner stack + 500 labels + 500 text = 1002 nodes.
    assert_eq!(output.snapshot.tree.root.node_count(), 1002);

    let tight = Limits { max_nodes: 100, ..Limits::defaults() };
    let error = UiSession::mount(SRC, None, params, 1, tight)
        .map(|_| ())
        .expect_err("node budget rejects the big list");
    assert_eq!(error.code, "UI_LIMIT_NODES");
}

#[test]
fn each_renders_keyed_list_from_params() {
    const LIST: &str = r#"
screen List(items: List(Text)) start
  view start
    stack start
      each item in items key item start
        label start
          text item
        end
      end
    end
  end
end
"#;
    let mut params = BTreeMap::new();
    params.insert(
        "items".to_owned(),
        UiVal::List(vec![UiVal::Text("first".to_owned()), UiVal::Text("second".to_owned())]),
    );
    let (_, output) =
        UiSession::mount(LIST, None, params, 5, Limits::defaults()).expect("list screen mounts");
    let mut texts = Vec::new();
    collect_texts(&output.snapshot.tree.root, &mut texts);
    assert_eq!(texts, vec!["first".to_owned(), "second".to_owned()]);
}
