//! Snapshot and atomic patch protocol plus a keyed tree-diff.
//!
//! `diff` compares two immutable trees and produces an ordered operation list
//! that, applied in order to a staging copy of the old tree, yields the new
//! tree. Reconciliation matches children by their stable [`NodeId`], so keyed
//! reordering preserves identity and emits moves. See
//! `docs/spec/UI_IR_PROTOCOL.md`.

use std::collections::BTreeMap;

use crate::ui::event::{EventKind, HandlerBinding};
use crate::ui::identity::NodeId;
use crate::ui::ir::{Node, UiTree};
use crate::ui::value::PropValue;

/// A session-scoped monotonic revision counter.
pub type Revision = u64;

/// A full tree, sent on mount or explicit resynchronization.
#[derive(Debug, Clone, PartialEq)]
pub struct Snapshot {
    pub session_id: u64,
    pub tree_revision: Revision,
    pub tree: UiTree,
}

/// An atomic, ordered set of operations transforming one revision into the next.
#[derive(Debug, Clone, PartialEq)]
pub struct Patch {
    pub session_id: u64,
    pub base_revision: Revision,
    pub target_revision: Revision,
    pub operations: Vec<PatchOp>,
}

/// A single patch operation. Each references the tree produced by all preceding
/// operations in the same transaction.
#[derive(Debug, Clone, PartialEq)]
pub enum PatchOp {
    Insert { parent: NodeId, index: usize, node: Node },
    Remove { node: NodeId },
    Move { node: NodeId, parent: NodeId, index: usize },
    Replace { node: NodeId, replacement: Node },
    SetProperty { node: NodeId, property: String, value: PropValue },
    RemoveProperty { node: NodeId, property: String },
    SetText { node: NodeId, text: String },
    BindHandler { node: NodeId, event_kind: EventKind, binding: HandlerBinding },
    UnbindHandler { node: NodeId, event_kind: EventKind },
}

/// Computes the ordered operations that transform `old` into `new`.
#[must_use]
pub fn diff(old: &UiTree, new: &UiTree) -> Vec<PatchOp> {
    let mut ops = Vec::new();
    diff_node(&old.root, &new.root, &mut ops);
    ops
}

fn diff_node(old: &Node, new: &Node, ops: &mut Vec<PatchOp>) {
    // Same-path nodes share an id. A differing id at the same position means the
    // node was structurally replaced.
    if old.id() != new.id() {
        ops.push(PatchOp::Replace { node: old.id(), replacement: new.clone() });
        return;
    }

    match (old, new) {
        (Node::Text(old_text), Node::Text(new_text)) => {
            if old_text.text != new_text.text {
                ops.push(PatchOp::SetText { node: new_text.id, text: new_text.text.clone() });
            }
        }
        (Node::Element(old_el), Node::Element(new_el)) => {
            if old_el.kind != new_el.kind {
                ops.push(PatchOp::Replace {
                    node: old_el.id,
                    replacement: Node::Element(new_el.clone()),
                });
                return;
            }
            let id = new_el.id;

            for (property, value) in &new_el.props {
                if old_el.props.get(property) != Some(value) {
                    ops.push(PatchOp::SetProperty {
                        node: id,
                        property: property.clone(),
                        value: value.clone(),
                    });
                }
            }
            for property in old_el.props.keys() {
                if !new_el.props.contains_key(property) {
                    ops.push(PatchOp::RemoveProperty { node: id, property: property.clone() });
                }
            }

            for (kind, binding) in &new_el.handlers {
                if old_el.handlers.get(kind) != Some(binding) {
                    ops.push(PatchOp::BindHandler {
                        node: id,
                        event_kind: *kind,
                        binding: binding.clone(),
                    });
                }
            }
            for kind in old_el.handlers.keys() {
                if !new_el.handlers.contains_key(kind) {
                    ops.push(PatchOp::UnbindHandler { node: id, event_kind: *kind });
                }
            }

            diff_children(id, &old_el.children, &new_el.children, ops);
        }
        // A text/element swap at the same id is a replacement.
        _ => ops.push(PatchOp::Replace { node: old.id(), replacement: new.clone() }),
    }
}

fn diff_children(parent: NodeId, old: &[Node], new: &[Node], ops: &mut Vec<PatchOp>) {
    let old_by_id: BTreeMap<NodeId, &Node> = old.iter().map(|node| (node.id(), node)).collect();
    let new_ids: BTreeMap<NodeId, ()> = new.iter().map(|node| (node.id(), ())).collect();

    // Remove children absent from the new tree.
    for node in old {
        if !new_ids.contains_key(&node.id()) {
            ops.push(PatchOp::Remove { node: node.id() });
        }
    }

    // Working order after removals: old survivors in old order.
    let mut working: Vec<NodeId> =
        old.iter().map(Node::id).filter(|id| new_ids.contains_key(id)).collect();

    for (index, new_child) in new.iter().enumerate() {
        let nid = new_child.id();
        if let Some(old_child) = old_by_id.get(&nid) {
            diff_node(old_child, new_child, ops);
            let current = working.iter().position(|id| *id == nid).expect("survivor present");
            if current != index {
                working.remove(current);
                working.insert(index, nid);
                ops.push(PatchOp::Move { node: nid, parent, index });
            }
        } else {
            working.insert(index, nid);
            ops.push(PatchOp::Insert { parent, index, node: new_child.clone() });
        }
    }
}
