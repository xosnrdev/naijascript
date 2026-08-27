//! Mandatory resource limits enforced before allocation or dispatch.
//!
//! Every limit is positive and host-configured; there is no implicit default
//! beyond the conservative values here, which a host may override. Limit checks
//! run while constructing IR and reject the whole snapshot or transaction. See
//! `docs/spec/UI_IR_PROTOCOL.md`.

use crate::ui::error::UiError;
use crate::ui::event::UiEvent;
use crate::ui::ir::{Node, UiTree};

/// Host-configured resource ceilings.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Limits {
    pub max_nodes: u32,
    pub max_depth: u32,
    pub max_children_per_node: u32,
    pub max_props_per_node: u32,
    pub max_text_bytes: u32,
    pub max_patch_ops: u32,
    pub max_event_bytes: u32,
}

impl Limits {
    /// Conservative defaults suitable for education-scale UIs.
    #[must_use]
    pub const fn defaults() -> Self {
        Self {
            max_nodes: 10_000,
            max_depth: 128,
            max_children_per_node: 1_024,
            max_props_per_node: 64,
            max_text_bytes: 65_536,
            max_patch_ops: 20_000,
            max_event_bytes: 65_536,
        }
    }

    /// Validates a whole tree against the configured limits.
    pub fn check_tree(&self, tree: &UiTree) -> Result<(), UiError> {
        let mut node_budget = self.max_nodes;
        self.check_node(&tree.root, 1, &mut node_budget)
    }

    fn check_node(&self, node: &Node, depth: u32, node_budget: &mut u32) -> Result<(), UiError> {
        if *node_budget == 0 {
            return Err(UiError::limit("UI_LIMIT_NODES", "tree exceeds max_nodes"));
        }
        *node_budget -= 1;

        if depth > self.max_depth {
            return Err(UiError::limit("UI_LIMIT_DEPTH", "tree exceeds max_depth"));
        }

        match node {
            Node::Text(text) => {
                if text.text.len() > self.max_text_bytes as usize {
                    return Err(UiError::limit("UI_LIMIT_TEXT", "text exceeds max_text_bytes"));
                }
            }
            Node::Element(element) => {
                if element.children.len() > self.max_children_per_node as usize {
                    return Err(UiError::limit(
                        "UI_LIMIT_CHILDREN",
                        "element exceeds max_children_per_node",
                    ));
                }
                if element.props.len() > self.max_props_per_node as usize {
                    return Err(UiError::limit(
                        "UI_LIMIT_PROPS",
                        "element exceeds max_props_per_node",
                    ));
                }
                let mut text_budget = self.max_text_bytes as usize;
                for value in element.props.values() {
                    let weight = value.text_weight();
                    if weight > text_budget {
                        return Err(UiError::limit(
                            "UI_LIMIT_TEXT",
                            "property text exceeds max_text_bytes",
                        ));
                    }
                    text_budget -= weight;
                }
                for child in &element.children {
                    self.check_node(child, depth + 1, node_budget)?;
                }
            }
        }
        Ok(())
    }

    /// Validates an inbound event payload size before dispatch.
    pub fn check_event(&self, event: &UiEvent) -> Result<(), UiError> {
        if event.payload.byte_weight() > self.max_event_bytes as usize {
            return Err(UiError::dropped_event(
                "UI_LIMIT_EVENT",
                "event payload exceeds max_event_bytes",
            ));
        }
        Ok(())
    }

    /// Validates a patch operation count before emission.
    pub fn check_patch_ops(&self, ops: usize) -> Result<(), UiError> {
        if ops > self.max_patch_ops as usize {
            return Err(UiError::limit("UI_LIMIT_PATCH_OPS", "patch exceeds max_patch_ops"));
        }
        Ok(())
    }
}

impl Default for Limits {
    fn default() -> Self {
        Self::defaults()
    }
}
