//! The renderer-neutral, immutable UI intermediate representation.
//!
//! Conditions and lists never appear here: the engine evaluates them and emits
//! the selected, ordered nodes. Property maps use canonical ordering so hashing
//! and encoding are deterministic. See `docs/spec/UI_IR_PROTOCOL.md`.

use std::collections::BTreeMap;

use crate::ui::event::{EventKind, HandlerBinding};
use crate::ui::identity::NodeId;
use crate::ui::value::PropValue;

/// The semantic primitive set. These are schema identifiers, not web tags.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum PrimitiveKind {
    Stack,
    Row,
    Scroll,
    Label,
    Button,
    Input,
    Image,
    Spacer,
}

impl PrimitiveKind {
    #[must_use]
    pub const fn as_str(self) -> &'static str {
        match self {
            PrimitiveKind::Stack => "stack",
            PrimitiveKind::Row => "row",
            PrimitiveKind::Scroll => "scroll",
            PrimitiveKind::Label => "label",
            PrimitiveKind::Button => "button",
            PrimitiveKind::Input => "input",
            PrimitiveKind::Image => "image",
            PrimitiveKind::Spacer => "spacer",
        }
    }

    #[must_use]
    #[allow(clippy::should_implement_trait)]
    pub fn from_str(name: &str) -> Option<Self> {
        match name {
            "stack" => Some(PrimitiveKind::Stack),
            "row" => Some(PrimitiveKind::Row),
            "scroll" => Some(PrimitiveKind::Scroll),
            "label" => Some(PrimitiveKind::Label),
            "button" => Some(PrimitiveKind::Button),
            "input" => Some(PrimitiveKind::Input),
            "image" => Some(PrimitiveKind::Image),
            "spacer" => Some(PrimitiveKind::Spacer),
            _ => None,
        }
    }
}

/// A node in the immutable UI tree.
#[derive(Debug, Clone, PartialEq)]
pub enum Node {
    Element(ElementNode),
    Text(TextNode),
}

/// A semantic element with typed properties, handlers, and children.
#[derive(Debug, Clone, PartialEq)]
pub struct ElementNode {
    pub id: NodeId,
    pub kind: PrimitiveKind,
    pub props: BTreeMap<String, PropValue>,
    pub handlers: BTreeMap<EventKind, HandlerBinding>,
    pub children: Vec<Node>,
}

/// A text leaf.
#[derive(Debug, Clone, PartialEq)]
pub struct TextNode {
    pub id: NodeId,
    pub text: String,
}

impl Node {
    #[must_use]
    pub const fn id(&self) -> NodeId {
        match self {
            Node::Element(element) => element.id,
            Node::Text(text) => text.id,
        }
    }

    /// Total node count including this node.
    #[must_use]
    pub fn node_count(&self) -> usize {
        match self {
            Node::Text(_) => 1,
            Node::Element(element) => {
                1 + element.children.iter().map(Node::node_count).sum::<usize>()
            }
        }
    }
}

/// A complete UI tree with a single root.
#[derive(Debug, Clone, PartialEq)]
pub struct UiTree {
    pub root: Node,
}

impl UiTree {
    #[must_use]
    pub const fn new(root: Node) -> Self {
        Self { root }
    }
}
