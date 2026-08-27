//! Renderer-neutral declarative UI engine for `NaijaScript`.
//!
//! This module owns the portable half of the UI boundary: the typed IR,
//! deterministic identity, diff/patch protocol, handler registry, resource
//! limits, the `.nui` declarative DSL, and the retained engine session. It
//! deliberately depends on no serialization format or UI framework; transport
//! adapters live outside the core crate. See `docs/spec/UI_IR_PROTOCOL.md` and
//! `docs/adr/0001-renderer-neutral-ui-boundary.md`.

pub mod dsl;
pub mod engine;
pub mod error;
pub mod event;
pub mod identity;
pub mod ir;
pub mod limits;
pub mod patch;
pub mod value;

pub use engine::{MountOutput, UiSession};
pub use error::{Recoverability, UiError, UiErrorCategory, UiResult};
pub use event::{
    ActiveHandler, EventKind, EventPayload, HandlerBinding, HandlerGeneration, HandlerId,
    HandlerRegistry, UiEvent,
};
pub use identity::{IdPath, NodeId, PathSegment};
pub use ir::{ElementNode, Node, PrimitiveKind, TextNode, UiTree};
pub use limits::Limits;
pub use patch::{Patch, PatchOp, Revision, Snapshot, diff};
pub use value::{Color, EnumType, ImageSource, Length, PropValue};
