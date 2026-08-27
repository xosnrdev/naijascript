//! Typed events and the core-owned handler registry.
//!
//! Renderers emit data, never callbacks. Every event is validated against the
//! active `(node, event kind, handler id, generation)` tuple before any user
//! code runs, giving deterministic stale-event rejection. See
//! `docs/spec/UI_IR_PROTOCOL.md`.

use std::collections::BTreeMap;

use crate::ui::error::UiError;
use crate::ui::identity::NodeId;

/// A closed set of event kinds for the MVP.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum EventKind {
    Tap,
    Change,
    Submit,
    Focus,
}

impl EventKind {
    #[must_use]
    pub const fn as_str(self) -> &'static str {
        match self {
            EventKind::Tap => "tap",
            EventKind::Change => "change",
            EventKind::Submit => "submit",
            EventKind::Focus => "focus",
        }
    }

    #[must_use]
    #[allow(clippy::should_implement_trait)]
    pub fn from_str(name: &str) -> Option<Self> {
        match name {
            "tap" => Some(EventKind::Tap),
            "change" => Some(EventKind::Change),
            "submit" => Some(EventKind::Submit),
            "focus" => Some(EventKind::Focus),
            _ => None,
        }
    }
}

/// An opaque handler routing identifier (not an authorization token).
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct HandlerId(pub u64);

/// A monotonically increasing generation scoped to a session.
pub type HandlerGeneration = u64;

/// The routing record bound to a node's event slot in the IR.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct HandlerBinding {
    pub id: HandlerId,
    pub generation: HandlerGeneration,
    pub event_kind: EventKind,
}

/// The closed set of typed event payloads.
#[derive(Debug, Clone, PartialEq)]
pub enum EventPayload {
    Tap,
    Change { value: String },
    Submit { value: Option<String> },
    Focus { focused: bool },
}

impl EventPayload {
    #[must_use]
    pub const fn kind(&self) -> EventKind {
        match self {
            EventPayload::Tap => EventKind::Tap,
            EventPayload::Change { .. } => EventKind::Change,
            EventPayload::Submit { .. } => EventKind::Submit,
            EventPayload::Focus { .. } => EventKind::Focus,
        }
    }

    /// Approximate payload size for the `max_event_bytes` limit.
    #[must_use]
    pub fn byte_weight(&self) -> usize {
        match self {
            EventPayload::Change { value } => value.len(),
            EventPayload::Submit { value } => value.as_ref().map_or(0, String::len),
            EventPayload::Tap | EventPayload::Focus { .. } => 0,
        }
    }
}

/// A fully-specified inbound event from a renderer.
#[derive(Debug, Clone, PartialEq)]
pub struct UiEvent {
    pub node_id: NodeId,
    pub handler_id: HandlerId,
    pub handler_generation: HandlerGeneration,
    pub payload: EventPayload,
}

/// The logical handler a validated event resolves to.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ActiveHandler {
    /// The named handler declared in source (e.g. `increment`).
    pub name: String,
    pub id: HandlerId,
    pub generation: HandlerGeneration,
    pub event_kind: EventKind,
}

/// Core-owned map from active binding tuples to logical handlers.
///
/// Rebuilt on every render. Only bindings registered by the most recent
/// accepted tree are dispatchable.
#[derive(Debug, Default)]
pub struct HandlerRegistry {
    active: BTreeMap<(NodeId, EventKind), ActiveHandler>,
}

impl HandlerRegistry {
    #[must_use]
    pub fn new() -> Self {
        Self { active: BTreeMap::new() }
    }

    /// Clears all bindings, retiring the previous generation.
    pub fn begin_generation(&mut self) {
        self.active.clear();
    }

    /// Registers an active binding for the current generation.
    pub fn register(&mut self, node: NodeId, handler: ActiveHandler) {
        self.active.insert((node, handler.event_kind), handler);
    }

    #[must_use]
    pub fn binding(&self, node: NodeId, kind: EventKind) -> Option<&ActiveHandler> {
        self.active.get(&(node, kind))
    }

    /// Validates an inbound event against the active registry.
    ///
    /// Returns the resolved handler, or a non-fatal dropped-event error when
    /// the node is unknown, the binding is absent, the kind mismatches, or the
    /// generation is stale.
    pub fn validate(&self, event: &UiEvent) -> Result<ActiveHandler, UiError> {
        let kind = event.payload.kind();
        let Some(active) = self.active.get(&(event.node_id, kind)) else {
            return Err(UiError::dropped_event(
                "UI_STALE_HANDLER",
                "no active handler for this node and event kind",
            ));
        };
        if active.id != event.handler_id {
            return Err(UiError::dropped_event(
                "UI_STALE_HANDLER",
                "handler id does not match the active binding",
            ));
        }
        if active.generation != event.handler_generation {
            return Err(UiError::dropped_event("UI_STALE_HANDLER", "handler generation is stale"));
        }
        Ok(active.clone())
    }
}
