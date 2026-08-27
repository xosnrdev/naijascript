//! Deterministic, renderer-independent node identity.
//!
//! A [`NodeId`] is derived only from the mounted screen instance, the static
//! compiler-assigned child slot at each boundary, and explicit list/element
//! keys on the path. It never contains a memory address, source path, user
//! text hash of secrets, or a dynamic array index. The derivation is a pure
//! function of the path, so it is stable across process runs, architectures,
//! and map iteration order. See `docs/spec/UI_IR_PROTOCOL.md`.

/// An opaque, stable identifier for a UI node.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct NodeId(pub u64);

/// One step along a node's identity path.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PathSegment {
    /// A static, compiler-assigned child slot.
    Slot(u32),
    /// An explicit `key` on a list item or element.
    Key(String),
}

/// A canonical identity path rooted at a screen instance.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IdPath {
    instance: u64,
    segments: Vec<PathSegment>,
}

impl IdPath {
    /// Starts a path at the given screen instance id.
    #[must_use]
    pub fn root(instance: u64) -> Self {
        Self { instance, segments: Vec::new() }
    }

    /// Returns a new path extended by a static slot.
    #[must_use]
    pub fn slot(&self, slot: u32) -> Self {
        let mut next = self.clone();
        next.segments.push(PathSegment::Slot(slot));
        next
    }

    /// Returns a new path extended by an explicit key.
    #[must_use]
    pub fn key(&self, key: impl Into<String>) -> Self {
        let mut next = self.clone();
        next.segments.push(PathSegment::Key(key.into()));
        next
    }

    /// Derives the opaque node id for this path.
    ///
    /// Uses FNV-1a over a canonical, unambiguous encoding. Length-prefixed
    /// keys prevent collisions between paths whose segment boundaries differ.
    #[must_use]
    pub fn node_id(&self) -> NodeId {
        const OFFSET: u64 = 0xcbf2_9ce4_8422_2325;
        const PRIME: u64 = 0x0000_0100_0000_01b3;

        let mut hash = OFFSET;
        let mut mix = |bytes: &[u8]| {
            for &byte in bytes {
                hash ^= u64::from(byte);
                hash = hash.wrapping_mul(PRIME);
            }
        };

        mix(b"nsui/1\x00");
        mix(&self.instance.to_le_bytes());
        for segment in &self.segments {
            match segment {
                PathSegment::Slot(slot) => {
                    mix(&[0x01]);
                    mix(&slot.to_le_bytes());
                }
                PathSegment::Key(key) => {
                    mix(&[0x02]);
                    let len = u64::try_from(key.len()).unwrap_or(u64::MAX);
                    mix(&len.to_le_bytes());
                    mix(key.as_bytes());
                }
            }
        }

        // Reserve 0 as a sentinel for "no node".
        NodeId(hash | 1)
    }
}
