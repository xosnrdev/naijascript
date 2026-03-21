use std::fmt;
use std::ops::Deref;

use super::{Arena, ArenaString, PoolSet};

/// A custom clone-on-write smart pointer for [`super::ArenaString`].
///
/// Similar to [`std::borrow::Cow<str>`] but [`ArenaString`] for owned data.
/// Uses a single lifetime because arena bytes are at a stable address
/// and source strings satisfy `'src: 'a`.
#[derive(Debug)]
pub enum ArenaCow<'a> {
    Borrowed(&'a str),
    Owned(ArenaString<'a>),
}

impl Clone for ArenaCow<'_> {
    /// Cloning an Owned string returns Borrowed pointing to the same arena bytes.
    /// No allocation, just a pointer copy.
    fn clone(&self) -> Self {
        match self {
            ArenaCow::Borrowed(s) => ArenaCow::Borrowed(s),
            ArenaCow::Owned(s) => ArenaCow::Borrowed(s.as_arena_str()),
        }
    }
}

impl<'a> ArenaCow<'a> {
    /// Creates a new borrowed `ArenaCow` from a string reference.
    #[inline]
    #[must_use]
    pub const fn borrowed(s: &'a str) -> Self {
        ArenaCow::Borrowed(s)
    }

    /// Creates a new owned [`ArenaCow`] from an `ArenaString`.
    #[inline]
    #[must_use]
    pub const fn owned(s: ArenaString<'a>) -> Self {
        ArenaCow::Owned(s)
    }

    /// Creates an owned [`ArenaCow`] by copying a string into the arena.
    #[inline]
    pub fn from_str(arena: &'a Arena, s: &str) -> Self {
        ArenaCow::Owned(ArenaString::from_str(arena, s))
    }

    /// Returns `true` if the data is borrowed.
    #[inline]
    #[must_use]
    pub const fn is_borrowed(&self) -> bool {
        matches!(self, ArenaCow::Borrowed(..))
    }

    /// Returns `true` if the data is owned.
    #[inline]
    #[must_use]
    pub const fn is_owned(&self) -> bool {
        matches!(self, ArenaCow::Owned(..))
    }

    /// Extracts the owned data, cloning if the data is borrowed.
    pub fn into_owned(self, arena: &'a Arena) -> ArenaString<'a> {
        match self {
            ArenaCow::Borrowed(s) => ArenaString::from_str(arena, s),
            ArenaCow::Owned(s) => s,
        }
    }

    /// Gets a mutable reference to the owned data, cloning if necessary.
    pub fn to_mut(&mut self, arena: &'a Arena) -> &mut ArenaString<'a> {
        match self {
            ArenaCow::Borrowed(s) => {
                *self = ArenaCow::Owned(ArenaString::from_str(arena, s));
                match self {
                    ArenaCow::Owned(s) => s,
                    ArenaCow::Borrowed(_) => unreachable!(),
                }
            }
            ArenaCow::Owned(s) => s,
        }
    }

    /// Returns the length of the string in bytes.
    #[inline]
    #[must_use]
    pub fn len(&self) -> usize {
        self.as_ref().len()
    }

    /// Returns `true` if the string is empty.
    #[inline]
    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.as_ref().is_empty()
    }

    /// Copies transient string data to a pool slot (or arena fallback).
    /// Borrowed pointers into the frame arena or pool slots are copied
    /// to their own pool slots. Source literals and persistent-arena
    /// Borrowed pointers pass through unchanged.
    /// Owned strings already on the persistent arena pass through.
    #[must_use]
    pub(crate) fn promote(self, pool: &PoolSet<'a>, frame: &Arena) -> Self {
        let persistent = pool.arena();
        match self {
            ArenaCow::Borrowed(s) => {
                if frame.contains_ptr(s.as_ptr()) || pool.contains(s.as_ptr()) {
                    ArenaCow::Owned(pool.alloc_str(s))
                } else {
                    ArenaCow::Borrowed(s)
                }
            }
            ArenaCow::Owned(s) => {
                if std::ptr::eq(s.arena(), persistent) {
                    return ArenaCow::Owned(s);
                }
                // Pool-allocated strings report the backing arena as their allocator,
                // so the ptr_eq check above already catches double-promotes for
                // pool-backed strings.
                ArenaCow::Owned(pool.alloc_str(s.as_str()))
            }
        }
    }
}

impl AsRef<str> for ArenaCow<'_> {
    #[inline]
    fn as_ref(&self) -> &str {
        match self {
            ArenaCow::Borrowed(s) => s,
            ArenaCow::Owned(s) => s.as_str(),
        }
    }
}

impl Deref for ArenaCow<'_> {
    type Target = str;

    #[inline]
    fn deref(&self) -> &Self::Target {
        self.as_ref()
    }
}

impl fmt::Display for ArenaCow<'_> {
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(self.as_ref(), f)
    }
}

impl PartialEq for ArenaCow<'_> {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        self.as_ref() == other.as_ref()
    }
}

impl PartialEq<str> for ArenaCow<'_> {
    #[inline]
    fn eq(&self, other: &str) -> bool {
        self.as_ref() == other
    }
}

impl PartialEq<&str> for ArenaCow<'_> {
    #[inline]
    fn eq(&self, other: &&str) -> bool {
        self.as_ref() == *other
    }
}

impl PartialEq<String> for ArenaCow<'_> {
    #[inline]
    fn eq(&self, other: &String) -> bool {
        self.as_ref() == other.as_str()
    }
}

impl Eq for ArenaCow<'_> {}

impl PartialOrd for ArenaCow<'_> {
    #[inline]
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for ArenaCow<'_> {
    #[inline]
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.as_ref().cmp(other.as_ref())
    }
}

impl<'a> From<&'a str> for ArenaCow<'a> {
    #[inline]
    fn from(s: &'a str) -> Self {
        ArenaCow::Borrowed(s)
    }
}

impl<'a> From<ArenaString<'a>> for ArenaCow<'a> {
    #[inline]
    fn from(s: ArenaString<'a>) -> Self {
        ArenaCow::Owned(s)
    }
}
