use std::fmt;
use std::ops::Deref;

use super::{Arena, ArenaString};

/// A custom clone-on-write smart pointer for [`super::ArenaString`].
///
/// Similar to [`std::borrow::Cow<str>`] but [`ArenaString`] for owned data.
#[derive(Debug, Clone)]
pub enum ArenaCow<'arena, 'src> {
    Borrowed(&'src str),
    Owned(ArenaString<'arena>),
}

impl<'arena, 'src> ArenaCow<'arena, 'src> {
    /// Creates a new borrowed `ArenaCow` from a string reference.
    #[inline]
    #[must_use]
    pub const fn borrowed(s: &'src str) -> Self {
        ArenaCow::Borrowed(s)
    }

    /// Creates a new owned [`ArenaCow`] from an `ArenaString`.
    #[inline]
    #[must_use]
    pub const fn owned(s: ArenaString<'arena>) -> Self {
        ArenaCow::Owned(s)
    }

    /// Creates an owned [`ArenaCow`] by copying a string into the arena.
    #[inline]
    pub fn from_str(arena: &'arena Arena, s: &str) -> Self {
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
    pub fn into_owned(self, arena: &'arena Arena) -> ArenaString<'arena> {
        match self {
            ArenaCow::Borrowed(s) => ArenaString::from_str(arena, s),
            ArenaCow::Owned(s) => s,
        }
    }

    /// Gets a mutable reference to the owned data, cloning if necessary.
    pub fn to_mut(&mut self, arena: &'arena Arena) -> &mut ArenaString<'arena> {
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
}

impl AsRef<str> for ArenaCow<'_, '_> {
    #[inline]
    fn as_ref(&self) -> &str {
        match self {
            ArenaCow::Borrowed(s) => s,
            ArenaCow::Owned(s) => s.as_str(),
        }
    }
}

impl Deref for ArenaCow<'_, '_> {
    type Target = str;

    #[inline]
    fn deref(&self) -> &Self::Target {
        self.as_ref()
    }
}

impl fmt::Display for ArenaCow<'_, '_> {
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(self.as_ref(), f)
    }
}

impl PartialEq for ArenaCow<'_, '_> {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        self.as_ref() == other.as_ref()
    }
}

impl PartialEq<str> for ArenaCow<'_, '_> {
    #[inline]
    fn eq(&self, other: &str) -> bool {
        self.as_ref() == other
    }
}

impl PartialEq<&str> for ArenaCow<'_, '_> {
    #[inline]
    fn eq(&self, other: &&str) -> bool {
        self.as_ref() == *other
    }
}

impl PartialEq<String> for ArenaCow<'_, '_> {
    #[inline]
    fn eq(&self, other: &String) -> bool {
        self.as_ref() == other.as_str()
    }
}

impl Eq for ArenaCow<'_, '_> {}

impl PartialOrd for ArenaCow<'_, '_> {
    #[inline]
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for ArenaCow<'_, '_> {
    #[inline]
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.as_ref().cmp(other.as_ref())
    }
}

impl<'src> From<&'src str> for ArenaCow<'_, 'src> {
    #[inline]
    fn from(s: &'src str) -> Self {
        ArenaCow::Borrowed(s)
    }
}

impl<'arena> From<ArenaString<'arena>> for ArenaCow<'arena, '_> {
    #[inline]
    fn from(s: ArenaString<'arena>) -> Self {
        ArenaCow::Owned(s)
    }
}
