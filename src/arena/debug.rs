use std::alloc::{AllocError, Allocator, Layout};
use std::mem::{self, MaybeUninit};
use std::ptr::NonNull;

use super::bump;

/// A debug wrapper for [`bump::Arena`].
///
/// The problem with [`super::ScratchArena`] is that it only "borrows" an underlying
/// [`bump::Arena`]. Once the [`super::ScratchArena`] is dropped it resets the watermark
/// of the underlying [`bump::Arena`], freeing all allocations done since borrowing it.
///
/// It is completely valid for the same [`bump::Arena`] to be borrowed multiple times at once,
/// *as long as* you only use the most recent borrow. Bad example:
/// ```should_panic
/// use naijascript::arena::scratch_arena;
///
/// let mut scratch1 = scratch_arena(None);
/// let mut scratch2 = scratch_arena(None);
///
/// let foo = scratch1.alloc_uninit::<usize>();
///
/// // This will also reset `scratch1`'s allocation.
/// drop(scratch2);
///
/// *foo; // BOOM! ...if it wasn't for our debug wrapper.
/// ```
///
/// To avoid this, this wraps the real [`bump::Arena`] in a "debug" one, which pretends as if every
/// instance of itself is a distinct [`bump::Arena`] instance. Then we use this "debug" [`bump::Arena`]
/// for [`super::ScratchArena`] which allows us to track which borrow is the most recent one.
#[derive(Debug)]
pub enum Arena {
    // Delegate is 'static, because bump::Arena requires no lifetime
    // annotations, and so this mere debug helper cannot use them either.
    Delegated { delegate: &'static bump::Arena, borrow: usize },
    Owned { arena: bump::Arena },
}

impl Drop for Arena {
    fn drop(&mut self) {
        if let Self::Delegated { delegate, borrow } = self {
            let borrows = delegate.borrows.get();
            assert_eq!(*borrow, borrows);
            delegate.borrows.set(borrows - 1);
        }
    }
}

impl Default for Arena {
    fn default() -> Self {
        Self::empty()
    }
}

impl Arena {
    #[must_use]
    pub const fn empty() -> Self {
        Self::Owned { arena: bump::Arena::empty() }
    }

    pub fn new(capacity: usize) -> Result<Self, u32> {
        Ok(Self::Owned { arena: bump::Arena::new(capacity)? })
    }

    pub(super) fn delegated(delegate: &bump::Arena) -> Self {
        let borrow = delegate.borrows.get() + 1;
        delegate.borrows.set(borrow);
        #[allow(clippy::missing_transmute_annotations)]
        Self::Delegated { delegate: unsafe { mem::transmute(delegate) }, borrow }
    }

    #[inline]
    pub(super) fn delegate_target(&self) -> &bump::Arena {
        match *self {
            Self::Delegated { delegate, borrow } => {
                assert!(
                    borrow == delegate.borrows.get(),
                    "Arena already borrowed by a newer ScratchArena"
                );
                delegate
            }
            Self::Owned { ref arena } => arena,
        }
    }

    #[inline]
    pub(super) fn delegate_target_unchecked(&self) -> &bump::Arena {
        match self {
            Self::Delegated { delegate, .. } => delegate,
            Self::Owned { arena } => arena,
        }
    }

    pub fn offset(&self) -> usize {
        self.delegate_target().offset()
    }

    #[allow(clippy::missing_safety_doc)]
    pub unsafe fn reset(&self, to: usize) {
        unsafe { self.delegate_target().reset(to) }
    }

    pub fn alloc_uninit<T>(&self) -> &mut MaybeUninit<T> {
        self.delegate_target().alloc_uninit()
    }

    pub fn alloc_uninit_slice<T>(&self, count: usize) -> &mut [MaybeUninit<T>] {
        self.delegate_target().alloc_uninit_slice(count)
    }

    #[must_use]
    pub fn vec_into_slice<T: Copy>(vec: Vec<T, &Self>) -> &[T] {
        #[allow(clippy::missing_transmute_annotations)]
        let vec = unsafe { mem::transmute(vec) };
        bump::Arena::vec_into_slice(vec)
    }
}

unsafe impl Allocator for Arena {
    fn allocate(&self, layout: Layout) -> Result<NonNull<[u8]>, AllocError> {
        self.delegate_target().alloc_raw(layout.size(), layout.align())
    }

    fn allocate_zeroed(&self, layout: Layout) -> Result<NonNull<[u8]>, AllocError> {
        self.delegate_target().allocate_zeroed(layout)
    }

    // While it is possible to shrink the tail end of the arena, it is
    // not very useful given the existence of scoped scratch arenas.
    unsafe fn deallocate(&self, ptr: NonNull<u8>, layout: Layout) {
        unsafe { self.delegate_target().deallocate(ptr, layout) }
    }

    unsafe fn grow(
        &self,
        ptr: NonNull<u8>,
        old_layout: Layout,
        new_layout: Layout,
    ) -> Result<NonNull<[u8]>, AllocError> {
        unsafe { self.delegate_target().grow(ptr, old_layout, new_layout) }
    }

    unsafe fn grow_zeroed(
        &self,
        ptr: NonNull<u8>,
        old_layout: Layout,
        new_layout: Layout,
    ) -> Result<NonNull<[u8]>, AllocError> {
        unsafe { self.delegate_target().grow_zeroed(ptr, old_layout, new_layout) }
    }

    unsafe fn shrink(
        &self,
        ptr: NonNull<u8>,
        old_layout: Layout,
        new_layout: Layout,
    ) -> Result<NonNull<[u8]>, AllocError> {
        unsafe { self.delegate_target().shrink(ptr, old_layout, new_layout) }
    }
}
