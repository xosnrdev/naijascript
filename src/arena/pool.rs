use std::alloc::{Allocator, Layout};
use std::cell::Cell;
use std::ptr::NonNull;

use super::{Arena, ArenaString};

/// Number of size classes managed by the pool.
const CLASS_COUNT: u32 = 20;

/// Slot sizes for each class.
/// Classes 0-15: 8-byte spacing (8, 16, 24, ..., 128).
/// Classes 16-19: 32-byte spacing (160, 192, 224, 256).
const SLOT_SIZES: [u32; CLASS_COUNT as usize] = {
    let mut sizes = [0u32; CLASS_COUNT as usize];
    let mut i = 0u32;
    while i < 16 {
        sizes[i as usize] = (i + 1) * 8;
        i += 1;
    }
    while i < CLASS_COUNT {
        sizes[i as usize] = 128 + (i - 15) * 32;
        i += 1;
    }
    sizes
};

/// Slot counts per class. Budget: ~1 MiB total.
/// Popular small classes get more slots; rare large classes get fewer.
const SLOT_COUNTS: [u32; CLASS_COUNT as usize] = {
    let mut counts = [0u32; CLASS_COUNT as usize];
    let mut i = 0u32;
    while i < 4 {
        counts[i as usize] = 16_384;
        i += 1;
    }
    while i < 8 {
        counts[i as usize] = 4_096;
        i += 1;
    }
    while i < 16 {
        counts[i as usize] = 1_024;
        i += 1;
    }
    while i < CLASS_COUNT {
        counts[i as usize] = 512;
        i += 1;
    }
    counts
};

/// Maps a byte count to the pool class index (0..19).
/// Returns `None` if the request exceeds the largest pooled size (256 bytes).
#[inline]
fn size_class(n: u32) -> Option<u32> {
    if n <= 128 {
        // Classes 0..15, 8-byte spacing. n=0 maps to class 0 (8-byte slot).
        Some(n.saturating_sub(1) / 8)
    } else if n <= 256 {
        Some(16 + (n - 129) / 32)
    } else {
        None
    }
}

/// A contiguous block of equal-sized slots, bump-allocated from the backing arena.
struct SlotBlock {
    base: NonNull<u8>,
    slot_size: u32,
    slot_count: u32,
    /// Next never-used slot index. Slots below `bump` have been handed out
    /// at least once. Slots at or above `bump` are virgin.
    bump: Cell<u32>,
}

impl SlotBlock {
    /// Allocates the block from the arena. All slot sizes are multiples of 8,
    /// so an 8-byte aligned base gives every slot natural alignment.
    fn new(arena: &Arena, slot_size: u32, slot_count: u32) -> Self {
        debug_assert!(slot_size > 0 && slot_size.is_multiple_of(8));
        debug_assert!(slot_count > 0);

        let total = slot_size as usize * slot_count as usize;
        let layout = Layout::from_size_align(total, 8).expect("invalid layout");
        let raw = arena.allocate(layout).expect("arena capacity exceeded during pool init");

        Self { base: raw.cast(), slot_size, slot_count, bump: Cell::new(0) }
    }

    #[inline]
    fn slot_ptr(&self, index: u32) -> NonNull<u8> {
        debug_assert!(index < self.slot_count);
        unsafe { self.base.add(index as usize * self.slot_size as usize) }
    }

    /// Computes the slot index for a pointer. Returns `None` if the pointer
    /// is outside this block or not aligned to a slot boundary.
    #[inline]
    fn index_of(&self, ptr: *const u8) -> Option<u32> {
        let offset = (ptr as usize).wrapping_sub(self.base.as_ptr() as usize);
        let total = self.slot_size as usize * self.slot_count as usize;
        if offset >= total || !offset.is_multiple_of(self.slot_size as usize) {
            return None;
        }
        Some((offset / self.slot_size as usize) as u32)
    }

    #[inline]
    fn contains(&self, ptr: *const u8) -> bool {
        let offset = (ptr as usize).wrapping_sub(self.base.as_ptr() as usize);
        offset < self.slot_size as usize * self.slot_count as usize
    }
}

/// LIFO stack of free slot indices. Stored as an external `u32` array
/// allocated from the backing arena.
struct FreeList {
    indices: NonNull<u32>,
    capacity: u32,
    len: Cell<u32>,
}

impl FreeList {
    fn new(arena: &Arena, capacity: u32) -> Self {
        debug_assert!(capacity > 0);

        let layout =
            Layout::from_size_align(capacity as usize * size_of::<u32>(), align_of::<u32>())
                .expect("invalid layout");
        let raw = arena.allocate(layout).expect("arena capacity exceeded during pool init");

        Self { indices: raw.cast(), capacity, len: Cell::new(0) }
    }

    #[inline]
    fn push(&self, index: u32) {
        let len = self.len.get();
        debug_assert!(len < self.capacity, "free list overflow");
        unsafe { self.indices.as_ptr().add(len as usize).write(index) };
        self.len.set(len + 1);
    }

    #[inline]
    fn pop(&self) -> Option<u32> {
        let len = self.len.get();
        if len == 0 {
            return None;
        }
        let new_len = len - 1;
        self.len.set(new_len);
        Some(unsafe { self.indices.as_ptr().add(new_len as usize).read() })
    }

    #[inline]
    fn len(&self) -> u32 {
        self.len.get()
    }
}

/// One pool per size class. Manages a `SlotBlock` of equal-sized slots
/// with a `FreeList` for O(1) alloc/dealloc.
struct Pool {
    block: SlotBlock,
    free: FreeList,
    live_count: Cell<u32>,
}

impl Pool {
    fn new(arena: &Arena, slot_size: u32, slot_count: u32) -> Self {
        Self {
            block: SlotBlock::new(arena, slot_size, slot_count),
            free: FreeList::new(arena, slot_count),
            live_count: Cell::new(0),
        }
    }

    /// Allocates a slot. Returns a fat pointer (ptr + `slot_size` as length).
    /// Returns `None` if the pool is exhausted.
    fn alloc(&self) -> Option<NonNull<[u8]>> {
        let ptr;

        if let Some(index) = self.free.pop() {
            ptr = self.block.slot_ptr(index);

            if cfg!(debug_assertions) {
                let slot = unsafe {
                    std::slice::from_raw_parts(ptr.as_ptr(), self.block.slot_size as usize)
                };
                assert!(
                    slot.iter().all(|&b| b == 0xDD),
                    "free-list slot was not properly poisoned — possible double-alloc or corruption"
                );
            }
        } else {
            let bump = self.block.bump.get();
            if bump >= self.block.slot_count {
                return None;
            }
            ptr = self.block.slot_ptr(bump);
            self.block.bump.set(bump + 1);
        }

        self.live_count.set(self.live_count.get() + 1);
        self.assert_conservation();
        Some(NonNull::slice_from_raw_parts(ptr, self.block.slot_size as usize))
    }

    /// Returns a slot to the pool.
    ///
    /// # Safety
    ///
    /// Caller must guarantee no live references exist into this slot.
    unsafe fn dealloc(&self, ptr: NonNull<u8>) {
        let index = self
            .block
            .index_of(ptr.as_ptr())
            .expect("dealloc pointer not within pool block or misaligned");
        debug_assert!(
            index < self.block.bump.get(),
            "dealloc of virgin slot (index {index} >= bump {})",
            self.block.bump.get()
        );

        if cfg!(debug_assertions) {
            unsafe { ptr.as_ptr().write_bytes(0xDD, self.block.slot_size as usize) };
        }

        self.free.push(index);

        let live = self.live_count.get();
        debug_assert!(live > 0, "dealloc when live_count is zero (double-free?)");
        self.live_count.set(live - 1);
        self.assert_conservation();
    }

    /// Resets the pool to its initial state. O(1).
    ///
    /// # Safety
    ///
    /// Caller must guarantee no live references exist into any slot.
    #[cfg(test)]
    unsafe fn reset(&self) {
        if cfg!(debug_assertions) {
            let total = self.block.slot_size as usize * self.block.slot_count as usize;
            unsafe { self.block.base.as_ptr().write_bytes(0xDD, total) };
        }
        self.free.len.set(0);
        self.block.bump.set(0);
        self.live_count.set(0);
    }

    #[inline]
    fn contains(&self, ptr: *const u8) -> bool {
        self.block.contains(ptr)
    }

    /// Invariant I-2: live + free + virgin == total.
    #[inline]
    fn assert_conservation(&self) {
        debug_assert_eq!(
            self.live_count.get()
                + self.free.len()
                + (self.block.slot_count - self.block.bump.get()),
            self.block.slot_count,
            "pool conservation invariant violated"
        );
    }
}

/// Collection of all 20 size-class pools. Single entry point for the runtime.
pub(crate) struct PoolSet<'a> {
    pools: [Pool; CLASS_COUNT as usize],
    arena: &'a Arena,
}

impl<'a> PoolSet<'a> {
    /// Creates all 20 pools, allocating `SlotBlocks` and `FreeLists` from the arena.
    /// Consumes ~1.3 MiB of arena space at startup.
    pub(crate) fn new(arena: &'a Arena) -> Self {
        let pools = std::array::from_fn(|i| Pool::new(arena, SLOT_SIZES[i], SLOT_COUNTS[i]));
        Self { pools, arena }
    }

    /// Allocates a byte buffer of at least `size` bytes.
    /// Small strings (<= 256 bytes) are served from the pool.
    /// Oversized or pool-exhausted requests fall back to arena bump.
    pub(crate) fn alloc(&self, size: u32) -> NonNull<[u8]> {
        if let Some(class) = size_class(size)
            && let Some(ptr) = self.pools[class as usize].alloc()
        {
            return ptr;
        }
        let layout = Layout::from_size_align(size as usize, 1).expect("invalid layout");
        self.arena.allocate(layout).expect("arena capacity exceeded")
    }

    /// Returns a buffer to its pool. No-op for oversized or arena-fallback buffers.
    ///
    /// # Safety
    ///
    /// Caller must guarantee no live references exist into the buffer.
    /// `size` must match the size passed to the corresponding `alloc`.
    pub(crate) unsafe fn dealloc(&self, ptr: NonNull<u8>, size: u32) {
        if let Some(class) = size_class(size)
            && self.pools[class as usize].contains(ptr.as_ptr())
        {
            unsafe { self.pools[class as usize].dealloc(ptr) };
        }
    }

    /// Returns true if the pointer falls within any pool's `SlotBlock`.
    pub(crate) fn contains(&self, ptr: *const u8) -> bool {
        self.pools.iter().any(|pool| pool.contains(ptr))
    }

    /// Allocates a pool slot and copies the string bytes into it.
    /// Falls through to the backing arena for strings > 256 bytes.
    pub(crate) fn alloc_str(&self, s: &str) -> ArenaString<'a> {
        let len = s.len();
        let slot = self.alloc(len as u32);
        let ptr = slot.cast();
        unsafe {
            std::ptr::copy_nonoverlapping(s.as_ptr(), ptr.as_ptr(), len);
            ArenaString::from_raw_parts(ptr, len, self.arena)
        }
    }

    /// Returns the backing arena for non-pool operations.
    #[inline]
    pub(crate) fn arena(&self) -> &'a Arena {
        self.arena
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::helpers::MEBI;

    const fn slot_size_of(class: u32) -> u32 {
        SLOT_SIZES[class as usize]
    }

    fn with_arena(f: impl FnOnce(&Arena)) {
        let arena = Arena::new(4 * MEBI).unwrap();
        f(&arena);
    }

    #[test]
    fn size_class_boundaries() {
        // Zero maps to class 0 (8-byte slot).
        assert_eq!(size_class(0), Some(0));
        assert_eq!(slot_size_of(0), 8);

        // 1 byte maps to class 0.
        assert_eq!(size_class(1), Some(0));

        // 8 bytes maps to class 0.
        assert_eq!(size_class(8), Some(0));

        // 9 bytes maps to class 1 (16-byte slot).
        assert_eq!(size_class(9), Some(1));
        assert_eq!(slot_size_of(1), 16);

        // 128 bytes maps to class 15.
        assert_eq!(size_class(128), Some(15));
        assert_eq!(slot_size_of(15), 128);

        // 129 bytes maps to class 16 (160-byte slot).
        assert_eq!(size_class(129), Some(16));
        assert_eq!(slot_size_of(16), 160);

        // 160 bytes maps to class 16.
        assert_eq!(size_class(160), Some(16));

        // 161 bytes maps to class 17 (192-byte slot).
        assert_eq!(size_class(161), Some(17));
        assert_eq!(slot_size_of(17), 192);

        // 256 bytes maps to class 19.
        assert_eq!(size_class(256), Some(19));
        assert_eq!(slot_size_of(19), 256);

        // 257 bytes is oversized.
        assert_eq!(size_class(257), None);
    }

    #[test]
    fn pool_alloc_dealloc_cycle() {
        with_arena(|arena| {
            let pool = Pool::new(arena, 32, 4);

            // Allocate all 4 slots.
            let a = pool.alloc().unwrap();
            let b = pool.alloc().unwrap();
            let c = pool.alloc().unwrap();
            let d = pool.alloc().unwrap();

            assert_eq!(pool.live_count.get(), 4);
            assert!(pool.alloc().is_none(), "pool should be exhausted");

            // Free slot b (middle of the block).
            unsafe { pool.dealloc(b.cast()) };
            assert_eq!(pool.live_count.get(), 3);

            // Reallocate — should reuse b's slot.
            let e = pool.alloc().unwrap();
            assert_eq!(e.cast::<u8>(), b.cast::<u8>());
            assert_eq!(pool.live_count.get(), 4);

            // Free all.
            unsafe {
                pool.dealloc(a.cast());
                pool.dealloc(c.cast());
                pool.dealloc(d.cast());
                pool.dealloc(e.cast());
            }
            assert_eq!(pool.live_count.get(), 0);
        });
    }

    #[test]
    fn pool_reset() {
        with_arena(|arena| {
            let pool = Pool::new(arena, 16, 8);

            let _ = pool.alloc().unwrap();
            let _ = pool.alloc().unwrap();
            assert_eq!(pool.live_count.get(), 2);

            unsafe { pool.reset() };
            assert_eq!(pool.live_count.get(), 0);
            assert_eq!(pool.free.len(), 0);
            assert_eq!(pool.block.bump.get(), 0);

            // Slots are usable again after reset.
            let _ = pool.alloc().unwrap();
            assert_eq!(pool.live_count.get(), 1);
        });
    }

    #[test]
    fn poolset_alloc_and_contains() {
        with_arena(|arena| {
            let set = PoolSet::new(arena);

            // Small string (5 bytes) → class 0 (8-byte slot).
            let ptr = set.alloc(5);
            assert!(set.contains(ptr.cast().as_ptr()));
            assert_eq!(ptr.len(), 8);

            // Exact boundary (128 bytes) → class 15.
            let ptr128 = set.alloc(128);
            assert!(set.contains(ptr128.cast().as_ptr()));
            assert_eq!(ptr128.len(), 128);

            // Oversized (300 bytes) → arena fallback.
            let big = set.alloc(300);
            assert!(!set.contains(big.cast().as_ptr()));
            assert_eq!(big.len(), 300);

            // Dealloc the small string.
            unsafe { set.dealloc(ptr.cast(), 5) };
            // Dealloc the oversized string is a no-op (doesn't crash).
            unsafe { set.dealloc(big.cast(), 300) };
        });
    }

    #[test]
    fn poolset_recycle_eliminates_growth() {
        with_arena(|arena| {
            let set = PoolSet::new(arena);
            let baseline = arena.offset();

            // Allocate and free 10,000 times. Pool should recycle the same slot.
            for _ in 0..10_000 {
                let ptr = set.alloc(12);
                unsafe { set.dealloc(ptr.cast(), 12) };
            }

            // Arena should not have grown beyond the pool's initial allocation.
            assert_eq!(arena.offset(), baseline, "pool recycling must not grow the arena");
        });
    }

    #[test]
    fn conservation_holds_under_pressure() {
        with_arena(|arena| {
            let pool = Pool::new(arena, 24, 64);

            let mut ptrs = Vec::new();
            // Allocate 32 slots.
            for _ in 0..32 {
                ptrs.push(pool.alloc().unwrap());
            }
            // Free every other slot.
            for i in (0..32).step_by(2) {
                unsafe { pool.dealloc(ptrs[i].cast()) };
            }
            // Reallocate 16 — should reuse freed slots.
            for _ in 0..16 {
                ptrs.push(pool.alloc().unwrap());
            }
            // Conservation is checked inside alloc/dealloc via debug_assert.
            assert_eq!(pool.live_count.get(), 32);
        });
    }
}
