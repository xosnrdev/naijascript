//! `memchr` but with a needle.

use std::ptr;

/// `memchr`, but with a needle.
///
/// Returns the index of the first occurrence of `needle` in the
/// `haystack`. If not found, `haystack.len()` is returned. `offset`
/// specifies the index to start searching from.
pub fn memchr(needle: u8, haystack: &[u8], offset: usize) -> usize {
    unsafe {
        let beg = haystack.as_ptr();
        let end = beg.add(haystack.len());
        let it = beg.add(offset.min(haystack.len()));
        let it = memchr_raw(needle, it, end);
        it.offset_from_unsigned(beg)
    }
}

unsafe fn memchr_raw(needle: u8, beg: *const u8, end: *const u8) -> *const u8 {
    #[cfg(any(target_arch = "x86", target_arch = "x86_64", target_arch = "loongarch64"))]
    return unsafe { MEMCHR_DISPATCH(needle, beg, end) };

    #[cfg(target_arch = "aarch64")]
    return unsafe { memchr_neon(needle, beg, end) };

    #[allow(unreachable_code)]
    return unsafe { memchr_fallback(needle, beg, end) };
}

unsafe fn memchr_fallback(needle: u8, mut beg: *const u8, end: *const u8) -> *const u8 {
    unsafe {
        while !ptr::eq(beg, end) {
            let ch = *beg;
            if ch == needle {
                break;
            }
            beg = beg.add(1);
        }
        beg
    }
}

// In order to make `memchr_raw` slim and fast, we use a function pointer that updates
// itself to the correct implementation on the first call. This reduces binary size.
// It would also reduce branches if we had >2 implementations (a jump still needs to be predicted).
// NOTE that this ONLY works if Control Flow Guard is disabled on Windows.
#[cfg(any(target_arch = "x86", target_arch = "x86_64", target_arch = "loongarch64"))]
static mut MEMCHR_DISPATCH: unsafe fn(needle: u8, beg: *const u8, end: *const u8) -> *const u8 =
    memchr_dispatch;

#[cfg(any(target_arch = "x86", target_arch = "x86_64"))]
unsafe fn memchr_dispatch(needle: u8, beg: *const u8, end: *const u8) -> *const u8 {
    let func = if is_x86_feature_detected!("avx2") { memchr_avx2 } else { memchr_fallback };
    unsafe { MEMCHR_DISPATCH = func };
    unsafe { func(needle, beg, end) }
}

#[cfg(any(target_arch = "x86", target_arch = "x86_64"))]
#[target_feature(enable = "avx2")]
unsafe fn memchr_avx2(needle: u8, mut beg: *const u8, end: *const u8) -> *const u8 {
    unsafe {
        // FWIW, I found that adding support for AVX512 was not useful at the time,
        // as it only marginally improved file load performance by <5%.
        #[cfg(target_arch = "x86")]
        use std::arch::x86::*;
        #[cfg(target_arch = "x86_64")]
        use std::arch::x86_64::*;

        let n = _mm256_set1_epi8(needle as i8);
        let mut remaining = end.offset_from_unsigned(beg);

        while remaining >= 32 {
            let v = _mm256_loadu_si256(beg as *const _);
            let a = _mm256_cmpeq_epi8(v, n);
            let m = _mm256_movemask_epi8(a) as u32;

            if m != 0 {
                return beg.add(m.trailing_zeros() as usize);
            }

            beg = beg.add(32);
            remaining -= 32;
        }

        memchr_fallback(needle, beg, end)
    }
}

#[cfg(target_arch = "loongarch64")]
unsafe fn memchr_dispatch(needle: u8, beg: *const u8, end: *const u8) -> *const u8 {
    use std::arch::is_loongarch_feature_detected;

    let func = if is_loongarch_feature_detected!("lasx") {
        memchr_lasx
    } else if is_loongarch_feature_detected!("lsx") {
        memchr_lsx
    } else {
        memchr_fallback
    };
    unsafe { MEMCHR_DISPATCH = func };
    unsafe { func(needle, beg, end) }
}

#[cfg(target_arch = "loongarch64")]
#[target_feature(enable = "lasx")]
unsafe fn memchr_lasx(needle: u8, mut beg: *const u8, end: *const u8) -> *const u8 {
    unsafe {
        use std::arch::loongarch64::*;

        let n = lasx_xvreplgr2vr_b(needle as i32);

        let off = beg.align_offset(32);
        if off != 0 && off < end.offset_from_unsigned(beg) {
            beg = memchr_lsx(needle, beg, beg.add(off));
        }

        while end.offset_from_unsigned(beg) >= 32 {
            let v = lasx_xvld::<0>(beg as *const _);
            let a = lasx_xvseq_b(v, n);
            let m = lasx_xvmskltz_b(a);
            let l = lasx_xvpickve2gr_wu::<0>(m);
            let h = lasx_xvpickve2gr_wu::<4>(m);
            let m = (h << 16) | l;

            if m != 0 {
                return beg.add(m.trailing_zeros() as usize);
            }

            beg = beg.add(32);
        }

        memchr_fallback(needle, beg, end)
    }
}

#[cfg(target_arch = "loongarch64")]
#[target_feature(enable = "lsx")]
unsafe fn memchr_lsx(needle: u8, mut beg: *const u8, end: *const u8) -> *const u8 {
    unsafe {
        use std::arch::loongarch64::*;

        let n = lsx_vreplgr2vr_b(needle as i32);

        let off = beg.align_offset(16);
        if off != 0 && off < end.offset_from_unsigned(beg) {
            beg = memchr_fallback(needle, beg, beg.add(off));
        }

        while end.offset_from_unsigned(beg) >= 16 {
            let v = lsx_vld::<0>(beg as *const _);
            let a = lsx_vseq_b(v, n);
            let c = lsx_vmskltz_b(a);
            let m = lsx_vpickve2gr_wu::<0>(c);

            if m != 0 {
                return beg.add(m.trailing_zeros() as usize);
            }

            beg = beg.add(16);
        }

        memchr_fallback(needle, beg, end)
    }
}

#[cfg(target_arch = "aarch64")]
unsafe fn memchr_neon(needle: u8, mut beg: *const u8, end: *const u8) -> *const u8 {
    unsafe {
        use std::arch::aarch64::*;

        if end.offset_from_unsigned(beg) >= 16 {
            let n = vdupq_n_u8(needle);

            loop {
                let v = vld1q_u8(beg as *const _);
                let a = vceqq_u8(v, n);

                // https://community.arm.com/arm-community-blogs/b/servers-and-cloud-computing-blog/posts/porting-x86-vector-bitmask-optimizations-to-arm-neon
                let m = vreinterpretq_u16_u8(a);
                let m = vshrn_n_u16(m, 4);
                let m = vreinterpret_u64_u8(m);
                let m = vget_lane_u64(m, 0);

                if m != 0 {
                    return beg.add(m.trailing_zeros() as usize >> 2);
                }

                beg = beg.add(16);
                if end.offset_from_unsigned(beg) < 16 {
                    break;
                }
            }
        }

        memchr_fallback(needle, beg, end)
    }
}

#[cfg(test)]
mod tests {
    use std::slice;

    use super::*;
    use crate::KIBI;
    use crate::sys::{self, VirtualMemory};

    #[test]
    fn test_empty() {
        assert_eq!(memchr(b'a', b"", 0), 0);
    }

    #[test]
    fn test_basic() {
        let haystack = b"abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ";
        let haystack = &haystack[..43];

        assert_eq!(memchr(b'a', haystack, 0), 0);
        assert_eq!(memchr(b'p', haystack, 0), 15);
        assert_eq!(memchr(b'Q', haystack, 0), 42);
        assert_eq!(memchr(b'0', haystack, 0), haystack.len());
    }

    // Test that it doesn't match before/after the start offset respectively.
    #[test]
    fn test_with_offset() {
        let haystack = b"abcdefghabcdefghabcdefghabcdefghabcdefgh";

        assert_eq!(memchr(b'a', haystack, 0), 0);
        assert_eq!(memchr(b'a', haystack, 1), 8);
        assert_eq!(memchr(b'a', haystack, 2), 8);
        assert_eq!(memchr(b'a', haystack, 9), 16);
        assert_eq!(memchr(b'a', haystack, 16), 16);
        assert_eq!(memchr(b'a', haystack, 41), 40);
    }

    // Test memory access safety at page boundaries.
    // The test is a success if it doesn't segfault.
    #[test]
    fn test_page_boundary() {
        let page = unsafe {
            const PAGE_SIZE: usize = 64 * KIBI; // 64 KiB to cover many architectures.

            // 3 pages: uncommitted, committed, uncommitted
            let ptr = sys::virtual_memory::reserve(PAGE_SIZE * 3).unwrap();
            sys::virtual_memory::commit(ptr.add(PAGE_SIZE), PAGE_SIZE).unwrap();
            slice::from_raw_parts_mut(ptr.add(PAGE_SIZE).as_ptr(), PAGE_SIZE)
        };

        page.fill(b'a');

        // Test if it seeks beyond the page boundary.
        assert_eq!(memchr(b'\0', &page[page.len() - 40..], 0), 40);
        // Test if it seeks before the page boundary for the masked/partial load.
        assert_eq!(memchr(b'\0', &page[..10], 0), 10);
    }
}
