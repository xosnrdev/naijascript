use crate::arena::{Arena, ArenaString};
use crate::builtins::find;

pub fn replace<'arena>(
    arena: &'arena Arena,
    haystack: &str,
    from: &str,
    to: &str,
) -> ArenaString<'arena> {
    if from.is_empty() {
        let mut buffer =
            ArenaString::with_capacity_in(haystack.len() + (haystack.len() + 1) * to.len(), arena);
        for (i, ch) in haystack.char_indices() {
            buffer.push_str(to);
            buffer.push(ch);
            if i + ch.len_utf8() == haystack.len() {
                buffer.push_str(to);
            }
        }
        if haystack.is_empty() {
            buffer.push_str(to);
        }
        return buffer;
    }

    let mut buffer = ArenaString::with_capacity_in(haystack.len(), arena);
    let mut pos = 0;

    while let Some(index) = find(
        // SAFETY: pos is always <= haystack.len()
        unsafe { haystack.get_unchecked(pos..) },
        from,
    ) {
        let index = pos + index;
        buffer.push_str(
            // SAFETY: pos is always <= index
            unsafe { haystack.get_unchecked(pos..index) },
        );
        buffer.push_str(to);
        pos = index + from.len();
    }

    buffer.push_str(
        // SAFETY: pos is always <= haystack.len()
        unsafe { haystack.get_unchecked(pos..) },
    );
    buffer
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::helpers::KIBI;

    fn arena() -> Arena {
        Arena::new(KIBI).unwrap()
    }

    // T1: No match at all
    #[test]
    fn t1_no_match() {
        let arena = arena();
        let s = "abc";
        assert_eq!(replace(&arena, s, "x", "y"), "abc");
    }

    // T2: Single simple match
    #[test]
    fn t2_single_match() {
        let arena = arena();
        let s = "abc";
        assert_eq!(replace(&arena, s, "b", "x"), "axc");
    }

    // T3: Multiple distinct non-overlapping matches
    #[test]
    fn t3_multiple_matches() {
        let arena = arena();
        let s = "aba";
        assert_eq!(replace(&arena, s, "a", "x"), "xbx");
    }

    // T4: Replacement contains pattern text (no recursion)
    #[test]
    fn t4_replacement_contains_pattern() {
        let arena = arena();
        let s = "aa";
        assert_eq!(replace(&arena, s, "a", "aa"), "aaaa");
    }

    // T5: Overlapping matches scenario
    #[test]
    fn t5_overlapping_matches() {
        let arena = arena();
        let s = "aaaa";
        assert_eq!(replace(&arena, s, "aa", "b"), "bb");
    }

    // T6: Pattern at start and end (boundary matches)
    #[test]
    fn t6_start_and_end_matches() {
        let arena = arena();
        let s = "aba";
        assert_eq!(replace(&arena, s, "a", "x"), "xbx");
    }

    // T7: Empty replacement string (deletion)
    #[test]
    fn t7_empty_replacement() {
        let arena = arena();
        let s = "aba";
        assert_eq!(replace(&arena, s, "a", ""), "b");
    }

    // T8: Replacement shorter & longer than pattern
    #[test]
    fn t8_replacement_length_diff() {
        let arena = arena();
        let s = "foofoo";
        assert_eq!(replace(&arena, s, "foo", "f"), "ff");
        assert_eq!(replace(&arena, s, "foo", "foobar"), "foobarfoobar");
    }

    // T9: Empty pattern (matches every boundary)
    #[test]
    fn t9_empty_pattern() {
        let arena = arena();
        let s = "ab";
        assert_eq!(replace(&arena, s, "", "-"), "-a-b-");
    }

    // T10: Unicode / multibyte characters
    #[test]
    fn t10_unicode_multibyte() {
        let arena = arena();
        let s = "mañana";
        assert_eq!(replace(&arena, s, "ña", "NYA"), "maNYAna");
    }
}
