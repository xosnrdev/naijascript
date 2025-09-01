use crate::arena::{Arena, ArenaString};

#[inline]
pub fn string_slice<'arena>(
    s: &str,
    start: f64,
    end: Option<f64>,
    arena: &'arena Arena,
) -> ArenaString<'arena> {
    let len = s.chars().count() as isize;
    let mut start = start.floor() as isize;
    let mut end = end.map_or(len, |e| e.floor() as isize);

    if start < 0 {
        start += len;
    }
    if end < 0 {
        end += len;
    }

    (start, end) = (start.clamp(0, len), end.clamp(0, len));
    if start >= end {
        return ArenaString::new_in(arena);
    }

    if start == 0 && end == len {
        return ArenaString::from_str(arena, s);
    }

    let mut buffer = ArenaString::with_capacity_in((end - start) as usize, arena);
    s.chars().skip(start as usize).take((end - start) as usize).for_each(|ch| buffer.push(ch));
    buffer
}

#[inline]
pub fn string_upper<'arena>(s: &str, arena: &'arena Arena) -> ArenaString<'arena> {
    let mut buffer = ArenaString::with_capacity_in(s.len(), arena);
    s.chars().flat_map(char::to_uppercase).for_each(|ch| buffer.push(ch));
    buffer
}

#[inline]
pub fn string_lower<'arena>(s: &str, arena: &'arena Arena) -> ArenaString<'arena> {
    let mut buffer = ArenaString::with_capacity_in(s.len(), arena);
    s.chars().flat_map(char::to_lowercase).for_each(|ch| buffer.push(ch));
    buffer
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::KIBI;
    use crate::arena::Arena;

    #[test]
    fn test_string_slice() {
        let arena = Arena::new(KIBI).unwrap();
        let s = string_slice("Hello, 世界! 🌎", 0.0, Some(5.0), &arena);
        assert_eq!(s, "Hello");
    }

    #[test]
    fn test_string_upper() {
        let arena = Arena::new(KIBI).unwrap();
        let s = string_upper("abcdefghijklmnopqrstuvwxyz", &arena);
        assert_eq!(s, "ABCDEFGHIJKLMNOPQRSTUVWXYZ");
    }

    #[test]
    fn test_string_lower() {
        let arena = Arena::new(KIBI).unwrap();
        let s = string_lower("ABCDEFGHIJKLMNOPQRSTUVWXYZ", &arena);
        assert_eq!(s, "abcdefghijklmnopqrstuvwxyz");
    }
}
