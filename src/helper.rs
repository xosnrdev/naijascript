use std::fmt;

pub struct LenWriter(pub usize);

impl fmt::Write for LenWriter {
    fn write_str(&mut self, s: &str) -> fmt::Result {
        self.0 += s.len();
        Ok(())
    }
}
