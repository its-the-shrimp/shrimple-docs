use {
    crate::utils::NL,
    crossterm::QueueableCommand,
    std::io::{self, Write},
};

/// - Converts newlines to [`crossterm::cursor::MoveToNextLine`];
/// - Skips first N lines;
/// - Passes the next M lines through to `Writer`.
///
/// DO NOT write newlines into this, they won't be detected
pub struct LineView<Writer> {
    pub inner: Writer,
    /// Number of lines left to skip.
    skip_rem: usize,
    /// Number of lines left to pass through.
    pass_rem: usize,
}

impl<Writer: Write> Write for LineView<Writer> {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        if self.skip_rem == 0 && self.pass_rem != 0 {
            self.inner.write(buf)
        } else {
            Ok(0)
        }
    }

    fn write_all(&mut self, buf: &[u8]) -> io::Result<()> {
        if self.skip_rem == 0 && self.pass_rem != 0 {
            self.inner.write_all(buf)?;
        }
        Ok(())
    }

    fn flush(&mut self) -> io::Result<()> {
        self.inner.flush()
    }
}

impl<Writer: Write> LineView<Writer> {
    pub const fn new(inner: Writer, to_skip: usize, height: usize) -> Self {
        Self { inner, skip_rem: to_skip, pass_rem: height }
    }

    pub fn new_line(&mut self) -> io::Result<()> {
        if let Some(new_skip_rem) = self.skip_rem.checked_sub(1) {
            self.skip_rem = new_skip_rem;
        } else if let Some(new_pass_rem) = self.pass_rem.checked_sub(1) {
            self.inner.queue(NL)?;
            self.pass_rem = new_pass_rem;
        }
        Ok(())
    }

    pub fn indent(&mut self, n: usize) -> io::Result<()> {
        if self.skip_rem == 0 && self.pass_rem != 0 {
            for _ in 0 .. n {
                self.inner.write_all(b"  ")?;
            }
        }
        Ok(())
    }
}
