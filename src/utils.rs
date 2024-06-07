use std::{
    fmt::{Display, Formatter},
    io::{BufRead, BufReader, Lines, Read},
    slice::from_raw_parts,
    str::from_utf8_unchecked,
};

use crossterm::{QueueableCommand, cursor::MoveToPreviousLine, event::{KeyCode, KeyEvent, KeyModifiers}};

pub type Result<T = (), E = anyhow::Error> = std::result::Result<T, E>;
pub const OK: Result = Ok(());

pub const INVERT: &str = "\x1b[7m";
pub const BOLD: &str = "\x1b[1m";
pub const NOSTYLE: &str = "\x1b[0m";
pub const NL: &str = "\x1b[1E\x1b[0G";
pub const CLEARLINE: &str = "\x1b[2K\r";
pub const NULL_EVENT: KeyEvent = KeyEvent::new(KeyCode::Null, KeyModifiers::NONE);

pub trait StrExt {
    /// Extends the start of the string by `n` bytes, lengthening the string by the same `n` bytes.
    /// Safety:
    /// - Memory spanned by the resulting string must belong to the same allocation;
    /// - Bytes in the newly gained memory span must be valid UTF-8;
    /// - Adding `n` to the string's length must not overflow it.
    unsafe fn stretch_left(&self, n: usize) -> &Self;
}

impl StrExt for str {
    unsafe fn stretch_left(&self, n: usize) -> &Self {
        from_utf8_unchecked(from_raw_parts(
            self.as_ptr().sub(n),
            self.len().wrapping_add(n),
        ))
    }
}

pub trait StringExt: Sized {
    /// Truncates the string so that it matches the view of itself returned by `f`.
    /// If the view overflows the string in either direction, the string is left unchanged in that
    /// direction.
    /// Forwards the error returned from `f`.
    fn try_map<E>(self, f: impl FnOnce(&str) -> Result<&str, E>) -> Result<Self, E>;
}

impl StringExt for String {
    fn try_map<E>(mut self, f: impl FnOnce(&str) -> Result<&str, E>) -> Result<Self, E> {
        let res = f(&self)?;
        let start = usize::saturating_sub(res.as_ptr() as _, self.as_ptr() as _);
        let len = res.len();
        self.drain(..start);
        self.truncate(len);
        Ok(self)
    }
}

pub trait BoolExt {
    fn pick<T>(self, on_true: T, on_false: T) -> T;
}

impl BoolExt for bool {
    fn pick<T>(self, on_true: T, on_false: T) -> T {
        if self {on_true} else {on_false}
    }
}

pub trait ReadExt: Read + Sized {
    fn buffered_lines(self) -> Lines<BufReader<Self>> {
        BufReader::new(self).lines()
    }
}

impl<T: Read> ReadExt for T {}

/**
 * `levenshtein-rs` - levenshtein
 *
 * MIT licensed.
 *
 * Copyright (c) 2016 Titus Wormer <tituswormer@gmail.com>
 */
#[must_use]
pub fn levenshtein(a: &str, b: &str) -> usize {
    let mut result = 0;

    /* Shortcut optimizations / degenerate cases. */
    if a == b {
        return result;
    }

    let length_a = a.chars().count();
    let length_b = b.chars().count();

    if length_a == 0 {
        return length_b;
    }

    if length_b == 0 {
        return length_a;
    }

    /* Initialize the vector.
     *
     * This is why it’s fast, normally a matrix is used,
     * here we use a single vector. */
    let mut cache: Vec<usize> = (1..).take(length_a).collect();
    let mut distance_a;
    let mut distance_b;

    /* Loop. */
    for (index_b, code_b) in b.chars().enumerate() {
        result = index_b;
        distance_a = index_b;

        for (index_a, code_a) in a.chars().enumerate() {
            distance_b = if code_a == code_b {
                distance_a
            } else {
                distance_a + 1
            };

            distance_a = cache[index_a];

            result = if distance_a > result {
                if distance_b > result {
                    result + 1
                } else {
                    distance_b
                }
            } else if distance_b > distance_a {
                distance_a + 1
            } else {
                distance_b
            };

            cache[index_a] = result;
        }
    }

    result
}

#[derive(Debug)]
pub struct StringView {
    content: String,
    min_shift:  isize,
    max_shift:  isize,
    min_scroll: isize,
    max_scroll: isize,
    scroll: isize,
    shift: isize,
}

impl StringView {
    pub fn new(content: String, min_shift: isize, min_scroll: isize) -> Self {
        let mut max_scroll = 0;
        let max_shift = content.lines()
            .inspect(|_| max_scroll += 1)
            .map(str::len)
            .max()
            .map_or(0, |x| x.try_into().unwrap_or(isize::MAX));
        Self {
            content, min_shift, max_shift, min_scroll, max_scroll,
            scroll: min_scroll, shift: min_shift,
        }
    }

    pub const fn scroll(&self) -> isize {
        self.scroll
    }

    pub fn set_scroll(&mut self, scroll: isize) {
        self.scroll = scroll.clamp(self.min_scroll, self.max_scroll);
    }

    pub const fn shift(&self) -> isize {
        self.shift
    }

    pub fn set_shift(&mut self, shift: isize) {
        self.shift = shift.clamp(self.min_shift, self.max_shift);
    }

    /// Returns a boolean indicating whether the view needs needs to be re-rendered
    pub fn process_key_event(&mut self, event: &KeyEvent) -> bool {
        match event.code {
            KeyCode::Down => self.scroll = match event.modifiers {
                KeyModifiers::NONE => self.scroll.saturating_add(1).min(self.max_scroll),
                KeyModifiers::SHIFT => self.max_scroll,
                _ => return false,
            },

            KeyCode::Up => self.scroll = match event.modifiers {
                KeyModifiers::NONE => self.scroll.saturating_sub(1).max(self.min_scroll),
                KeyModifiers::SHIFT => self.max_scroll,
                _ => return false,
            },

            KeyCode::Right => self.shift = match event.modifiers {
                KeyModifiers::NONE => self.shift.saturating_add(1).min(self.max_shift),
                KeyModifiers::SHIFT => self.max_shift,
                _ => return false,
            },

            KeyCode::Left => self.shift = match event.modifiers {
                KeyModifiers::NONE => self.shift.saturating_sub(1).max(self.min_shift),
                KeyModifiers::SHIFT => self.min_shift,
                _ => return false,
            },

            _ => return false,
        }
        true
    }

    pub fn print(&self, out: &mut impl std::io::Write) -> Result {
        for _ in self.scroll .. 0 {
            write!(out, "{NL}")?;
        }
        let shift = self.shift.try_into().unwrap_or(0usize);
        for line in self.content.lines().skip(self.scroll.try_into().unwrap_or(0)) {
            for _ in self.shift .. 0 {
                out.write_all(b" ")?;
            }
            write!(out, "{}{NL}", line.get(shift..).unwrap_or(""))?;
        }
        out.queue(MoveToPreviousLine(u16::try_from(self.max_scroll - self.scroll)?))?;
        Ok(())
    }
}

#[derive(Debug)]
pub struct EmptyError;

impl Display for EmptyError {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        f.write_str(CLEARLINE)
    }
}

#[derive(Debug)]
pub struct Exit;

impl Display for Exit {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        f.write_str(CLEARLINE)
    }
}
