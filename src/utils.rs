use std::{
    cmp::Ordering,
    fmt::{Debug, Display, Formatter, Write},
    ops::Deref,
    ptr::copy_nonoverlapping,
    slice,
    str::from_utf8_unchecked,
};
use crossterm::{QueueableCommand, cursor::MoveToPreviousLine, event::{KeyCode, KeyEvent, KeyModifiers}};

pub type Result<T = (), E = anyhow::Error> = std::result::Result<T, E>;
pub const OK: Result = Ok(());

pub const INVERT: &str = "\x1b[7m";
pub const BOLD: &str = "\x1b[1m";
pub const GREEN: &str = "\x1b[32m";
pub const NOSTYLE: &str = "\x1b[0m";
pub const NL: &str = "\x1b[1E\x1b[0G";
pub const CLEARLINE: &str = "\x1b[2K\r";
pub const NULL_EVENT: KeyEvent = KeyEvent::new(KeyCode::Null, KeyModifiers::NONE);

pub trait BoolExt {
    fn pick<T>(self, on_true: T, on_false: T) -> T;
}

impl BoolExt for bool {
    fn pick<T>(self, on_true: T, on_false: T) -> T {
        if self {on_true} else {on_false}
    }
}

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
    content:       String,
    last_line_len: isize,
    min_shift:     isize,
    max_shift:     isize,
    min_scroll:    isize,
    max_scroll:    isize,
    scroll:        isize,
    shift:         isize,
}

impl Write for StringView {
    fn write_str(&mut self, s: &str) -> std::fmt::Result {
        self.content.push_str(s);
        for line in s.split_inclusive('\n') {
            if line.ends_with('\n') {
                let len = line.len().try_into().unwrap_or(isize::MAX);
                self.max_shift = self.max_shift.max(self.last_line_len + len);
                self.last_line_len = 0;
                self.max_scroll += 1;
            } else {
                self.last_line_len = line.len().try_into().unwrap_or(isize::MAX);
            }
        }
        Ok(())
    }
}

impl StringView {
    pub fn new(content: String, min_shift: isize, min_scroll: isize) -> Self {
        let mut max_scroll = 0;
        let mut last_line_len = 0isize;
        let max_shift = content.lines()
            .inspect(|line| {
                max_scroll += 1;
                last_line_len = line.len().try_into().unwrap_or(isize::MAX);
            })
            .map(str::len)
            .max()
            .map_or(0, |x| x.try_into().unwrap_or(isize::MAX));
        Self {
            content, min_shift, max_shift, min_scroll, max_scroll, last_line_len,
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

#[derive(Clone, Copy)]
pub struct ShortStr<const CAP: usize = 15> {
    // safety invariant: `self.len <= CAP`
    len: u8,
    buf: [u8; CAP],
}

#[macro_export]
macro_rules! stack_format {
    (cap: $cap:literal, $($arg:expr),+ $(,)?) => {{
        let mut res = $crate::utils::ShortStr::<$cap>::default();
        std::fmt::Write::write_fmt(&mut res, format_args!($($arg),+)).map(|_| res)
    }};
}

impl<const CAP: usize> Debug for ShortStr<CAP> {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        Debug::fmt(self.as_str(), f)
    }
}

impl<const CAP: usize> Display for ShortStr<CAP> {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        Display::fmt(self.as_str(), f)
    }
}

impl<const CAP: usize> PartialEq for ShortStr<CAP> {
    fn eq(&self, other: &Self) -> bool {
        self.as_str().eq(other.as_str())
    }
}

impl<const CAP: usize> Eq for ShortStr<CAP> {}

impl<const CAP: usize> PartialOrd for ShortStr<CAP> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl<const CAP: usize> Ord for ShortStr<CAP> {
    fn cmp(&self, other: &Self) -> Ordering {
        self.as_str().cmp(other.as_str())
    }
}

impl<const CAP: usize> AsRef<str> for ShortStr<CAP> {
    fn as_ref(&self) -> &str {
        self.as_str()
    }
}

impl<const CAP: usize> Deref for ShortStr<CAP> {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        self.as_str()
    }
}

impl<const CAP: usize> Default for ShortStr<CAP> {
    fn default() -> Self {
        Self { len: 0, buf: [0; CAP] }
    }
}

impl<const CAP: usize> Write for ShortStr<CAP> {
    fn write_str(&mut self, s: &str) -> std::fmt::Result {
        let s_len: u8 = s.len().try_into().map_err(|_| std::fmt::Error)?;
        if self.len() + s_len as usize > CAP {
            return Err(std::fmt::Error);
        }
        // SAFETY: the condition above asserts that `self` has at least `s_len` free bytes
        unsafe {
            let rest = self.buf.as_mut_ptr().add(self.len());
            copy_nonoverlapping(s.as_ptr(), rest, s_len as usize);
            self.len += s_len;
        }
        Ok(())
    }
}

impl<const CAP: usize> ShortStr<CAP> {
    pub const fn len(&self) -> usize {
        self.len as usize
    }

    pub const fn as_str(&self) -> &str {
        // SAFETY: `new` & `write_str` are the only functions that modify the string, and they only
        // write `&str`s to it.
        unsafe { from_utf8_unchecked(slice::from_raw_parts(self.buf.as_ptr(), self.len as usize)) }
    }
}

pub const fn str_char_count(s: &str) -> usize {
    let bytes = s.as_bytes();
    let len = bytes.len();
    let [mut res, mut i] = [0, 0];
    while i < len {
        res += (bytes[i].leading_ones() != 1) as usize;
        i += 1;
    }
    res
}
