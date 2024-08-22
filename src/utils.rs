use {
    crossterm::{
        cursor::{MoveLeft, MoveToNextLine, MoveToPreviousLine},
        event::{KeyCode, KeyEvent, KeyModifiers},
        style::{Attribute, Color, SetForegroundColor}, terminal::Clear,
    },
    paste::paste,
    std::{
        cmp::Ordering,
        error::Error,
        fmt::{Debug, Display, Formatter},
        ops::Deref,
        ptr::copy_nonoverlapping,
        slice,
        str::from_utf8_unchecked,
    },
};

pub type Result<T = (), E = anyhow::Error> = std::result::Result<T, E>;
pub const OK: Result = Ok(());

pub const UNDERLINE: Attribute = Attribute::Underlined;
pub const BACK: MoveLeft = MoveLeft(1);
pub const RESET: Attribute = Attribute::Reset;
pub const REVERSE: Attribute = Attribute::Reverse;
pub const BOLD: Attribute = Attribute::Bold;
pub const GREEN: SetForegroundColor = SetForegroundColor(Color::Green);
pub const YELLOW: SetForegroundColor = SetForegroundColor(Color::Yellow);
pub const PREVLINE: MoveToPreviousLine = MoveToPreviousLine(1);
pub const NL: MoveToNextLine = MoveToNextLine(1);
pub const CLEARLINE: Clear = Clear(crossterm::terminal::ClearType::CurrentLine);
pub const NULL_EVENT: KeyEvent = KeyEvent::new(KeyCode::Null, KeyModifiers::NONE);

pub fn cmp<T: Ord>(a: &T, b: &T) -> Ordering {
    a.cmp(b)
}

pub trait OptionExt {
    fn inspect_none(self, f: impl FnOnce()) -> Self;
}

impl<T> OptionExt for Option<T> {
    fn inspect_none(self, f: impl FnOnce()) -> Self {
        if self.is_none() {
            f();
        }
        self
    }
}

pub trait IteratorExt<T, E>: Sized + Iterator<Item = Result<T, E>> {
    fn try_collect<U: FromIterator<T>>(self) -> Result<U, E> {
        self.collect()
    }
}

impl<T, E, I: Iterator<Item = Result<T, E>>> IteratorExt<T, E> for I {}

pub trait BoolExt {
    fn pick<T>(self, on_true: T, on_false: T) -> T;
}

impl BoolExt for bool {
    fn pick<T>(self, on_true: T, on_false: T) -> T {
        if self {on_true} else {on_false}
    }
}

#[derive(Debug)]
pub struct EmptyError;

impl Display for EmptyError {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(f, "{CLEARLINE}{PREVLINE}")
    }
}

impl Error for EmptyError {}

#[derive(Debug)]
pub struct Exit;

impl Display for Exit {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(f, "{CLEARLINE}{PREVLINE}")
    }
}

impl Error for Exit {}

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

impl<const CAP: usize> std::fmt::Write for ShortStr<CAP> {
    fn write_str(&mut self, s: &str) -> std::fmt::Result {
        #[allow(clippy::cast_possible_truncation)]
        let cap = const { CAP as u8 };

        let s_len: u8 = s.len().try_into().map_err(|_| std::fmt::Error)?;
        let Some(new_len) = self.len.checked_add(s_len).filter(|&x| x <= cap) else {
            return Err(std::fmt::Error);
        };
        // SAFETY: the condition above asserts that `self` has at least `s_len` free bytes
        unsafe {
            let rest = self.buf.as_mut_ptr().add(self.len());
            copy_nonoverlapping(s.as_ptr(), rest, s_len as usize);
            self.len = new_len;
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

macro_rules! impl_int_exts {
    ($trait_name:ident : $($int:ty),+ as $ptrsized_int:ty) => {
        paste! {
            #[allow(dead_code)]
            pub trait $trait_name {
                fn [<into_ $ptrsized_int>](self) -> $ptrsized_int;
                fn r#if(self, condition: bool) -> Self;
            }

            $(
                #[cfg(any(target_pointer_width = "64", target_pointer_width = "32"))]
                impl $trait_name for $int {
                    fn [<into_ $ptrsized_int>](self) -> $ptrsized_int {
                        self as $ptrsized_int
                    }

                    fn r#if(self, condition: bool) -> Self {
                        self.wrapping_mul(Self::from(condition))
                    }
                }
            )+
        }
    };
}

impl_int_exts!(IntExt: i16, i32 as isize);
impl_int_exts!(UIntExt: u16, u32 as usize);

#[macro_export]
macro_rules! errfmt {
    ($fmt:literal, $($arg:expr),+) => {
        || format!(concat!("failed to ", $fmt), $($arg),+)
    };
}
