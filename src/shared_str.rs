use {
    shrimple_parser::Input,
    std::{
        cmp::Ordering,
        fmt::{Debug, Display, Formatter},
        hash::{Hash, Hasher},
        iter::FusedIterator,
        marker::PhantomData,
        mem::{replace, take},
        ops::{Deref, Not},
        ptr::NonNull,
        sync::Arc,
    }
};

/// Stores (a substring of) either a borrowed string or a string shared via an Arc
pub struct SharedStr<'borrow> {
    ptr: NonNull<str>,
    /// If this is [`usize::MAX`], this is a borrowed string, and no Drop logic is needed.
    /// At the very worst, this means leaking an Arc if it has devoured the entirety of the address
    /// space, which isn't the worst thing to happen in such a peculiar circumstance.
    off: usize,
    _covar: PhantomData<&'borrow str>,
}

unsafe impl Send for SharedStr<'_> {}
unsafe impl Sync for SharedStr<'_> {}

impl<'borrow> From<&'borrow str> for SharedStr<'borrow> {
    fn from(value: &'borrow str) -> Self {
        Self { ptr: value.into(), off: usize::MAX, _covar: PhantomData }
    }
}

impl From<Arc<str>> for SharedStr<'static> {
    fn from(value: Arc<str>) -> Self {
        let ptr = unsafe {
            NonNull::new_unchecked(Arc::into_raw(value).cast_mut())
        };
        Self { ptr, off: 0, _covar: PhantomData }
    }
}

impl From<Box<str>> for SharedStr<'static> {
    fn from(value: Box<str>) -> Self {
        Arc::<str>::from(value).into()
    }
}

impl From<String> for SharedStr<'static> {
    fn from(value: String) -> Self {
        Arc::<str>::from(value).into()
    }
}

impl From<rustdoc_types::Id> for SharedStr<'static> {
    fn from(value: rustdoc_types::Id) -> Self {
        value.0.into()
    }
}

impl Default for SharedStr<'_> {
    fn default() -> Self {
        "".into()
    }
}

impl Drop for SharedStr<'_> {
    fn drop(&mut self) {
        if self.off != usize::MAX {
            unsafe {
                Arc::decrement_strong_count(self.ptr.byte_sub(self.off).as_ptr());
            }
        }
    }
}

impl Deref for SharedStr<'_> {
    type Target = str;
    fn deref(&self) -> &Self::Target {
        unsafe { self.ptr.as_ref() }
    }
}

impl Hash for SharedStr<'_> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.deref().hash(state);
    }
}

impl<Str: Deref<Target = str>> PartialEq<Str> for SharedStr<'_> {
    fn eq(&self, other: &Str) -> bool {
        **self == **other
    }
}

impl Eq for SharedStr<'_> {}

impl<Str: Deref<Target = str>> PartialOrd<Str> for SharedStr<'_> {
    fn partial_cmp(&self, other: &Str) -> Option<Ordering> {
        self.deref().partial_cmp(&**other)
    }
}

impl Ord for SharedStr<'_> {
    fn cmp(&self, other: &Self) -> Ordering {
        self.deref().cmp(&**other)
    }
}

impl Debug for SharedStr<'_> {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        Debug::fmt(&**self, f)
    }
}

impl Display for SharedStr<'_> {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        Display::fmt(&**self, f)
    }
}

impl Clone for SharedStr<'_> {
    fn clone(&self) -> Self {
        if self.off != usize::MAX {
            unsafe {
                Arc::increment_strong_count(self.ptr.byte_sub(self.off).as_ptr());
            }
        }

        Self { ptr: self.ptr, off: self.off, _covar: PhantomData }
    }
}

impl<'borrow> Input for SharedStr<'borrow> {
    fn before(mut self, index: usize) -> Self {
        self.truncate(index);
        self
    }

    fn after(mut self, index: usize) -> Self {
        self.advance(index);
        self
    }

    fn split_at(self, mid: usize) -> (Self, Self) {
        (self.clone().before(mid), self.after(mid))
    }
}

impl<'borrow> SharedStr<'borrow> {
    /// Unlike the `From<&'borrow str>` impl, this functions puts no lifetimes contraints, and
    /// clones the string slice instead.
    pub fn from_str(src: &str) -> Self {
        Arc::<str>::from(src).into()
    }

    pub fn truncate(&mut self, new_len: usize) {
        let ptr = unsafe { self.ptr.as_ref() };
        self.ptr = ptr[..new_len].into();
    }

    #[expect(
        clippy::arithmetic_side_effects,
        reason = "ur gonna run out of memory before getting that to panic"
    )]
    pub fn advance(&mut self, by: usize) {
        let ptr = unsafe { self.ptr.as_ref() };
        self.ptr = ptr[by..].into();
        if self.off != usize::MAX {
            self.off += by;
        }
    }

    pub fn split_off(&mut self, at: usize) -> Self {
        let res = self.clone().after(at);
        self.truncate(at);
        res
    }

    pub fn lines(self) -> impl FusedIterator<Item = Self> {
        struct Iter<'borrow> {
            rest: SharedStr<'borrow>,
        }

        impl<'borrow> Iterator for Iter<'borrow> {
            type Item = SharedStr<'borrow>;

            fn next(&mut self) -> Option<Self::Item> {
                let Some(at) = self.rest.find('\n') else {
                    return self.rest.is_empty().not().then_some(take(&mut self.rest));
                };
                let mut new_rest = self.rest.split_off(at);
                new_rest.advance(1);
                Some(replace(&mut self.rest, new_rest))
            }
        }

        impl FusedIterator for Iter<'_> {}

        Iter { rest: self }
    }
}

#[test]
fn shared_str_split_at() {
    assert_eq!(
        SharedStr::from("abcdef").split_at(3),
        (SharedStr::from("abc"), SharedStr::from("def"))
    );
}
