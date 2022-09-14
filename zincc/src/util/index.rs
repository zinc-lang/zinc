//! Utility structures for dealing with storing objects and creating indices to them.

use std::fmt::{self, Debug};
use std::hash::Hash;
use std::marker::PhantomData;
use std::ops::Range;

/// Dry macro for creating simple NewType structs that implement [`Idx`].
///
/// # Examples
///
/// ```
/// # use crate::util::index;
/// // There are convenience overloads for common types
/// index::define_idx! { pub struct MyU32Index: u32 }
/// // or for a NonZeroU32:
/// index::define_idx! { pub struct MyU32Index: u32 != 0 }
///
/// // or if you need finer control over the implementation.
/// // `new` and `get` correspond to the `new` and `index` methods of `idx` directly.
/// # type MyIntegerType = i32;
/// index::define_idx! {
///    pub struct MyIndex: MyIntegerType {
///        new => |x: usize| { x.try_into().unwrap() }
///        get => |s: MyIndex| { s.0.try_into().unwrap() }
///    }
/// }
/// ```
#[macro_export]
macro_rules! define_idx {
    {
        $(#[$outer:meta])*
        $vis:vis struct $IndexTypeName:ident: $T:ty {
            new => $blk_new:expr,
            get => $blk_get:expr,
        }
    } => {
        $(#[$outer])*
        #[derive(Clone, Copy, PartialEq, Eq, Hash)]
        $vis struct $IndexTypeName($T);

        impl $crate::util::index::Idx for $IndexTypeName {
            fn new(idx: usize) -> Self {
                Self($blk_new(idx))
            }

            fn index(self) -> usize {
                $blk_get(self)
            }
        }

        impl std::fmt::Debug for $IndexTypeName {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                write!(
                    f,
                    "{}({})",
                    stringify!($IndexTypeName),
                    $crate::util::index::Idx::index(*self)
                )
            }
        }
    };
    {
        $(#[$outer:meta])*
        $vis:vis struct $IndexTypeName:ident: u32
    } => {
        $(#[$outer])*
        $crate::util::index::define_idx! {
            $vis struct $IndexTypeName: u32 {
                new => |x: usize| { x.try_into().unwrap() },
                get => |s: $IndexTypeName| { s.0 as usize },
            }
        }
    };
    {
        $(#[$outer:meta])*
        $vis:vis struct $IndexTypeName:ident: u32 != 0
    } => {
        $(#[$outer])*
        $crate::util::index::define_idx! {
            $vis struct $IndexTypeName: std::num::NonZeroU32 {
                new => |x: usize| {
                    std::num::NonZeroU32::new(x as u32 + 1).unwrap()
                },
                get => |s: $IndexTypeName| { s.0.get() as usize - 1 },
            }
        }
    };
}
pub use define_idx;

pub fn indices_of_range<I: Idx>(range: &Range<I>) -> impl Iterator<Item = I> {
    (range.start.index()..range.end.index())
        .into_iter()
        .map(|i| I::new(i))
}

pub fn empty_range<I: Idx>() -> Range<I> {
    let zero = I::new(0);
    zero..zero
}

pub trait Idx: 'static + Copy + Eq + Hash {
    /// Instantiate this object given a [`usize`]
    fn new(idx: usize) -> Self;

    /// Get the index of this object as a [`usize`]
    fn index(self) -> usize;
}

#[derive(Clone)]
pub struct IndexVec<I: Idx, T> {
    raw: Vec<T>,
    _m: PhantomData<I>,
}

impl<I: Idx, T: Debug> Debug for IndexVec<I, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_map()
            .entries(self.raw.iter().enumerate().map(|(k, v)| (k, v)))
            .finish()
    }
}

impl<T, I: Idx> Default for IndexVec<I, T> {
    fn default() -> Self {
        Self::new()
    }
}

impl<I: Idx, T> std::ops::Index<I> for IndexVec<I, T> {
    type Output = T;

    #[track_caller]
    fn index(&self, index: I) -> &Self::Output {
        self.get(index).unwrap()
    }
}

impl<I: Idx, T> std::ops::IndexMut<I> for IndexVec<I, T> {
    #[track_caller]
    fn index_mut(&mut self, index: I) -> &mut Self::Output {
        self.get_mut(index).unwrap()
    }
}

impl<I: Idx, T> std::ops::Index<Range<I>> for IndexVec<I, T> {
    type Output = [T];

    #[track_caller]
    fn index(&self, index: Range<I>) -> &Self::Output {
        let start = index.start.index();
        let end = index.end.index();
        &self.raw[start..end]
    }
}

impl<I: Idx, T> std::ops::Deref for IndexVec<I, T> {
    type Target = [T];

    fn deref(&self) -> &Self::Target {
        self.raw()
    }
}

impl<I: Idx, T> IndexVec<I, T> {
    #[inline]
    pub fn new() -> Self {
        Self::from_raw(Vec::new())
    }

    #[inline]
    pub fn from_raw(raw: Vec<T>) -> Self {
        Self {
            raw,
            _m: PhantomData,
        }
    }

    #[inline]
    pub fn raw(&self) -> &Vec<T> {
        &self.raw
    }

    #[inline]
    pub fn push(&mut self, item: T) -> I {
        let idx = I::new(self.raw.len());
        self.raw.push(item);
        idx
    }

    pub fn push_range(&mut self, iter: impl Iterator<Item = T>) -> Range<I> {
        let start = I::new(self.raw.len());
        iter.for_each(|item| self.raw.push(item));
        let end = I::new(self.raw.len());

        start..end
    }

    #[inline]
    pub fn get(&self, index: I) -> Option<&T> {
        self.raw.get(index.index())
    }

    /// # Safety
    /// UB if it is not a valid index.
    #[inline]
    pub unsafe fn get_unchecked(&self, index: I) -> &T {
        self.get(index).unwrap_unchecked()
    }

    #[inline]
    pub fn get_mut(&mut self, index: I) -> Option<&mut T> {
        self.raw.get_mut(index.index())
    }

    pub fn indices(&self) -> impl Iterator<Item = I> + '_ {
        self.raw.iter().enumerate().map(|(idx, _)| I::new(idx))
    }
}

#[derive(Clone)]
pub struct InterningIndexVec<I: Idx, T: Eq> {
    raw: IndexVec<I, T>,
}

impl<I: Idx, T: Eq + Debug> Debug for InterningIndexVec<I, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(&self.raw, f)
    }
}

impl<I: Idx, T: Eq> Default for InterningIndexVec<I, T> {
    fn default() -> Self {
        Self {
            raw: Default::default(),
        }
    }
}

impl<I: Idx, T: Eq> std::ops::Index<I> for InterningIndexVec<I, T> {
    type Output = T;

    #[track_caller]
    fn index(&self, index: I) -> &Self::Output {
        self.get(index).unwrap()
    }
}

impl<I: Idx, T: Eq> InterningIndexVec<I, T> {
    #[inline]
    pub fn new() -> Self {
        Self::default()
    }

    pub fn raw(&self) -> &IndexVec<I, T> {
        &self.raw
    }

    #[inline]
    pub fn from_raw(raw: IndexVec<I, T>) -> Self {
        Self { raw }
    }

    #[inline]
    pub fn is_interned(&self, t: &T) -> bool {
        self.raw.raw.iter().any(|it| it == t)
    }

    #[inline]
    pub fn get_from_value(&self, t: &T) -> Option<I> {
        self.raw
            .raw
            .iter()
            .position(|it| it == t)
            .map(|i| I::new(i))
    }

    /// # Safety
    /// UB if it has not been interned.
    #[inline]
    pub unsafe fn get_from_value_unchecked(&self, t: &T) -> I {
        self.get_from_value(t).unwrap_unchecked()
    }

    #[inline]
    pub fn get_or_intern(&mut self, t: T) -> I {
        self.get_from_value(&t).unwrap_or_else(|| self.intern(t))
    }

    #[inline]
    pub fn intern(&mut self, t: T) -> I {
        // Make sure that is not already in the vec
        debug_assert!(!self.raw.raw.iter().any(|it| it == &t));
        self.raw.push(t)
    }

    #[inline]
    pub fn get(&self, index: I) -> Option<&T> {
        self.raw.get(index)
    }
}

define_idx! { pub struct StringSymbol: u32 != 0 }

/// Small type specialization to allow checking if a string is interned from just a `&str`
/// without having to first turn it into a `String`
pub type StringInterningVec = InterningIndexVec<StringSymbol, String>;

impl StringInterningVec {
    pub fn str_is_interned(&self, str: &str) -> bool {
        self.raw.raw.iter().any(|it| it == str)
    }

    pub fn str_get_from_value(&self, str: &str) -> Option<StringSymbol> {
        self.raw
            .raw
            .iter()
            .position(|it| it == str)
            .map(StringSymbol::new)
    }

    pub fn str_get_or_intern(&mut self, str: &str) -> StringSymbol {
        self.str_get_from_value(str)
            .unwrap_or_else(|| self.intern(str.to_string()))
    }

    /// # Safety
    /// Same as [`InterningVec`]'s `get_from_value_unchecked`
    ///
    /// [`InterningVec`]: ./struct.InterningVec.html
    pub unsafe fn str_get_from_value_unchecked(&self, str: &str) -> StringSymbol {
        self.str_get_from_value(str).unwrap_unchecked()
    }
}

// #[derive(Debug)]
// pub struct IndexSet<I: Idx> {
//     counter: usize,
//     _m: PhantomData<I>,
// }

// impl<I: Idx> Default for IndexSet<I> {
//     fn default() -> Self {
//         Self::new()
//     }
// }

// impl<I: Idx> IndexSet<I> {
//     pub fn new() -> Self {
//         Self {
//             counter: 0,
//             _m: PhantomData,
//         }
//     }

//     pub fn push(&mut self) -> I {
//         self.counter += 1;
//         I::new(self.counter - 1)
//     }
// }
