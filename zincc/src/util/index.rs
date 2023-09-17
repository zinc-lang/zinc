//! Utility structures for dealing with storing objects and creating indices to them.

use std::fmt::{self, Debug};
use std::hash::Hash;
use std::marker::PhantomData;
use std::ops::Range;

use super::nvec;

// use std::cmp::PartialOrd

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

        #[allow(clippy::redundant_closure_call)]
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

        impl PartialOrd for $IndexTypeName {
            fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
                let this = $crate::util::index::Idx::index(*self);
                let other = $crate::util::index::Idx::index(*other);
                this.partial_cmp(&other)
            }
        }

        impl std::iter::Step for $IndexTypeName {
            fn steps_between(start: &Self, end: &Self) -> Option<usize> {
                let start = $crate::util::index::Idx::index(*start);
                let end = $crate::util::index::Idx::index(*end);
                Some(start - end)
            }

            fn forward_checked(start: Self, count: usize) -> Option<Self> {
                let start = $crate::util::index::Idx::index(start);
                let ret = $crate::util::index::Idx::new(start + count);
                Some(ret)
            }

            fn backward_checked(start: Self, count: usize) -> Option<Self> {
                let start = $crate::util::index::Idx::index(start);
                let ret = $crate::util::index::Idx::new(start - count);
                Some(ret)
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

// pub fn indices_of_range<I: Idx>(range: &Range<I>) -> impl Iterator<Item = I> {
//     (range.start.index()..range.end.index())
//         .into_iter()
//         .map(|i| I::new(i))
// }

// pub fn empty_range<I: Idx>() -> Range<I> {
//     let zero = I::new(0);
//     zero..zero
// }

pub trait Idx: 'static + Copy + Eq + Hash {
    /// Instantiate this object given a [`usize`]
    fn new(idx: usize) -> Self;

    /// Get the index of this object as a [`usize`]
    fn index(self) -> usize;

    fn empty_range() -> Range<Self> {
        let zero = Self::new(0);
        zero..zero
    }
}

pub trait IndexingVec: Default {
    type Internal;
    type Index: Idx;
    type Item;

    fn new() -> Self;

    fn from_raw(raw: Self::Internal) -> Self;
}

#[derive(Clone)]
pub struct IndexVec<I: Idx, T> {
    raw: Vec<T>,
    _m: PhantomData<I>,
}

// impl<I: Idx, T> IndexingVec for IndexVec<I, T> {
//     type Internal = Vec<T>;

//     type Index = I;

//     type Item = T;

//     fn new() -> Self {
//         todo!()
//     }

//     fn from_raw(raw: Self::Internal) -> Self {
//         todo!()
//     }
// }

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
    #[must_use]
    pub fn new() -> Self {
        Self::from_raw(Vec::new())
    }

    #[inline]
    #[must_use]
    pub fn with_capacity(capacity: usize) -> Self {
        Self::from_raw(Vec::with_capacity(capacity))
    }

    #[inline]
    #[must_use]
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
pub struct IndexVec2<I: Idx, T1, T2> {
    raw: nvec::Vec2<T1, T2>,
    _m: PhantomData<I>,
}

impl<I: Idx, T1: Debug, T2: Debug> Debug for IndexVec2<I, T1, T2> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_map()
            .entries(self.raw.iter().enumerate().map(|(k, v)| (k, v)))
            .finish()
    }
}

impl<I: Idx, T1, T2> IndexVec2<I, T1, T2> {
    #[inline]
    #[must_use]
    pub fn new() -> Self {
        Self::from_vec(nvec::Vec2::new())
    }

    #[inline]
    #[must_use]
    pub fn with_capacity(capacity: usize) -> Self {
        Self::from_vec(nvec::Vec2::with_capacity(capacity))
    }

    #[inline]
    #[must_use]
    pub fn from_vec(raw: nvec::Vec2<T1, T2>) -> Self {
        Self {
            raw,
            _m: PhantomData,
        }
    }

    #[inline]
    pub fn vec(&self) -> &nvec::Vec2<T1, T2> {
        &self.raw
    }

    #[inline]
    pub fn raw(&self) -> &nvec::Vec2<T1, T2> {
        &self.raw
    }

    #[inline]
    pub fn raw_mut(&mut self) -> &mut nvec::Vec2<T1, T2> {
        &mut self.raw
    }

    #[inline]
    pub fn push(&mut self, a: T1, b: T2) -> I {
        let idx = I::new(self.raw.len());
        self.raw.push(a, b);
        idx
    }

    pub fn push_range(&mut self, iter: impl Iterator<Item = (T1, T2)>) -> Range<I> {
        let start = I::new(self.raw.len());
        iter.for_each(|(a, b)| self.raw.push(a, b));
        let end = I::new(self.raw.len());

        start..end
    }

    #[inline]
    pub fn get(&self, index: I) -> Option<(&T1, &T2)> {
        self.raw.get(index.index())
    }

    #[inline]
    pub fn get_mut(&mut self, index: I) -> Option<(&mut T1, &mut T2)> {
        self.raw.get_mut(index.index())
    }

    #[inline]
    pub fn get_a(&self, index: I) -> Option<&T1> {
        self.raw.get_a(index.index())
    }

    #[inline]
    pub fn get_a_mut(&mut self, index: I) -> Option<&mut T1> {
        self.raw.get_a_mut(index.index())
    }

    #[inline]
    pub fn get_b(&self, index: I) -> Option<&T2> {
        self.raw.get_b(index.index())
    }

    #[inline]
    pub fn get_b_mut(&mut self, index: I) -> Option<&mut T2> {
        self.raw.get_b_mut(index.index())
    }

    /// # Safety
    /// UB if it is not a valid index.
    #[inline]
    pub unsafe fn get_unchecked(&self, index: I) -> (&T1, &T2) {
        self.get(index).unwrap_unchecked()
    }

    pub fn indices(&self) -> impl Iterator<Item = I> + '_ {
        self.raw.iter().enumerate().map(|(idx, _)| I::new(idx))
    }
}

impl<I: Idx, T1, T2> Default for IndexVec2<I, T1, T2> {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Clone)]
pub struct IndexVec3<I: Idx, T1, T2, T3> {
    raw: nvec::Vec3<T1, T2, T3>,
    _m: PhantomData<I>,
}

impl<I: Idx, T1: Debug, T2: Debug, T3: Debug> Debug for IndexVec3<I, T1, T2, T3> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_map()
            .entries(self.raw.iter().enumerate().map(|(k, v)| (k, v)))
            .finish()
    }
}

impl<I: Idx, T1, T2, T3> IndexVec3<I, T1, T2, T3> {
    #[inline]
    #[must_use]
    pub fn new() -> Self {
        Self::from_raw(nvec::Vec3::new())
    }

    #[inline]
    #[must_use]
    pub fn with_capacity(capacity: usize) -> Self {
        Self::from_raw(nvec::Vec3::with_capacity(capacity))
    }

    #[inline]
    #[must_use]
    pub fn from_raw(raw: nvec::Vec3<T1, T2, T3>) -> Self {
        Self {
            raw,
            _m: PhantomData,
        }
    }

    #[inline]
    pub fn raw(&self) -> &nvec::Vec3<T1, T2, T3> {
        &self.raw
    }

    #[inline]
    pub fn push(&mut self, a: T1, b: T2, c: T3) -> I {
        let idx = I::new(self.raw.len());
        self.raw.push(a, b, c);
        idx
    }

    pub fn push_range(&mut self, iter: impl Iterator<Item = (T1, T2, T3)>) -> Range<I> {
        let start = I::new(self.raw.len());
        iter.for_each(|(a, b, c)| self.raw.push(a, b, c));
        let end = I::new(self.raw.len());

        start..end
    }

    #[inline]
    pub fn get(&self, index: I) -> Option<(&T1, &T2, &T3)> {
        self.raw.get(index.index())
    }

    #[inline]
    pub fn get_mut(&mut self, index: I) -> Option<(&mut T1, &mut T2, &mut T3)> {
        self.raw.get_mut(index.index())
    }

    #[inline]
    pub fn get_a(&self, index: I) -> Option<&T1> {
        self.raw.get_a(index.index())
    }

    #[inline]
    pub fn get_a_mut(&mut self, index: I) -> Option<&mut T1> {
        self.raw.get_a_mut(index.index())
    }

    #[inline]
    pub fn get_b(&self, index: I) -> Option<&T2> {
        self.raw.get_b(index.index())
    }

    #[inline]
    pub fn get_b_mut(&mut self, index: I) -> Option<&mut T2> {
        self.raw.get_b_mut(index.index())
    }

    #[inline]
    pub fn get_c(&self, index: I) -> Option<&T3> {
        self.raw.get_c(index.index())
    }

    #[inline]
    pub fn get_c_mut(&mut self, index: I) -> Option<&mut T3> {
        self.raw.get_c_mut(index.index())
    }

    /// # Safety
    /// UB if it is not a valid index.
    #[inline]
    pub unsafe fn get_unchecked(&self, index: I) -> (&T1, &T2, &T3) {
        self.get(index).unwrap_unchecked()
    }

    pub fn indices(&self) -> impl Iterator<Item = I> + '_ {
        self.raw.iter().enumerate().map(|(idx, _)| I::new(idx))
    }
}

impl<I: Idx, T1, T2, T3> Default for IndexVec3<I, T1, T2, T3> {
    fn default() -> Self {
        Self::new()
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
