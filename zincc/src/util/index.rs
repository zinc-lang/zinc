//! Utility structures for dealing with storing objects and creating indices to them.

use std::fmt::{self, Debug};
use std::hash::Hash;
use std::marker::PhantomData;

macro_rules! define_idx {
    ($name:ident, $inner_ty:ty, $blk_new:expr, $blk_get:expr) => {
        #[derive(Clone, Copy, PartialEq, Eq, Hash)]
        pub struct $name($inner_ty);

        impl std::fmt::Debug for $name {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                write!(
                    f,
                    "{}({})",
                    stringify!($name),
                    crate::util::index::Idx::index(*self)
                )
            }
        }

        impl crate::util::index::Idx for $name {
            fn new(idx: usize) -> Self {
                Self($blk_new(idx))
            }

            fn index(self) -> usize {
                $blk_get(self)
            }
        }
    };
}
pub(crate) use define_idx;

// macro_rules! define_usize_idx {
//     ($name:ident) => {
//         $crate::util::index::define_idx!($name, usize, |x| { x }, |s: $name| { s.0 });
//     };
// }
// pub(crate) use define_usize_idx;

// macro_rules! define_non_zero_usize_idx {
//     ($name:ident) => {
//         $crate::util::index::define_idx!(
//             $name,
//             std::num::NonZeroUsize,
//             |x| { std::num::NonZeroUsize::new(x + 1).unwrap() },
//             |s: $name| { s.0.get() - 1 }
//         );
//     };
// }
// pub(crate) use define_non_zero_usize_idx;

macro_rules! define_u32_idx {
    ($name:ident) => {
        $crate::util::index::define_idx!(
            $name,
            u32,
            |x: usize| { x.try_into().unwrap() },
            |s: $name| { s.0 as usize }
        );
    };
}
pub(crate) use define_u32_idx;

macro_rules! define_non_zero_u32_idx {
    ($name:ident) => {
        $crate::util::index::define_idx!(
            $name,
            std::num::NonZeroU32,
            |x: usize| { std::num::NonZeroU32::new(x as u32 + 1).unwrap() },
            |s: $name| { s.0.get() as usize - 1 }
        );
    };
}
pub(crate) use define_non_zero_u32_idx;

pub trait Idx: 'static + Copy + Eq + Hash {
    fn new(idx: usize) -> Self;
    fn index(self) -> usize;
}

#[derive(Clone)]
pub struct IndexVec<T, I: Idx> {
    raw: Vec<T>,
    _m: PhantomData<I>,
}

impl<T: Debug, I: Idx> fmt::Debug for IndexVec<T, I> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_map()
            .entries(self.raw.iter().enumerate().map(|(k, v)| (k, v)))
            .finish()
    }
}

impl<T, I: Idx> Default for IndexVec<T, I> {
    fn default() -> Self {
        Self::new()
    }
}

impl<T, I: Idx> IndexVec<T, I> {
    #[inline]
    pub fn new() -> Self {
        Self::from_raw(vec![])
    }

    #[inline]
    pub fn from_raw(raw: Vec<T>) -> Self {
        Self {
            raw,
            _m: PhantomData,
        }
    }

    pub fn raw(&self) -> &Vec<T> {
        &self.raw
    }

    #[inline]
    pub fn push(&mut self, t: T) -> I {
        let idx = I::new(self.raw.len());
        self.raw.push(t);
        idx
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

    #[inline]
    pub fn indices(&self) -> Vec<I> {
        self.raw
            .iter()
            .enumerate()
            .map(|(idx, _)| I::new(idx))
            .collect()
    }
}

#[derive(Clone)]
#[non_exhaustive]
pub struct InterningIndexVec<T: Eq, I: Idx> {
    raw: IndexVec<T, I>,
}

impl<T: Eq + Debug, I: Idx> fmt::Debug for InterningIndexVec<T, I> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(&self.raw, f)
    }
}

impl<T: Eq, I: Idx> Default for InterningIndexVec<T, I> {
    fn default() -> Self {
        Self {
            raw: Default::default(),
        }
    }
}

impl<T: Eq, I: Idx> InterningIndexVec<T, I> {
    #[inline]
    pub fn new() -> Self {
        Self::default()
    }

    pub fn raw(&self) -> &IndexVec<T, I> {
        &self.raw
    }

    #[inline]
    pub fn from_raw(raw: IndexVec<T, I>) -> Self {
        Self { raw }
    }

    #[inline]
    pub fn is_interned(&mut self, t: &T) -> bool {
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

define_non_zero_u32_idx!(StringSymbol);

/// Small type specialization to allow checking if a string is interned from just a `&str`
/// without having to first turn it into a `String`
pub type StringInterningVec = InterningIndexVec<String, StringSymbol>;

impl StringInterningVec {
    pub fn is_str_interned(&self, str: &str) -> bool {
        self.raw.raw.iter().any(|it| it == str)
    }

    pub fn get_from_str_value(&self, str: &str) -> Option<StringSymbol> {
        self.raw
            .raw
            .iter()
            .position(|it| it == str)
            .map(StringSymbol::new)
    }

    /// # Safety
    /// Same as [`InterningVec`]'s `get_from_value_unchecked`
    ///
    /// [`InterningVec`]: ./struct.InterningVec.html
    pub unsafe fn get_from_str_value_unchecked(&self, str: &str) -> StringSymbol {
        self.get_from_str_value(str).unwrap_unchecked()
    }
}
