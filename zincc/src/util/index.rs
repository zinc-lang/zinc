//! Utility structures for dealing with storing objects and creating indices to them.

use std::fmt::{self, Debug};
use std::hash::Hash;
use std::marker::PhantomData;

/// Dry macro for creating simple NewType structs that implement [`Idx`].
///
/// # Examples
///
/// ```
/// # use crate::util::index;
/// // There are convenance overloads for common types
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
    // {
    //     $(#[$outer:meta])*
    //     $vis:vis struct $IndexTypeName:ident: usize
    // } => {
    //     $(#[$outer])*
    //     $crate::util::index::define_idx! {
    //         $vis struct $IndexTypeName: usize {
    //             new => |x: usize| { x },
    //             get => |s: $IndexTypeName| { s },
    //         }
    //     }
    // };
    // {
    //     $(#[$outer:meta])*
    //     $vis:vis struct $IndexTypeName:ident: usize != 0
    // } => {
    //     $(#[$outer])*
    //     $crate::util::index::define_idx! {
    //         $vis struct $IndexTypeName: std::num::NonZeroUsize {
    //             new => |x: std::num::NonZeroUsize| {
    //                 std::num::NonZeroUsize::new(x + 1).unwrap()
    //             },
    //             get => |s: $IndexTypeName| { s.0.get() - 1 },
    //         }
    //     }
    // };
}
pub use define_idx;

pub trait Idx: 'static + Copy + Eq + Hash {
    /// Instantiate this object given a [`usize`]
    fn new(idx: usize) -> Self;

    /// Get the index of this object as a [`usize`]
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
        Self::from_raw(Vec::new())
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

define_idx! { pub struct StringSymbol: u32 != 0 }

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
