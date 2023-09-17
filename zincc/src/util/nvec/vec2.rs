use super::base::{VecN, VecNIter};
use std::{
    alloc::Layout,
    fmt::Debug,
    marker::PhantomData,
    mem::{self, ManuallyDrop},
};

struct RawVec2<T1, T2> {
    nvec: VecN<2>,
    _m: PhantomData<(T1, T2)>,
}

impl<T1, T2> RawVec2<T1, T2> {
    fn new() -> Self {
        assert!(mem::size_of::<T1>() != 0);
        assert!(mem::size_of::<T2>() != 0);
        Self {
            nvec: VecN::new(),
            _m: PhantomData,
        }
    }

    fn with_capacity(capacity: usize) -> Self {
        Self {
            nvec: VecN::with_capacity(capacity, Self::layouts()),
            _m: PhantomData,
        }
    }

    fn layouts() -> [Layout; 2] {
        [Layout::new::<T1>(), Layout::new::<T2>()]
    }

    fn grow(&mut self) {
        self.nvec.grow(Self::layouts())
    }

    fn shrink_to_fit(&mut self, cap: usize) {
        unsafe { self.nvec.shrink_to_fit(cap, Self::layouts()) }
    }

    unsafe fn shallow_clone(&self) -> Self {
        Self {
            nvec: self.nvec.shallow_clone(),
            _m: self._m,
        }
    }
}

impl<T1, T2> Drop for RawVec2<T1, T2> {
    fn drop(&mut self) {
        if self.nvec.cap != 0 {
            unsafe { self.nvec.dealloc(Self::layouts()) };
        }
    }
}

pub struct Vec2<T1, T2> {
    raw: RawVec2<T1, T2>,
    len: usize,
}

// unsafe impl<T1, T2> Send for Vec2<T1, T2> {}
// unsafe impl<T1, T2> Sync for Vec2<T1, T2> {}

impl<T1, T2> Vec2<T1, T2> {
    pub fn new() -> Self {
        Self {
            raw: RawVec2::new(),
            len: 0,
        }
    }

    pub fn with_capacity(capacity: usize) -> Self {
        Self {
            raw: RawVec2::with_capacity(capacity),
            len: 0,
        }
    }

    #[inline]
    pub fn len(&self) -> usize {
        self.len
    }

    #[inline]
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn push(&mut self, a: T1, b: T2) {
        if self.len == self.raw.nvec.cap {
            self.raw.grow();
        }

        unsafe {
            self.raw.nvec.write_to::<0, T1>(self.len, a);
            self.raw.nvec.write_to::<1, T2>(self.len, b);
        }

        self.len += 1;
    }

    pub fn pop(&mut self) -> Option<(T1, T2)> {
        if self.len == 0 {
            None
        } else {
            self.len -= 1;
            unsafe {
                let a = self.raw.nvec.read_from::<0, T1>(self.len);
                let b = self.raw.nvec.read_from::<1, T2>(self.len);

                Some((a, b))
            }
        }
    }

    pub fn get(&self, index: usize) -> Option<(&T1, &T2)> {
        if index <= self.len {
            unsafe {
                let a = self.raw.nvec.read_ref::<0, T1>(index);
                let b = self.raw.nvec.read_ref::<1, T2>(index);

                Some((a, b))
            }
        } else {
            None
        }
    }

    pub fn get_mut(&mut self, index: usize) -> Option<(&mut T1, &mut T2)> {
        if index <= self.len {
            unsafe {
                let a = self.raw.nvec.read_ref_mut::<0, T1>(index);
                let b = self.raw.nvec.read_ref_mut::<1, T2>(index);

                Some((a, b))
            }
        } else {
            None
        }
    }

    pub fn get_a(&self, index: usize) -> Option<&T1> {
        unsafe { self.raw.nvec.get_n::<0, T1>(index, self.len) }
    }

    pub fn get_a_mut(&mut self, index: usize) -> Option<&mut T1> {
        unsafe { self.raw.nvec.get_n_mut::<0, T1>(index, self.len) }
    }

    pub fn get_b(&self, index: usize) -> Option<&T2> {
        unsafe { self.raw.nvec.get_n::<1, T2>(index, self.len) }
    }

    pub fn get_b_mut(&mut self, index: usize) -> Option<&mut T2> {
        unsafe { self.raw.nvec.get_n_mut::<1, T2>(index, self.len) }
    }

    pub fn insert(&mut self, index: usize, a: T1, b: T2) {
        assert!(index <= self.len, "index out of bounds");

        if self.raw.nvec.cap == self.len {
            self.raw.grow()
        }

        unsafe {
            self.raw.nvec.shift_right_at::<0, T1>(self.len, index);
            self.raw.nvec.shift_right_at::<1, T2>(self.len, index);

            self.raw.nvec.write_to::<0, T1>(index, a);
            self.raw.nvec.write_to::<1, T2>(index, b);

            self.len += 1;
        }
    }

    pub fn remove(&mut self, index: usize) -> (T1, T2) {
        assert!(index < self.len, "index out of bounds");

        unsafe {
            self.len -= 1;

            let a = self.raw.nvec.read_from::<0, T1>(index);
            let b = self.raw.nvec.read_from::<1, T2>(index);

            self.raw.nvec.shift_left_at::<0, T1>(self.len, index);
            self.raw.nvec.shift_left_at::<1, T2>(self.len, index);

            (a, b)
        }
    }

    pub fn shrink_to_fit(&mut self) {
        if self.raw.nvec.cap > self.len {
            self.raw.shrink_to_fit(self.len)
        }
    }

    pub fn into_boxed_slices(mut self) -> (Box<[T1]>, Box<[T2]>) {
        self.shrink_to_fit();

        let this = ManuallyDrop::new(self);

        let ptr_a = this.raw.nvec.ptrs[0] as *mut T1;
        let ptr_b = this.raw.nvec.ptrs[1] as *mut T2;

        let len = this.len;

        unsafe {
            let slice_a = std::slice::from_raw_parts_mut::<T1>(ptr_a, len);
            let slice_b = std::slice::from_raw_parts_mut::<T2>(ptr_b, len);

            (Box::from_raw(slice_a), Box::from_raw(slice_b))
        }
    }

    pub fn iter(&self) -> Vec2Iter<'_, T1, T2> {
        self.into_iter()
    }
}

impl<T1: Debug, T2: Debug> Debug for Vec2<T1, T2> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_list().entries(self.iter()).finish()
    }
}

impl<T1: Clone, T2: Clone> Clone for Vec2<T1, T2> {
    fn clone(&self) -> Self {
        let mut new = Self::with_capacity(self.raw.nvec.cap);
        for (a, b) in self {
            new.push(a.clone(), b.clone())
        }
        new
    }
}

impl<T1, T2> Default for Vec2<T1, T2> {
    fn default() -> Self {
        Self::new()
    }
}

impl<T1, T2> Drop for Vec2<T1, T2> {
    fn drop(&mut self) {
        #[allow(clippy::redundant_pattern_matching)]
        while let Some(_) = self.pop() {}
    }
}

impl<T1, T2> IntoIterator for Vec2<T1, T2> {
    type Item = (T1, T2);
    type IntoIter = Vec2IntoIter<T1, T2>;

    fn into_iter(self) -> Self::IntoIter {
        // Allocated memory later dropped from the shallow cloned RawVec
        let vec = ManuallyDrop::new(self);

        unsafe {
            // Shallow clone okay as self as moved into here
            let raw = vec.raw.shallow_clone();
            let len = vec.len;

            let end_ptr1 = raw.nvec.ptr::<0, T1>().add(len) as *mut u8;
            let end_ptr2 = raw.nvec.ptr::<1, T2>().add(len) as *mut u8;

            Vec2IntoIter {
                iter: VecNIter {
                    start_ptrs: raw.nvec.ptrs,
                    end_ptrs: [end_ptr1, end_ptr2],
                },
                _raw: raw,
            }
        }
    }
}

pub struct Vec2IntoIter<T1, T2> {
    _raw: RawVec2<T1, T2>,
    iter: VecNIter<2>,
}

impl<T1, T2> Drop for Vec2IntoIter<T1, T2> {
    fn drop(&mut self) {
        #[allow(clippy::while_let_on_iterator)]
        while let Some(_) = self.next() {}
    }
}

impl<T1, T2> Iterator for Vec2IntoIter<T1, T2> {
    type Item = (T1, T2);

    fn next(&mut self) -> Option<Self::Item> {
        if self.iter.at_end() {
            None
        } else {
            unsafe {
                let a = self.iter.read_start::<0, T1>();
                let b = self.iter.read_start::<1, T2>();

                self.iter.increment_start::<0, T1>();
                self.iter.increment_start::<1, T2>();

                Some((a, b))
            }
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let len1 = self.iter.length::<0, T1>();
        let len2 = self.iter.length::<1, T2>();
        assert!(len1 == len2);
        (len1, Some(len1))
    }
}

impl<T1, T2> DoubleEndedIterator for Vec2IntoIter<T1, T2> {
    fn next_back(&mut self) -> Option<Self::Item> {
        if self.iter.at_end() {
            None
        } else {
            unsafe {
                self.iter.decrement_end::<0, T1>();
                self.iter.decrement_end::<1, T2>();

                let a = self.iter.read_end::<0, T1>();
                let b = self.iter.read_end::<1, T2>();

                Some((a, b))
            }
        }
    }
}

impl<'a, T1, T2> IntoIterator for &'a Vec2<T1, T2> {
    type Item = (&'a T1, &'a T2);
    type IntoIter = Vec2Iter<'a, T1, T2>;

    fn into_iter(self) -> Self::IntoIter {
        let raw = &self.raw;
        let len = self.len;

        unsafe {
            let end_ptr1 = raw.nvec.ptr::<0, T1>().add(len) as *mut u8;
            let end_ptr2 = raw.nvec.ptr::<1, T2>().add(len) as *mut u8;

            Vec2Iter {
                _raw: raw,
                iter: VecNIter {
                    start_ptrs: raw.nvec.ptrs,
                    end_ptrs: [end_ptr1, end_ptr2],
                },
            }
        }
    }
}

pub struct Vec2Iter<'a, T1: 'a, T2: 'a> {
    _raw: &'a RawVec2<T1, T2>,
    iter: VecNIter<2>,
}

impl<'a, T1, T2> Iterator for Vec2Iter<'a, T1, T2> {
    type Item = (&'a T1, &'a T2);

    fn next(&mut self) -> Option<Self::Item> {
        if self.iter.at_end() {
            None
        } else {
            unsafe {
                let a = self.iter.read_start_ref::<0, T1>();
                let b = self.iter.read_start_ref::<1, T2>();

                self.iter.increment_start::<0, T1>();
                self.iter.increment_start::<1, T2>();

                Some((a, b))
            }
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let len1 = self.iter.length::<0, T1>();
        let len2 = self.iter.length::<1, T2>();
        assert!(len1 == len2);
        (len1, Some(len1))
    }
}

impl<'a, T1, T2> DoubleEndedIterator for Vec2Iter<'a, T1, T2> {
    fn next_back(&mut self) -> Option<Self::Item> {
        if self.iter.at_end() {
            None
        } else {
            unsafe {
                self.iter.decrement_end::<0, T1>();
                self.iter.decrement_end::<1, T2>();

                let a = self.iter.read_end_ref::<0, T1>();
                let b = self.iter.read_end_ref::<1, T2>();

                Some((a, b))
            }
        }
    }
}
