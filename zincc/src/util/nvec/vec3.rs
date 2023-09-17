use super::base::{VecN, VecNIter};
use std::{
    alloc::Layout,
    fmt::Debug,
    marker::PhantomData,
    mem::{self, ManuallyDrop},
};

struct RawVec3<T1, T2, T3> {
    nvec: VecN<3>,
    _m: PhantomData<(T1, T2, T3)>,
}

impl<T1, T2, T3> RawVec3<T1, T2, T3> {
    fn new() -> Self {
        assert!(mem::size_of::<T1>() != 0);
        assert!(mem::size_of::<T2>() != 0);
        assert!(mem::size_of::<T3>() != 0);
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

    fn layouts() -> [Layout; 3] {
        [
            Layout::new::<T1>(),
            Layout::new::<T2>(),
            Layout::new::<T3>(),
        ]
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

impl<T1, T2, T3> Drop for RawVec3<T1, T2, T3> {
    fn drop(&mut self) {
        if self.nvec.cap != 0 {
            unsafe { self.nvec.dealloc(Self::layouts()) };
        }
    }
}

pub struct Vec3<T1, T2, T3> {
    raw: RawVec3<T1, T2, T3>,
    len: usize,
}

impl<T1, T2, T3> Vec3<T1, T2, T3> {
    pub fn new() -> Self {
        Self {
            raw: RawVec3::new(),
            len: 0,
        }
    }

    pub fn with_capacity(capacity: usize) -> Self {
        Self {
            raw: RawVec3::with_capacity(capacity),
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

    pub fn push(&mut self, a: T1, b: T2, c: T3) {
        if self.len == self.raw.nvec.cap {
            self.raw.grow();
        }

        unsafe {
            self.raw.nvec.write_to::<0, T1>(self.len, a);
            self.raw.nvec.write_to::<1, T2>(self.len, b);
            self.raw.nvec.write_to::<2, T3>(self.len, c);
        }

        self.len += 1;
    }

    pub fn pop(&mut self) -> Option<(T1, T2, T3)> {
        if self.len == 0 {
            None
        } else {
            self.len -= 1;
            unsafe {
                let a = self.raw.nvec.read_from::<0, T1>(self.len);
                let b = self.raw.nvec.read_from::<1, T2>(self.len);
                let c = self.raw.nvec.read_from::<2, T3>(self.len);

                Some((a, b, c))
            }
        }
    }

    pub fn get(&self, index: usize) -> Option<(&T1, &T2, &T3)> {
        if index <= self.len {
            unsafe {
                let a = self.raw.nvec.read_ref::<0, T1>(index);
                let b = self.raw.nvec.read_ref::<1, T2>(index);
                let c = self.raw.nvec.read_ref::<2, T3>(index);

                Some((a, b, c))
            }
        } else {
            None
        }
    }

    pub fn get_mut(&mut self, index: usize) -> Option<(&mut T1, &mut T2, &mut T3)> {
        if index <= self.len {
            unsafe {
                let a = self.raw.nvec.read_ref_mut::<0, T1>(index);
                let b = self.raw.nvec.read_ref_mut::<1, T2>(index);
                let c = self.raw.nvec.read_ref_mut::<2, T3>(index);

                Some((a, b, c))
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

    pub fn get_c(&self, index: usize) -> Option<&T3> {
        unsafe { self.raw.nvec.get_n::<2, T3>(index, self.len) }
    }

    pub fn get_c_mut(&mut self, index: usize) -> Option<&mut T3> {
        unsafe { self.raw.nvec.get_n_mut::<2, T3>(index, self.len) }
    }

    pub fn insert(&mut self, index: usize, a: T1, b: T2, c: T3) {
        assert!(index <= self.len, "index out of bounds");

        if self.raw.nvec.cap == self.len {
            self.raw.grow()
        }

        unsafe {
            self.raw.nvec.shift_right_at::<0, T1>(self.len, index);
            self.raw.nvec.shift_right_at::<1, T2>(self.len, index);
            self.raw.nvec.shift_right_at::<2, T3>(self.len, index);

            self.raw.nvec.write_to::<0, T1>(index, a);
            self.raw.nvec.write_to::<1, T2>(index, b);
            self.raw.nvec.write_to::<2, T3>(index, c);

            self.len += 1;
        }
    }

    pub fn remove(&mut self, index: usize) -> (T1, T2, T3) {
        assert!(index < self.len, "index out of bounds");

        unsafe {
            self.len -= 1;

            let a = self.raw.nvec.read_from::<0, T1>(index);
            let b = self.raw.nvec.read_from::<1, T2>(index);
            let c = self.raw.nvec.read_from::<2, T3>(index);

            self.raw.nvec.shift_left_at::<0, T1>(self.len, index);
            self.raw.nvec.shift_left_at::<1, T2>(self.len, index);
            self.raw.nvec.shift_left_at::<2, T3>(self.len, index);

            (a, b, c)
        }
    }

    pub fn shrink_to_fit(&mut self) {
        if self.raw.nvec.cap > self.len {
            self.raw.shrink_to_fit(self.len)
        }
    }

    #[allow(clippy::type_complexity)]
    pub fn into_boxed_slices(mut self) -> (Box<[T1]>, Box<[T2]>, Box<[T3]>) {
        self.shrink_to_fit();

        let this = ManuallyDrop::new(self);

        let ptr_a = this.raw.nvec.ptrs[0] as *mut T1;
        let ptr_b = this.raw.nvec.ptrs[1] as *mut T2;
        let ptr_c = this.raw.nvec.ptrs[2] as *mut T3;

        let len = this.len;

        unsafe {
            let slice_a = std::slice::from_raw_parts_mut::<T1>(ptr_a, len);
            let slice_b = std::slice::from_raw_parts_mut::<T2>(ptr_b, len);
            let slice_c = std::slice::from_raw_parts_mut::<T3>(ptr_c, len);

            (
                Box::from_raw(slice_a),
                Box::from_raw(slice_b),
                Box::from_raw(slice_c),
            )
        }
    }

    pub fn iter(&self) -> Vec3Iter<'_, T1, T2, T3> {
        self.into_iter()
    }
}

impl<T1: Debug, T2: Debug, T3: Debug> Debug for Vec3<T1, T2, T3> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_list().entries(self.iter()).finish()
    }
}

impl<T1: Clone, T2: Clone, T3: Clone> Clone for Vec3<T1, T2, T3> {
    fn clone(&self) -> Self {
        let mut new = Self::with_capacity(self.raw.nvec.cap);
        for (a, b, c) in self {
            new.push(a.clone(), b.clone(), c.clone())
        }
        new
    }
}

impl<T1, T2, T3> Drop for Vec3<T1, T2, T3> {
    fn drop(&mut self) {
        #[allow(clippy::redundant_pattern_matching)]
        while let Some(_) = self.pop() {}
    }
}

impl<T1, T2, T3> Default for Vec3<T1, T2, T3> {
    fn default() -> Self {
        Self::new()
    }
}

impl<T1, T2, T3> IntoIterator for Vec3<T1, T2, T3> {
    type Item = (T1, T2, T3);
    type IntoIter = Vec3IntoIter<T1, T2, T3>;

    fn into_iter(self) -> Self::IntoIter {
        // Allocated memory later dropped from the shallow cloned RawVec
        let vec = ManuallyDrop::new(self);

        unsafe {
            // Shallow clone okay as self as moved into here
            let raw = vec.raw.shallow_clone();
            let len = vec.len;

            let end_ptr1 = raw.nvec.ptr::<0, T1>().add(len) as *mut u8;
            let end_ptr2 = raw.nvec.ptr::<1, T2>().add(len) as *mut u8;
            let end_ptr3 = raw.nvec.ptr::<2, T3>().add(len) as *mut u8;

            Vec3IntoIter {
                iter: VecNIter {
                    start_ptrs: raw.nvec.ptrs,
                    end_ptrs: [end_ptr1, end_ptr2, end_ptr3],
                },
                _raw: raw,
            }
        }
    }
}

pub struct Vec3IntoIter<T1, T2, T3> {
    _raw: RawVec3<T1, T2, T3>,
    iter: VecNIter<3>,
}

impl<T1, T2, T3> Drop for Vec3IntoIter<T1, T2, T3> {
    fn drop(&mut self) {
        #[allow(clippy::while_let_on_iterator)]
        while let Some(_) = self.next() {}
    }
}

impl<T1, T2, T3> Iterator for Vec3IntoIter<T1, T2, T3> {
    type Item = (T1, T2, T3);

    fn next(&mut self) -> Option<Self::Item> {
        if self.iter.at_end() {
            None
        } else {
            unsafe {
                let a = self.iter.read_start::<0, T1>();
                let b = self.iter.read_start::<1, T2>();
                let c = self.iter.read_start::<2, T3>();

                self.iter.increment_start::<0, T1>();
                self.iter.increment_start::<1, T2>();
                self.iter.increment_start::<2, T3>();

                Some((a, b, c))
            }
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let len1 = self.iter.length::<0, T1>();
        let len2 = self.iter.length::<1, T2>();
        let len3 = self.iter.length::<2, T3>();
        assert!(len1 == len2);
        assert!(len1 == len3);
        (len1, Some(len1))
    }
}

impl<T1, T2, T3> DoubleEndedIterator for Vec3IntoIter<T1, T2, T3> {
    fn next_back(&mut self) -> Option<Self::Item> {
        if self.iter.at_end() {
            None
        } else {
            unsafe {
                self.iter.decrement_end::<0, T1>();
                self.iter.decrement_end::<1, T2>();
                self.iter.decrement_end::<2, T3>();

                let a = self.iter.read_end::<0, T1>();
                let b = self.iter.read_end::<1, T2>();
                let c = self.iter.read_end::<2, T3>();

                Some((a, b, c))
            }
        }
    }
}

impl<'a, T1, T2, T3> IntoIterator for &'a Vec3<T1, T2, T3> {
    type Item = (&'a T1, &'a T2, &'a T3);
    type IntoIter = Vec3Iter<'a, T1, T2, T3>;

    fn into_iter(self) -> Self::IntoIter {
        let raw = &self.raw;
        let len = self.len;

        unsafe {
            let end_ptr1 = raw.nvec.ptr::<0, T1>().add(len) as *mut u8;
            let end_ptr2 = raw.nvec.ptr::<1, T2>().add(len) as *mut u8;
            let end_ptr3 = raw.nvec.ptr::<2, T3>().add(len) as *mut u8;

            Vec3Iter {
                _raw: raw,
                iter: VecNIter {
                    start_ptrs: raw.nvec.ptrs,
                    // end_ptrs: raw.nvec.ptrs.map(|it| it.add(len)),
                    end_ptrs: [end_ptr1, end_ptr2, end_ptr3],
                },
            }
        }
    }
}

pub struct Vec3Iter<'a, T1: 'a, T2: 'a, T3: 'a> {
    _raw: &'a RawVec3<T1, T2, T3>,
    iter: VecNIter<3>,
}

// impl<'a, T1, T2, T3> Vec3Iter<'a, T1, T2, T3> {
//     pub fn new(vec: &'a Vec3<T1, T2, T3>) -> Self {
//         let raw = &vec.raw;
//         let len = vec.len;
//         Self {
//             _raw: raw,
//             iter: VecNIntoIter {
//                 start_ptrs: raw.nvec.ptrs,
//                 end_ptrs: unsafe { raw.nvec.ptrs.map(|it| it.add(len)) },
//             },
//         }
//     }
// }

impl<'a, T1, T2, T3> Iterator for Vec3Iter<'a, T1, T2, T3> {
    type Item = (&'a T1, &'a T2, &'a T3);

    fn next(&mut self) -> Option<Self::Item> {
        if self.iter.at_end() {
            None
        } else {
            unsafe {
                let a = self.iter.read_start_ref::<0, T1>();
                let b = self.iter.read_start_ref::<1, T2>();
                let c = self.iter.read_start_ref::<2, T3>();

                // self.iter.increment_start();
                self.iter.increment_start::<0, T1>();
                self.iter.increment_start::<1, T2>();
                self.iter.increment_start::<2, T3>();

                Some((a, b, c))
            }
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let len1 = self.iter.length::<0, T1>();
        let len2 = self.iter.length::<1, T2>();
        let len3 = self.iter.length::<2, T3>();
        assert!(len1 == len2);
        assert!(len1 == len3);
        (len1, Some(len1))
    }
}

impl<'a, T1, T2, T3> DoubleEndedIterator for Vec3Iter<'a, T1, T2, T3> {
    fn next_back(&mut self) -> Option<Self::Item> {
        if self.iter.at_end() {
            None
        } else {
            unsafe {
                // self.iter.decrement_end();
                self.iter.decrement_end::<0, T1>();
                self.iter.decrement_end::<1, T2>();
                self.iter.decrement_end::<2, T3>();

                let a = self.iter.read_end_ref::<0, T1>();
                let b = self.iter.read_end_ref::<1, T2>();
                let c = self.iter.read_end_ref::<2, T3>();

                Some((a, b, c))
            }
        }
    }
}
