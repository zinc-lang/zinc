use std::{
    alloc::{self, Layout},
    mem, ptr,
};

pub struct VecN<const N: usize> {
    pub ptrs: [*mut u8; N],
    pub cap: usize,
}

unsafe impl<const N: usize> Send for VecN<N> {}
unsafe impl<const N: usize> Sync for VecN<N> {}

impl<const N: usize> VecN<N> {
    pub fn new() -> Self {
        Self {
            ptrs: [ptr::null_mut(); N],
            cap: 0,
        }
    }

    pub fn with_capacity(capacity: usize, layouts: [Layout; N]) -> Self {
        let mut ptrs: [*mut u8; N] = unsafe { std::mem::zeroed() };
        for i in 0..N {
            let layout = layouts[i];
            let layout = Layout::from_size_align(layout.size() * capacity, layout.align()).unwrap();

            let new_ptr = unsafe { alloc::alloc(layout) };
            if new_ptr.is_null() {
                alloc::handle_alloc_error(layout);
            }

            ptrs[i] = new_ptr;
        }

        let mut this = Self::new();
        this.ptrs = ptrs;
        this.cap = capacity;
        this
    }

    pub fn grow(&mut self, layouts: [Layout; N]) {
        let (new_cap, new_layouts) = if self.cap == 0 {
            (1, layouts)
        } else {
            let new_cap = 2 * self.cap;

            let mut new_layouts: [Layout; N] = unsafe { std::mem::zeroed() };
            for i in 0..N {
                let layout = layouts[i];
                new_layouts[i] =
                    Layout::from_size_align(layout.size() * new_cap, layout.align()).unwrap();
            }
            (new_cap, new_layouts)
        };

        #[cfg(debug_assertions)]
        for layout in new_layouts {
            assert!(layout.size() <= isize::MAX as usize, "Allocation too large")
        }

        let new_ptrs = if self.cap == 0 {
            let mut new_ptrs: [*mut u8; N] = unsafe { std::mem::zeroed() };
            for i in 0..N {
                let layout = new_layouts[i];
                new_ptrs[i] = unsafe { alloc::alloc(layout) };
            }
            new_ptrs
        } else {
            let mut old_layouts: [Layout; N] = unsafe { std::mem::zeroed() };
            for i in 0..N {
                let layout = layouts[i];
                old_layouts[i] =
                    Layout::from_size_align(layout.size() * self.cap, layout.align()).unwrap();
            }

            let old_ptrs = self.ptrs;
            let mut new_ptrs: [*mut u8; N] = unsafe { std::mem::zeroed() };
            for i in 0..N {
                let old_layout = old_layouts[i];
                let old_ptr = old_ptrs[i];
                let new_layout = new_layouts[i];
                new_ptrs[i] = unsafe { alloc::realloc(old_ptr, old_layout, new_layout.size()) };
            }
            new_ptrs
        };

        for i in 0..N {
            let new_ptr = new_ptrs[i];
            if new_ptr.is_null() {
                let layout = new_layouts[i];
                alloc::handle_alloc_error(layout);
            }
        }

        self.ptrs = new_ptrs;
        self.cap = new_cap;
    }

    pub unsafe fn shrink_to_fit(&mut self, cap: usize, layouts: [Layout; N]) {
        assert!(cap <= self.cap);

        // Couldn't tell you why clippy had a problem with this loop and not one of the others
        #[allow(clippy::needless_range_loop)]
        for i in 0..N {
            let layout = layouts[i];
            let layout = Layout::from_size_align(layout.size() * cap, layout.align()).unwrap();
            let ptr = self.ptrs[i];
            self.ptrs[i] = unsafe { alloc::realloc(ptr, layout, cap) }
        }
        self.cap = cap
    }

    pub unsafe fn ptr<const I: usize, T>(&self) -> *mut T {
        assert!(I < N);
        self.ptrs[I] as *mut T
    }

    pub unsafe fn write_to<const I: usize, T>(&mut self, offset: usize, elem: T) {
        ptr::write(self.ptr::<I, T>().add(offset), elem);
    }

    pub unsafe fn read_from<const I: usize, T>(&self, offset: usize) -> T {
        ptr::read(self.ptr::<I, T>().add(offset))
    }

    pub unsafe fn read_ref<const I: usize, T>(&self, offset: usize) -> &T {
        &*self.ptr::<I, T>().add(offset)
    }

    #[allow(clippy::mut_from_ref)]
    pub unsafe fn read_ref_mut<'b, const I: usize, T>(&mut self, offset: usize) -> &'b mut T {
        &mut *self.ptr::<I, T>().add(offset)
    }

    pub unsafe fn shift_right_at<const I: usize, T>(&mut self, len: usize, offset: usize) {
        ptr::copy(
            self.ptr::<I, T>().add(offset),
            self.ptr::<I, T>().add(offset + 1),
            len - offset,
        );
    }

    pub unsafe fn shift_left_at<const I: usize, T>(&mut self, len: usize, offset: usize) {
        ptr::copy(
            self.ptr::<I, T>().add(offset + 1),
            self.ptr::<I, T>().add(offset),
            len - offset,
        );
    }

    pub unsafe fn shallow_clone(&self) -> Self {
        Self {
            ptrs: self.ptrs,
            cap: self.cap,
        }
    }

    pub unsafe fn dealloc(&mut self, layouts: [Layout; N]) {
        #[allow(clippy::needless_range_loop)]
        for i in 0..N {
            let layout = layouts[i];
            let layout = Layout::from_size_align(layout.size() * self.cap, layout.align()).unwrap();
            let ptr = self.ptrs[i];
            alloc::dealloc(ptr, layout)
        }
        self.cap = 0;
    }

    pub unsafe fn get_n<const I: usize, T>(&self, index: usize, len: usize) -> Option<&T> {
        if index <= len {
            unsafe { Some(self.read_ref::<I, T>(index)) }
        } else {
            None
        }
    }

    pub unsafe fn get_n_mut<const I: usize, T>(
        &mut self,
        index: usize,
        len: usize,
    ) -> Option<&mut T> {
        if index <= len {
            unsafe { Some(self.read_ref_mut::<I, T>(index)) }
        } else {
            None
        }
    }
}

pub struct VecNIter<const N: usize> {
    pub start_ptrs: [*mut u8; N],
    pub end_ptrs: [*mut u8; N],
}

impl<const N: usize> VecNIter<N> {
    unsafe fn read_ptr<const I: usize, T>(ptrs: [*mut u8; N]) -> T {
        assert!(I < N);
        ptr::read(ptrs[I] as *const T)
    }

    unsafe fn read_ptr_ref<'b, const I: usize, T>(ptrs: [*mut u8; N]) -> &'b T {
        assert!(I < N);
        &*(ptrs[I] as *const T)
    }

    pub unsafe fn read_start<const I: usize, T>(&mut self) -> T {
        Self::read_ptr::<I, T>(self.start_ptrs)
    }

    pub unsafe fn read_start_ref<'b, const I: usize, T>(&mut self) -> &'b T {
        Self::read_ptr_ref::<I, T>(self.start_ptrs)
    }

    pub unsafe fn read_end<const I: usize, T>(&mut self) -> T {
        Self::read_ptr::<I, T>(self.start_ptrs)
    }

    pub unsafe fn read_end_ref<'b, const I: usize, T>(&mut self) -> &'b T {
        Self::read_ptr_ref::<I, T>(self.start_ptrs)
    }

    pub fn length<const I: usize, T>(&self) -> usize {
        assert!(I < N);
        (self.end_ptrs[I] as usize - self.start_ptrs[I] as usize) / mem::size_of::<T>()
    }

    pub fn at_end(&self) -> bool {
        self.start_ptrs == self.end_ptrs
    }

    pub unsafe fn increment_start<const I: usize, T>(&mut self) {
        self.start_ptrs[I] = (self.start_ptrs[I] as *const T).offset(1) as *mut u8;
    }

    pub unsafe fn decrement_end<const I: usize, T>(&mut self) {
        self.end_ptrs[I] = (self.end_ptrs[I] as *const T).offset(-1) as *mut u8;
    }
}
