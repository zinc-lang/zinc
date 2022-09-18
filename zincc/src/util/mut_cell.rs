use std::{cell::RefCell, ops::Deref};

/// An escape hatch for when you do not care about mutating the held item when immutable
/// references are held.
#[derive(Debug)]
pub struct MutCell<T> {
    value: RefCell<T>,
}

impl<T> MutCell<T> {
    pub fn new(value: T) -> Self {
        let value = RefCell::new(value);
        Self { value }
    }

    #[track_caller]
    pub fn mutate<R>(&self, func: impl FnOnce(&mut T) -> R) -> R {
        let value = &mut *self.value.borrow_mut();
        func(value)
    }

    pub fn into_inner(self) -> T {
        self.value.into_inner()
    }
}

impl<T> Deref for MutCell<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        unsafe { &*self.value.as_ptr() as &Self::Target }
    }
}

impl<T: std::fmt::Display> std::fmt::Display for MutCell<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.deref().fmt(f)
    }
}

impl<T: Default> Default for MutCell<T> {
    fn default() -> Self {
        Self {
            value: Default::default(),
        }
    }
}
