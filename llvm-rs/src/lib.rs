pub extern crate llvm_c_2_sys;

mod constant;
mod ir_builder;
mod ty;
mod value;
mod verifier;

pub use llvm_c_2_sys as c;
pub use {constant::*, ir_builder::*, ty::*, value::*, verifier::*};

use std::ptr::null_mut;

#[inline(always)]
fn str_ptr(str: &str) -> *const i8 {
    str.as_ptr() as *const i8
}

#[macro_export]
macro_rules! impl_from_as_ptr {
    ($T:ty, $ty_ptr:ty) => {
        impl $T {
            /// # Safety
            /// Could be the wrong pointer
            #[inline]
            pub unsafe fn from_ptr(ptr: $ty_ptr) -> Self {
                Self { ptr }
            }

            /// # Safety
            /// You should only need this if you are already in an unsafe context
            #[inline]
            pub unsafe fn as_ptr(&self) -> $ty_ptr {
                self.ptr
            }
        }
    };
}

#[derive(Debug)]
pub struct Context {
    ptr: c::llvm_ContextRef,
}

// @FIXME: SIGSEGV
// impl Drop for Context {
//     fn drop(&mut self) {
//         unsafe { c::llvm_Context_dispose(self.ptr) };
//     }
// }

impl Default for Context {
    fn default() -> Self {
        Self::new()
    }
}

impl_from_as_ptr!(Context, c::llvm_ContextRef);

impl Context {
    pub fn new() -> Self {
        let ptr = unsafe { c::llvm_Context_create() };
        Self { ptr }
    }
}

#[derive(Debug)]
pub struct Module {
    ptr: c::llvm_ModuleRef,
}

// @FIXME: SIGSEGV
// impl Drop for Module {
//     fn drop(&mut self) {
//         unsafe { c::llvm_Module_dispose(self.ptr) };
//     }
// }

impl_from_as_ptr!(Module, c::llvm_ModuleRef);

impl Module {
    pub fn new(name: &str, ctx: Option<&Context>) -> Self {
        let ptr = unsafe {
            c::llvm_Module_create(
                str_ptr(name),
                name.len() as u64,
                ctx.map(|s| s.ptr).unwrap_or(null_mut()),
            )
        };
        Self { ptr }
    }

    pub fn dump(&self) {
        unsafe { c::llvm_Module_dump(self.ptr) };
    }
}
