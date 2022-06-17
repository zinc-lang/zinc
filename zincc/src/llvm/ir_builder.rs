use super::*;
use crate::impl_from_as_ptr;

#[derive(Debug)]
pub struct BasicBlock {
    ptr: c::llvm_BasicBlockRef,
}

impl Drop for BasicBlock {
    fn drop(&mut self) {
        unsafe { c::llvm_BasicBlock_dispose(self.ptr) };
    }
}

impl_from_as_ptr!(BasicBlock, c::llvm_BasicBlockRef);

impl BasicBlock {
    pub fn new(
        ctx: &Context,
        name: &str,
        parent: &Function,
        insert_before: Option<&BasicBlock>,
    ) -> Self {
        let ptr = unsafe {
            c::llvm_BasicBlock_create(
                ctx.ptr,
                str_ptr(name),
                name.len() as u64,
                parent.as_ptr(),
                insert_before.map(|bb| bb.ptr).unwrap_or(null_mut()),
            )
        };
        Self { ptr }
    }
}

#[derive(Debug)]
pub struct IRBuilder {
    ptr: c::llvm_IRBuilderDefaultRef,
}

impl Drop for IRBuilder {
    fn drop(&mut self) {
        unsafe { c::llvm_IRBuilderDefault_dispose(self.ptr) };
    }
}

impl_from_as_ptr!(IRBuilder, c::llvm_IRBuilderDefaultRef);

impl IRBuilder {
    pub fn new(ctx: &Context) -> Self {
        let ptr = unsafe { c::llvm_IRBuilderDefault_create(ctx.ptr) };
        Self { ptr }
    }

    pub fn set_insertion_point(&self, bb: &BasicBlock) {
        unsafe { c::llvm_IRBuilderDefault_setInsertionPoint_BasicBlock(self.ptr, bb.ptr) };
    }

    pub fn create_add(&mut self, lhs: &Value, rhs: &Value, name: &str) -> Value {
        unsafe {
            let ptr = c::llvm_IRBuilderDefault_createAdd(
                self.ptr,
                lhs.as_ptr(),
                rhs.as_ptr(),
                str_ptr(name),
                name.len() as u64,
            );
            Value::from_ptr(ptr)
        }
    }

    pub fn create_ret(&mut self, val: Option<&Value>) -> Value {
        unsafe {
            let ptr = c::llvm_IRBuilderDefault_createRet(
                self.ptr,
                val.map(|v| v.as_ptr()).unwrap_or(null_mut()),
            );
            Value::from_ptr(ptr)
        }
    }

    pub fn create_ret_void(&mut self) -> Value {
        unsafe {
            let ptr = c::llvm_IRBuilderDefault_createRetVoid(self.ptr);
            Value::from_ptr(ptr)
        }
    }
}
