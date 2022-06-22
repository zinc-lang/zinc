use super::*;

pub struct Constant {
    ptr: c::llvm_ConstantRef,
}

impl_from_as_ptr!(Constant, c::llvm_ConstantRef);

impl Constant {
    pub fn new_integer_value(ty: &Type, val: u64) -> Self {
        unsafe { Self::from_ptr(c::llvm_Constant_getIntegerValue(ty.as_ptr(), val)) }
    }
}
