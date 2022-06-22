use super::*;
use crate::impl_from_as_ptr;

#[derive(Debug, Clone, Copy)]
pub struct Value {
    ptr: c::llvm_ValueRef,
}

#[derive(Debug)]
pub struct ValueConversionError;

impl TryInto<Function> for Value {
    type Error = ValueConversionError;

    fn try_into(self) -> Result<Function, Self::Error> {
        if self.get_type().get_type_id() == TypeID::Function {
            Ok(Function {
                ptr: self.ptr as c::llvm_FunctionRef,
            })
        } else {
            Err(ValueConversionError)
        }
    }
}

impl From<Function> for Value {
    fn from(func: Function) -> Self {
        let ptr = unsafe { func.as_ptr() as c::llvm_ValueRef };
        Self { ptr }
    }
}

impl From<Constant> for Value {
    fn from(k: Constant) -> Self {
        let ptr = unsafe { k.as_ptr() as c::llvm_ValueRef };
        Self { ptr }
    }
}

impl_from_as_ptr!(Value, c::llvm_ValueRef);

impl Value {
    pub fn get_type(&self) -> Type {
        unsafe {
            let ptr = c::llvm_Value_getType(self.ptr);
            Type::from_ptr(ptr)
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
#[repr(u32)]
pub enum LinkageType {
    ExternalLinkage = c::llvm_LinkageType_llvm_LinkageType_ExternalLinkage,
    AvailableExternallyLinkage = c::llvm_LinkageType_llvm_LinkageType_AvailableExternallyLinkage,
    LinkOnceAnyLinkage = c::llvm_LinkageType_llvm_LinkageType_LinkOnceAnyLinkage,
    LinkOnceODRLinkage = c::llvm_LinkageType_llvm_LinkageType_LinkOnceODRLinkage,
    WeakAnyLinkage = c::llvm_LinkageType_llvm_LinkageType_WeakAnyLinkage,
    WeakODRLinkage = c::llvm_LinkageType_llvm_LinkageType_WeakODRLinkage,
    AppendingLinkage = c::llvm_LinkageType_llvm_LinkageType_AppendingLinkage,
    InternalLinkage = c::llvm_LinkageType_llvm_LinkageType_InternalLinkage,
    PrivateLinkage = c::llvm_LinkageType_llvm_LinkageType_PrivateLinkage,
    ExternalWeakLinkage = c::llvm_LinkageType_llvm_LinkageType_ExternalWeakLinkage,
    CommonLinkage = c::llvm_LinkageType_llvm_LinkageType_CommonLinkage,
}

#[derive(Debug, Clone)]
pub struct Function {
    ptr: c::llvm_FunctionRef,
}

// impl Drop for Function {
//     fn drop(&mut self) {
//         unsafe { c::llvm_Function_dispose(self.ptr) };
//     }
// }

impl_from_as_ptr!(Function, c::llvm_FunctionRef);

impl Function {
    pub fn new(
        ty: &FunctionType,
        linkage: LinkageType,
        addr_space: u32,
        name: &str,
        module: Option<&Module>,
    ) -> Self {
        let ptr = unsafe {
            c::llvm_Function_create(
                ty.as_ptr(),
                linkage as u32,
                addr_space,
                str_ptr(name),
                name.len() as u64,
                if let Some(m) = module {
                    m.ptr
                } else {
                    null_mut()
                },
            )
        };
        Self { ptr }
    }

    pub fn get_arg(&self, i: u32) -> Value {
        unsafe {
            let ptr = c::llvm_Function_getArg(self.ptr, i);
            Value { ptr }
        }
    }
}
