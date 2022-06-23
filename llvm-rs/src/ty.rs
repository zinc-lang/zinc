use super::*;
use crate::impl_from_as_ptr;

#[derive(Debug, Clone, Copy)]
pub struct Type {
    ptr: c::llvm_TypeRef,
}

#[derive(Debug, PartialEq, Eq)]
#[repr(u32)]
pub enum TypeID {
    // Primitive types
    Half = c::llvm_TypeID_llvm_TypeID_Half,
    BFloat = c::llvm_TypeID_llvm_TypeID_BFloat,
    Float = c::llvm_TypeID_llvm_TypeID_Float,
    Double = c::llvm_TypeID_llvm_TypeID_Double,
    X86FP80 = c::llvm_TypeID_llvm_TypeID_X86_FP80,
    FP128 = c::llvm_TypeID_llvm_TypeID_FP128,
    PPCFP128 = c::llvm_TypeID_llvm_TypeID_PPC_FP128,
    Void = c::llvm_TypeID_llvm_TypeID_Void,
    Label = c::llvm_TypeID_llvm_TypeID_Label,
    Metadata = c::llvm_TypeID_llvm_TypeID_Metadata,
    X86MMX = c::llvm_TypeID_llvm_TypeID_X86_MMX,
    X86AMX = c::llvm_TypeID_llvm_TypeID_X86_AMX,
    Token = c::llvm_TypeID_llvm_TypeID_Token,

    // Derived types
    Integer = c::llvm_TypeID_llvm_TypeID_Integer,
    Function = c::llvm_TypeID_llvm_TypeID_Function,
    Pointer = c::llvm_TypeID_llvm_TypeID_Pointer,
    Struct = c::llvm_TypeID_llvm_TypeID_Struct,
    Array = c::llvm_TypeID_llvm_TypeID_Array,
    FixedVector = c::llvm_TypeID_llvm_TypeID_FixedVector,
    ScalableVector = c::llvm_TypeID_llvm_TypeID_ScalableVector,
}

#[derive(Debug)]
pub struct TypeConversionError;

impl TryInto<IntegerType> for Type {
    type Error = TypeConversionError;

    fn try_into(self) -> Result<IntegerType, Self::Error> {
        if self.get_type_id() == TypeID::Integer {
            Ok(IntegerType {
                ptr: self.ptr as c::llvm_IntegerTypeRef,
            })
        } else {
            Err(TypeConversionError)
        }
    }
}

impl TryInto<FunctionType> for Type {
    type Error = TypeConversionError;

    fn try_into(self) -> Result<FunctionType, Self::Error> {
        if self.get_type_id() == TypeID::Function {
            Ok(FunctionType {
                ptr: self.ptr as c::llvm_FunctionTypeRef,
            })
        } else {
            Err(TypeConversionError)
        }
    }
}

impl From<IntegerType> for Type {
    fn from(ty: IntegerType) -> Self {
        let ptr = unsafe { ty.as_ptr() as c::llvm_TypeRef };
        Self { ptr }
    }
}

impl From<FunctionType> for Type {
    fn from(ty: FunctionType) -> Self {
        let ptr = unsafe { ty.as_ptr() as c::llvm_TypeRef };
        Self { ptr }
    }
}

impl_from_as_ptr!(Type, c::llvm_TypeRef);

impl Type {
    pub fn get_void_ty(ctx: &Context) -> Self {
        let ptr = unsafe { c::llvm_Type_getVoidTy(ctx.as_ptr()) };
        Self { ptr }
    }

    pub fn get_type_id(&self) -> TypeID {
        unsafe { std::mem::transmute(c::llvm_Type_getTypeID(self.ptr)) }
    }

    pub fn dump(&self) {
        unsafe { c::llvm_Type_dump(self.ptr) };
    }
}

#[derive(Debug, Clone, Copy)]
pub struct IntegerType {
    ptr: c::llvm_IntegerTypeRef,
}

impl_from_as_ptr!(IntegerType, c::llvm_IntegerTypeRef);

impl IntegerType {
    pub fn get(ctx: &Context, num_bits: u32) -> Self {
        let ptr = unsafe { c::llvm_IntegerType_get(ctx.ptr, num_bits) };
        Self { ptr }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct FunctionType {
    ptr: c::llvm_FunctionTypeRef,
}

impl_from_as_ptr!(FunctionType, c::llvm_FunctionTypeRef);

impl FunctionType {
    pub fn new(result: &Type, params: &[Type], is_var_arg: bool) -> Self {
        let ptr = unsafe {
            c::llvm_FunctionType_get(
                result.ptr,
                params.as_ptr() as *const c::llvm_TypeRef,
                params.len() as u64,
                is_var_arg,
            )
        };
        Self { ptr }
    }
}
