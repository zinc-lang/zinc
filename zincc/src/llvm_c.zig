const std = @import("std");

pub const c = @cImport({
    @cInclude("llvm_c_2/IR/Context.h");
    @cInclude("llvm_c_2/IR/Module.h");
    @cInclude("llvm_c_2/IR/IRBuilder.h");
    @cInclude("llvm_c_2/IR/Type.h");
    @cInclude("llvm_c_2/IR/Function.h");
    @cInclude("llvm_c_2/IR/BasicBlock.h");
    @cInclude("llvm_c_2/IR/Constant.h");
    @cInclude("llvm_c_2/IR/InlineAsm.h");

    @cInclude("llvm_c_2/Support/TargetSelect.h");
    @cInclude("llvm_c_2/ExecutionEngine/ExecutionEngine.h");
});

pub fn InitializeNativeTarget() bool {
    return c.llvm_InitializeNativeTarget();
}

pub fn InitializeNativeTargetAsmPrinter() bool {
    return c.llvm_InitializeNativeTargetAsmPrinter();
}

pub fn InitializeNativeTargetAsmParser() bool {
    return c.llvm_InitializeNativeTargetAsmParser();
}

pub const EngineBuilder = struct {
    ref: c.llvm_EngineBuilderRef,

    pub fn init() EngineBuilder {
        return .{ .ref = c.llvm_EngineBuilder_construct() };
    }

    pub fn initWithModule(module: Module) EngineBuilder {
        return .{ .ref = c.llvm_EngineBuilder_construct_Module(module.ref) };
    }

    pub fn create(self: *EngineBuilder) ExecutionEngine {
        const ee = .{ .ref = c.llvm_EngineBuilder_create(self.ref) };
        c.llvm_EngineBuilder_deconstruct(self.ref);
        self.* = undefined;
        return ee;
    }
};

pub const ExecutionEngine = struct {
    ref: c.llvm_ExecutionEngineRef,

    pub fn findFunctionNamed(self: *ExecutionEngine, name: []const u8) Function {
        return .{ .ref = c.llvm_ExecutionEngine_findFunctionNamed(self.ref, name.ptr, name.len) };
    }

    pub fn runFunction(
        self: *ExecutionEngine,
        func: Function,
        args: []const GenericValue,
    ) GenericValue {
        return .{ .ref = c.llvm_ExecutionEngine_runFunction(
            self.ref,
            func.ref,
            @ptrCast([*c]const c.llvm_GenericValueRef, args.ptr),
            args.len,
        ) };
    }
};

/// This an ExecutionEngine type
pub const GenericValue = struct {
    ref: c.llvm_GenericValueRef,

    pub fn deinit(self: *GenericValue) void {
        c.llvm_GenericValue_free(self.ref);
        self.* = undefined;
    }
};

pub const Context = struct {
    ref: c.llvm_ContextRef,

    pub fn init() Context {
        return .{ .ref = c.llvm_Context_create() };
    }

    pub fn deinit(self: Context) void {
        c.llvm_Context_dispose(self.ref);
    }
};

pub const Module = struct {
    ref: c.llvm_ModuleRef,

    pub fn init(name: []const u8, ctx: Context) Module {
        return .{ .ref = c.llvm_Module_create(name.ptr, name.len, ctx.ref) };
    }

    pub fn deinit(self: Module) void {
        c.llvm_Module_dispose(self.ref);
    }

    pub fn dump(self: *const Module) void {
        c.llvm_Module_dump(self.ref);
    }
};

pub const IRBuilderDefault = struct {
    ref: c.llvm_IRBuilderDefaultRef,

    pub fn init(ctx: Context) IRBuilderDefault {
        return .{ .ref = c.llvm_IRBuilderDefault_create(ctx.ref) };
    }

    pub fn deinit(self: IRBuilderDefault) void {
        c.llvm_IRBuilderDefault_dispose(self.ref);
    }

    pub fn setInsertionPoint(self: IRBuilderDefault, bb: BasicBlock) void {
        c.llvm_IRBuilderDefault_setInsertionPoint_BasicBlock(self.ref, bb.ref);
    }

    pub fn createGlobalString(
        self: IRBuilderDefault,
        str: []const u8,
        name: []const u8,
        addr_space: u32,
        module: ?Module,
    ) Constant {
        return .{ .ref = c.llvm_IRBuilderDefault_createGlobalString(
            self.ref,
            str.ptr,
            str.len,
            name.ptr,
            name.len,
            addr_space,
            if (module) |m| m.ref else null,
        ) };
    }

    pub fn createGlobalStringPtr(
        self: IRBuilderDefault,
        str: []const u8,
        name: []const u8,
        addr_space: u32,
        module: ?Module,
    ) Constant {
        return .{ .ref = c.llvm_IRBuilderDefault_createGlobalStringPtr(
            self.ref,
            str.ptr,
            str.len,
            name.ptr,
            name.len,
            addr_space,
            if (module) |m| m.ref else null,
        ) };
    }

    pub fn createAdd(
        self: IRBuilderDefault,
        lhs: Value,
        rhs: Value,
        name: []const u8,
    ) Value {
        return .{ .ref = c.llvm_IRBuilderDefault_createAdd(
            self.ref,
            lhs.ref,
            rhs.ref,
            name.ptr,
            name.len,
        ) };
    }

    pub fn createRet(self: IRBuilderDefault, value: ?Value) Value {
        return .{ .ref = c.llvm_IRBuilderDefault_createRet(self.ref, if (value) |v| v.ref else null) };
    }

    pub fn createRetVoid(self: IRBuilderDefault) Value {
        return .{ .ref = c.llvm_IRBuilderDefault_createRetVoid(self.ref) };
    }

    pub fn createCall(
        self: IRBuilderDefault,
        ty: FunctionType,
        callee: Value,
        args: []const Value,
        name: []const u8,
    ) Value {
        return .{ .ref = c.llvm_IRBuilderDefault_createCall(
            self.ref,
            ty.ref,
            callee.ref,
            @ptrCast([*c]const c.llvm_ValueRef, args.ptr),
            args.len,
            name.ptr,
            name.len,
        ) };
    }

    pub fn createAlloca(
        self: IRBuilderDefault,
        ty: Type,
        addr_space: u32,
        array_size: ?Value,
        name: []const u8,
    ) Value {
        return .{ .ref = c.llvm_IRBuilderDefault_createAlloca(
            self.ref,
            ty.ref,
            addr_space,
            if (array_size) |a| a.ref else null,
            name.ptr,
            name.len,
        ) };
    }

    pub fn createStore(
        self: IRBuilderDefault,
        val: Value,
        ptr: Value,
        is_volatile: bool,
    ) Value {
        return .{ .ref = c.llvm_IRBuilderDefault_createStore(
            self.ref,
            val.ref,
            ptr.ref,
            is_volatile,
        ) };
    }

    pub fn createGEP(
        self: IRBuilderDefault,
        ty: Type,
        ptr: Value,
        idx: Value,
        name: []const u8,
    ) Value {
        return .{ .ref = c.llvm_IRBuilderDefault_createGEP(
            self.ref,
            ty.ref,
            ptr.ref,
            idx.ref,
            name.ptr,
            name.len,
        ) };
    }

    pub fn createGEPArr(
        self: IRBuilderDefault,
        ty: Type,
        ptr: Value,
        idx_list: []const Value,
        name: []const u8,
    ) Value {
        return .{ .ref = c.llvm_IRBuilderDefault_createGEP_arr(
            self.ref,
            ty.ref,
            ptr.ref,
            @ptrCast([*c]const c.llvm_ValueRef, idx_list.ptr),
            idx_list.len,
            name.ptr,
            name.len,
        ) };
    }

    pub fn createLoad(
        self: IRBuilderDefault,
        ty: Type,
        ptr: Value,
        is_volatile: bool,
        name: []const u8,
    ) Value {
        return .{ .ref = c.llvm_IRBuilderDefault_createLoad(
            self.ref,
            ty.ref,
            ptr.ref,
            is_volatile,
            name.ptr,
            name.len,
        ) };
    }

    pub fn createIntToPtr(
        self: IRBuilderDefault,
        value: Value,
        dest_ty: Type,
        name: []const u8,
    ) Value {
        return .{ .ref = c.llvm_IRBuilderDefault_createIntToPtr(
            self.ref,
            value.ref,
            dest_ty.ref,
            name.ptr,
            name.len,
        ) };
    }

    pub fn createPtrToInt(
        self: IRBuilderDefault,
        value: Value,
        dest_ty: Type,
        name: []const u8,
    ) Value {
        return .{ .ref = c.llvm_IRBuilderDefault_createPtrToInt(
            self.ref,
            value.ref,
            dest_ty.ref,
            name.ptr,
            name.len,
        ) };
    }
};

pub const Type = struct {
    ref: c.llvm_TypeRef,

    pub const TypeID = enum(u32) {
        Half = c.llvm_TypeID_Half,
        BFloat = c.llvm_TypeID_BFloat,
        Float = c.llvm_TypeID_Float,
        Double = c.llvm_TypeID_Double,
        X86_FP80 = c.llvm_TypeID_X86_FP80,
        FP128 = c.llvm_TypeID_FP128,
        PPC_FP128 = c.llvm_TypeID_PPC_FP128,
        Void = c.llvm_TypeID_Void,
        Label = c.llvm_TypeID_Label,
        Metadata = c.llvm_TypeID_Metadata,
        X86_MMX = c.llvm_TypeID_X86_MMX,
        X86_AMX = c.llvm_TypeID_X86_AMX,
        Token = c.llvm_TypeID_Token,
        Integer = c.llvm_TypeID_Integer,
        Function = c.llvm_TypeID_Function,
        Pointer = c.llvm_TypeID_Pointer,
        Struct = c.llvm_TypeID_Struct,
        Array = c.llvm_TypeID_Array,
        FixedVector = c.llvm_TypeID_FixedVector,
        ScalableVector = c.llvm_TypeID_ScalableVector,
    };

    pub fn getVoidTy(ctx: Context) Type {
        return .{ .ref = c.llvm_Type_getVoidTy(ctx.ref) };
    }

    pub fn getIntNTy(ctx: Context, n: u32) Type {
        return .{ .ref = c.llvm_Type_getIntNTy(ctx.ref, n) };
    }

    pub fn getArrayTy(ty: Type, count: u64) Type {
        return .{ .ref = c.llvm_ArrayType_get(ty.ref, count) };
    }

    pub fn getPointerTo(self: Type, addr_space: u32) PointerType {
        return .{ .ref = c.llvm_Type_getPointerTo(self.ref, addr_space) };
    }

    pub fn getTypeID(self: Type) TypeID {
        return @intToEnum(TypeID, c.llvm_Type_getTypeID(self.ref));
    }

    pub fn cast(self: Type, comptime T: type) T {
        std.debug.assert(self.getTypeID() == T.TyID);
        return @bitCast(T, self);
    }

    pub fn intoFunctionType(self: Type) FunctionType {
        return self.cast(FunctionType);
    }

    pub fn intoStructType(self: Type) FunctionType {
        return self.cast(StructType);
    }

    pub fn intoPointerType(self: Type) FunctionType {
        return self.cast(PointerType);
    }
};

pub const FunctionType = struct {
    ref: c.llvm_FunctionTypeRef,

    pub const TyID: Type.TypeID = .Function;

    pub fn get(result: Type, params: []Type, is_var_arg: bool) FunctionType {
        return .{ .ref = c.llvm_FunctionType_get(
            result.ref,
            @ptrCast([*c]c.llvm_TypeRef, params.ptr),
            params.len,
            is_var_arg,
        ) };
    }

    pub fn intoType(self: FunctionType) Type {
        return .{ .ref = @ptrCast(c.llvm_TypeRef, self.ref) };
    }
};

pub const StructType = struct {
    ref: c.llvm_StructTypeRef,

    pub const TyID: Type.TypeID = .Struct;

    pub fn get(ctx: Context, elements: []const Type, is_packed: bool) StructType {
        return .{ .ref = c.llvm_StructType_get(ctx.ref, @ptrCast([*c]const c.llvm_TypeRef, elements.ptr), elements.len, is_packed) };
    }

    pub fn setName(self: StructType, name: []const u8) void {
        c.llvm_StructType_setName(self.ref, name.ptr, name.len);
    }

    pub fn intoType(self: StructType) Type {
        return .{ .ref = @ptrCast(c.llvm_TypeRef, self.ref) };
    }
};

pub const PointerType = struct {
    ref: c.llvm_PointerTypeRef,

    pub const TyID: Type.TypeID = .Pointer;

    pub fn get(ty: Type, addr_space: u32) PointerType {
        return .{ .ref = c.llvm_PointerType_get(ty.ref, addr_space) };
    }

    pub fn getOpaque(ctx: Context, addr_space: u32) PointerType {
        return .{ .ref = c.llvm_PointerType_getOpaque(ctx.ref, addr_space) };
    }

    pub fn isOpaque(self: PointerType) bool {
        return c.llvm_PointerType_isOpaque(self.ref);
    }

    pub fn intoType(self: PointerType) Type {
        return .{ .ref = @ptrCast(c.llvm_TypeRef, self.ref) };
    }
};

const LinkageType = enum(c_uint) {
    External = c.llvm_LinkageType_ExternalLinkage,
    AvailableExternally = c.llvm_LinkageType_AvailableExternallyLinkage,
    LinkOnceAny = c.llvm_LinkageType_LinkOnceAnyLinkage,
    LinkOnceODR = c.llvm_LinkageType_LinkOnceODRLinkage,
    WeakAny = c.llvm_LinkageType_WeakAnyLinkage,
    WeakODR = c.llvm_LinkageType_WeakODRLinkage,
    Appending = c.llvm_LinkageType_AppendingLinkage,
    Internal = c.llvm_LinkageType_InternalLinkage,
    Private = c.llvm_LinkageType_PrivateLinkage,
    ExternalWeak = c.llvm_LinkageType_ExternalWeakLinkage,
    Common = c.llvm_LinkageType_CommonLinkage,
};

pub const Function = struct {
    ref: c.llvm_FunctionRef,

    pub fn init(
        ty: FunctionType,
        linkage: LinkageType,
        addr_space: u32,
        name: []const u8,
        module: Module,
    ) Function {
        return .{ .ref = c.llvm_Function_create(
            ty.ref,
            @enumToInt(linkage),
            addr_space,
            name.ptr,
            name.len,
            module.ref,
        ) };
    }

    pub fn getArg(self: *const Function, index: u32) Value {
        return .{ .ref = c.llvm_Function_getArg(self.ref, index) };
    }

    pub fn intoValue(self: Function) Value {
        return .{ .ref = @ptrCast(c.llvm_ValueRef, self.ref) };
    }
};

pub const InlineAsm = struct {
    ref: c.llvm_InlineAsmRef,

    pub const AsmDialect = enum(u32) {
        ATT = c.llvm_InlineAsm_AsmDialect_ATT,
        Intel = c.llvm_InlineAsm_AsmDialect_Intel,
    };

    pub fn get(
        ty: FunctionType,
        asm_string: []const u8,
        constrains: []const u8,
        has_side_effects: bool,
        is_align_stack: bool, // = false
        dialect: AsmDialect, // = .ATT
        can_throw: bool, // = false
    ) InlineAsm {
        return .{ .ref = c.llvm_InlineAsm_get(
            ty.ref,
            asm_string.ptr,
            asm_string.len,
            constrains.ptr,
            constrains.len,
            has_side_effects,
            is_align_stack,
            @enumToInt(dialect),
            can_throw,
        ) };
    }

    pub fn verify(ty: FunctionType, constraints: []const u8) bool {
        return c.llvm_InlineAsm_verify(ty.ref, constraints.ptr, constraints.len);
    }

    pub fn intoFunction(self: InlineAsm) Function {
        return .{ .ref = @ptrCast(c.llvm_FunctionRef, self.ref) };
    }
};

pub const Value = struct {
    ref: c.llvm_ValueRef,
};

pub const Constant = struct {
    ref: c.llvm_ConstantRef,

    pub fn getIntegerValue(ty: Type, value: u64) Constant {
        return .{ .ref = c.llvm_Constant_getIntegerValue(ty.ref, value) };
    }

    pub fn intoValue(self: Constant) Value {
        return .{ .ref = @ptrCast(c.llvm_ValueRef, self.ref) };
    }
};

pub const BasicBlock = struct {
    ref: c.llvm_BasicBlockRef,

    pub fn init(
        ctx: Context,
        name: []const u8,
        parent: ?Function,
        insert_before: ?BasicBlock,
    ) BasicBlock {
        return .{ .ref = c.llvm_BasicBlock_create(
            ctx.ref,
            name.ptr,
            name.len,
            if (parent) |v| v.ref else null,
            if (insert_before) |v| v.ref else null,
        ) };
    }
};
