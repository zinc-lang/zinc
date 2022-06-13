allocator: Allocator,

zir_ctx: *const zir.Context,

llvm_ctx: llvm.Context,
llvm_mod: llvm.Module,
llvm_builder: llvm.IRBuilderDefault,

type_cache: std.AutoHashMapUnmanaged(zir.Ty.Ref, llvm.Type) = .{},

functions: std.AutoHashMapUnmanaged(zir.Func.Ref, llvm.Function) = .{},

const std = @import("std");
const Allocator = std.mem.Allocator;

const zir = @import("zir.zig");
const llvm = @import("llvm_c.zig");

const Self = @This();

pub fn init(allocator: Allocator, zir_ctx: *const zir.Context) Self {
    const llvm_ctx = llvm.Context.init();
    return .{
        .allocator = allocator,
        .zir_ctx = zir_ctx,
        .llvm_ctx = llvm_ctx,
        .llvm_mod = llvm.Module.init("main", llvm_ctx),
        .llvm_builder = llvm.IRBuilderDefault.init(llvm_ctx),
    };
}

pub fn deinit(self: *Self) void {
    defer self.* = undefined;
    defer self.llvm_ctx.deinit();
    defer self.llvm_mod.deinit();
    defer self.llvm_builder.deinit();
    defer self.type_cache.deinit(self.allocator);
    defer self.functions.deinit(self.allocator);
}

fn getLLVMType(self: *Self, ref: zir.Ty.Ref) Allocator.Error!llvm.Type {
    if (self.type_cache.get(ref)) |ty| {
        return ty;
    } else {
        const ty = try self.generateLLVMType(self.zir_ctx.tys[ref]);
        try self.type_cache.put(self.allocator, ref, ty);
        return ty;
    }
}

fn generateLLVMType(self: *Self, zir_ty: zir.Ty) !llvm.Type {
    return switch (zir_ty) {
        .void => llvm.Type.getVoidTy(self.llvm_ctx),
        .sint => |n| llvm.Type.getIntNTy(self.llvm_ctx, n),
        .uint => |n| llvm.Type.getIntNTy(self.llvm_ctx, n),
        .sint_size, .uint_size => llvm.Type.getIntNTy(self.llvm_ctx, self.zir_ctx.target_info.ptrBitSize()),
        .func => |func| blk: {
            var args = std.ArrayListUnmanaged(llvm.Type){};
            defer args.deinit(self.allocator);

            const indices = self.zir_ctx.extra_data[func.args.start..func.args.end];
            for (indices) |ref| {
                const t = try self.getLLVMType(ref);
                try args.append(self.allocator, t);
            }

            const ret = try self.getLLVMType(func.ret);

            break :blk llvm.FunctionType.get(ret, args.items, false).intoType();
        },
        .array => |t| llvm.Type.getArrayTy(try self.getLLVMType(t.of), t.size),
        .slice => |slice_ty| blk: {
            var buf = std.ArrayList(u8).init(self.allocator);
            defer buf.deinit();

            var printer = zir.ZirPrinter(std.ArrayList(u8).Writer).init(self.zir_ctx, buf.writer());
            try printer.printTy(self.zir_ctx.tys[slice_ty]);

            const name = try std.fmt.allocPrint(self.allocator, "slice.{s}", .{buf.items});
            defer self.allocator.free(name);

            const ty = self.generateLLVMSliceType(try self.getLLVMType(slice_ty));
            ty.setName(name);

            break :blk ty.intoType();
        },
        .str => blk: {
            const ty = self.generateLLVMSliceType(llvm.Type.getIntNTy(self.llvm_ctx, 8));
            ty.setName("str");
            break :blk ty.intoType();
        },
        .ptr => |t| (try self.getLLVMType(t)).getPointerTo(0).intoType(),
    };
}

fn generateLLVMSliceType(self: Self, of: llvm.Type) llvm.StructType {
    return llvm.StructType.get(
        self.llvm_ctx,
        &.{
            // @TODO: Use opaque pointers when we update to LLVM-14
            // llvm.PointerType.getOpaque(self.llvm_ctx, 0).intoType(),
            llvm.PointerType.get(of, 0).intoType(),
            llvm.Type.getIntNTy(self.llvm_ctx, self.zir_ctx.target_info.ptrBitSize()),
        },
        false,
    );
}

pub fn gen(self: *Self) !void {
    for (self.zir_ctx.funcs) |_, i| {
        try self.genFunc(@intCast(zir.Func.Ref, i));
    }
}

pub fn genFunc(self: *Self, zir_func_ref: zir.Func.Ref) !void {
    const zir_func = self.zir_ctx.funcs[zir_func_ref];

    const func_ty = (try self.getLLVMType(zir_func.ty)).intoFunctionType();
    const func_name = self.zir_ctx.getString(zir_func.name);

    const func = llvm.Function.init(func_ty, .External, 0, func_name, self.llvm_mod);

    try self.genBlock(zir_func.block, func);
    try self.functions.put(self.allocator, zir_func_ref, func);
}

fn genBlock(self: *Self, block: zir.IrItem.Block, llvm_func: ?llvm.Function) !void {
    const block_name = self.zir_ctx.getString(block.label);
    const bb = llvm.BasicBlock.init(self.llvm_ctx, block_name, llvm_func, null);
    self.llvm_builder.setInsertionPoint(bb);

    var bindings = std.ArrayListUnmanaged(llvm.Value){};
    defer bindings.deinit(self.allocator);

    const ex_data = self.zir_ctx.extra_data[block.body.start..block.body.end];
    var inst_count: u32 = 0;
    for (ex_data) |ref| {
        const ir_item = self.zir_ctx.ir_items[ref];
        switch (ir_item) {
            .inst => |r| {
                const inst = self.zir_ctx.insts[r];
                const value = switch (inst) {
                    .@"const" => |k| try self.genValue(k.ty, k.value),
                    .add => |add| self.llvm_builder.createAdd(bindings.items[add.lhs], bindings.items[add.rhs], "tmp_add"),
                    .ret => |ret| blk: {
                        const ty = try self.getLLVMType(ret.ty);
                        break :blk if (ty.getTypeID() == .Void)
                            self.llvm_builder.createRetVoid()
                        else
                            self.llvm_builder.createRet(bindings.items[ret.bind]);
                    },
                    .arg => |i| if (llvm_func) |f| f.getArg(i) else @panic("used arg inst without a function passed"),
                    .@"asm" => |a| blk: {
                        const func_ty = (try self.getLLVMType(a.func_ty)).intoFunctionType();

                        const asm_str = self.zir_ctx.getString(a.asm_str);
                        const constraints = self.zir_ctx.getString(a.constraints);

                        // std.debug.assert(llvm.InlineAsm.verify(func_ty, constraints));

                        const syscall3_asm = llvm.InlineAsm.get(
                            func_ty,
                            asm_str,
                            constraints,
                            a.has_side_effects,
                            a.is_align_stack,
                            .ATT, // @TODO: Expose
                            false, // @TODO: Expose
                        );

                        const func = syscall3_asm.intoFunction().intoValue();

                        var args = std.ArrayList(llvm.Value).init(self.allocator);
                        defer args.deinit();
                        const args_slice = self.zir_ctx.extra_data[a.args.start..a.args.end];
                        for (args_slice) |idx| {
                            try args.append(bindings.items[idx]);
                        }

                        break :blk self.llvm_builder.createCall(func_ty, func, args.items, "asm_call");
                    },
                    .alloca => |alloc| self.llvm_builder.createAlloca(
                        try self.getLLVMType(alloc.ty),
                        0,
                        if (alloc.size) |size| bindings.items[size] else null,
                        "alloc",
                    ),
                    .store => |store| self.llvm_builder.createStore(
                        bindings.items[store.val],
                        bindings.items[store.ptr],
                        store.is_volatile,
                    ),
                    .getelementptr => |gep| blk: {
                        var idx_list = std.ArrayListUnmanaged(llvm.Value){};
                        defer idx_list.deinit(self.allocator);
                        const slice = self.zir_ctx.extra_data[gep.idx_list.start..gep.idx_list.end];
                        for (slice) |idx| {
                            try idx_list.append(self.allocator, bindings.items[idx]);
                        }
                        break :blk self.llvm_builder.createGEPArr(
                            try self.getLLVMType(gep.ty),
                            bindings.items[gep.ptr],
                            // bindings.items[gep.idx],
                            idx_list.items,
                            "gep",
                        );
                    },
                    .load => |load| self.llvm_builder.createLoad(try self.getLLVMType(load.ty), bindings.items[load.ptr], load.is_volatile, "load"),
                    .call => |call| blk: {
                        const slice = self.zir_ctx.extra_data[call.args.start..call.args.end];
                        var args = std.ArrayListUnmanaged(llvm.Value){};
                        defer args.deinit(self.allocator);
                        for (slice) |idx| {
                            try args.append(self.allocator, bindings.items[idx]);
                        }
                        break :blk self.llvm_builder.createCall(
                            (try self.getLLVMType(call.func_ty)).intoFunctionType(),
                            bindings.items[call.callee],
                            args.items,
                            "call",
                        );
                    },
                    .ptrtoint => |pti| self.llvm_builder.createPtrToInt(bindings.items[pti.ptr], try self.getLLVMType(pti.dest_ty), "pti"),
                };
                try bindings.insert(self.allocator, inst_count, value);
                inst_count += 1;
            },
            .block => @panic("todo"),
        }
    }
}

fn genValue(self: *Self, zir_ty: zir.Ty.Ref, zir_val: zir.Value.Ref) !llvm.Value {
    const ty = try self.getLLVMType(zir_ty);
    const val = self.zir_ctx.values[zir_val];
    return switch (val) {
        .integer => |int| llvm.Constant.getIntegerValue(ty, int).intoValue(),
        .func => |func| (self.functions.get(func) orelse unreachable).intoValue(),
        .str => |str_ref| blk2: {
            const str = self.zir_ctx.getString(str_ref);
            const str_ptr = self.llvm_builder.createGlobalStringPtr(str, "str_literal", 0, self.llvm_mod).intoValue();
            // const str_ptr = self.llvm_builder.createGlobalString(str, "str_literal", 0, self.llvm_mod).intoValue();

            const ty_i32 = llvm.Type.getIntNTy(self.llvm_ctx, 32);
            const ty_size = llvm.Type.getIntNTy(self.llvm_ctx, self.zir_ctx.target_info.ptrBitSize());
            const ty_i8 = llvm.Type.getIntNTy(self.llvm_ctx, 8);
            // const ty_i8_ptr = ty_i8.getPointerTo(0);
            const ty_slice = self.generateLLVMSliceType(ty_i8);

            const k0 = llvm.Constant.getIntegerValue(ty_i32, 0).intoValue();
            const k1 = llvm.Constant.getIntegerValue(ty_i32, 1).intoValue();

            const k_length = llvm.Constant.getIntegerValue(ty_size, str.len).intoValue();

            const ref_slice_ptr = self.llvm_builder.createAlloca(ty_slice.intoType(), 0, null, "str_ptr");
            const ref_slice_ptr_ptr = self.llvm_builder.createGEPArr(ty_slice.intoType(), ref_slice_ptr, &.{ k0, k0 }, "str_ptr_ptr");
            const ref_slice_ptr_len = self.llvm_builder.createGEPArr(ty_slice.intoType(), ref_slice_ptr, &.{ k0, k1 }, "str_ptr_len");
            _ = self.llvm_builder.createStore(str_ptr, ref_slice_ptr_ptr, false);
            _ = self.llvm_builder.createStore(k_length, ref_slice_ptr_len, false);

            const ref_slice = self.llvm_builder.createLoad(ty_slice.intoType(), ref_slice_ptr, false, "str");

            break :blk2 ref_slice;
        },
    };
}
