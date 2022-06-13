//! Things we can do with this:
//!   Generate llvm-ir
//!   Transpile to c (kinda)
//!   Run in a vm (won't be that performant but cool anyway)
//!     We just need to run a pre-pass to determine the bindings
//!     plus making a vm is effort

const std = @import("std");
const Allocator = std.mem.Allocator;

pub const ExSpan = struct {
    start: u32,
    end: u32,

    pub fn empty() ExSpan {
        return .{ .start = 0, .end = 0 };
    }
};

pub const StringRef = ExSpan;

pub const BindRef = u32; // %N

pub const Context = struct {
    target_info: TargetInformation,

    strings: []const u8,
    extra_data: []const u32,

    insts: []const Inst,
    ir_items: []const IrItem,
    values: []const Value,
    tys: []const Ty,
    funcs: []const Func,

    pub fn deinit(self: Context, allocator: Allocator) void {
        allocator.free(self.strings);
        allocator.free(self.extra_data);

        allocator.free(self.insts);
        allocator.free(self.ir_items);
        allocator.free(self.values);
        allocator.free(self.tys);
        allocator.free(self.funcs);
    }

    pub fn getString(self: Context, ref: StringRef) []const u8 {
        return self.strings[ref.start..ref.end];
    }
};

pub const TargetInformation = struct {
    arch: Arch,

    pub const Arch = enum {
        x86,
        x86_64,
    };

    pub fn ptrBitSize(self: TargetInformation) u8 {
        return switch (self.arch) {
            .x86 => 32,
            .x86_64 => 64,
        };
    }
};

pub const Builder = struct {
    allocator: Allocator,
    target_info: TargetInformation,

    strings: std.ArrayListUnmanaged(u8) = .{},
    // string_indices: std.ArrayListUnmanaged(u32) = .{},

    extra_data: std.ArrayListUnmanaged(u32) = .{},

    insts: std.ArrayListUnmanaged(Inst) = .{},
    ir_items: std.ArrayListUnmanaged(IrItem) = .{},
    values: std.ArrayListUnmanaged(Value) = .{},
    tys: std.ArrayListUnmanaged(Ty) = .{},
    funcs: std.ArrayListUnmanaged(Func) = .{},

    pub const InstBuilder = struct {
        builder: *Builder,

        insts: std.ArrayListUnmanaged(u32) = .{},

        pub fn finish(self: *InstBuilder) !ExSpan {
            defer self.insts.deinit(self.builder.allocator);
            return self.builder.createExSpan(self.insts.items);
        }

        pub fn addInst(self: *InstBuilder, inst: Inst) !BindRef {
            const ref = self.insts.items.len;
            try self.insts.append(
                self.builder.allocator,
                try self.builder.addIrItem(.{ .inst = try self.builder.addInst(inst) }),
            );
            return @intCast(BindRef, ref);
        }

        pub fn mkConst(self: *InstBuilder, inst: Inst.InstConst) !BindRef {
            return self.addInst(.{ .@"const" = inst });
        }

        pub fn mkAdd(self: *InstBuilder, inst: Inst.InstAdd) !BindRef {
            return self.addInst(.{ .add = inst });
        }

        pub fn mkRet(self: *InstBuilder, inst: Inst.InstRet) !BindRef {
            return self.addInst(.{ .ret = inst });
        }

        pub fn mkArg(self: *InstBuilder, idx: u8) !BindRef {
            return self.addInst(.{ .arg = idx });
        }

        pub const InstAsmArgs = struct {
            func_ty: Ty.Ref, // FuncTy
            asm_str: []const u8,
            constraints: []const u8,
            has_side_effects: bool,
            is_align_stack: bool = false,
            args: []const BindRef,
        };

        pub fn mkAsm(self: *InstBuilder, args: InstAsmArgs) !BindRef {
            return self.addInst(.{ .@"asm" = .{
                .func_ty = args.func_ty,
                .asm_str = try self.builder.getString(args.asm_str),
                .constraints = try self.builder.getString(args.constraints),
                .has_side_effects = args.has_side_effects,
                .is_align_stack = args.is_align_stack,
                .args = try self.builder.createExSpan(args.args),
            } });
        }

        pub fn mkAlloca(self: *InstBuilder, inst: Inst.InstAlloca) !BindRef {
            return self.addInst(.{ .alloca = inst });
        }

        pub fn mkStore(self: *InstBuilder, inst: Inst.InstStore) !BindRef {
            return self.addInst(.{ .store = inst });
        }

        pub const InstGEPArgs = struct {
            ty: Ty.Ref,
            ptr: BindRef,
            idx_list: []const BindRef,
        };

        pub fn mkGEP(self: *InstBuilder, args: InstGEPArgs) !BindRef {
            return self.addInst(.{ .getelementptr = .{
                .ty = args.ty,
                .ptr = args.ptr,
                .idx_list = try self.builder.createExSpan(args.idx_list),
            } });
        }

        pub fn mkLoad(self: *InstBuilder, inst: Inst.InstLoad) !BindRef {
            return self.addInst(.{ .load = inst });
        }

        pub const InstCallArgs = struct {
            func_ty: Ty.Ref,
            callee: BindRef,
            args: []const BindRef,
        };

        pub fn mkCall(self: *InstBuilder, args: InstCallArgs) !BindRef {
            return self.addInst(.{ .call = .{
                .func_ty = args.func_ty,
                .callee = args.callee,
                .args = try self.builder.createExSpan(args.args),
            } });
        }

        pub fn mkPtrToInt(self: *InstBuilder, inst: Inst.InstPtrToInt) !BindRef {
            return self.addInst(.{ .ptrtoint = inst });
        }
    };

    pub fn init(allocator: Allocator, target_info: TargetInformation) Builder {
        return .{ .allocator = allocator, .target_info = target_info };
    }

    pub fn done(self: *Builder) Context {
        return .{
            .target_info = self.target_info,

            .strings = self.strings.toOwnedSlice(self.allocator),
            .extra_data = self.extra_data.toOwnedSlice(self.allocator),

            .insts = self.insts.toOwnedSlice(self.allocator),
            .ir_items = self.ir_items.toOwnedSlice(self.allocator),
            .values = self.values.toOwnedSlice(self.allocator),
            .tys = self.tys.toOwnedSlice(self.allocator),
            .funcs = self.funcs.toOwnedSlice(self.allocator),
        };
    }

    pub fn initInstBuilder(self: *Builder) InstBuilder {
        return .{ .builder = self };
    }

    pub fn getString(self: *Builder, str: []const u8) !StringRef {
        // @TODO: String interning
        // for (self.string_indices.items) |index, i| {
        //     // if we are at the end
        //     if (i + 1 >= self.string_indices.items.len)
        //         break;

        //     // if the lengths are not equal
        //     const len = self.string_indices.items[i + 1] - index;
        //     std.log.debug("@ len: {}", .{len});
        //     if (len != str.len)
        //         break;

        //     // if the contents are not equal
        //     const slice = self.strings.items[index..(index + len)];
        //     if (!std.mem.eql(u8, slice, str))
        //         break;

        //     // if all of the above is true then they match and we return the span
        //     return StringRef{ .start = index, .end = index + len };
        // }

        const start = @intCast(u32, self.strings.items.len);
        try self.strings.appendSlice(self.allocator, str);
        const end = @intCast(u32, self.strings.items.len);
        // try self.string_indices.append(self.allocator, start);
        return StringRef{ .start = start, .end = end };
    }

    fn addT(comptime T: type, allocator: Allocator, list: *std.ArrayListUnmanaged(T), t: T) !T.Ref {
        const index = list.items.len;
        try list.append(allocator, t);
        return @intCast(T.Ref, index);
    }

    pub fn addValue(self: *Builder, value: Value) !Value.Ref {
        return addT(Value, self.allocator, &self.values, value);
    }

    pub fn addInst(self: *Builder, inst: Inst) !IrItem.Ref {
        return addT(Inst, self.allocator, &self.insts, inst);
    }

    pub fn addIrItem(self: *Builder, item: IrItem) !IrItem.Ref {
        return addT(IrItem, self.allocator, &self.ir_items, item);
    }

    pub fn addFunc(self: *Builder, func: Func) !Func.Ref {
        return addT(Func, self.allocator, &self.funcs, func);
    }

    // pub fn addIrItems(self: *Builder, items: []const IrItem) !IrItem.Span {
    //     var list = std.ArrayListUnmanaged(IrItem.Ref){};
    //     defer list.deinit(self.allocator);
    //     for (items) |item| {
    //         try list.append(self.allocator, try self.addIrItem(item));
    //     }
    //     return self.createExSpan(list.items);
    // }

    pub fn addTy(self: *Builder, ty: Ty) !Ty.Ref {
        return addT(Ty, self.allocator, &self.tys, ty);
    }

    pub fn createExSpan(self: *Builder, arr: []const u32) !ExSpan {
        const start = self.extra_data.items.len;
        for (arr) |ref| {
            try self.extra_data.append(self.allocator, ref);
        }
        const end = self.extra_data.items.len;
        return ExSpan{ .start = @intCast(u32, start), .end = @intCast(u32, end) };
    }

    pub fn findFuncTypeFromName(self: *Builder, name: []const u8) ?Ty.Ref {
        for (self.funcs.items) |func| {
            const f_name = self.strings.items[func.name.start..func.name.end];
            if (std.mem.eql(u8, name, f_name)) {
                return func.ty;
            }
        }
        return null;
    }

    pub fn getConstOfFuncFromName(self: *Builder, name: []const u8) !?Inst.InstConst {
        // value holding ref to func
        // type of func from the ref
        // ref of func

        for (self.funcs.items) |func, ref| {
            const f_name = self.strings.items[func.name.start..func.name.end];
            if (std.mem.eql(u8, name, f_name)) {
                const value = try self.addValue(.{ .func = @intCast(Func.Ref, ref) });
                return Inst.InstConst{ .ty = func.ty, .value = value };
            }
        }
        return null;
    }
};

pub const IrItem = union(enum) {
    inst: Inst.Ref,
    block: Block,

    pub const Ref = u32;
    pub const Span = ExSpan;

    pub const Block = struct {
        label: StringRef,
        body: IrItem.Span,
    };
};

/// Instruction
pub const Inst = union(enum) {
    @"const": InstConst,
    add: InstAdd,
    ret: InstRet,
    arg: u8,
    @"asm": InstAsm,
    alloca: InstAlloca,
    store: InstStore,
    getelementptr: InstGEP,
    load: InstLoad,
    call: InstCall,
    ptrtoint: InstPtrToInt,

    pub const Ref = u32;
    pub const Span = ExSpan;

    pub const InstConst = struct {
        ty: Ty.Ref,
        value: Value.Ref,
    };

    pub const InstAdd = struct {
        ty: Ty.Ref,
        lhs: BindRef,
        rhs: BindRef,
    };

    pub const InstRet = struct {
        ty: Ty.Ref,
        bind: BindRef,
    };

    pub const InstAsm = struct {
        func_ty: Ty.Ref, // FuncTy
        asm_str: StringRef,
        constraints: StringRef,
        has_side_effects: bool,
        is_align_stack: bool = false,
        args: ExSpan, // list of 'BindRef's
    };

    pub const InstAlloca = struct {
        ty: Ty.Ref,
        size: ?BindRef = null,
    };

    pub const InstStore = struct {
        val: BindRef,
        ptr: BindRef,
        is_volatile: bool = false,
    };

    pub const InstGEP = struct {
        ty: Ty.Ref,
        ptr: BindRef,
        idx_list: ExSpan, // list of `BindRef`s
    };

    pub const InstLoad = struct {
        ty: Ty.Ref,
        ptr: BindRef,
        is_volatile: bool = false,
    };

    pub const InstCall = struct {
        func_ty: Ty.Ref,
        callee: BindRef, // value of function
        args: ExSpan, // list of 'BindRef's
    };

    pub const InstPtrToInt = struct {
        ptr: BindRef,
        dest_ty: Ty.Ref,
    };
};

pub const Value = union(enum) {
    integer: u64,
    func: Func.Ref,
    str: StringRef,

    pub const Ref = u32;
};

pub const Ty = union(enum) {
    void: void,
    sint: u8,
    uint: u8,
    sint_size: void, // size of ptr
    uint_size: void, // size of ptr
    func: FuncTy, // fn(...)
    array: ArrayTy, // [N]T
    slice: Ty.Ref, // []T
    // nullable: Ty.Ref, // ?T
    str: void,
    ptr: Ty.Ref, // *T

    pub const Ref = u32;
    pub const Span = ExSpan;

    pub const FuncTy = struct {
        ret: Ty.Ref,
        args: Ty.Span,
    };

    pub const ArrayTy = struct {
        size: usize,
        of: Ty.Ref,
    };

    pub fn eql(self: Ty, other: Ty) bool {
        if (@enumToInt(self) != @enumToInt(other))
            return false;
        return switch (self) {
            .void, .sint_size, .uint_size, .str => true,
            else => @panic("todo"),
        };
    }
};

pub const Func = struct {
    name: StringRef,
    ty: Ty.Ref,
    block: IrItem.Block,

    pub const Ref = u32;
};

pub fn ZirPrinter(comptime Writer: type) type {
    return struct {
        ctx: *const Context,
        out: out_t,

        const Ais = @import("AutoIndentingStream.zig").AutoIndentingStream;
        const out_t = Ais(Writer);
        const Error = out_t.WriteError;
        const Self = @This();

        pub fn init(ctx: *const Context, writer: Writer) Self {
            return .{
                .ctx = ctx,
                .out = out_t{ .underlying_writer = writer, .indent_delta = 4 },
            };
        }

        fn print(self: *Self, comptime format: []const u8, args: anytype) Error!void {
            try self.out.writer().print(format, args);
        }

        pub fn printCtx(self: *Self, ctx: Context) Error!void {
            for (ctx.funcs) |func| {
                try self.printFunc(func);
                try self.print("\n", .{});
            }
        }

        pub fn printFunc(self: *Self, func: Func) Error!void {
            const name = self.ctx.getString(func.name);

            try self.print("'{s}' :: ", .{name});
            try self.printTy(self.ctx.tys[func.ty]);
            try self.print(" {{\n", .{});
            defer self.print("}}\n", .{}) catch {};
            self.out.pushIndent();
            defer self.out.popIndent();

            // try self.printIrItem(self.ctx.ir_items[func.body]);
            try self.printIrItem(.{ .block = func.block });
            try self.print("\n", .{});
        }

        pub fn printIrItem(self: *Self, ir_item: IrItem) Error!void {
            switch (ir_item) {
                .inst => |ref| try self.printInst(self.ctx.insts[ref]),
                .block => |block| {
                    blk: {
                        const label = self.ctx.getString(block.label);
                        if (label.len == 0)
                            break :blk;

                        self.out.setIndentDelta(2);
                        defer self.out.setIndentDelta(4);
                        self.out.popIndent();
                        defer self.out.pushIndent();

                        try self.print("{s}:\n", .{label});
                    }

                    const ex_slice = self.ctx.extra_data[block.body.start..block.body.end];
                    for (ex_slice) |ref, i| {
                        try self.print("%{} = ", .{i});
                        try self.printIrItem(self.ctx.ir_items[ref]);
                        if (i != ex_slice.len - 1)
                            try self.print("\n", .{});
                    }
                },
            }
        }

        pub fn printInst(self: *Self, inst: Inst) Error!void {
            switch (inst) {
                .@"const" => |k| {
                    try self.print("const ", .{});
                    try self.printTy(self.ctx.tys[k.ty]);
                    try self.print(" ", .{});
                    try self.printValue(self.ctx.values[k.value]);
                },
                .add => |add| {
                    try self.print("add ", .{});
                    try self.printTy(self.ctx.tys[add.ty]);
                    try self.print(" %{} %{}", .{ add.lhs, add.rhs });
                },
                .ret => |ret| {
                    try self.print("ret ", .{});
                    const ty = self.ctx.tys[ret.ty];
                    try self.printTy(ty);
                    if (!ty.eql(.{ .void = void{} }))
                        try self.print(" %{}", .{ret.bind});
                },
                .arg => |i| {
                    try self.print("arg @{}", .{i});
                },
                .@"asm" => |a| {
                    try self.print("asm <", .{});
                    try self.printTy(self.ctx.tys[a.func_ty]);
                    try self.print("> ", .{});

                    if (a.has_side_effects)
                        try self.print("sideeffect ", .{});
                    if (a.is_align_stack)
                        try self.print("alignstack ", .{});

                    try self.print("\"", .{});
                    try self.printString(a.asm_str);
                    try self.print("\" ", .{});

                    try self.print("\"", .{});
                    try self.printString(a.constraints);
                    try self.print("\" (", .{});

                    const slice = self.ctx.extra_data[a.args.start..a.args.end];
                    for (slice) |idx, i| {
                        try self.print("%{}", .{idx});
                        if (i != slice.len - 1)
                            try self.print(", ", .{});
                    }
                    try self.print(")", .{});
                },
                .alloca => |a| {
                    try self.print("alloca ", .{});
                    try self.printTy(self.ctx.tys[a.ty]);
                    if (a.size) |size| {
                        try self.print(", %{}", .{size});
                    }
                },
                .store => |store| try self.print("store %{}, %{}", .{ store.val, store.ptr }),
                .getelementptr => |gep| {
                    try self.print("getelementptr ", .{});
                    try self.printTy(self.ctx.tys[gep.ty]);
                    try self.print(" %{}, ", .{gep.ptr});
                    const slice = self.ctx.extra_data[gep.idx_list.start..gep.idx_list.end];
                    for (slice) |ref, i| {
                        try self.print("%{}", .{ref});
                        if (i != slice.len - 1)
                            try self.print(", ", .{});
                    }
                },
                .load => |load| {
                    try self.print("load ", .{});
                    try self.printTy(self.ctx.tys[load.ty]);
                    try self.print(" %{}", .{load.ptr});
                    if (load.is_volatile)
                        try self.print(" is_volatile", .{});
                },
                .call => |call| {
                    try self.print("call ", .{});
                    try self.printTy(self.ctx.tys[call.func_ty]);

                    try self.print(" %{} ", .{call.callee});

                    try self.print("(", .{});
                    const slice = self.ctx.extra_data[call.args.start..call.args.end];
                    for (slice) |ref, i| {
                        try self.print("%{}", .{ref});
                        if (i != slice.len - 1)
                            try self.print(", ", .{});
                    }
                    try self.print(")", .{});
                },
                .ptrtoint => |i| {
                    try self.print("ptrtoint %{}, ", .{i.ptr});
                    try self.printTy(self.ctx.tys[i.dest_ty]);
                },
            }
        }

        pub fn printTy(self: *Self, ty: Ty) Error!void {
            switch (ty) {
                .void => try self.print("void", .{}),
                .sint => |n| try self.print("s{}", .{n}),
                .uint => |n| try self.print("u{}", .{n}),
                .sint_size => try self.print("sint_size", .{}),
                .uint_size => try self.print("uint_size", .{}),
                .func => |func| {
                    try self.print("fn(", .{});
                    const slice = self.ctx.extra_data[func.args.start..func.args.end];
                    for (slice) |ref, i| {
                        try self.printTy(self.ctx.tys[ref]);
                        if (i != slice.len - 1)
                            try self.print(", ", .{});
                    }
                    try self.print("): ", .{});
                    try self.printTy(self.ctx.tys[func.ret]);
                },
                .array => |t| {
                    try self.print("[{}]", .{t.size});
                    try self.printTy(self.ctx.tys[t.of]);
                },
                .slice => |t| {
                    try self.print("[]", .{});
                    try self.printTy(self.ctx.tys[t]);
                },
                .str => try self.print("str", .{}),
                .ptr => |t| {
                    try self.print("*", .{});
                    try self.printTy(self.ctx.tys[t]);
                },
            }
        }

        pub fn printValue(self: *Self, value: Value) Error!void {
            switch (value) {
                .integer => |int| try self.print("{}", .{int}),
                .func => |ref| {
                    const func = self.ctx.funcs[ref];
                    const name = self.ctx.getString(func.name);

                    try self.print("'{s}' :: ", .{name});
                    try self.printTy(self.ctx.tys[func.ty]);
                },
                .str => |str| try self.print("\"{s}\"", .{self.ctx.getString(str)}),
            }
        }

        fn printString(self: *Self, ref: StringRef) !void {
            const str = self.ctx.getString(ref);
            try self.print("{s}", .{str});
        }
    };
}
