const std = @import("std");
const Allocator = std.mem.Allocator;

const zir = @import("zir.zig");
const llvm = @import("llvm_c.zig");

const CodeGen = @import("CodeGen.zig");

pub fn zirTest(allocator: Allocator) !void {
    var builder = zir.Builder.init(allocator, .{ .arch = .x86_64 });

    // try mkFuncThree(&builder);
    // try mkFuncSum(&builder);
    try mkFuncSyscall3(&builder);
    // try mkFuncRando(&builder);
    try mkFuncWriteStr(&builder);
    try mkFuncMain(&builder);

    const ctx = builder.done();
    defer ctx.deinit(allocator);

    var printer = zir.ZirPrinter(std.fs.File.Writer).init(&ctx, std.io.getStdOut().writer());
    std.debug.print("\n=-= Zir dump =-=\n\n", .{});
    try printer.printCtx(ctx);

    var codegen = CodeGen.init(allocator, &ctx);
    defer codegen.deinit();
    try codegen.gen();

    std.debug.print("\n=-= LLVM Module dump =-=\n\n", .{});
    codegen.llvm_mod.dump();

    _ = llvm.InitializeNativeTarget();
    _ = llvm.InitializeNativeTargetAsmPrinter();
    _ = llvm.InitializeNativeTargetAsmParser();

    var ee = llvm.EngineBuilder.initWithModule(codegen.llvm_mod).create();
    // defer ee.deinti

    std.debug.print("\n=-= Executing program =-=\n\n", .{});

    const main_func = ee.findFunctionNamed("main");
    var ret = ee.runFunction(main_func, &.{});
    ret.deinit();
}

fn mkFuncThree(b: *zir.Builder) !void {
    const ty_s32 = try b.addTy(.{ .sint = 32 });

    const func_ty = try b.addTy(.{ .func = .{ .ret = ty_s32, .args = zir.ExSpan.empty() } });

    const k1 = try b.addValue(.{ .integer = 1 });
    const k2 = try b.addValue(.{ .integer = 2 });

    var inst_b = b.initInstBuilder();
    const ref_k1 = try inst_b.mkConst(.{ .ty = ty_s32, .value = k1 });
    const ref_k2 = try inst_b.mkConst(.{ .ty = ty_s32, .value = k2 });
    const ref_add = try inst_b.mkAdd(.{ .ty = ty_s32, .lhs = ref_k1, .rhs = ref_k2 });
    _ = try inst_b.mkRet(.{ .ty = ty_s32, .bind = ref_add });

    const block = zir.IrItem.Block{ .label = try b.getString("entry"), .body = try inst_b.finish() };
    const func = zir.Func{ .name = try b.getString("three"), .ty = func_ty, .block = block };
    _ = try b.addFunc(func);
}

fn mkFuncSum(b: *zir.Builder) !void {
    const ty_s32 = try b.addTy(.{ .sint = 32 });

    const func_ty = try b.addTy(.{ .func = .{
        .ret = ty_s32,
        .args = try b.createExSpan(&.{ ty_s32, ty_s32, ty_s32 }),
    } });

    var inst_b = b.initInstBuilder();
    const arg0 = try inst_b.mkArg(0);
    const arg1 = try inst_b.mkArg(1);
    const arg2 = try inst_b.mkArg(2);
    const ref_add0 = try inst_b.mkAdd(.{ .ty = ty_s32, .lhs = arg0, .rhs = arg1 });
    const ref_add1 = try inst_b.mkAdd(.{ .ty = ty_s32, .lhs = ref_add0, .rhs = arg2 });
    _ = try inst_b.mkRet(.{ .ty = ty_s32, .bind = ref_add1 });

    const block = zir.IrItem.Block{ .label = try b.getString("entry"), .body = try inst_b.finish() };
    const func = zir.Func{ .name = try b.getString("sum"), .ty = func_ty, .block = block };
    _ = try b.addFunc(func);
}

fn mkFuncSyscall3(b: *zir.Builder) !void {
    const ty_sint_size = try b.addTy(.{ .sint_size = void{} });

    const func_ty = try b.addTy(.{ .func = .{
        .ret = ty_sint_size,
        .args = try b.createExSpan(&.{ ty_sint_size, ty_sint_size, ty_sint_size, ty_sint_size }),
    } });

    var inst_b = b.initInstBuilder();
    const arg0 = try inst_b.mkArg(0);
    const arg1 = try inst_b.mkArg(1);
    const arg2 = try inst_b.mkArg(2);
    const arg3 = try inst_b.mkArg(3);
    const ref_asm_call = try inst_b.mkAsm(.{
        .func_ty = func_ty,
        .asm_str = "syscall",
        .constraints = if (b.target_info.arch == .x86_64)
            "={rax},{rax},{rdi},{rsi},{rdx}"
        else
            "={eax},{eax},{edi},{esi},{edx}",
        .has_side_effects = true,
        .args = &.{ arg0, arg1, arg2, arg3 },
    });
    _ = try inst_b.mkRet(.{ .ty = ty_sint_size, .bind = ref_asm_call });

    const block = zir.IrItem.Block{ .label = zir.StringRef.empty(), .body = try inst_b.finish() };
    const func = zir.Func{ .name = try b.getString("syscall3"), .ty = func_ty, .block = block };
    _ = try b.addFunc(func);
}

fn mkFuncRando(b: *zir.Builder) !void {
    const ty_void = try b.addTy(.{ .void = void{} });

    const ty_s32 = try b.addTy(.{ .sint = 32 });

    const ty_4xs32 = try b.addTy(.{ .array = .{ .of = ty_s32, .size = 4 } });

    const ty_s32_slice = try b.addTy(.{ .slice = ty_s32 });

    const ty_str = try b.addTy(.{ .str = void{} });

    const func_ty = try b.addTy(.{ .func = .{
        .ret = ty_void,
        .args = try b.createExSpan(&.{ ty_4xs32, ty_s32_slice, ty_str }),
    } });

    const block = zir.IrItem.Block{ .label = zir.StringRef.empty(), .body = zir.Inst.Span.empty() };
    const func = zir.Func{ .name = try b.getString("rando"), .ty = func_ty, .block = block };
    _ = try b.addFunc(func);
}

fn mkFuncWriteStr(b: *zir.Builder) !void {
    const ty_uint_size = try b.addTy(.{ .uint_size = void{} });
    const ty_u32 = try b.addTy(.{ .uint = 32 });
    const ty_str = try b.addTy(.{ .str = void{} });
    const ty_u8_ptr = try b.addTy(.{ .ptr = try b.addTy(.{ .uint = 8 }) });

    const func_ty = try b.addTy(.{ .func = .{
        .ret = ty_uint_size,
        .args = try b.createExSpan(&.{ ty_uint_size, ty_str }),
    } });

    var ib = b.initInstBuilder();
    const arg_fd = try ib.mkArg(0);
    const arg_str = try ib.mkArg(1);
    const ref_str_ptr = try ib.mkAlloca(.{ .ty = ty_str });
    _ = try ib.mkStore(.{ .val = arg_str, .ptr = ref_str_ptr });

    const k0 = try b.addValue(.{ .integer = 0 });
    const k1 = try b.addValue(.{ .integer = 1 });
    const ref_k0 = try ib.mkConst(.{ .ty = ty_u32, .value = k0 });
    const ref_k1 = try ib.mkConst(.{ .ty = ty_u32, .value = k1 });

    const ref_str_chars_ptr = try ib.mkGEP(.{ .ty = ty_str, .ptr = ref_str_ptr, .idx_list = &.{ ref_k0, ref_k0 } });
    const ref_str_size_ptr = try ib.mkGEP(.{ .ty = ty_str, .ptr = ref_str_ptr, .idx_list = &.{ ref_k0, ref_k1 } });

    const ref_str_chars = try ib.mkLoad(.{ .ty = ty_u8_ptr, .ptr = ref_str_chars_ptr });
    const ref_str_size = try ib.mkLoad(.{ .ty = ty_uint_size, .ptr = ref_str_size_ptr });

    const func_syscall3_const_inst = (try b.getConstOfFuncFromName("syscall3")) orelse unreachable;
    const ty_func_syscall3 = func_syscall3_const_inst.ty;
    const ref_func_syscall3 = try ib.mkConst(func_syscall3_const_inst);

    const ref_str_chars_int = try ib.mkPtrToInt(.{ .ptr = ref_str_chars, .dest_ty = ty_uint_size });

    const ref_k1_size = try ib.mkConst(.{ .ty = ty_uint_size, .value = k1 });
    const ref_call = try ib.mkCall(.{
        .func_ty = ty_func_syscall3,
        .callee = ref_func_syscall3,
        .args = &.{ ref_k1_size, arg_fd, ref_str_chars_int, ref_str_size },
    });
    _ = try ib.mkRet(.{ .ty = ty_uint_size, .bind = ref_call });

    const block = zir.IrItem.Block{ .label = zir.StringRef.empty(), .body = try ib.finish() };
    const func = zir.Func{ .name = try b.getString("write_str"), .ty = func_ty, .block = block };
    _ = try b.addFunc(func);
}

fn mkFuncMain(b: *zir.Builder) !void {
    const ty_void = try b.addTy(.{ .void = void{} });
    const ty_uint_size = try b.addTy(.{ .uint_size = void{} });
    const ty_str = try b.addTy(.{ .str = void{} });

    const func_ty = try b.addTy(.{ .func = .{
        .ret = ty_void,
        .args = zir.ExSpan.empty(),
    } });

    var ib = b.initInstBuilder();

    const ref_k1 = try ib.mkConst(.{ .ty = ty_uint_size, .value = try b.addValue(.{ .integer = 1 }) });

    const ref_str = try ib.mkConst(.{ .ty = ty_str, .value = try b.addValue(.{ .str = try b.getString("Hay, sailor!\n") }) });

    const func_write_str_const_inst = (try b.getConstOfFuncFromName("write_str")) orelse unreachable;
    const ty_func_write_str = func_write_str_const_inst.ty;
    const ref_func_write_str = try ib.mkConst(func_write_str_const_inst);

    _ = try ib.mkCall(.{
        .func_ty = ty_func_write_str,
        .callee = ref_func_write_str,
        .args = &.{ ref_k1, ref_str },
    });

    _ = try ib.mkRet(.{ .ty = ty_void, .bind = 0 });

    const block = zir.IrItem.Block{ .label = zir.StringRef.empty(), .body = try ib.finish() };
    const func = zir.Func{ .name = try b.getString("main"), .ty = func_ty, .block = block };
    _ = try b.addFunc(func);
}
