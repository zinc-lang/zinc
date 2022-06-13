const std = @import("std");
const Allocator = std.mem.Allocator;

const clap = @import("clap");

const util = @import("util.zig");
const term = @import("term.zig");
const debug = @import("debug.zig");

const parse = @import("parse/!mod.zig");

const zir_test = @import("zir_test.zig");

const CodeLens = @import("CodeLens.zig");

pub fn main() anyerror!void {
    var gpa = std.heap.GeneralPurposeAllocator(.{
        // // uncomment to print more useful information when debugging allocation related bugs,
        // // such as double-frees.
        // // Be warned: will use *a lot* more memory and be *much* slower.
        // .never_unmap = true,
        // .retain_metadata = true,
        // .stack_trace_frames = 8,
    }){
        // .backing_allocator = std.heap.c_allocator,
    };
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();
    // const allocator = std.heap.c_allocator;

    // try zir_test.zirTest(allocator);

    term.init();
    defer term.deinit();

    const opts = try getOptions(allocator);
    defer opts.deinit(allocator);

    const source = try util.readFileBytes(allocator, opts.input_file);
    defer allocator.free(source);

    var timer = try std.time.Timer.start();

    var lex_res = parse.Lexer.lex(allocator, source);
    defer lex_res.deinit(allocator);
    const duration_lex = timer.read();

    const spans = lex_res.spans;

    // // @Verbose: Printing tokens
    // try debug.printTokens(allocator, lex_res.tokens, spans, source);
    // term.println("", .{});

    const token_kinds = lex_res.tokens.items(.kind);

    timer.reset();
    var parse_res = parse.Parser.parse(allocator, token_kinds, source);
    defer parse_res.deinit(allocator);
    const duration_parse = timer.read();

    // // @Verbose: Printing cst
    // try debug.printCst(allocator, parse_res.cst, token_kinds, spans, source);

    // @TODO: Better context and reporting of errors
    if (parse_res.errors.items.len != 0) {
        term.eprintln("", .{});
        for (parse_res.errors.items) |err| {
            const loc = switch (err) {
                .expected => |data| blk: {
                    const found = token_kinds[data.found];

                    term.eprintln("error: Expected '{?}' in '{?}', but found '{?}'", .{ data.what, data.in, found });

                    // const index = if (found == .EOF) data.found - 1 else data.found;
                    const at = token_kinds[data.at];
                    const index = if (at == .EOF) data.at - 1 else data.at;

                    const loc = parse.FileLocation.get(source, spans[index].start);
                    break :blk parse.FileLocationSpan{ .start = loc, .end = loc };
                },
            };

            const lens = CodeLens{ .allocator = allocator, .source = source, .filename = opts.input_file };
            try lens.printLens(loc);

            term.eprintln("", .{});
        }
    }

    // @Verbose: Printing times
    term.println("\ntimes:", .{});
    term.print("  lexing: \t", .{});
    printTimeLen(duration_lex);
    term.print("  parsing: \t", .{});
    printTimeLen(duration_parse);
    term.print("  total: \t", .{});
    printTimeLen(duration_lex + duration_parse);
}

pub const Options = struct {
    input_file: []const u8,

    pub fn deinit(self: Options, allocator: Allocator) void {
        allocator.free(self.input_file);
    }
};

pub fn getOptions(allocator: Allocator) !Options {
    const params = comptime [_]clap.Param(clap.Help){
        // clap.parseParam("-h, --help             Display this help and exit.") catch unreachable,
        // clap.parseParam("-n, --number <NUM>     An option parameter, which takes a value.") catch unreachable,
        // clap.parseParam("-s, --string <STR>...  An option parameter which can be specified multiple times.") catch unreachable,
        clap.parseParam("<FILE>") catch unreachable,
    };
    _ = allocator;

    var diag = clap.Diagnostic{};
    var res = clap.parse(clap.Help, &params, .{ .diagnostic = &diag }) catch |err| {
        // Report useful error and exit
        diag.report(term.stdout.writer(), err) catch {};
        return err;
    };
    defer res.deinit();

    // if (res.flag("--help"))

    const positionals = res.positionals();
    if (positionals.len != 1) {
        term.eprintln("Expected one <FILE> parameter", .{});
        std.process.exit(2);
    }

    return Options{
        .input_file = try allocator.dupe(u8, positionals[0]),
    };
}

fn printTimeLen(ns: u64) void {
    if (ns < 1000) {
        term.println("{} ns", .{ns});
        return;
    }

    const us = ns / 1000;
    if (us < 1000) {
        term.println("{}.{} μs", .{ us, ns - us * 1000 });
        return;
    }

    const ms = us / 1000;
    if (ms < 1000) {
        term.println("{}.{} ms", .{ ms, us - ms * 1000 });
        return;
    }

    const s = ms / 1000;
    if (s < 1000) {
        term.println("{}.{} s", .{ s, ms - s * 1000 });
        return;
    }
}
