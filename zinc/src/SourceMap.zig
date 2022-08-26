const std = @import("std");
const Allocator = std.mem.Allocator;

const util = @import("util/util.zig");

const parse_glue = @import("parse/glue.zig");

files: SourceFiles = .{},
root: SourceFiles.Ref,

pub const SourceFiles = util.IndexList(SourceFile, u32);

pub const SourceFile = struct {
    path: []const u8,
    src: [:0]const u8,

    flags: packed struct {
        lexed: bool = false,
        parsed: bool = false,
        has_ast: bool = false,
    } = .{},

    line_offsets: []u32 = &.{},
    tokens: parse_glue.Token.List.Slice = undefined,
    ctxkws: parse_glue.Token.Kind.CtxKws = .{},

    cst: parse_glue.Cst = undefined,

    ast: parse_glue.Ast = undefined,

    pub fn deinit(self: *SourceFile, alloc: Allocator) void {
        alloc.free(self.path);
        alloc.free(self.src);

        if (self.flags.lexed) {
            alloc.free(self.line_offsets);
            self.tokens.deinit(alloc);
            self.ctxkws.deinit(alloc);
        }

        if (self.flags.parsed)
            self.cst.deinit(alloc);

        if (self.flags.has_ast)
            self.ast.deinit(alloc);

        self.* = undefined;
    }

    pub fn getLineOffsetIndex(self: SourceFile, offset: u32) u32 {
        return for (self.line_offsets) |it, i| {
            if (it > offset)
                break @intCast(u32, i - 1);
        } else if (self.line_offsets[self.line_offsets.len - 1] == offset)
            self.line_offsets[self.line_offsets.len - 1]
        else
            util.bug(
                "impossible source code offset '{}' - last line offset: '{}' - line count: '{}'",
                .{ offset, self.line_offsets[self.line_offsets.len - 1], self.line_offsets.len },
            );
    }

    pub fn getLineSlice(self: SourceFile, line: u32) []const u8 {
        const start = self.line_offsets[line];
        const end = self.line_offsets[line + 1] - 1;
        return self.src[start..end];
    }

    pub fn getLexeme(self: SourceFile, token_index: u32) []const u8 {
        const range = self.tokens.items(.range)[token_index];
        return self.src[range.start..range.end];
    }
};

pub fn deinit(self: *@This(), alloc: Allocator) void {
    for (self.files.list.items) |*file|
        file.deinit(alloc);
    self.files.deinit(alloc);
    self.* = undefined;
}
