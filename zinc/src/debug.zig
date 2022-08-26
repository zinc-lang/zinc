const std = @import("std");
const Allocator = std.mem.Allocator;

const term = @import("term.zig");
const util = @import("util/util.zig");

const parse_glue = @import("parse/glue.zig");

const SourceMap = @import("SourceMap.zig");

pub fn formatToken(
    alloc: Allocator,
    file: SourceMap.SourceFile,
    index: u32,
    use_color: bool,
) ![]u8 {
    const ty_path = @typeName(parse_glue.Token.Kind);

    const range = file.tokens.items(.range)[index];
    const kind = if (file.ctxkws.get(index)) |k| k else file.tokens.items(.kind)[index];

    const line_offset_index = file.getLineOffsetIndex(range.start);
    const line_number = line_offset_index + 1;
    const column_number = range.start - file.line_offsets[line_offset_index];

    const slice = try util.unescapeStr(alloc, file.src[range.start..range.end]);
    defer alloc.free(slice);

    // @NOTE: Keep both branches in sync
    if (use_color) {
        const buf = try std.fmt.allocPrint(alloc, "{?}", .{kind});
        defer alloc.free(buf);

        var out = std.ArrayList(u8).init(alloc);
        var writer = out.writer();

        // Write token kind
        try writer.print("{}", .{term.Attr{ .color = .cyan }});
        try writer.print("{s}{s}", .{ buf[ty_path.len + 1 ..], term.Reset });

        // Write '@'
        try writer.print(
            "{}@{s}",
            .{ term.Attr{ .color = .white, .bold = true }, term.Reset },
        );

        // Write range as 'start..end'
        try writer.print("{}..{}", .{ range.start, range.end });

        // Write source location as '[line:column]'
        try writer.print("{}", .{term.Attr{ .color = .white, .bold = true }});
        try writer.print(" [{}:{}, i={}]", .{ line_number, column_number, index });
        try writer.print("{s}", .{term.Reset});

        // Write lexeme
        try writer.print("{}", .{term.Attr{ .color = .green }});
        try writer.print("  '{s}'{s}", .{ slice, term.Reset });

        return out.toOwnedSlice();
    } else {
        const str = try std.fmt.allocPrint(
            alloc,
            "{?}@{}..{} [{}:{}]  '{s}'",
            .{
                kind,
                range.start,
                range.end,
                line_number,
                column_number,
                slice,
            },
        );
        return str[ty_path.len + 1 ..];
    }
}

pub fn printTokens(
    comptime W: type,
    alloc: Allocator,
    writer: W,
    file: SourceMap.SourceFile,
    use_color: bool,
    skip_trivia: bool,
) !void {
    for (file.tokens.items(.kind)) |kind, i| {
        if (skip_trivia and kind.isTrivia())
            continue;

        const tok = try formatToken(alloc, file, @intCast(u32, i), use_color);
        defer alloc.free(tok);
        try writer.print("{s}\n", .{tok});
    }
}

pub fn CstWriter(comptime W: type) type {
    return struct {
        alloc: Allocator,
        writer: util.aiw.AutoIndentingWriter(W),
        file: SourceMap.SourceFile,

        use_color: bool,
        skip_trivia: bool,

        const Self = @This();

        pub fn init(
            alloc: Allocator,
            writer: W,
            file: SourceMap.SourceFile,
            use_color: bool,
            skip_trivia: bool,
        ) Self {
            return .{
                .alloc = alloc,
                .writer = util.aiw.AutoIndentingWriter(W){
                    .inner = writer,
                    .indent_delta = 2,
                },
                .file = file,
                .use_color = use_color,
                .skip_trivia = skip_trivia,
            };
        }

        pub fn write(self: *Self, index: u32) anyerror!void {
            const elems_range = self.file.cst.nodes.items(.elems)[index];
            const elems = self.file.cst.elems[elems_range.start..elems_range.end];
            for (elems) |elem| {
                switch (elem) {
                    .token => |i| {
                        const str = try formatToken(self.alloc, self.file, i, self.use_color);
                        defer self.alloc.free(str);

                        _ = try self.writer.write(str);
                        _ = try self.writer.write("\n");
                    },
                    .node => |i| {
                        const kind = self.file.cst.nodes.items(.kind)[i];

                        const ty_path = @typeName(parse_glue.Cst.Node.Kind);

                        const buf = try std.fmt.allocPrint(self.alloc, "{?}", .{kind});
                        defer self.alloc.free(buf);

                        const name = buf[ty_path.len + 1 ..];

                        const writer = self.writer.writer();

                        if (self.use_color) {
                            try writer.print("{}", .{term.Attr{ .color = .magenta }});
                            try writer.print("{s}{s}", .{ name, term.Reset });
                        } else {
                            try writer.writeAll(name);
                        }

                        try writer.print("{}", .{term.Attr{ .color = .white, .bold = true }});
                        try writer.print(" [i={}]", .{i});
                        try writer.print("{s}", .{term.Reset});

                        try writer.writeByte('\n');

                        self.writer.pushIndent();
                        try self.write(i);
                        self.writer.popIndent();
                    },
                }
            }
        }
    };
}
