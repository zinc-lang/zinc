const std = @import("std");
const Allocator = std.mem.Allocator;

const term = @import("term.zig");
const util = @import("util.zig");
const parse = @import("parse/!mod.zig");

/// Use the `parse.Cst.Printer` to print the tree structure of a cst
pub fn printCst(
    allocator: Allocator,
    cst: parse.Cst,
    tokens: []const parse.Token.Kind,
    spans: []const parse.SpanData,
    src: []const u8,
) !void {
    var printer = parse.Cst.Printer{
        .allocator = allocator,
        .cst = cst,
        .tokens = tokens,
        .spans = spans,
        .src = src,
    };
    try printer.print();
}

/// Print a list of tokens and their whitespace
pub fn printTokens(
    allocator: Allocator,
    tokens: std.MultiArrayList(parse.Token).Slice,
    spans: []const parse.SpanData,
    src: []const u8,
) !void {
    const ws_lens = tokens.items(.preceding_ws);

    for (tokens.items(.kind)) |kind, i| {
        const span = spans[i];
        const ws_len = ws_lens[i];

        const start = span.start - ws_len;
        const end = span.start;
        const slice = src[start..end];
        const unesc_slice = try util.unescapeString(allocator, slice);
        defer allocator.free(unesc_slice);
        if (unesc_slice.len != 0) {
            term.printAttr(" '{s}'", .{unesc_slice}, .{ .col = .white });
        }
        term.println("", .{});

        try printToken(allocator, kind, span, src);
    }
    term.println("", .{});
}

/// Print a single token and its span
pub fn printToken(
    allocator: Allocator,
    kind: parse.Token.Kind,
    span: parse.SpanData,
    src: []const u8,
) !void {
    var slice = src[span.start..span.end];
    var slice_unesc = try util.unescapeString(allocator, slice);
    defer allocator.free(slice_unesc);

    const kind_str = try std.fmt.allocPrint(allocator, "{?}", .{kind});
    defer allocator.free(kind_str);

    term.printAttr("{s}", .{kind_str[5..]}, .{ .col = .cyan });
    term.printAttr("@{}..{}", .{ span.start, span.end }, .{ .col = .white });
    term.printAttr(" '{s}'", .{slice_unesc}, .{ .col = .green });
}

/// Helper to print a piece of the `src` represented by a `span`
fn printLexeme(allocator: Allocator, span: parse.SpanData, src: []const u8) !void {
    var slice = src[span.start..span.end];
    var slice_unesc = try util.unescapeString(allocator, slice);
    defer allocator.free(slice_unesc);
    term.print("{s}", .{slice});
}
