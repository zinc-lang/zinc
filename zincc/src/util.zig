const std = @import("std");
const Allocator = std.mem.Allocator;

/// Caller owns memory
pub fn readFileBytes(allocator: Allocator, path: []const u8) ![]const u8 {
    const cwd = std.fs.cwd();
    var file = try cwd.openFile(path, .{});
    defer file.close();
    try file.seekTo(0);
    return file.readToEndAlloc(allocator, std.math.maxInt(usize));
}

/// Un-escape a given string.
/// ie. Turn a newline into a literal '\n' & etc.
///
/// Caller owns memory
pub fn unescapeString(allocator: Allocator, str: []const u8) ![]u8 {
    var out = std.ArrayList(u8).init(allocator);
    errdefer out.deinit();
    for (str) |c| {
        switch (c) {
            '\t' => try out.appendSlice("\\t"),
            '\n' => try out.appendSlice("\\n"),
            '\r' => try out.appendSlice("\\r"),
            '\'' => try out.appendSlice("\\'"),
            '\"' => try out.appendSlice("\\\""),
            ' ' => try out.appendSlice("·"),
            else => try out.append(c),
        }
    }
    return out.toOwnedSlice();
}
