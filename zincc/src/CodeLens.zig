allocator: Allocator,
source: []const u8,
filename: []const u8,

const std = @import("std");
const Allocator = std.mem.Allocator;

// const zigstr = @import("zigstr");

const term = @import("term.zig");
const parse = @import("parse/!mod.zig");

const Self = @This();

/// Print a code lens at the given location
pub fn printLens(self: *const Self, loc: parse.FileLocationSpan) !void {
    if (loc.start.line == loc.end.line) {
        std.debug.assert(loc.start.column <= loc.end.column);
        const span_diff = loc.end.column - loc.start.column;

        // one char
        if (span_diff <= 1) {
            try self.printLensSingleChar(loc.start);
        }
        // more than one character
        else {
            try self.printLensMultiChar(loc.start);
        }
    }
    // end is column 1 of next line, therefore one line
    else if (loc.start.line == (loc.end.line - 1) and loc.end.column == 1) {
        try self.printLensMultiChar(loc.start);
    }
    // multiline lens
    else {
        // @TODO: Create method for printing a lens over multiple lines
        term.println("TODO: multiline lens: {?}", .{loc});
    }
}

/// Print a code lens pointing to a single character
fn printLensSingleChar(self: *const Self, loc: parse.FileLocation) !void {
    const offending_line = try self.getLine(loc.line - 1);
    defer self.allocator.free(offending_line);

    try self.singleLineGeneric(offending_line, loc);

    // pointing caret
    term.eprintln("^", .{});
}

/// Print a code lens pointing to multiple characters on a single line
fn printLensMultiChar(self: *const Self, loc: parse.FileLocation) !void {
    const offending_line = try self.getLine(loc.line - 1);
    defer self.allocator.free(offending_line);

    try self.singleLineGeneric(offending_line, loc);

    // pointing carets
    try term.stderr.writer().writeByteNTimes('^', (offending_line.len + 1) - (loc.column - 1));
    term.println("", .{});
}

/// Gets the bytes of a line from a line number.
/// Caller owns memory
fn getLine(self: *const Self, line_index: usize) ![]const u8 {
    // var zstr = try zigstr.fromBytes(self.allocator, self.source);
    // defer zstr.deinit();
    // const lines = try zstr.lines(self.allocator);
    // defer self.allocator.free(lines);

    // const offending_line = lines[line];
    // return try self.allocator.dupe(u8, offending_line);

    var lines = std.mem.split(u8, self.source, "\n");
    var i: usize = 0;
    while (lines.next()) |line| : (i += 1) {
        if (line_index == i)
            return self.allocator.dupe(u8, line);
    }
    unreachable;
}

/// Common code for printing a lens for a single line
fn singleLineGeneric(self: *const Self, offending_line: []const u8, loc: parse.FileLocation) !void {
    const number_str = try std.fmt.allocPrint(self.allocator, "{}", .{loc.line});
    defer self.allocator.free(number_str);
    const padding = number_str.len + 1;

    // in which file and where
    try term.stderr.writer().writeByteNTimes(' ', padding);
    term.eprintln("@ {s}:{}:{}", .{ self.filename, loc.line, loc.column });

    // code lens
    try term.stderr.writer().writeByteNTimes(' ', padding);
    term.eprintln("|", .{});

    term.eprintln("{} | {s}", .{ loc.line, offending_line });

    try term.stderr.writer().writeByteNTimes(' ', padding);
    term.eprint("|", .{});
    try term.stderr.writer().writeByteNTimes(' ', loc.column - 1);
}
