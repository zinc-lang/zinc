const std = @import("std");
const io = std.io;
const fs = std.fs;

pub const getStdout = std.io.getStdOut;
pub const getStderr = std.io.getStdErr;

pub const ESC = "\x1b";
pub const CSI = ESC ++ "[";

pub const ClearLine = CSI ++ "2K";
pub const Reset = CSI ++ "0m";

var stdout: io.BufferedWriter(512, fs.File.Writer) = undefined;
var stderr: fs.File = undefined;

pub fn init() void {
    stdout = .{ .unbuffered_writer = io.getStdOut().writer() };
    stderr = std.io.getStdErr();
}

pub fn flush() void {
    stdout.flush() catch {};
}

pub fn print(comptime fmt: []const u8, args: anytype) void {
    inline for (fmt) |ch|
        if (ch == '\n')
            flush();

    stdout.writer().print(fmt, args) catch {};
}

pub fn println(comptime fmt: []const u8, args: anytype) void {
    print(fmt ++ "\n", args);
}

pub fn eprint(comptime fmt: []const u8, args: anytype) void {
    stderr.writer().print(fmt, args) catch {};
}

pub fn eprintln(comptime fmt: []const u8, args: anytype) void {
    eprint(fmt ++ "\n", args);
}
pub const Attr = struct {
    color: ?Color = null,
    bold: bool = false,

    pub const Color = enum(u3) {
        black,
        red,
        green,
        yellow,
        blue,
        magenta,
        cyan,
        white,
    };

    pub fn format(
        self: Attr,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = options;
        _ = fmt;

        try writer.writeAll(CSI);

        if (self.color) |col| {
            const str = switch (col) {
                .black => "30",
                .red => "31",
                .green => "32",
                .yellow => "33",
                .blue => "34",
                .magenta => "35",
                .cyan => "36",
                .white => "37",
            };
            try writer.writeAll(str);
        }

        if (self.bold)
            try writer.writeAll(";1");

        try writer.writeAll("m");
    }
};
