const std = @import("std");
const File = std.fs.File;

pub const ESC = "\x1b";
pub const CSI = ESC ++ "[";

pub var stdout: File = undefined;
pub var stdout_buffer: std.io.BufferedWriter(2048, File.Writer) = undefined;
pub var stderr: File = undefined;

pub fn init() void {
    stdout = std.io.getStdOut();
    stdout_buffer = @TypeOf(stdout_buffer){ .unbuffered_writer = stdout.writer() };
    stderr = std.io.getStdErr();
}

pub fn deinit() void {
    flush();
}

pub fn print(comptime format: []const u8, args: anytype) void {
    inline for (format) |c|
        if (c == '\n')
            flush();
    stdout_buffer.writer().print(format, args) catch return;
}

pub fn println(comptime format: []const u8, args: anytype) void {
    print(format ++ "\n", args);
}

pub fn flush() void {
    stdout_buffer.flush() catch return;
}

pub fn eprint(comptime format: []const u8, args: anytype) void {
    stderr.writer().print(format, args) catch return;
}

pub fn eprintln(comptime format: []const u8, args: anytype) void {
    eprint(format ++ "\n", args);
}

pub fn clearLine() void {
    print(CSI ++ "2K", .{});
}

pub fn printAttr(comptime format: []const u8, args: anytype, attr: Attr) void {
    print("{}", .{attr});
    print(format, args);
    print("{}", .{Attr{ .reset = true }});
}

pub fn printlnAttr(comptime format: []const u8, args: anytype, attr: Attr) void {
    printAttr(format, args, attr);
    println("", .{});
}

/// Ansi text attributes
pub const Attr = struct {
    col: ?Color = null,
    bold: bool = false,
    reset: bool = false,

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

    pub fn format(self: Attr, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = options;
        _ = fmt;

        try writer.writeAll("\x1b[");

        if (self.reset) {
            try writer.writeAll("0m");
            return;
        }

        if (self.col) |col| {
            switch (col) {
                .black => try writer.writeAll("30"),
                .red => try writer.writeAll("31"),
                .green => try writer.writeAll("32"),
                .yellow => try writer.writeAll("33"),
                .blue => try writer.writeAll("34"),
                .magenta => try writer.writeAll("35"),
                .cyan => try writer.writeAll("36"),
                .white => try writer.writeAll("37"),
            }
        }

        if (self.bold)
            try writer.writeAll(";1");

        try writer.writeAll("m");
    }
};
