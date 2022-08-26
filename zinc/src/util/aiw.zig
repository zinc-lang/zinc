const std = @import("std");

pub fn AutoIndentingWriter(comptime W: type) type {
    return struct {
        inner: W,

        // Do not write to inner, but still track indentation
        disable: bool = false,

        indent_delta: u8,
        indent_count: u16 = 0,

        is_current_line_empty: bool = true,
        // indent_one_shot_count: u16 = 0,
        // indent_next_line: u16 = 0,

        const Self = @This();

        pub const Writer = std.io.Writer(*Self, W.Error, write);

        pub fn writer(self: *Self) Writer {
            return Writer{ .context = self };
        }

        pub fn writeNoIdent(self: *Self, buf: []const u8) !usize {
            if (buf.len == 0) {
                return 0;
            } else {
                if (!self.disable)
                    try self.inner.writeAll(buf);
                if (buf[buf.len - 1] == '\n')
                    self.resetLine();
                return buf.len;
            }
        }

        fn resetLine(self: *Self) void {
            self.is_current_line_empty = true;
            // self.indent_next_line = 0;
        }

        fn applyIndent(self: *Self) !void {
            const current = self.getCurrentIndent();
            if (self.is_current_line_empty and current > 0 and !self.disable) {
                var i: usize = 0;
                while (i < current) : (i += 1) {
                    try self.inner.writeAll(" ");
                }
            }

            // self.indent_count -= self.indent_one_shot_count;
            // self.indent_one_shot_count = 0;
            self.is_current_line_empty = false;
        }

        fn getCurrentIndent(self: Self) usize {
            var current = self.indent_count;
            if (current > 0) {
                // const count = self.indent_count - self.indent_next_line;
                // current = count * self.indent_delta;
                current = self.indent_count * self.indent_delta;
            }
            return current;
        }

        pub fn write(self: *Self, buf: []const u8) !usize {
            if (buf.len == 0) {
                const z: usize = 0;
                return z;
            } else {
                try self.applyIndent();
                return self.writeNoIdent(buf);
            }
        }

        pub fn pushIndent(self: *Self) void {
            self.indent_count += 1;
        }

        pub fn popIndent(self: *Self) void {
            std.debug.assert(self.indent_count != 0);
            self.indent_count -= 1;

            // if (self.indent_count > 0)
            //     self.indent_next_line -= 1;
        }
    };
}
