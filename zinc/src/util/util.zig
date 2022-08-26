const std = @import("std");
const Allocator = std.mem.Allocator;

const term = @import("../term.zig");

pub const aiw = @import("aiw.zig");
pub const clap = @import("clap.zig");

pub fn unescapeStr(alloc: Allocator, str: []const u8) ![]u8 {
    var out = std.ArrayList(u8).init(alloc);

    for (str) |ch| {
        switch (ch) {
            '\n' => try out.appendSlice("\\n"),
            '\r' => try out.appendSlice("\\r"),
            '\t' => try out.appendSlice("\\t"),
            else => try out.append(ch),
        }
    }

    return out.toOwnedSlice();
}

pub fn todo(comptime format: []const u8, args: anytype) noreturn {
    term.eprintln("todo: \n\t" ++ format ++ "\n\n", args);
    unreachable;
}

pub fn bug(comptime format: []const u8, args: anytype) noreturn {
    term.eprintln("reached bug: " ++ format ++ "\n\n", args);
    unreachable;
}

pub fn readFile(alloc: Allocator, path: []const u8) ![:0]u8 {
    const cwd = std.fs.cwd();

    const file = try cwd.openFile(path, .{});
    const size = (try file.getEndPos()) + 1;

    const out = try alloc.allocSentinel(u8, size, 0);
    errdefer alloc.free(out);

    const read = try file.readAll(out);
    std.debug.assert(read + 1 == size);

    out[out.len - 1] = '\n';
    return out;
}

pub const Timer = struct {
    timer: std.time.Timer,

    pub const Duration = struct {
        duration: u64,

        pub fn format(
            self: Duration,
            comptime fmt: []const u8,
            options: std.fmt.FormatOptions,
            writer: anytype,
        ) !void {
            _ = fmt;
            _ = options;

            const total_ns = self.duration;
            const time = std.time;

            const s = total_ns / time.ns_per_s;
            const ms = total_ns / time.ns_per_ms - s * time.ms_per_s;
            const us = total_ns / time.ns_per_us - ms * time.us_per_ms;
            // const ns = total_ns - us * time.ns_per_us;

            // try writer.print("{}s {}ms {}μs {}ns", .{ s, ms, us, ns });
            try writer.print("{}s {}ms {}μs", .{ s, ms, us });
        }
    };

    pub fn start() !Timer {
        return Timer{
            .timer = try std.time.Timer.start(),
        };
    }

    pub fn reset(self: *Timer) void {
        self.timer.reset();
    }

    pub fn read(self: *Timer) Duration {
        const ns = self.timer.read();
        return .{ .duration = ns };
    }
};

pub fn IndexList(comptime T: type, comptime IndexType: type) type {
    return struct {
        list: std.ArrayListUnmanaged(T) = .{},

        pub const Ref = struct {
            raw: IndexType,

            pub fn zero() Ref {
                return .{ .raw = 0 };
            }

            pub fn max() Ref {
                return .{ .raw = std.math.maxInt(IndexType) };
            }

            pub const Range = struct {
                start: Ref,
                end: Ref,

                pub fn empty() Range {
                    return .{ .start = Ref.zero(), .end = Ref.zero() };
                }
            };
        };

        const Self = @This();

        pub fn deinit(self: *Self, alloc: Allocator) void {
            self.list.deinit(alloc);
            self.* = undefined;
        }

        pub fn currRef(self: Self) Ref {
            return .{ .raw = @intCast(IndexType, self.list.items.len) };
        }

        pub fn append(self: *Self, alloc: Allocator, item: T) !Ref {
            const ref = self.currRef();
            try self.list.append(alloc, item);
            return ref;
        }

        pub fn appendRange(self: *Self, alloc: Allocator, items: []T) !Ref.Range {
            const start = self.currRef();
            try self.list.appendSlice(alloc, items);
            const end = self.currRef();
            return Ref.Range{ .start = start, .end = end };
        }

        pub fn get(self: Self, ref: Ref) *const T {
            return &self.list.items[@intCast(usize, ref.raw)];
        }

        pub fn getMut(self: Self, ref: Ref) *T {
            return &self.list.items[@intCast(usize, ref.raw)];
        }

        pub fn getRange(self: Self, range: Ref.Range) []const T {
            return self.list.items[range.start.raw..range.end.raw];
        }

        pub fn getRangeMut(self: Self, range: Ref.Range) []T {
            return self.list.items[range.start.raw..range.end.raw];
        }

        pub fn shrinkToFit(self: Self, alloc: Allocator) void {
            self.list.shrinkAndFree(alloc, self.list.items.len);
        }
    };
}

pub fn InterningIndexList(comptime T: type, comptime IndexType: type) type {
    const eql = if (@hasDecl(T, "eql"))
        T.eql
    else
        struct {
            fn eql(a: T, b: T) bool {
                return std.meta.eql(a, b);
            }
        }.eql;

    return struct {
        list: IndexList(T, IndexType) = .{},

        pub fn append(self: *Self, alloc: Allocator, item: T) !Ref {
            for (self.list.list.items) |it, i| {
                if (eql(it, item)) {
                    return .{ .raw = @intCast(IndexType, i) };
                }
            }

            return self.list.append(item);
        }

        pub fn get(self: Self, ref: Ref) *const T {
            return self.list.get(ref);
        }

        pub fn getMut(self: Self, ref: Ref) *T {
            return self.list.getMut(ref);
        }

        pub fn shrinkToFit(self: Self, alloc: Allocator) void {
            self.list.shrinkToFit(alloc);
        }
    };
}

pub const Strings = struct {
    formatted: std.ArrayListUnmanaged(u8) = .{},
    static: std.ArrayListUnmanaged([*]const u8) = .{},

    pub const Ref = packed struct {
        index: u16,
        len: u15,
        static: bool,
    };

    pub fn deinit(self: *Strings, alloc: Allocator) void {
        self.formatted.deinit(alloc);
        self.static.deinit(alloc);
        self.* = undefined;
    }

    pub fn get(self: Strings, ref: Ref) []const u8 {
        return if (ref.static)
            self.static.items[ref.index][0..ref.len]
        else
            self.formatted.items[ref.index .. ref.index + ref.len];
    }

    pub fn staticString(self: *Strings, alloc: Allocator, str: []const u8) !Ref {
        const index = @intCast(u16, self.static.items.len);
        try self.static.append(alloc, str.ptr);

        const len = str.len;
        if (len > std.math.maxInt(u15))
            bug("string to large", .{});

        return Ref{
            .index = index,
            .len = @intCast(u15, len),
            .static = true,
        };
    }

    pub fn formatString(
        self: *Strings,
        alloc: Allocator,
        comptime format: []const u8,
        args: anytype,
    ) !Ref {
        const start = self.formatted.items.len;
        try self.formatted.writer(alloc).print(format, args);
        const end = self.formatted.items.len;

        const index = @intCast(u16, start);
        const len = end - start;

        if (len > std.math.maxInt(u15))
            bug("string to large", .{});

        return Ref{
            .index = index,
            .len = @intCast(u15, len),
            .static = false,
        };
    }

    pub fn shrinkToFit(self: *Strings, alloc: Allocator) void {
        self.formatted.shrinkAndFree(alloc, self.formatted.items.len);
        self.static.shrinkAndFree(alloc, self.static.items.len);
    }
};
