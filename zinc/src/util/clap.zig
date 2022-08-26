const std = @import("std");
const Allocator = std.mem.Allocator;

pub const Rune = u21;
pub const str = []const u8;

// @TODO: Support '--foo=bar' syntax

pub const parse = @import("clap_parse.zig");

pub const Command = struct {
    name: str,
    version: ?std.SemanticVersion = null,

    short_help: str = "",
    long_help: str = "",

    args: []const Arg = &.{},

    sub_commands: []const Command = &.{},

    pub fn generateLongHelp(self: Command, alloc: Allocator) !str {
        var list = std.ArrayList(u8).init(alloc);
        errdefer list.deinit();
        var out = list.writer();

        try out.writeAll(self.name);
        if (self.version) |ver| {
            try out.print(" {}", .{ver});
        }

        try out.writeByte('\n');
        try out.writeAll(self.long_help);

        try out.writeAll("\n\nUsage:\n");

        try out.writeByteNTimes(' ', 4);
        try out.writeAll(self.name);
        try out.writeAll(" [OPTIONS]");

        for (self.args) |arg| {
            if (arg.isPositional()) {
                try out.print(" {s}", .{arg.name});
            }
        }

        if (self.sub_commands.len != 0) {
            try out.writeAll(" [SUBCOMMAND]");
        }

        try out.writeAll("\n\nOPTIONS:\n");

        for (self.args) |arg| {
            const s = try arg.generateHelp(alloc);
            defer alloc.free(s);
            try out.writeByteNTimes(' ', 4);
            try out.writeAll(s);
            try out.writeByte('\n');
        }

        if (self.sub_commands.len != 0) {
            try out.writeAll("\nSUBCOMMANDS:\n");

            for (self.sub_commands) |cmd| {
                const s = try cmd.generateShortHelp(alloc);
                defer alloc.free(s);
                try out.writeByteNTimes(' ', 4);
                try out.writeAll(s);
                try out.writeByte('\n');
            }
        }

        return list.toOwnedSlice();
    }

    pub fn generateShortHelp(self: Command, alloc: Allocator) !str {
        var list = std.ArrayList(u8).init(alloc);
        errdefer list.deinit();
        var out = list.writer();

        try out.writeAll(self.name);
        try out.writeByte(' ');
        try out.writeAll(self.short_help);

        if (self.short_help.len != 0) {
            const space_diff = 25 - list.items.len;
            try out.writeByteNTimes(' ', space_diff);

            try out.writeAll(self.short_help);
        }

        return list.toOwnedSlice();
    }

    pub fn getMatches(self: Command, alloc: Allocator, argv: []const []const u8) !parse.Matches {
        const tokens = try parse.lex(alloc, argv);
        defer alloc.free(tokens);
        return parse.resolve(alloc, argv, self, tokens);
    }

    pub fn debug(self: Command, alloc: Allocator, argv: []const []const u8) !parse.Matches {
        const term = @import("../term.zig");

        const tokens = try parse.lex(alloc, argv);
        defer alloc.free(tokens);

        for (tokens) |token| {
            term.eprint("{?}", .{token});
            const slice = argv[token.arg_index][token.start..token.end];
            term.eprintln(" -> '{s}'", .{slice});
        }

        const matches = try parse.resolve(alloc, argv, self, tokens);
        // defer matches.deinit(alloc);

        term.eprintln("\n", .{});
        term.eprintln("len: {}", .{matches.matches.len});

        for (matches.matches) |match| {
            term.eprintln("name: '{s}', value: '{s}'", .{ match.arg.name, if (match.value) |v| v else "null" });
        }

        return matches;
    }
};

pub const Arg = struct {
    name: str,

    short: ?Rune = null,
    long: ?str = null,

    // append: bool = false,
    // required: bool = false,

    takes_value: bool = false,
    value_name: ?str = null,
    // value_hint: ValueHint = .default,
    // value_parser:

    short_help: str = "",
    // long_help: str = "",

    // pub const ValueHint = enum {
    //     default,
    //     none,
    //     any_path,
    //     file_path,
    //     dir_path,
    //     executable_path,
    //     command_name,
    //     command_string,
    //     // command_with_arguments,
    //     username,
    //     hostname,
    //     url,
    //     email_address,
    // };

    pub inline fn isPositional(self: Arg) bool {
        return self.short == null and self.long == null;
    }

    pub fn generateHelp(self: Arg, alloc: Allocator) !str {
        var list = std.ArrayList(u8).init(alloc);
        errdefer list.deinit();
        var out = list.writer();

        if (self.isPositional()) {
            try out.writeAll(self.name);
        } else {
            const short = if (self.short) |rune| blk: {
                var bytes: [4]u8 = undefined;
                const len = try std.unicode.utf8Encode(rune, &bytes);
                break :blk bytes[0..len];
            } else null;

            if (short != null and self.long != null) {
                try out.print("-{s}, --{s}", .{ short.?, self.long.? });
            } else if (short != null) { // self.long == null
                try out.print("-{s}", .{short.?});
            } else { // self.long != null
                try out.print("    --{s}", .{self.long.?});
            }
        }

        if (self.takes_value) {
            if (self.value_name) |name| {
                try out.print(" <{s}>", .{name});
            }
        }

        if (self.short_help.len != 0) {
            const space_diff = 25 - list.items.len;
            try out.writeByteNTimes(' ', space_diff);

            try out.writeAll(self.short_help);
        }

        return list.toOwnedSlice();
    }
};
