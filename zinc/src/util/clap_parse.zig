const std = @import("std");
const Allocator = std.mem.Allocator;

const clap = @import("clap.zig");

pub const Rune = clap.Rune;
pub const str = clap.str;

pub const TokenKind = enum {
    /// '-{char}'
    short_tag,

    /// '--{str}'
    long_tag,

    /// '{str}'
    text,

    /// '--'
    separator,

    /// every arg after a '--'
    rest,

    end,
};

pub const Token = struct {
    kind: TokenKind,
    arg_index: u16,
    start: u16,
    end: u16,
};

pub fn lex(alloc: Allocator, argv: []const []const u8) ![]Token {
    var lexer = Lexer{ .alloc = alloc, .argv = argv };
    try lexer.doWork();
    return lexer.tokens.toOwnedSlice(alloc);
}

const Lexer = struct {
    alloc: Allocator,
    argv: []const []const u8,

    tokens: std.ArrayListUnmanaged(Token) = .{},

    fn doWork(self: *Lexer) !void {
        for (self.argv) |arg, arg_index_| {
            const arg_index = @intCast(u16, arg_index_);
            var cursor: usize = 0;

            if (arg[cursor] == '-') {
                cursor += 1;
                if (arg[cursor] == '-') {
                    if (arg.len == 2) {
                        // '--'
                        try self.tokens.append(self.alloc, .{
                            .kind = .separator,
                            .arg_index = arg_index,
                            .start = 0,
                            .end = 2,
                        });
                        // rest
                        const prefix = arg_index + 1;
                        for (self.argv[prefix..]) |slice, index| {
                            try self.tokens.append(self.alloc, .{
                                .kind = .rest,
                                .arg_index = prefix + @intCast(u16, index),
                                .start = 0,
                                .end = @intCast(u16, slice.len),
                            });
                        }
                        return;
                    } else {
                        // '--{str}'
                        try self.tokens.append(self.alloc, .{
                            .kind = .long_tag,
                            .arg_index = arg_index,
                            .start = 2,
                            .end = @intCast(u16, arg.len),
                        });
                    }
                } else {
                    // '-{char}'
                    try self.tokens.append(self.alloc, .{
                        .kind = .short_tag,
                        .arg_index = arg_index,
                        .start = 1,
                        .end = 2,
                    });
                    if (arg.len > 2) {
                        // '-{char}{str}'
                        try self.tokens.append(self.alloc, .{
                            .kind = .text,
                            .arg_index = arg_index,
                            .start = 2,
                            .end = @intCast(u16, arg.len),
                        });
                    }
                }
            } else {
                // '{str}'
                try self.tokens.append(self.alloc, .{
                    .kind = .text,
                    .arg_index = arg_index,
                    .start = 0,
                    .end = @intCast(u16, arg.len),
                });
            }
        }
    }
};

pub fn resolve(
    alloc: Allocator,
    argv: []const []const u8,
    command: clap.Command,
    tokens: []Token,
) !Matches {
    var resolver = Resolver{
        .alloc = alloc,
        .argv = argv,
        .command = command,
        .tokens = tokens,
    };
    try resolver.doWork();
    return Matches{
        .matches = resolver.out.matches.toOwnedSlice(alloc),
    };
}

pub const Resolver = struct {
    alloc: Allocator,
    argv: []const []const u8,
    command: clap.Command,

    tokens: []Token,
    cursor: u32 = 1, // skip the command name

    positionals_seen: u32 = 0,

    out: struct {
        matches: std.ArrayListUnmanaged(Matches.Match) = .{},
    } = .{},

    pub fn doWork(self: *Resolver) !void {
        while (self.cursor < self.tokens.len) {
            switch (self.peek()) {
                .short_tag => try self.resolveShort(),
                .long_tag => try self.resolveLong(),
                .text => {
                    const s = self.getSlice(0);
                    self.cursor += 1;

                    var positions_to_see: u32 = 0;
                    var maybe_arg: ?*const clap.Arg = for (self.command.args) |it, i| {
                        if (it.isPositional()) {
                            if (positions_to_see != self.positionals_seen) {
                                positions_to_see += 1;
                                continue;
                            }

                            self.positionals_seen += 1;
                            break &self.command.args[i];
                        }
                    } else null;

                    if (maybe_arg == null) {
                        // @TODO: error, unexpected positional
                        unreachable;
                    }

                    const match = Matches.Match{
                        .arg = maybe_arg.?,
                        .value = s,
                    };
                    try self.out.matches.append(self.alloc, match);
                },
                .separator => unreachable,
                .rest => unreachable,
                .end => break,
            }
        }
    }

    fn resolveShort(self: *Resolver) !void {
        std.debug.assert(self.at(.short_tag));

        const slice = self.getSlice(0);
        self.cursor += 1;
        std.debug.assert(slice.len == 1);
        const arg_char = slice[0];

        // find arg
        const maybe_arg: ?*const clap.Arg = for (self.command.args) |it, i| {
            if (it.short) |ch|
                if (ch == arg_char)
                    break &self.command.args[i];
        } else null;

        if (maybe_arg == null) {
            // @TODO: error, no matching short arg
            unreachable;
        }

        var match = Matches.Match{
            .arg = maybe_arg.?,
            .value = null,
        };

        if (match.arg.takes_value) {
            match.value = self.tryGetText();
        }

        try self.out.matches.append(self.alloc, match);
    }

    fn resolveLong(self: *Resolver) !void {
        std.debug.assert(self.at(.long_tag));

        const arg_slice = self.getSlice(0);
        self.cursor += 1;

        // find arg
        const maybe_arg: ?*const clap.Arg = for (self.command.args) |it, i| {
            if (it.long) |long|
                if (std.mem.eql(u8, long, arg_slice))
                    break &self.command.args[i];
        } else null;

        if (maybe_arg == null) {
            // @TODO: error, no matching long arg
            unreachable;
        }

        var match = Matches.Match{
            .arg = maybe_arg.?,
            .value = null,
        };

        if (match.arg.takes_value) {
            match.value = self.tryGetText();
        }

        try self.out.matches.append(self.alloc, match);
    }

    fn tryGetText(self: *Resolver) []const u8 {
        if (self.at(.text)) {
            self.cursor += 1;
            return self.getSlice(-1);
        } else {
            // @TODO: error, no value when expecting value
            unreachable;
        }
    }

    fn peek(self: Resolver) TokenKind {
        return if (self.cursor >= self.tokens.len)
            .end
        else
            self.tokens[self.cursor].kind;
    }

    fn at(self: Resolver, tk: TokenKind) bool {
        return self.peek() == tk;
    }

    fn getSlice(self: Resolver, offset: i8) str {
        const token = self.tokens[@intCast(usize, @intCast(isize, self.cursor) + offset)];
        return self.argv[token.arg_index][token.start..token.end];
    }
};

pub const Matches = struct {
    matches: []Match = &.{},

    pub fn deinit(self: *Matches, alloc: Allocator) void {
        alloc.free(self.matches);
    }

    pub fn has(self: Matches, name: str) bool {
        return self.getMatch(name) != null;
    }

    pub fn getMatch(self: Matches, name: str) ?Match {
        return for (self.matches) |it| {
            if (it.is(name))
                break it;
        } else null;
    }

    pub fn valueOf(self: Matches, name: str) ?str {
        const match = self.getMatch(name) orelse return null;
        return match.value;
    }

    pub const Match = struct {
        arg: *const clap.Arg,
        value: ?str, // slice to argv

        pub fn is(self: Match, name: str) bool {
            return std.mem.eql(u8, self.arg.name, name);
        }
    };
};
