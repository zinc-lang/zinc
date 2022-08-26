const std = @import("std");
const Allocator = std.mem.Allocator;

const glue = @import("glue.zig");
const Token = glue.Token;

const report = @import("../report.zig");

const util = @import("../util/util.zig");

const SourceMap = @import("../SourceMap.zig");

const Self = @This();

alloc: Allocator,
source: [:0]const u8,
file_ref: SourceMap.SourceFiles.Ref,

start: u32 = 0,
end: u32 = 0,

tokens: Token.List = .{},
ctxkws: Token.Kind.CtxKws = .{},

strings: util.Strings = .{},
reps: std.ArrayListUnmanaged(report.Report) = .{},

line_offsets: std.ArrayListUnmanaged(u32) = .{},

pub const LexerOutput = struct {
    strings: util.Strings,
    reps: []const report.Report,

    pub fn deinit(self: *LexerOutput, alloc: Allocator) void {
        self.strings.deinit(alloc);
        alloc.free(self.reps);

        self.* = undefined;
    }
};

pub fn lexFile(
    alloc: Allocator,
    source_map: SourceMap,
    file_ref: SourceMap.SourceFiles.Ref,
) !LexerOutput {
    const file = source_map.files.getMut(file_ref);

    var lexer = Self{
        .alloc = alloc,
        .source = file.src,
        .file_ref = file_ref,
    };

    try lexer.doLex();

    file.line_offsets = lexer.line_offsets.toOwnedSlice(alloc);
    file.tokens = lexer.tokens.slice();
    file.ctxkws = lexer.ctxkws;
    file.flags.lexed = true;

    lexer.strings.shrinkToFit(alloc);
    return LexerOutput{
        .strings = lexer.strings,
        .reps = lexer.reps.toOwnedSlice(alloc),
    };
}

fn addReport(self: *Self, rep: struct {
    level: report.Report.Level,
    message: util.Strings.Ref,
    primary_range: ?glue.Token.SourceLocalRange = null,
}) !void {
    try self.reps.append(self.alloc, .{
        .level = rep.level,
        .message = rep.message,
        .primary_range = if (rep.primary_range) |r| r else self.getRange(),
        .file_ref = self.file_ref,
    });
}

fn doLex(self: *Self) !void {
    try self.line_offsets.append(self.alloc, 0);

    lex_loop: while (true) {
        const ch = self.advance();

        switch (ch) {
            '\n' => {
                try self.tok(.tr_newline);
                try self.line_offsets.append(self.alloc, self.start);
            },
            ' ', '\r' => {
                while (switch (self.peek()) {
                    ' ', '\r' => true,
                    else => false,
                }) {
                    _ = self.advance();
                }
                try self.tok(.tr_whitespace);
            },

            '\t' => {
                const message = try self.strings.staticString(
                    self.alloc,
                    "tabs are not permitted",
                );
                try self.addReport(.{ .level = .error_, .message = message });

                try self.tok(.err);
            },

            '(' => try self.tok(.brkt_paren_open),
            ')' => try self.tok(.brkt_paren_close),
            '{' => try self.tok(.brkt_brace_open),
            '}' => try self.tok(.brkt_brace_close),
            '[' => try self.tok(.brkt_square_open),
            ']' => try self.tok(.brkt_square_close),

            '&' => try self.tok(.punct_ampersand),
            '!' => try self.ifMatch('=', .punct_notEq, .punct_exclamation),
            ':' => try self.ifMatch(':', .punct_doubleColon, .punct_colon),
            ';' => try self.tok(.punct_semiColon),
            ',' => try self.tok(.punct_comma),
            '.' => try if (self.eat('.'))
                if (self.eat('.'))
                    self.tok(.punct_tripleDot)
                else
                    self.tok(.punct_doubleDot)
            else
                self.tok(.punct_dot),
            '=' => try if (self.eat('='))
                self.tok(.punct_eqEq)
            else if (self.eat('>'))
                self.tok(.punct_fatArrow)
            else
                self.tok(.punct_eq),
            '>' => try self.ifMatch('=', .punct_greaterThanEq, .punct_greaterThan),
            '<' => try self.ifMatch('=', .punct_lessThanEq, .punct_lessThan),
            '-' => try self.ifMatch('>', .punct_rThinArrow, .punct_minus),
            '|' => try self.tok(.punct_pipe),
            '+' => try self.tok(.punct_plus),
            '?' => try self.tok(.punct_question),
            '*' => try self.tok(.punct_star),

            '/' => {
                if (self.eat('/')) {
                    var p = self.peek();
                    while (p != '\n' and p != 0) : (p = self.peek()) {
                        _ = self.advance();
                    }
                    try self.tok(.tr_comment);
                } else {
                    try self.tok(.punct_slash);
                }
            },

            '"' => try self.lexString(),

            '0' => {
                switch (self.peek()) {
                    'x' => try self.lexSpNumber(.int_hex, is.hex),
                    'b' => try self.lexSpNumber(.int_bin, is.binary),
                    'o' => try self.lexSpNumber(.int_oct, is.octal),
                    else => try self.lexNumber(),
                }
            },

            0 => break :lex_loop,

            else => {
                if (is.identStart(ch)) {
                    try self.lexIdent();
                } else if (is.numberStart(ch)) {
                    try self.lexNumber();
                } else {
                    const message = try self.strings.formatString(
                        self.alloc,
                        "unknown character `{c}`",
                        .{ch},
                    );
                    try self.addReport(.{ .level = .error_, .message = message });

                    try self.tok(.err);
                }
            },
        }
    }

    self.start -= 1;
    self.end -= 1;
    try self.tok(.eof);
}

fn getRange(self: Self) Token.SourceLocalRange {
    return .{
        .start = self.start,
        .end = self.end,
    };
}

fn tok(self: *Self, kind: Token.Kind) !void {
    const range = self.getRange();
    self.start = self.end;

    try self.tokens.append(self.alloc, Token{
        .kind = kind,
        .range = range,
    });
}

inline fn advance(self: *Self) u8 {
    self.end += 1;
    return self.source[self.end - 1];
}

fn advanceNoEof(self: *Self) !void {
    const ch = self.advance();
    if (ch == 0) {
        const message = try self.strings.staticString(
            self.alloc,
            "unexpected end of file",
        );
        try self.addReport(.{ .level = .error_, .message = message });
    }
}

inline fn peek(self: Self) u8 {
    return self.source[self.end];
}

inline fn at(self: Self, ch: u8) bool {
    return self.peek() == ch;
}

inline fn eat(self: *Self, ch: u8) bool {
    const cond = self.at(ch);
    if (cond) {
        _ = self.advance();
    }
    return cond;
}

inline fn ifMatch(
    self: *Self,
    ch: u8,
    yes: Token.Kind,
    no: Token.Kind,
) !void {
    if (self.eat(ch)) {
        try self.tok(yes);
    } else {
        try self.tok(no);
    }
}

fn lexString(self: *Self) !void {
    const start_offset = self.end - 1;
    try self.tok(.string_open);

    var p = self.peek();
    while (p != '\"' and p != 0 and p != '\n') : (p = self.peek()) {
        if (p == '\\') {
            if (self.start != self.end)
                try self.tok(.string_literal);

            try self.advanceNoEof();
            try self.lexEscape();
        } else {
            try self.advanceNoEof();
        }
    }

    if (self.start != self.end)
        try self.tok(.string_literal);

    if (!self.eat('\"')) {
        const message = try self.strings.staticString(
            self.alloc,
            "unterminated string literal",
        );
        try self.addReport(.{
            .level = .error_,
            .message = message,
            .primary_range = .{ .start = start_offset, .end = self.end },
        });
    }

    try self.tok(.string_close);
}

fn lexEscape(self: *Self) !void {
    const kind: Token.Kind = switch (self.advance()) {
        'n' => .esc_char_newline,
        'r' => .esc_char_return,
        't' => .esc_char_tab,
        '\\' => .esc_char_backslash,
        '\"' => .esc_char_quote_double,
        '\'' => .esc_char_quote_single,
        'x' => blk: {
            const message = try self.strings.staticString(
                self.alloc,
                "TODO: ascii char escape",
            );
            try self.addReport(.{ .level = .unimpl, .message = message });

            break :blk .err;
        },
        'u' => blk: {
            const message = try self.strings.staticString(
                self.alloc,
                "TODO: unicode char escape",
            );
            try self.addReport(.{ .level = .unimpl, .message = message });

            break :blk .err;
        },
        else => .esc_char_other,
    };
    try self.tok(kind);
}

fn lexWhile(self: *Self, cond: fn (ch: u8) callconv(.Inline) bool) void {
    while (cond(self.peek())) {
        _ = self.advance();
    }
}

fn lexNumber(self: *Self) !void {
    self.lexWhile(is.numberMid);

    var kind = Token.Kind.int_dec;

    if (self.eat('.') and is.numberStart(self.peek())) {
        kind = .float;
        self.lexWhile(is.numberMid);

        if (self.eat('E')) {
            switch (self.advance()) {
                '+', '-' => {},
                else => {
                    const message = try self.strings.staticString(
                        self.alloc,
                        "expected '+' or '-' after 'E' within float literal",
                    );
                    try self.addReport(.{
                        .level = .error_,
                        .message = message,
                        .primary_range = .{ .start = self.end - 1, .end = self.end },
                    });
                },
            }

            self.lexWhile(is.numberMid);
        }
    }

    try self.tok(kind);
}

fn lexSpNumber(self: *Self, kind: Token.Kind, cond: fn (u8) callconv(.Inline) bool) !void {
    _ = self.advance();
    self.lexWhile(cond);
    try self.tok(kind);
}

fn lexIdent(self: *Self) !void {
    self.lexWhile(is.identMid);

    const slice = self.source[self.start..self.end];

    if (Token.Kind.CtxKwsMap.get(slice)) |ctx_kw| {
        try self.ctxkws.put(self.alloc, @intCast(u32, self.tokens.len), ctx_kw);
        try self.tok(.ident);
    } else if (Token.Kind.Keywords.get(slice)) |tk|
        try self.tok(tk)
    else
        try self.tok(.ident);
}

const is = struct {
    inline fn numberStart(ch: u8) bool {
        return switch (ch) {
            '0'...'9' => true,
            else => false,
        };
    }

    inline fn numberMid(ch: u8) bool {
        return switch (ch) {
            '0'...'9', '_' => true,
            else => false,
        };
    }

    inline fn hex(ch: u8) bool {
        return switch (ch) {
            '0'...'9', 'A'...'F', 'a'...'f', '_' => true,
            else => false,
        };
    }

    inline fn binary(ch: u8) bool {
        return switch (ch) {
            '0', '1', '_' => true,
            else => false,
        };
    }

    inline fn octal(ch: u8) bool {
        return switch (ch) {
            '0'...'7', '_' => true,
            else => false,
        };
    }

    inline fn identStart(ch: u8) bool {
        return switch (ch) {
            'a'...'z', 'A'...'Z', '_' => true,
            else => false,
        };
    }

    inline fn identMid(ch: u8) bool {
        return switch (ch) {
            'a'...'z', 'A'...'Z', '0'...'9', '_' => true,
            else => false,
        };
    }
};
