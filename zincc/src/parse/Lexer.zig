allocator: Allocator,

src: []const u8,
iter: std.unicode.Utf8Iterator,

span: SpanData = .{ .start = 0, .end = 0 },
whitespace_len: u16 = 0,

tokens: std.MultiArrayList(Token) = .{},
spans: std.ArrayListUnmanaged(SpanData) = .{},

//

const std = @import("std");
const Allocator = std.mem.Allocator;

const super = @import("!mod.zig");
const Token = super.Token;
const SpanData = super.SpanData;

const Self = @This();
const Rune = u21;

const term = @import("../term.zig");

//

pub const LexResult = struct {
    spans: []const SpanData,
    tokens: std.MultiArrayList(Token).Slice,

    pub fn deinit(self: *LexResult, allocator: Allocator) void {
        allocator.free(self.spans);
        self.tokens.deinit(allocator);
    }
};

pub fn lex(allocator: Allocator, src: []const u8) LexResult {
    var lexer = Self.init(allocator, src);
    lexer.performLex();
    return .{
        .spans = lexer.spans.toOwnedSlice(allocator),
        .tokens = lexer.tokens.toOwnedSlice(),
    };
}

//

fn init(allocator: Allocator, src: []const u8) Self {
    const iter = std.unicode.Utf8Iterator{ .bytes = src, .i = 0 };

    return .{
        .allocator = allocator,
        .src = src,
        .iter = iter,
    };
}

fn performLex(self: *Self) void {
    while (self.next()) {}
    self.tok(.EOF);
}

fn next(self: *Self) bool {
    const char = self.advance();

    switch (char) {
        ' ', '\n', '\t', '\r' => {
            self.whitespace_len += 1;

            while (switch (self.peek()) {
                ' ', '\n', '\t', '\r' => true,
                else => false,
            }) {
                std.debug.assert(self.advance() != 0);
                self.whitespace_len += 1;
            }
        },

        '(' => self.tok(.brkt_paren_open),
        ')' => self.tok(.brkt_paren_close),
        '{' => self.tok(.brkt_brace_open),
        '}' => self.tok(.brkt_brace_close),
        '[' => self.tok(.brkt_square_open),
        ']' => self.tok(.brkt_square_close),

        ':' => self.tok(if (self.match(':')) .punct_dblColon else .punct_colon),
        ';' => self.tok(.punct_semiColon),
        ',' => self.tok(.punct_comma),

        // '=' => self.tok(.punct_eq),
        '=' => self.tok(if (self.match('>')) .punct_fat_arrow else .punct_eq),
        '+' => self.tok(.punct_plus),
        '-' => self.tok(.punct_minus),
        '*' => self.tok(.punct_star),

        '/' => if (self.match('/')) {
            self.whitespace_len += 2; // '//'
            while (switch (self.peek()) { // while not at end or newline
                '\n', 0 => false,
                else => true,
            }) {
                std.debug.assert(self.advance() != 0);
                self.whitespace_len += 1;
            }
        } else {
            self.tok(.punct_slash);
        },

        0 => return false,

        else => {
            // number literals
            if (char == '0') {
                switch (self.peek()) {
                    'x' => { // 0x..
                        self.lexBasicNumberGeneric(.int_hex, isCharNumberHex);
                        return true;
                    },
                    'o' => { // 0o..
                        self.lexBasicNumberGeneric(.int_oct, isCharNumberOctal);
                        return true;
                    },
                    'b' => { // 0b..
                        self.lexBasicNumberGeneric(.int_bin, isCharNumberBinary);
                        return true;
                    },
                    else => {
                        // continue with other checks below
                    },
                }
            }

            if (isCharNumberStart(char)) {
                self.lexNumber();
            } else if (isCharIdentStart(char)) {
                self.lexIdent();
            } else {
                @panic("todo: report error; unknown char");
            }
        },
    }

    return true;
}

fn tok(self: *Self, kind: Token.Kind) void {
    const token = Token{ .kind = kind, .preceding_ws = self.whitespace_len };
    const span = SpanData{ .start = self.span.start + self.whitespace_len, .end = self.span.end };

    self.tokens.append(self.allocator, token) catch unreachable;
    self.spans.append(self.allocator, span) catch unreachable;

    self.whitespace_len = 0;
    self.span.start = self.span.end;
}

fn advance(self: *Self) Rune {
    const slice = self.iter.nextCodepointSlice() orelse return 0;
    self.span.end += @intCast(u32, slice.len);
    return std.unicode.utf8Decode(slice) catch unreachable;
}

// fn advanceAssume(self: *Self) void {
//     _ = self.advance() orelse unreachable;
// }

/// This should not be called *anywhere* except peek() and peekNext()
fn peekRaw(self: *Self, n: usize) Rune {
    // const slice = self.iter.peek(n);
    self.iter.i += n - 1;
    const slice = self.iter.nextCodepointSlice() orelse return 0;
    self.iter.i -= n;
    if (slice.len == 0)
        return 0;
    return std.unicode.utf8Decode(slice) catch unreachable;
}

fn peek(self: *Self) Rune {
    return self.peekRaw(1);
}

fn peekNext(self: *Self) Rune {
    return self.peekRaw(2);
}

fn at(self: *Self, ch: Rune) bool {
    return self.peek() == ch;
}

fn match(self: *Self, expected: Rune) bool {
    if (self.at(expected)) {
        std.debug.assert(self.advance() != 0);
        return true;
    }
    return false;
}

// fn consume(self: *Self) Rune {
//     if (self.advance()) |ch| {
//         return ch;
//     } else {
//         // self.reportHere(.UnexpectedEof);
//         // return 0;
//         @panic("todo: report error; at end in lexer consume()");
//     }
// }

fn lexNumber(self: *Self) void {
    var kind = Token.Kind.int_dec;

    self.lexWhileCondition(isCharNumberMid);

    if (self.peek() == '.' and isCharNumberMid(self.peekNext())) {
        kind = .float;
        std.debug.assert(self.advance() != 0);
        self.lexWhileCondition(isCharNumberMid);
        if (self.match('E')) {
            switch (self.peek()) {
                '+', '-' => std.debug.assert(self.advance() != 0),
                // else => self.reportAtCurrentChar(.ExpectedSignInFloatExponent),
                else => @panic("todo: report error; in lex float exponent"),
            }
            self.lexWhileCondition(isCharNumberMid);
        }
    }

    self.tok(kind);
    // self.lexLiteralSuffix();
}

fn lexBasicNumberGeneric(self: *Self, kind: Token.Kind, condition: fn (Rune) callconv(.Inline) bool) void {
    std.debug.assert(self.advance() != 0);
    self.lexWhileCondition(condition);
    self.tok(kind);
    // self.lexLiteralSuffix();
}

fn lexString(self: *Self) void {
    // const start = self.span.start;

    self.tok(.string_open);

    var p = self.peek();
    while (p != '\"' and p != 0) : (p = self.peek()) {
        if (p == '\\') {
            if (self.span.start != self.span.end)
                self.tok(.string_literal);

            _ = self.advance() orelse unreachable;
            self.lexEscape();
        } else {
            _ = self.advance() orelse unreachable;
        }
    }

    if (self.span.start != self.span.end)
        self.tok(.string_literal);

    if (!self.match('\"'))
        // self.report(.{ .kind = .UnterminatedString, .span = .{ .start = start, .end = self.span.end } });
        @panic("todo: report error; did not terminate string");

    self.tok(.string_close);
}

fn lexEscape(self: *Self) void {
    const kind: Token.Kind = switch (self.advance().?) {
        // '\'' => .esc_quote_single,
        // '\"' => .esc_quote_double,

        // 'n' => .esc_ascii_newline,
        // 'r' => .esc_ascii_carriageReturn,
        // 't' => .esc_ascii_tab,
        // '\\' => .esc_ascii_backslash,
        // '0' => .esc_ascii_null,
        // 'e' => .esc_ascii_escape,
        'x' => blk: {
            self.lexAsciicodeEscapeChar();
            self.lexAsciicodeEscapeChar();
            break :blk .esc_asciicode;
        },

        // 'u' => blk: {
        //     // self.lexEscapeUnicode() catch {};
        //     // break :blk .Esc_Unicode;
        // },
        'u' => @panic("todo: report error; lex unicode escape"),
        // else => {
        //     // self.reportAtLastChar(.UnexpectedEscape);
        //     // return;
        //     @panic("todo: report error");
        // },
        else => .esc_char,
    };
    self.tok(kind);
}

fn lexAsciicodeEscapeChar(self: *Self) void {
    const ch = self.advance().?;
    if (!isCharNumberHex(ch))
        // self.reportAtLastChar(.ExpectedHexDigit);
        @panic("todo: report error; not hex char in ascii escape");
}

fn lexWhileCondition(self: *Self, condition: fn (Rune) callconv(.Inline) bool) void {
    while (condition(self.peek()))
        std.debug.assert(self.advance() != 0);
}

fn lexIdent(self: *Self) void {
    self.lexWhileCondition(isCharIdentMid);

    const slice = self.src;
    const start = self.whitespace_len + self.span.start;

    const kind: Token.Kind = switch (slice[start]) {
        // // and
        // 'a' => self.checkKw(1, "nd", .Kw_and),

        // const
        'c' => self.checkKw(1, "onst", .kw_const),

        // fn
        'f' => switch (slice[start + 1]) {
            'n' => self.checkKw(2, "", .kw_fn), // @Speed
            else => .ident,
        },

        // let
        'l' => self.checkKw(1, "et", .kw_let),

        // // mut
        // 'm' => self.checkKw(1, "ut", .Kw_mut),

        // // not
        // 'n' => self.checkKw(1, "ot", .Kw_not),

        // // or
        // 'o' => self.checkKw(1, "r", .Kw_or),

        // return
        'r' => self.checkKw(1, "eturn", .kw_return),

        // // use
        // 'u' => self.checkKw(1, "se", .kw_use),

        else => .ident,
    };
    self.tok(kind);
}

fn checkKw(self: *Self, start: usize, rest: []const u8, kind: Token.Kind) Token.Kind {
    const lex_start = self.whitespace_len + self.span.start;
    const len = rest.len;
    const str = self.src[lex_start + start .. lex_start + start + len];
    return if (self.span.end - lex_start == start + len and std.mem.eql(u8, rest, str))
        kind
    else
        .ident;
}

//

inline fn isCharAlpha(ch: Rune) bool {
    return (ch >= 'a' and ch <= 'z') or (ch >= 'A' and ch <= 'Z');
}
inline fn isCharNumeric(ch: Rune) bool {
    return (ch >= '0' and ch <= '9');
}
inline fn isCharAlphanumeric(ch: Rune) bool {
    return isCharAlpha(ch) or isCharNumeric(ch);
}

inline fn isCharIdentStart(ch: Rune) bool {
    return isCharAlpha(ch) or ch == '_';
}

inline fn isCharIdentMid(ch: Rune) bool {
    return isCharAlphanumeric(ch) or ch == '_';
}

inline fn isCharNumberStart(ch: Rune) bool {
    return isCharNumeric(ch);
}

inline fn isCharNumberMid(ch: Rune) bool {
    return isCharNumeric(ch) or ch == '_';
}

inline fn isCharNumberHex(ch: Rune) bool {
    return (ch >= 'a' and ch <= 'f') or (ch >= 'A' and ch <= 'F') or isCharNumeric(ch);
}

inline fn isCharNumberBinary(ch: Rune) bool {
    return switch (ch) {
        '0', '1', '_' => true,
        else => false,
    };
}

inline fn isCharNumberOctal(ch: Rune) bool {
    return switch (ch) {
        '0'...'7', '_' => true,
        else => false,
    };
}
