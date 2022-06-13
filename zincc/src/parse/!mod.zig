pub const Lexer = @import("Lexer.zig");
pub const Cst = @import("Cst.zig");
pub const Parser = @import("Parser.zig");

pub const SpanData = struct {
    start: u32,
    end: u32,
};

pub const Token = struct {
    kind: Kind,
    // @TODO...
    // Figure out how big this tends to be and optimize as such,
    // this is almost definitely overkill atm.
    preceding_ws: u16,

    pub const Kind = enum {
        EOF,

        brkt_paren_open, // (
        brkt_paren_close, // )
        brkt_brace_open, // {
        brkt_brace_close, // }
        brkt_square_open, // [
        brkt_square_close, // ]

        punct_colon, // :
        punct_dblColon, // ::
        punct_semiColon, // ;
        punct_comma, // ,

        punct_eq, // =
        punct_plus, // +
        punct_minus, // -
        punct_star, // *
        punct_slash, // /

        punct_fat_arrow, // =>

        ident,

        float,
        int_dec,
        int_hex, // 0x
        int_oct, // 0o
        int_bin, // 0b

        string_open,
        string_literal,
        string_close,

        esc_char, // \<X>
        esc_asciicode, // \x<XX>
        esc_unicode, // \u{XXXX}

        kw_fn,
        kw_let,
        kw_const,
        kw_return,
    };
};

pub const TokenKind = Token.Kind;

/// Describes a single location in a file as a line and column
pub const FileLocation = struct {
    line: usize,
    column: usize,

    pub fn get(str: []const u8, offset: usize) @This() {
        var line: usize = 0;
        var column: usize = 0;
        for (str) |ch, i| {
            switch (ch) {
                '\n' => {
                    line += 1;
                    column = 0;
                },
                else => column += 1,
            }
            if (offset == i)
                break;
        }
        return .{ .line = line + 1, .column = column + 1 };
    }
};

/// Describes a span over two file locations
pub const FileLocationSpan = struct {
    start: FileLocation,
    end: FileLocation,

    pub fn get(str: []const u8, span: SpanData) @This() {
        return .{
            .start = FileLocation.get(str, span.start),
            .end = FileLocation.get(str, span.end),
        };
    }
};
