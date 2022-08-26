const std = @import("std");
const Allocator = std.mem.Allocator;

const glue = @import("glue.zig");
const Cst = glue.Cst;
const Token = glue.Token;

const util = @import("../util/util.zig");
const report = @import("../report.zig");

const SourceMap = @import("../SourceMap.zig");

const Self = @This();

alloc: Allocator,

tokens: []Token.Kind,
ranges: []glue.Token.SourceLocalRange,
ctxkws: Token.Kind.CtxKws,

file_ref: SourceMap.SourceFiles.Ref,

panicking: bool = false,
cursor: u32 = 0,

nodes: Cst.Node.List = .{},
elems: std.ArrayListUnmanaged(Cst.Element) = .{},

strings: util.Strings = .{},
reps: std.ArrayListUnmanaged(report.Report) = .{},

pub const ParserOutput = struct {
    strings: util.Strings,
    reps: []const report.Report,

    pub fn deinit(self: *ParserOutput, alloc: Allocator) void {
        self.strings.deinit(alloc);
        alloc.free(self.reps);

        self.* = undefined;
    }
};

// @TODO: Return list of child modules/files to further parse
pub fn parseFile(
    alloc: Allocator,
    source_map: SourceMap,
    file_ref: SourceMap.SourceFiles.Ref,
) ParserOutput {
    const file = source_map.files.getMut(file_ref);

    std.debug.assert(file.flags.lexed);

    var parser = Self{
        .alloc = alloc,
        .tokens = file.tokens.items(.kind),
        .ranges = file.tokens.items(.range),
        .ctxkws = file.ctxkws,
        .file_ref = file_ref,
    };

    const root = parser.parseTopLevel();
    root.build(&parser) catch unreachable;

    file.cst = .{
        .nodes = parser.nodes.slice(),
        .elems = parser.elems.toOwnedSlice(alloc),
    };

    file.flags.parsed = true;

    parser.strings.shrinkToFit(alloc);
    return .{
        .strings = parser.strings,
        .reps = parser.reps.toOwnedSlice(alloc),
    };
}

fn parseTopLevel(self: *Self) PNode {
    var root = PNode{ .kind = .root };

    while (!self.at(.eof)) {
        self.parseItemDecl(&root);
    }
    self.skipTr();

    return root;
}

const PNode = struct {
    kind: Cst.Node.Kind,
    elems: std.ArrayListUnmanaged(Cst.Element) = .{},

    // inline
    fn appendToken(self: *PNode, parser: *Self) void {
        self.elems.append(parser.alloc, .{ .token = parser.cursor }) catch unreachable;
        parser.cursor += 1;
    }

    // inline
    fn appendNode(self: *PNode, parser: *Self, pn: PNode) void {
        const index = parser.nodes.len;
        pn.build(parser) catch unreachable;
        self.elems.append(parser.alloc, .{ .node = @intCast(u32, index) }) catch unreachable;
    }

    fn build(self: PNode, parser: *Self) !void {
        var elems_start = parser.elems.items.len;
        for (self.elems.items) |elem| {
            try parser.elems.append(parser.alloc, elem);
        }
        var elems_end = parser.elems.items.len;

        var node = Cst.Node{
            .kind = self.kind,
            .elems = .{
                .start = @intCast(u32, elems_start),
                .end = @intCast(u32, elems_end),
            },
        };

        parser.alloc.free(self.elems.allocatedSlice());
        try parser.nodes.append(parser.alloc, node);
    }
};

// inline
fn skipTr(self: *Self) void {
    while (self.peekRaw().isTrivia())
        self.cursor += 1;
}

inline fn peekRaw(self: Self) Token.Kind {
    return if (self.cursor >= self.tokens.len)
        .eof
    else
        self.tokens[self.cursor];
}

// inline
fn peek(self: *Self) Token.Kind {
    self.skipTr();
    return self.peekRaw();
}

fn peekWithCtxkw(self: *Self) Token.Kind {
    const p = self.peek();
    return switch (p) {
        .ident => self.ctxkws.get(self.cursor) orelse .ident,
        else => p,
    };
}

inline fn at(self: *Self, tk: Token.Kind) bool {
    return self.peek() == tk;
}

fn atCtxKw(self: *Self, tk: Token.Kind) bool {
    return tk == self.ctxkws.get(self.cursor) orelse return false;
}

// inline
fn atOneOf(self: *Self, set: []const Token.Kind) bool {
    for (set) |it|
        if (self.at(it))
            return true;
    return false;
}

// inline
fn eat(self: *Self, tk: Token.Kind, pn: *PNode) bool {
    const cond = self.at(tk);
    if (cond)
        pn.appendToken(self);
    return cond;
}

// inline
fn eatOneOf(self: *Self, set: []const Token.Kind, pn: *PNode) bool {
    const cond = self.atOneOf(set);
    if (cond)
        pn.appendToken(self);
    return cond;
}

fn expect(self: *Self, tk: Token.Kind, pn: *PNode) void {
    if (!self.eat(tk, pn)) {
        self.tryReport(.{
            .level = .error_,
            .message = self.refFormatString(
                "expected {s}, but got {s} instead",
                .{ tk.getName(), self.peek().getName() },
            ),
        }, pn);
    }
}

fn expectOneOf(self: *Self, set: []const Token.Kind, pn: *PNode) void {
    if (!self.eatOneOf(set, pn)) {
        util.todo("error: expected one of {any}", .{set});

        var list = std.ArrayList(u8).init(self.alloc);
        defer list.deinit();
        var writer = list.writer();

        writer.writeByte('[') catch unreachable;
        for (set) |it, i| {
            writer.print("{}", .{it.getName()}) catch unreachable;
            if (i != it.len - 1)
                writer.writeAll(", ") catch unreachable;
        }
        writer.writeByte(']') catch unreachable;

        self.tryReport(.{
            .level = .error_,
            .message = self.refFormatString(
                "expected one of {s}, but got {s} instead",
                .{ list.items, self.peek().getName() },
            ),
        }, pn);
    }
}

fn bumpNode(self: *Self, nk: Cst.Node.Kind) PNode {
    var n = PNode{ .kind = nk };
    n.appendToken(self);
    return n;
}

// =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
//  Errors
// =-=-=-=-=

fn refFormatString(self: *Self, comptime format: []const u8, args: anytype) util.Strings.Ref {
    return self.strings.formatString(self.alloc, format, args) catch unreachable;
}

fn refStaticString(self: *Self, str: []const u8) util.Strings.Ref {
    return self.strings.staticString(self.alloc, str) catch unreachable;
}

fn tryReport(
    self: *Self,
    rep: struct {
        level: report.Report.Level,
        message: util.Strings.Ref,
        primary_range: ?glue.Token.SourceLocalRange = null,
    },
    pn: *PNode,
) void {
    if (!self.panicking) {
        self.panicking = true;

        self.reps.append(
            self.alloc,
            .{
                .level = rep.level,
                .message = rep.message,
                .primary_range = if (rep.primary_range) |r| r else self.ranges[self.cursor],
                .file_ref = self.file_ref,
            },
        ) catch unreachable;
    }

    if (self.atOneOf(&.{
        .punct_doubleColon,
        .brkt_brace_close,
    })) {
        self.panicking = false;
    } else {
        var e = PNode{ .kind = .err };
        e.appendToken(self);
        pn.appendNode(self, e);

        if (self.atOneOf(&.{
            .punct_semiColon,
            .eof,
        })) {
            self.panicking = false;
        }
    }
}

// =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
//  Parse Basic
// =-=-=-=-=-=-=

fn parsePath(self: *Self) PNode {
    var path = PNode{ .kind = .path };

    self.expect(.ident, &path);
    while (self.eat(.punct_doubleColon, &path)) {
        self.expect(.ident, &path);
    }

    return path;
}

fn parseInt(self: *Self) PNode {
    var int = PNode{ .kind = .int };
    self.expectOneOf(&.{ .int_dec, .int_bin, .int_hex, .int_oct }, &int);
    return int;
}

fn parseFloat(self: *Self) PNode {
    var float = PNode{ .kind = .float };
    self.expect(.float, &float);
    return float;
}

fn parseString(self: *Self) PNode {
    var str = PNode{ .kind = .string };

    self.expect(.string_open, &str);
    defer self.expect(.string_close, &str);

    while (self.atOneOf(&.{
        .string_literal,
        .esc_char_newline,
        .esc_char_return,
        .esc_char_tab,
        .esc_char_backslash,
        .esc_char_quote_double,
        .esc_char_quote_single,
        .esc_char_other,
        .esc_ascii,
        .esc_unicode,
        .err, // @TODO: Remove
    })) {
        str.appendToken(self);
    }

    return str;
}

// =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
//  Parse Item/Decl
// =-=-=-=-=-=-=-=-=

fn parseItemDecl(self: *Self, pn: *PNode) void {
    var item_decl = PNode{ .kind = .err };
    defer pn.appendNode(self, item_decl);
    // defer std.debug.assert(item_decl.kind != undefined);

    self.expect(.punct_doubleColon, &item_decl);

    switch (self.peek()) {
        .kw_import => {
            self.tryReport(.{
                .level = .unimpl,
                .message = self.refStaticString("TODO: import item"),
            }, &item_decl);
        },
        .kw_impl => {
            self.tryReport(.{
                .level = .unimpl,
                .message = self.refStaticString("TODO: impl item"),
            }, &item_decl);
        },

        .ident => {
            item_decl.kind = .decl;
            item_decl.appendToken(self);

            // self.expect(.ident, &item_decl);

            switch (self.peek()) {
                .kw_fn => self.parseDeclFunc(&item_decl),
                else => {
                    self.tryReport(.{
                        .level = .error_,
                        .message = self.refFormatString(
                            "expected declaration type after name, but got {s}",
                            .{self.peek().getName()},
                        ),
                    }, &item_decl);
                },
            }
        },

        else => {
            self.tryReport(.{
                .level = .error_,
                .message = self.refFormatString(
                    "expected declaration name or item, but got {s}",
                    .{self.peek().getName()},
                ),
            }, &item_decl);
        },
    }
}

fn parseDeclFunc(self: *Self, pn: *PNode) void {
    var func = PNode{ .kind = .decl_func };
    defer pn.appendNode(self, func);

    self.parseFuncSig(&func);

    // var body = PNode{ .kind = .decl_func_body };
    // defer func.appendNode(self, body);

    if (self.eat(.punct_fatArrow, &func)) {
        const expr = self.parseExpr();
        // body.appendNode(self, expr);
        // self.expect(.punct_semiColon, &body);
        func.appendNode(self, expr);
        self.expect(.punct_semiColon, &func);
    } else {
        const block = self.parseExprBlock();
        // body.appendNode(self, block);
        func.appendNode(self, block);
    }
}

fn parseFuncSig(self: *Self, pn: *PNode) void {
    var sig = PNode{ .kind = .func_sig };
    defer pn.appendNode(self, sig);

    self.expect(.kw_fn, &sig);

    if (self.at(.brkt_square_open)) { // '['?
        const template = self.parseTemplateParams();
        sig.appendNode(self, template);
    }

    if (self.at(.brkt_paren_open)) { // '('?
        var params = PNode{ .kind = .func_sig_params };
        defer sig.appendNode(self, params);

        params.appendToken(self); // '('
        defer self.expect(.brkt_paren_close, &params); // ')'

        while (!self.at(.brkt_paren_close)) { // ')'?
            var param = PNode{ .kind = .func_sig_params_param };
            defer params.appendNode(self, param);

            self.expect(.ident, &param); // ident
            self.parseTy(&param); // ty

            if (self.at(.punct_eq)) { // '='?
                var val = PNode{ .kind = .func_sig_params_defaultValue };
                defer param.appendNode(self, val);

                val.appendToken(self); // '='

                const e = self.parseExpr(); // expr
                val.appendNode(self, e);
            }

            if (!self.eat(.punct_comma, &param)) // ','
                break;
        }
    }

    self.parseTy(&sig);
}

fn parseTemplateParams(self: *Self) PNode {
    var params = PNode{ .kind = .templateParams };

    self.expect(.brkt_square_open, &params); // '['
    defer self.expect(.brkt_square_close, &params); // ']'

    while (!self.at(.brkt_square_close)) { // ']'?
        var param = PNode{ .kind = .templateParams_type };
        defer params.appendNode(self, param);

        self.expect(.ident, &param); // ident

        if (self.eat(.punct_exclamation, &param)) { // '!'?
            param.kind = .templateParams_value;

            self.parseTy(&param); // ty

            if (self.at(.punct_eq)) { // '='?
                var val = PNode{ .kind = .templateParams_value_defaultValue };
                defer param.appendNode(self, val);

                val.appendToken(self); // '='

                const e = self.parseExpr(); // expr
                val.appendNode(self, e);
            }
        } else {
            while (!self.atOneOf(&.{ .punct_comma, .brkt_square_close })) {
                if (self.at(.punct_eq)) {
                    var val = PNode{ .kind = .templateParams_type_defaultValue };
                    defer param.appendNode(self, val);

                    val.appendToken(self); // '='
                    self.parseTy(&val); // ty

                    break; // `'=' ty` can only appear at the end
                }

                var constraint = PNode{ .kind = .templateParams_type_constraint };
                defer param.appendNode(self, constraint);

                self.parseTy(&constraint); // 'ty'

                if (!self.eat(.punct_plus, &constraint)) // '+'
                    break;
            }
        }

        if (!self.eat(.punct_comma, &param)) // ','
            break;
    }

    return params;
}

// =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
//  Parse Ty
// =-=-=-=-=-=

fn parseTy(self: *Self, pn: *PNode) void {
    const path = self.parsePath();
    pn.appendNode(self, path);
}

// =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
//  Parse Expr
// =-=-=-=-=-=-=

const Precedence = enum(u8) {
    // zig fmt: off
    none = 0,
    set_let,        // prefix(set let)
    logical_or,     // or
    logical_and,    // and
    equality,       // == !=
    comparison,     // > >= < <=
    sum,            // + -
    product,        // * /
    cast_or_prefix, // as prefix(! not)
    suffix,         // ( ? .
    // zig fmt: on

    inline fn asInt(self: Precedence) u8 {
        return @enumToInt(self);
    }

    inline fn fromInt(int: u8) Precedence {
        return @enumToInt(int);
    }

    // inline fn minusOne(self: Precedence) Precedence {
    //     return Precedence.fromInt(self.asInt() - 1);
    // }

    fn get(tk: glue.Token.Kind) Precedence {
        return switch (tk) {
            .ctxkw_or => .logical_or,
            .ctxkw_and => .logical_and,

            .punct_eqEq, .punct_notEq => .equality,

            .punct_greaterThan,
            .punct_greaterThanEq,
            .punct_lessThan,
            .punct_lessThanEq,
            => .comparison,

            .punct_plus, .punct_minus => .sum,

            .punct_star, .punct_slash => .product,

            .ctxkw_as => .cast_or_prefix,

            .brkt_paren_open,
            .punct_question,
            .punct_dot,
            => .suffix,

            else => .none,
        };
    }
};

fn parseExpr(self: *Self) PNode {
    return self.parseExprPrecedence(.none);
}

fn tryParseExpr(self: *Self) ?PNode {
    return self.tryParseExprPrecedence(.none);
}

fn parseExprPrecedence(self: *Self, prec: Precedence) PNode {
    if (self.tryParseExprPrecedence(prec)) |expr| {
        return expr;
    } else {
        var pn = PNode{ .kind = .err };
        self.tryReport(.{
            .level = .error_,
            .message = self.refFormatString(
                "expected expression, but got {s} instead",
                .{self.peek().getName()},
            ),
        }, &pn);
        return pn;
    }
}

fn tryParseExprPrecedence(self: *Self, prec: Precedence) ?PNode {
    var lhs = self.tryParseExprStart() orelse return null;

    const start_prec = prec;
    var p = self.peekWithCtxkw();
    var infix_prec = Precedence.get(p);

    while (start_prec.asInt() < infix_prec.asInt()) {
        lhs = self.parseExprMid(p, lhs, infix_prec);
        p = self.peekWithCtxkw();
        infix_prec = Precedence.get(p);
    }

    return lhs;
}

fn tryParseExprStart(self: *Self) ?PNode {
    return switch (self.peekWithCtxkw()) {
        .ctxkw_and,
        .ctxkw_as,
        .ctxkw_get,
        .ctxkw_or,
        .ident,
        => self.parsePath(),

        .string_open => self.parseString(),

        .int_dec, .int_hex, .int_bin, .int_oct => self.parseInt(),
        .float => self.parseFloat(),

        .kw_true, .kw_false => self.bumpNode(.boolean),

        .brkt_brace_open => self.parseExprBlock(),

        .brkt_paren_open => {
            var e = PNode{ .kind = .err };
            self.tryReport(.{
                .level = .unimpl,
                .message = self.refStaticString("TODO: paren expr"),
            }, &e);
            return e;
        },

        .kw_let => blk: {
            var let = PNode{ .kind = .expr_let };
            let.appendToken(self); // 'let'

            _ = self.eat(.punct_star, &let); // '*'

            self.expect(.ident, &let); // ident

            if (!self.at(.punct_eq)) {
                var ty = PNode{ .kind = .expr_let_ty };
                defer let.appendNode(self, ty);
                self.parseTy(&ty); // 'ty'
            }

            self.expect(.punct_eq, &let); // '='

            const expr = self.parseExprPrecedence(.set_let); // expr
            let.appendNode(self, expr);

            break :blk let;
        },

        .ctxkw_set => blk: {
            var set = PNode{ .kind = .expr_set };
            set.appendToken(self); // 'set'

            self.expect(.ident, &set); // ident

            self.expect(.punct_eq, &set); // '='

            const expr = self.parseExprPrecedence(.set_let); // expr
            set.appendNode(self, expr);

            break :blk set;
        },

        .punct_minus,
        .punct_exclamation,
        .punct_ampersand,
        .ctxkw_not,
        => blk: {
            var prefix = PNode{ .kind = .expr_prefix };

            { // op
                var op = PNode{ .kind = .expr_prefix_op };
                op.appendToken(self);
                prefix.appendNode(self, op);
            }

            const rhs = self.parseExprPrecedence(.cast_or_prefix);
            prefix.appendNode(self, rhs);

            break :blk prefix;
        },

        else => null,
    };
}

fn parseExprMid(
    self: *Self,
    p: Token.Kind,
    lhs: PNode,
    prec: Precedence,
) PNode {
    return switch (p) {
        .punct_plus,
        .punct_minus,
        .punct_star,
        .punct_slash,
        .ctxkw_or,
        .ctxkw_and,
        => self.parseExprInfix(lhs, prec),

        .ctxkw_as => util.todo("cast expr", .{}),

        .brkt_paren_open => blk: {
            var call = PNode{ .kind = .expr_call };

            call.appendNode(self, lhs); // callee expr

            call.appendToken(self); // '(

            {
                var args = PNode{ .kind = .expr_call_args };
                defer call.appendNode(self, args);

                while (!self.at(.brkt_paren_close)) {
                    const arg = self.parseExpr(); // arg expr
                    args.appendNode(self, arg);

                    if (!self.eat(.punct_comma, &args)) // ','
                        break;
                }
            }

            self.expect(.brkt_paren_close, &call); // ')'

            break :blk call;
        },

        // .punct_question => {},
        // .punct_dot => {},

        else => {
            util.todo("expr mid", .{});
        },
    };
}

fn parseExprInfix(self: *Self, lhs: PNode, prec: Precedence) PNode {
    var infix = PNode{ .kind = .expr_infix };

    infix.appendNode(self, lhs);

    { // op
        var op = PNode{ .kind = .expr_infix_op };
        defer infix.appendNode(self, op);

        // @TODO: Parse multiple operators
        op.appendToken(self);
    }

    const rhs = self.parseExprPrecedence(prec);
    infix.appendNode(self, rhs);

    return infix;
}

fn parseExprBlock(self: *Self) PNode {
    var block = PNode{ .kind = .expr_block };

    self.expect(.brkt_brace_open, &block); // '{'

    while (!self.at(.brkt_brace_close)) {
        if (self.at(.punct_doubleColon)) { // '::'
            self.parseItemDecl(&block);
        } else if (self.tryParseExpr()) |expr| {
            if (self.at(.punct_semiColon)) { // ';'
                block.appendNode(self, expr);
                self.expect(.punct_semiColon, &block);
            } else {
                var end = PNode{ .kind = .expr_block_end };
                end.appendNode(self, expr);
                block.appendNode(self, end);

                self.expect(.brkt_brace_close, &block); // '}'
                return block;
            }
        } else {
            self.tryReport(.{
                .level = .error_,
                .message = self.refFormatString(
                    "expected declaration or expression, but got {s}",
                    .{self.peek().getName()},
                ),
            }, &block);
        }
    }

    self.expect(.brkt_brace_close, &block); // '}'

    return block;
}
