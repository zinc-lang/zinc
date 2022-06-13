allocator: Allocator,
tokens: []const TokenKind,
src: []const u8,

panicking: bool = false,
cursor: usize = 0,

errors: std.ArrayListUnmanaged(ParseError) = .{},

//

const std = @import("std");
const Allocator = std.mem.Allocator;
const Tokens = std.MultiArrayList(Token).Slice;

const super = @import("!mod.zig");

const Token = super.Token;
const TokenKind = super.Token.Kind;

const Cst = super.Cst;
const NodeBuilder = Cst.NodeBuilder;

const Self = @This();

const PathStartSet: []const TokenKind = &.{ .ident, .punct_dblColon };
const IntegerLiteralSet: []const TokenKind = &.{ .int_dec, .int_hex, .int_bin, .int_oct };

pub fn parse(allocator: Allocator, tokens: []const TokenKind, src: []const u8) ParseResult {
    var parser = Self{
        .allocator = allocator,
        .tokens = tokens,
        .src = src,
    };
    var root = parser.parseToplevel();
    defer root.deinit(allocator);
    return .{
        .cst = Cst.buildCst(allocator, root) catch unreachable,
        .errors = parser.errors,
    };
}

pub const ParseResult = struct {
    cst: Cst,
    errors: std.ArrayListUnmanaged(ParseError),

    pub fn deinit(self: *@This(), allocator: Allocator) void {
        self.cst.deinit(allocator);

        self.errors.deinit(allocator);
    }
};

pub const ParseError = union(enum) {
    expected: Expected,

    pub const Expected = struct {
        what: What,
        at: usize,
        found: usize,
        in: Context,

        pub const What = union(enum) {
            item: Item,
            token: TokenKind,
            one_of: []const What,

            pub const Item = enum {
                decl,
                expr,
            };
        };

        pub const Context = enum {
            top_level,
            decl_func,
            stmt,
            stmt_let,
            stmt_return,
            expr_start,
            expr_paren,
            expr_block,
            expr_call,
            string,
            path,
        };
    };
};

//
// Token ops
//

fn atEnd(self: Self) bool {
    return self.at(.EOF);
}

fn peekN(self: Self, n: usize) TokenKind {
    if (self.cursor + n >= self.tokens.len)
        return .EOF;
    return self.tokens[self.cursor + n];
}

fn peek(self: Self) TokenKind {
    return self.peekN(0);
}

// fn atN(self: Self, n: usize, kind: TokenKind) bool {
//     return self.peekN(n) == kind;
// }

fn at(self: Self, kind: TokenKind) bool {
    return self.peek() == kind;
}

// fn atSetN(self: Self, n: usize, set: []const TokenKind) bool {
//     for (set) |kind| {
//         if (self.atN(n, kind))
//             return true;
//     }
//     return false;
// }

fn atSet(self: Self, set: []const TokenKind) bool {
    for (set) |kind| {
        if (self.at(kind))
            return true;
    }
    return false;
    // return self.atSetN(0);
}

fn match(self: *Self, kind: TokenKind, parent: *NodeBuilder) bool {
    if (self.at(kind)) {
        self.bump(parent);
        return true;
    }
    return false;
}

fn matchSet(self: *Self, set: []const TokenKind, parent: *NodeBuilder) bool {
    if (self.atSet(set)) {
        self.bump(parent);
        return true;
    }
    return false;
}

//
// Error ops
//

fn report(self: *Self, parent: *NodeBuilder, err: ParseError) void {
    if (!self.panicking) {
        self.panicking = true;

        self.errors.append(self.allocator, err) catch unreachable;
        return;
    }

    if (self.atSet(&[_]TokenKind{
        .brkt_brace_close,
        .kw_fn,
        .EOF,
    })) {
        self.bump(parent);
        self.panicking = false;
    } else {
        var node = NodeBuilder{ .kind = .skipped };
        self.bump(&node);
        self.appendNodeTo(node, parent);
    }
}

fn expect(
    self: *Self,
    what: TokenKind,
    in: ParseError.Expected.Context,
    parent: *NodeBuilder,
) void {
    if (!self.match(what, parent)) {
        self.report(parent, .{
            .expected = .{
                .what = .{ .token = what },
                .at = self.cursor - 1,
                .found = self.cursor,
                .in = in,
            },
        });
    }
}

//
// Node ops
//

fn bump(self: *Self, parent: *NodeBuilder) void {
    self.appendTokenTo(parent);
    self.cursor += 1;
}

fn appendNodeTo(self: *Self, node: NodeBuilder, to: *NodeBuilder) void {
    to.appendNode(self.allocator, node) catch unreachable;
}

fn appendTokenTo(self: *Self, to: *NodeBuilder) void {
    to.appendToken(self.allocator) catch unreachable;
}

//
// Parse
//

/// Parse the top level of a module/file
fn parseToplevel(self: *Self) NodeBuilder {
    var root = NodeBuilder{ .kind = .root };

    while (!self.atEnd()) {
        self.parseDecl(.top_level, &root);
    }

    // // @FIXME: Is this needed?
    // while (self.tokens[self.cursor] != .EOF) {
    //     self.appendTokenTo(&root);
    //     self.cursor += 1;
    // }
    self.appendTokenTo(&root);

    return root;
}

// @FIXME: 'foo::::' case crashing
fn parsePath(self: *Self) NodeBuilder {
    if (!self.panicking)
        std.debug.assert(self.atSet(PathStartSet));

    var path = NodeBuilder{ .kind = .path };

    _ = self.match(.punct_dblColon, &path);
    self.expect(.ident, .path, &path);

    while (self.at(.punct_dblColon)) {
        self.expect(.punct_dblColon, .path, &path);
        self.expect(.ident, .path, &path);
    }

    return path;
}

fn parseString(self: *Self) NodeBuilder {
    std.debug.assert(self.at(.string_open));

    var str = NodeBuilder{ .kind = .string };

    self.bump(&str);
    while (self.matchSet(&.{
        .string_literal,
        .esc_char,
        .esc_asciicode,
        .esc_unicode,
    }, &str)) {}
    // Any unclosed strings should be caught by the lexer,
    // there's not much a compiler can do to try recover from a malformed string
    self.expect(.string_close, .string, &str);

    return str;
}

fn parseLiteralInt(self: *Self) NodeBuilder {
    std.debug.assert(self.atSet(IntegerLiteralSet));
    var lit = NodeBuilder{ .kind = .literal_int };
    self.bump(&lit);
    return lit;
}

fn parseLiteralFloat(self: *Self) NodeBuilder {
    std.debug.assert(self.at(.float));
    var lit = NodeBuilder{ .kind = .literal_float };
    self.bump(&lit);
    return lit;
}

//
// Parse Decl
//

fn parseDecl(self: *Self, in: ParseError.Expected.Context, parent: *NodeBuilder) void {
    if (!self.tryParseDecl(parent)) {
        self.report(parent, .{
            .expected = .{
                .what = .{ .item = .decl },
                .at = self.cursor,
                .found = self.cursor,
                .in = in,
            },
        });
    }
}

fn tryParseDecl(self: *Self, parent: *NodeBuilder) bool {
    if (self.at(.ident)) {
        const p_next = self.peekN(1);
        var node = NodeBuilder{ .kind = undefined };
        switch (p_next) {
            .kw_fn => {
                node.kind = .decl_func;
                self.bump(&node); // ident
                self.parseDeclFunction(&node);
            },
            .kw_const => {
                node.kind = .decl_const;
                self.bump(&node); // ident
                self.bump(&node); // kw_const
                self.parseStmtExpr(&node);
            },
            else => return false,
        }
        self.appendNodeTo(node, parent);
        return true;
    }
    return false;
}

fn parseDeclFunction(self: *Self, func: *NodeBuilder) void {
    std.debug.assert(self.at(.kw_fn));

    {
        var sig = NodeBuilder{ .kind = .func_signature };
        defer self.appendNodeTo(sig, func);

        self.bump(&sig); // kw_fn

        if (self.match(.brkt_paren_open, &sig)) {
            if (!self.at(.brkt_paren_close)) {
                while (true) {
                    if (self.at(.brkt_paren_close))
                        break;

                    var arg = NodeBuilder{ .kind = .func_signature_arg };
                    defer self.appendNodeTo(arg, &sig);

                    self.expect(.ident, .decl_func, &arg);
                    self.parseType(&arg);

                    if (!self.match(.punct_comma, &sig))
                        break;
                }
            }

            self.expect(.brkt_paren_close, .decl_func, &sig);
        }

        if (!self.atSet(&.{ .punct_fat_arrow, .brkt_brace_open })) {
            var ret = NodeBuilder{ .kind = .func_signature_ret };
            defer self.appendNodeTo(ret, &sig);
            self.parseType(&ret);
        }
    }

    var body = NodeBuilder{ .kind = .decl_func_body };
    defer self.appendNodeTo(body, func);

    if (self.match(.punct_fat_arrow, &body)) {
        self.parseStmtExpr(&body);
    } else {
        self.appendNodeTo(self.parseExprBlock(), &body);
    }
}

//
// Parse type
//

fn parseType(self: *Self, parent: *NodeBuilder) void {
    self.appendNodeTo(self.parsePath(), parent);
}

//
// Parse stmt
//

fn parseStmt(self: *Self, parent: *NodeBuilder) void {
    switch (self.peek()) {
        .kw_let => self.parseStmtLet(parent),
        .kw_return => {
            var ret = NodeBuilder{ .kind = .stmt_return };
            defer self.appendNodeTo(ret, parent);
            self.bump(&ret);
            if (!self.at(.punct_semiColon)) {
                var expr = NodeBuilder{ .kind = .stmt_return_expr };
                defer self.appendNodeTo(expr, &ret);
                self.parseExpr(&expr);
            }
            self.expect(.punct_semiColon, .stmt_return, &ret);
        },
        else => self.parseStmtExpr(parent),
    }
}

fn parseStmtExpr(self: *Self, parent: *NodeBuilder) void {
    self.parseExpr(parent);

    if (self.tokens[self.cursor - 1] != .brkt_brace_close) {
        self.expect(.punct_semiColon, .stmt, parent);
    }
}

fn parseStmtLet(self: *Self, parent: *NodeBuilder) void {
    std.debug.assert(self.at(.kw_let));

    var let = NodeBuilder{ .kind = .stmt_let };
    defer self.appendNodeTo(let, parent);
    self.bump(&let); // kw

    self.expect(.ident, .stmt_let, &let); // ident

    if (self.match(.punct_colon, &let)) {
        var ty = NodeBuilder{ .kind = .stmt_let_ty };
        defer self.appendNodeTo(ty, &let);
        self.parseType(&ty);
    }

    self.expect(.punct_eq, .stmt_let, &let); // '='

    if (!self.at(.punct_semiColon)) {
        var expr = NodeBuilder{ .kind = .stmt_let_expr };
        defer self.appendNodeTo(expr, &let);

        self.parseExpr(&expr);
    }

    self.expect(.punct_semiColon, .stmt_let, &let);
}

//
// Parse expr
//

fn parseExpr(self: *Self, parent: *NodeBuilder) void {
    self.parseExprPrecedence(.none, parent);
}

const Precedence = enum(u8) {
    none = 0,
    assignment,
    // logical_or,
    // logical_and,
    sum,
    product,
    // prefix,
    call,

    pub fn get(kind: TokenKind) Precedence {
        return switch (kind) {
            .punct_eq => .assignment,
            // .Kw_or => .logical_or,
            // .Kw_and => .logical_and,
            .punct_plus, .punct_minus => .sum,
            .punct_star, .punct_slash => .product,
            // .Punct_Exclamation, .Kw_not => .prefix,
            .brkt_paren_open => .call,
            // .ident, .punct_dblColon => .call, // infix call
            else => .none,
        };
    }
};

fn parseExprPrecedence(self: *Self, precedence: Precedence, parent: *NodeBuilder) void {
    var lhs = if (self.parseExprStart()) |n| n else {
        self.report(parent, .{
            .expected = .{
                .what = .{ .item = .expr },
                .at = self.cursor,
                .found = self.cursor,
                .in = .expr_start,
            },
        });
        return;
    };

    const initial_prec = @enumToInt(precedence);
    var p = self.peek();
    var infix_prec = @enumToInt(Precedence.get(self.peek()));

    while (initial_prec < infix_prec) {
        if (self.parseExprInfix(p, lhs, infix_prec)) |node|
            lhs = node
        else {
            // @FIXME: Is this an error condition?
            self.appendNodeTo(lhs, parent);
            return;
        }

        p = self.peek();
        infix_prec = @enumToInt(Precedence.get(p));
    }

    self.appendNodeTo(lhs, parent);
}

fn parseExprStart(self: *Self) ?NodeBuilder {
    return switch (self.peek()) {
        .ident, .punct_dblColon => self.parsePath(),
        .string_open => self.parseString(),

        .int_dec, .int_hex, .int_bin, .int_oct => self.parseLiteralInt(),
        .float => self.parseLiteralFloat(),

        .brkt_brace_open => self.parseExprBlock(),

        // .brkt_paren_open => blk: {
        //     var expr = NodeBuilder{ .kind = undefined };

        //     self.bump(&expr); // '('

        //     // unit
        //     if (self.match(.brkt_paren_close, &expr)) { // ')'
        //         expr.kind = .expr_unit;
        //         break :blk expr;
        //     }
        // },

        else => return null,
    };
}

fn parseExprBlock(self: *Self) NodeBuilder {
    std.debug.assert(self.at(.brkt_brace_open));

    var block = NodeBuilder{ .kind = .expr_block };

    self.bump(&block); // '{'

    while (!self.atSet(&.{ .brkt_brace_close, .EOF })) {
        self.parseStmt(&block);
    }

    self.expect(.brkt_brace_close, .expr_block, &block); // '}'

    return block;
}

fn parseExprInfix(self: *Self, p: TokenKind, lhs: NodeBuilder, prec: u8) ?NodeBuilder {
    return switch (p) {
        .punct_eq,
        .punct_plus,
        .punct_minus,
        .punct_star,
        .punct_slash,
        => blk: {
            var infix = NodeBuilder{ .kind = .expr_infix };

            { // lhs
                var left = NodeBuilder{ .kind = .expr_infix_lhs };
                defer self.appendNodeTo(left, &infix);
                self.appendNodeTo(lhs, &left); // lhs
            }

            { // op
                var op = NodeBuilder{ .kind = .expr_infix_op };
                defer self.appendNodeTo(op, &infix);

                // @TODO: Parse multiple operators
                self.bump(&op); // operator
            }

            { // rhs
                var right = NodeBuilder{ .kind = .expr_infix_rhs };
                defer self.appendNodeTo(right, &infix);
                self.parseExprPrecedence(@intToEnum(Precedence, prec), &right); // rhs
            }

            break :blk infix;
        },
        .brkt_paren_open => blk: {
            var call = NodeBuilder{ .kind = .expr_call };

            {
                var callee = NodeBuilder{ .kind = .expr_call_callee };
                defer self.appendNodeTo(callee, &call);
                self.appendNodeTo(lhs, &callee);
            }

            self.bump(&call); // '('

            if (self.match(.brkt_paren_close, &call)) // ')'
                break :blk call;

            {
                var args = NodeBuilder{ .kind = .expr_call_args };
                defer self.appendNodeTo(args, &call);

                self.parseExpr(&args);

                while (self.match(.punct_comma, &args)) {
                    if (self.at(.brkt_paren_close))
                        break;
                    self.parseExpr(&args);
                }
            }

            self.expect(.brkt_paren_close, .expr_call, &call); // ')'

            break :blk call;
        },
        else => return null,
    };
}
