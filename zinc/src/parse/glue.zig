const std = @import("std");
const Allocator = std.mem.Allocator;

const util = @import("../util/util.zig");

const SourceMap = @import("../SourceMap.zig");

pub const Lexer = @import("Lexer.zig");
pub const Parser = @import("Parser.zig");
pub const Ast = @import("Ast.zig");

pub const Cst = struct {
    nodes: Node.List.Slice,
    elems: []Element,

    pub fn deinit(self: *Cst, alloc: Allocator) void {
        self.nodes.deinit(alloc);
        alloc.free(self.elems);

        self.* = undefined;
    }

    pub fn rootIndex(self: Cst) Node.Index {
        return @intCast(u32, self.nodes.len - 1);
    }

    pub fn getNodeKind(self: Cst, node_i: Node.Index) Node.Kind {
        return self.nodes.items(.kind)[node_i];
    }

    pub fn getNodeElemsRange(self: Cst, node_i: Node.Index) Node.ElemsRange {
        return self.nodes.items(.elems)[node_i];
    }

    pub fn getNodeElems(self: Cst, node_i: Node.Index) []const Element {
        return self.getElems(self.getNodeElemsRange(node_i));
    }

    pub fn getElems(self: Cst, range: Node.ElemsRange) []const Element {
        return self.elems[range.start..range.end];
    }

    pub fn getNthTokenIndex(self: Cst, node: Node.Index, n: u32) Token.Index {
        const elems = self.getNodeElems(node);

        var i: usize = 0;
        return for (elems) |elem| {
            switch (elem) {
                .token => |idx| {
                    i += 1;
                    if (i == n)
                        break idx;
                },
                .node => {},
            }
        } else util.bug("did not find nth ({}) token of node ({})", .{ n, node });
    }

    pub fn getNthNodeIndex(self: Cst, node: Node.Index, n: u32) Node.Index {
        const elems = self.getNodeElems(node);

        var i: usize = 0;
        return for (elems) |elem| {
            switch (elem) {
                .token => {},
                .node => |idx| {
                    i += 1;
                    if (i == n)
                        break idx;
                },
            }
        } else util.bug("did not find nth ({}) child node of node ({})", .{ n, node });
    }

    pub fn getFirstNodeIndex(self: Cst, node: Node.Index) Node.Index {
        const elems = self.getNodeElems(node);

        return for (elems) |elem| {
            switch (elem) {
                .token => {},
                .node => |i| break i,
            }
        } else util.bug("node ({}) does not have any child nodes", .{node});
    }

    pub fn getLastNodeIndex(self: Cst, node: Node.Index) Node.Index {
        const elems = self.getNodeElems(node);

        var last: isize = -1;
        for (elems) |elem| {
            switch (elem) {
                .token => {},
                .node => |idx| last = @intCast(isize, idx),
            }
        }

        if (last == -1)
            util.bug("node ({}) does not have any child nodes", .{node});
        return @intCast(u32, last);
    }

    pub fn getFirstTokenIndex(self: Cst, node: Node.Index) Token.Index {
        const elems = self.getNodeElems(node);

        return for (elems) |elem| {
            switch (elem) {
                .token => |i| break i,
                .node => {},
            }
        } else util.bug("node ({}) does not have any child tokens", .{node});
    }

    pub fn findChildNode(self: Cst, node: Node.Index, kind: Node.Kind) ?Node.Index {
        const elems = self.getNodeElems(node);

        return for (elems) |elem| {
            switch (elem) {
                .token => {},
                .node => |i| {
                    if (self.getNodeKind(i) == kind)
                        break i;
                },
            }
        } else null;
    }

    pub const Element = union(enum) {
        token: u32,
        node: u32,
    };

    pub const Node = struct {
        kind: Kind,
        elems: ElemsRange,

        pub const Index = u32;
        pub const ElemsRange = struct { start: u32, end: u32 };
        pub const List = std.MultiArrayList(Node);

        /// ListOf[item, sep] := item ( sep item )* sep?
        pub const Kind = enum {
            root,
            err,

            int,
            float,
            string,
            boolean,

            /// path := ident ( '::' ident )*
            path,

            // /// import := '::' 'import' path ';'
            // item_import,

            // /// impl := '::' 'impl' path ( 'for' path )? '{' '}'
            // item_impl,

            /// declKind :=
            ///   | func
            ///   | module
            ///   | struct
            ///   | enum
            ///   | union
            ///
            /// decl := '::' ident declKind
            decl,

            /// func := sig body
            decl_func,
            // /// body := ( '=>' expr ) | block
            // decl_func_body,

            // /// module := 'module' '{' item* '}'
            // decl_module,

            // /// struct :=
            // ///   'struct'
            // ///   comptimeParamList?
            // ///   '{' '}'
            // decl_struct,

            /// sig :=
            ///   templateParams?
            ///   params?
            ///   ty
            func_sig,
            /// params := '(' param ( ',' param )* ','? ')'
            func_sig_params,
            /// param := ident ty defaultValue?
            func_sig_params_param,
            /// defaultValue := '=' expr
            func_sig_params_defaultValue,
            // /// named := '.{' single ( ',' single ) ',' '}'
            // func_sig_namedParams,

            /// templateParams := '[' param ( ',' param )* ','? ']'
            templateParams,
            /// type := ident ( constraint ( '+' constraint )* )? defaultValue?
            templateParams_type,
            /// constraint := ty
            templateParams_type_constraint,
            /// defaultValue := '=' ty
            templateParams_type_defaultValue,
            /// value := ident! ty
            templateParams_value,
            /// defaultValue := '=' expr
            templateParams_value_defaultValue,
            // /// named := '.{' param ( ',' param )* ','? '}'
            // templateParams_named,

            /// block := '{' ( expr ';' )* ( end '}' | '}' )
            expr_block,
            /// end := expr
            expr_block_end,

            /// prefix := op expr
            expr_prefix,
            /// op := 'not' | '!' | '-' | '&'
            expr_prefix_op,

            /// let := 'let' '*'? ident ty? '=' expr
            expr_let,
            expr_let_ty,

            /// set := 'set' ident '=' expr
            expr_set,

            /// infix := expr op expr
            expr_infix,
            /// op :=
            ///   | '+' | '-' | '*' | '/' | '='
            ///   | '==' | '!='
            ///   | '>' | '<' | '>=' | '<='
            ///   | 'and' | 'or'
            // @TODO: Should we have a separate `op` for each operator?
            expr_infix_op,

            /// call := expr '(' args? ')'
            expr_call,
            /// args := expr (',' expr) ','?
            expr_call_args,

            // /// try := expr '?'
            // expr_try,

            // /// '?' ty,
            // ty_nullable,
            // /// '[]' '*'? ty,
            // ty_slice,
            // /// '&' '*'? ty,
            // ty_ref,
        };
    };
};

pub const Token = struct {
    kind: Kind,
    range: SourceLocalRange,

    pub const Index = u32;

    pub const SourceLocalRange = struct {
        start: u32,
        end: u32,
    };

    pub const List = std.MultiArrayList(Token);

    pub const Kind = enum(u7) {
        eof,
        err,

        tr_newline,
        tr_whitespace,
        tr_comment,

        // zig fmt: off
        brkt_paren_open,   // (
        brkt_paren_close,  // )
        brkt_brace_open,   // {
        brkt_brace_close,  // }
        brkt_square_open,  // [
        brkt_square_close, // ]

        punct_ampersand,     // &
        punct_exclamation,   // !
        punct_notEq,         // !=
        punct_colon,         // :
        punct_doubleColon,   // ::
        punct_semiColon,     // ;
        punct_comma,         // ,
        punct_dot,           // .
        punct_doubleDot,     // ..
        punct_tripleDot,     // ...
        punct_eq,            // =
        punct_eqEq,          // ==
        punct_fatArrow,      // =>
        punct_greaterThan,   // >
        punct_greaterThanEq, // >=
        punct_lessThan,      // <
        punct_lessThanEq,    // <=
        // punct_lThinArrow,    // <-
        punct_minus,         // -
        punct_rThinArrow,    // ->
        punct_pipe,          // |
        punct_plus,          // +
        punct_question,      // ?
        punct_slash,         // /
        punct_star,          // *
        // zig fmt: on

        ident,

        int_dec,
        int_hex,
        int_bin,
        int_oct,
        float,

        string_open,
        string_literal,
        string_close,

        // zig fmt: off
        esc_char_newline,      // \n
        esc_char_return,       // \r
        esc_char_tab,          // \t
        esc_char_backslash,    // \\
        esc_char_quote_double, // \"
        esc_char_quote_single, // \'
        esc_char_other,        // None of the above
        esc_ascii,             // \xNN
        esc_unicode,           // \u{N*[1-6]}
        // zig fmt: on

        kw_class,
        kw_else,
        kw_enum,
        kw_false,
        kw_fn,
        kw_if,
        kw_impl,
        kw_import,
        kw_let,
        kw_mixin,
        kw_module,
        kw_pub,
        kw_struct,
        kw_trait,
        kw_true,
        kw_union,

        // kw_rawfor ?

        ctxkw_and,
        ctxkw_as,
        ctxkw_get,
        ctxkw_not,
        ctxkw_or,
        ctxkw_set,

        pub fn getName(self: Kind) []const u8 {
            return switch (self) {
                .eof => "end of file",

                .brkt_paren_open => "`(`",
                .brkt_paren_close => "`)`",
                .brkt_brace_open => "`{`",
                .brkt_brace_close => "`}`",
                .brkt_square_open => "`[`",
                .brkt_square_close => "`]`",

                .ident => "ident",

                .punct_plus => "`+`",
                .punct_minus => "`-`",
                .punct_eq => "`=`",

                .punct_colon => "`:`",
                .punct_doubleColon => "`::`",
                .punct_semiColon => "`;`",

                else => util.todo("get token name '{?}'", .{self}),
            };
        }

        pub const Keywords = std.ComptimeStringMap(Kind, .{
            // zig fmt: off
            .{ "class",  .kw_class  },
            .{ "else",   .kw_else   },
            .{ "enum",   .kw_enum   },
            .{ "false",  .kw_false  },
            .{ "fn",     .kw_fn     },
            .{ "if",     .kw_if     },
            .{ "impl",   .kw_impl   },
            .{ "import", .kw_import },
            .{ "let",    .kw_let    },
            .{ "mixin",  .kw_mixin  },
            .{ "module", .kw_module },
            .{ "pub",    .kw_pub    },
            .{ "struct", .kw_struct },
            .{ "trait",  .kw_trait  },
            .{ "true",   .kw_true   },
            .{ "union",  .kw_union  },
            // zig fmt: on
        });

        pub const CtxKwsMap = std.ComptimeStringMap(Kind, .{
            // zig fmt: off
            .{ "and", .ctxkw_and },
            .{ "as",  .ctxkw_as  },
            .{ "get", .ctxkw_get },
            .{ "not", .ctxkw_not },
            .{ "or",  .ctxkw_or  },
            .{ "set", .ctxkw_set },
            // zig fmt: on
        });

        pub const CtxKws = std.AutoHashMapUnmanaged(u32, Kind);

        pub fn isTrivia(self: Kind) bool {
            return switch (self) {
                .tr_newline, .tr_whitespace, .tr_comment => true,
                else => false,
            };
        }
    };
};
