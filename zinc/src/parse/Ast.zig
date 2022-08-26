// Yes, I am well aware of the quality of the code in here.
// It's mostly in the state that it is in now because its design is not finalized.
// This will need to be heavily refactored once we need to deal with multiple files.

root: Root,

root_scope: Scope.Ref = Scope.Ref.zero(),
scopes: Scope.List = .{},

// strings: util.Strings = .{},
strings: util.InterningIndexList([]const u8, u32) = .{},

decls: Decl.List = .{},

tys: Ty.List = .{},
ty_funcs: Ty.Func.List = .{},
ty_func_params: Ty.Func.Param.List = .{},

exprs: Expr.List = .{},
blocks: Expr.Block.List = .{},

path_segments: Path.Segment.List = .{},

const std = @import("std");
const Allocator = std.mem.Allocator;

const util = @import("../util/util.zig");

const glue = @import("glue.zig");

pub const CstIndex = glue.Cst.Node.Index;
pub const TokenIndex = glue.Token.Index;

const Ast = @This();

pub fn deinit(self: *Ast, alloc: Allocator) void {
    self.strings.deinit(alloc);

    self.decls.deinit(alloc);

    self.tys.deinit(alloc);
    self.ty_funcs.deinit(alloc);
    self.ty_func_params.deinit(alloc);

    self.exprs.deinit(alloc);
    self.blocks.deinit(alloc);

    self.path_segments.deinit(alloc);

    self.* = undefined;
}

pub fn shrinkToFit(self: *Ast, alloc: Allocator) void {
    self.strings.shrinkToFit(alloc);

    self.decls.shrinkToFit(alloc);
    self.exprs.shrinkToFit(alloc);
    self.tys.shrinkToFit(alloc);

    self.ty_funcs.shrinkToFit(alloc);
    self.ty_func_params.shrinkToFit(alloc);

    self.path_segments.shrinkToFit(alloc);
}

pub const Root = struct {
    cst: CstIndex,
    decls: Decl.Ref.Range,
};

pub const Path = struct {
    cst: CstIndex,
    segments: Segment.Ref.Range,

    pub const Segment = union(enum) {
        ident: TokenIndex,

        pub const List = util.IndexList(@This(), u32);
        pub const Ref = @This().List.Ref;
    };
};

pub const Decl = struct {
    cst: CstIndex,
    name: TokenIndex,
    kind: union(enum) {
        func: Func,
    },

    pub const Func = struct {
        sig: Ty.Func.Ref,
        body: Expr.Ref,
    };

    pub const List = util.IndexList(@This(), u32);
    pub const Ref = @This().List.Ref;
};

pub const Ty = struct {
    cst: CstIndex,
    kind: Kind,

    pub const Kind = union(enum) {
        // path: TyPath,
        path: Path,
        func: Func,
    };

    // pub const TyPath = struct {
    //     path: Path,
    //     kind: TyPath.Kind,

    //     pub const Kind = union(enum) {
    //         prim: Prim,
    //     };

    //     pub const Prim = union(enum) {
    //         void,
    //         bool,
    //         never,
    //         integer: packed struct {
    //             signed: bool,
    //             size: u7,
    //         },
    //     };
    // };

    pub const Func = struct {
        // @TODO: comptime_params
        params: Param.Ref.Range,
        ret: Ty.Ref,

        pub const Param = struct {
            cst: CstIndex,
            name: TokenIndex,
            ty: Ty.Ref,

            pub const List = util.IndexList(@This(), u32);
            pub const Ref = @This().List.Ref;
        };

        pub const List = util.IndexList(@This(), u32);
        pub const Ref = @This().List.Ref;
    };

    pub const List = util.IndexList(@This(), u31);
    pub const Ref = @This().List.Ref;
};

pub const Expr = struct {
    cst: CstIndex,
    kind: Kind,

    pub const Kind = union(enum) {
        path: ExprPath,
        literal: Literal,
        let: Let,
        // set: Set,
        block: Block.Ref,
        // prefix: Prefix,
        infix: Infix,
        call: Call,
        // ret: ?Expr.Ref,
    };

    pub const ExprPath = struct {
        path: Path,
        kind: ExprPath.Kind,

        pub const Kind = union(enum) {
            decl: Decl.Ref,
            // local: Local.Ref,
            // arg: Arg.Ref,
        };
    };

    pub const Literal = union(enum) {
        string: util.Strings.Ref,
        integer: u64,
        float: f64,
        boolean: bool,
    };

    pub const Let = struct {
        name: TokenIndex,
        ty: ?Ty.Ref,
        expr: Expr.Ref,
    };

    // pub const Set = struct {
    //     name: TokenIndex,
    //     expr: Expr.Ref,
    // };

    pub const Block = struct {
        cst: CstIndex,
        decls: Decl.Ref.Range,
        exprs: Expr.Ref.Range,
        end: ?Expr.Ref,

        pub const List = util.IndexList(@This(), u32);
        pub const Ref = @This().List.Ref;
    };

    // pub const Prefix = struct {
    //     // @TODO: Multiple operators
    //     op: TokenIndex,
    //     rhs: Expr.Ref,
    // };

    pub const Infix = struct {
        lhs: Expr.Ref,
        // @TODO: Multiple operators
        op: TokenIndex,
        rhs: Expr.Ref,
    };

    pub const Call = struct {
        callee: Expr.Ref,
        // @TODO: template args
        args: Expr.Ref.Range,
    };

    pub const List = util.IndexList(@This(), u31);
    pub const Ref = @This().List.Ref;
};

const Scope = struct {
    kind: Kind,
    /// Undefined for `kind == .root`
    parent: Ref,
    children: std.ArrayListUnmanaged(Scope.Ref) = .{},
    decls: std.StringHashMapUnmanaged(Decl.Ref) = .{},

    pub const Kind = union(enum) {
        root,
        // module: util.Strings.Ref,
        block: Expr.Block.Ref,
    };

    pub fn deinit(self: *Scope, alloc: Allocator) void {
        self.children.deinit(alloc);
        self.decls.deinit(alloc);

        self.* = undefined;
    }

    pub const List = util.IndexList(@This(), u32);
    pub const Ref = @This().List.Ref;
};

pub const Generator = struct {
    const glue = @import("glue.zig");
    const SourceMap = @import("../SourceMap.zig");

    const log = std.log.scoped(.astgen);

    alloc: Allocator,
    file: *const SourceMap.SourceFile,

    ast: Ast = .{ .root = undefined },

    current_scope: Scope.Ref = Scope.Ref.zero(),

    // pub const Local = struct {
    //     name: TokenIndex,
    //     expr: Expr.Ref,

    //     pub const List = util.IndexList(@This(), u32);
    //     pub const Ref = @This().List.Ref;
    // };

    pub fn generate(
        alloc: Allocator,
        source_map: SourceMap,
        file_ref: SourceMap.SourceFiles.Ref,
    ) !void {
        const file = source_map.files.getMut(file_ref);

        std.debug.assert(file.flags.lexed);
        std.debug.assert(file.flags.parsed);

        var gen = Generator{ .alloc = alloc, .file = file };
        defer gen.deinit();

        log.debug("generating AST for file '{s}'", .{file.path});
        try gen.doWork();

        file.ast = gen.ast;
        file.flags.has_ast = true;
    }

    fn deinit(self: *Generator) void {
        self.strings.deinit(self.alloc);

        for (self.ast.scopes.list.items) |*scope| {
            scope.deinit(self.alloc);
        }
        self.ast.scopes.deinit(self.alloc);

        self.ast.locals.deinit(self.alloc);
    }

    fn doWork(self: *Generator) !void {
        self.root_scope = try self.scopes.append(self.alloc, .{
            .kind = .{ .root = void{} },
            .parent = Scope.Ref.max(),
        });
        self.current_scope = self.root_scope;

        const root = try self.genRoot(self.file.cst.rootIndex());
        self.ast.root = root;
    }

    fn genRoot(self: *Generator, n_idx: CstIndex) !Root {
        log.debug("generating Root: {{ i: {} }}", .{n_idx});

        const elems = self.file.cst.getNodeElems(n_idx);

        var decls = std.ArrayList(Decl).init(self.alloc);
        defer decls.deinit();

        log.debug(" > priming decls for root", .{});
        for (elems) |elem| {
            switch (elem) {
                .token => util.bug("token at top level", .{}),
                .node => |i| switch (self.file.cst.getNodeKind(i)) {
                    .decl => {
                        const decl = self.primeDecl(i);
                        try decls.append(decl);
                    },
                    else => util.todo("", .{}),
                },
            }
        }

        const decls_range = try self.allocDecls(decls.items);
        try self.fillDecls(decls_range);

        return Root{ .cst = n_idx, .decls = decls_range };
    }

    fn primeDecl(self: *Generator, n_idx: CstIndex) Decl {
        log.debug("priming Decl: {{ i: {} }}", .{n_idx});

        std.debug.assert(self.file.cst.getNodeKind(n_idx) == .decl);

        const name = self.file.cst.getNthTokenIndex(n_idx, 2);

        log.debug(" > {{ name: '{s}' }}", .{self.file.getLexeme(name)});

        const decl = Decl{
            .cst = n_idx,
            .name = name,
            .kind = undefined,
        };

        return decl;
    }

    fn allocDecls(self: *Generator, decls: []Decl) !Decl.Ref.Range {
        const start = self.ast.decls.currRef();
        for (decls) |decl| {
            const slice = self.file.getLexeme(decl.name);
            const ref = try self.ast.decls.append(self.alloc, decl);

            const scope = self.scopes.getMut(self.current_scope);
            try scope.decls.put(self.alloc, slice, ref);
        }
        const end = self.ast.decls.currRef();

        return .{ .start = start, .end = end };
    }

    fn fillDecls(self: *Generator, range: Decl.Ref.Range) !void {
        for (self.ast.decls.getRangeMut(range)) |*decl| {
            log.debug(
                "filling Decl: {{ i: {}, name: '{s}' }}",
                .{ decl.cst, self.file.getLexeme(decl.name) },
            );

            const kind_i = self.file.cst.getFirstNodeIndex(decl.cst);
            decl.kind = switch (self.file.cst.getNodeKind(kind_i)) {
                .decl_func => .{ .func = try self.genDeclFunc(kind_i) },
                else => util.todo("", .{}),
            };
        }
    }

    fn genDeclFunc(self: *Generator, n_idx: CstIndex) !Decl.Func {
        log.debug("generating Decl.Func: {{ i: {} }}", .{n_idx});

        const sig_ty = try self.genTyFunc(self.file.cst.getFirstNodeIndex(n_idx));
        const sig = try self.ast.ty_funcs.append(self.alloc, sig_ty);

        const body = try self.ast.exprs.append(
            self.alloc,
            try self.genExpr(self.file.cst.getLastNodeIndex(n_idx)),
        );

        return Decl.Func{ .sig = sig, .body = body };
    }

    fn genTy(self: *Generator, n_idx: CstIndex) Allocator.Error!Ty {
        log.debug("generating Ty: {{ i: {} }}", .{n_idx});

        const kind: Ty.Kind = switch (self.file.cst.getNodeKind(n_idx)) {
            .path => .{ .path = try self.genPath(n_idx) },
            .func_sig => .{ .func = try self.genTyFunc(n_idx) },
            else => util.todo("ast ty", .{}),
        };

        return Ty{ .cst = n_idx, .kind = kind };
    }

    fn genTyFunc(self: *Generator, n_idx: CstIndex) !Ty.Func {
        log.debug("generating Ty.Func: {{ i: {} }}", .{n_idx});

        const params_range = if (self.file.cst.findChildNode(
            n_idx,
            .func_sig_params,
        )) |params_index| blk: {
            var params = std.ArrayList(Ty.Func.Param).init(self.alloc);
            defer params.deinit();

            for (self.file.cst.getNodeElems(params_index)) |elem| {
                switch (elem) {
                    .token => {},
                    .node => |i| {
                        const name = self.file.cst.getFirstTokenIndex(i);
                        const ty = try self.genTy(self.file.cst.getFirstNodeIndex(i));
                        const ty_ref = try self.ast.tys.append(self.alloc, ty);

                        try params.append(.{ .cst = i, .name = name, .ty = ty_ref });
                    },
                }
            }

            break :blk try self.ast.ty_func_params.appendRange(self.alloc, params.items);
        } else Ty.Func.Param.Ref.Range.empty();

        const return_ty_i = self.file.cst.getLastNodeIndex(n_idx);
        const return_ty = try self.genTy(return_ty_i);
        const ret = try self.ast.tys.append(self.alloc, return_ty);

        return Ty.Func{ .params = params_range, .ret = ret };
    }

    fn genPath(self: *Generator, n_idx: CstIndex) !Path {
        log.debug("generating Path: {{ i: {} }}", .{n_idx});

        const elems = self.file.cst.getNodeElems(n_idx);

        var segments = std.ArrayList(Path.Segment).init(self.alloc);
        defer segments.deinit();

        for (elems) |elem| {
            const segment = switch (elem) {
                .token => |i| switch (self.file.tokens.items(.kind)[i]) {
                    .punct_doubleColon => continue,
                    .ident => Path.Segment{ .ident = i },
                    else => util.todo("more path segments", .{}),
                },
                .node => util.bug("node in path", .{}),
            };

            log.debug(" > path segment: {?}", .{segment});

            try segments.append(segment);
        }

        const range = try self.ast.path_segments.appendRange(self.alloc, segments.items);
        return Path{ .cst = n_idx, .segments = range };
    }

    fn genExpr(self: *Generator, n_idx: CstIndex) Allocator.Error!Expr {
        log.debug("generating Expr: {{ i: {} }}", .{n_idx});

        var expr = self.primeExpr(n_idx);
        try self.fillExpr(&expr);

        return expr;
    }

    fn primeExpr(self: *Generator, n_idx: CstIndex) Expr {
        _ = self;
        log.debug("priming Expr: {{ i: {} }}", .{n_idx});
        const e = Expr{ .cst = n_idx, .kind = undefined };
        return e;
    }

    fn fillExprs(self: *Generator, range: Expr.Ref.Range) !void {
        for (self.ast.exprs.getRangeMut(range)) |*expr| {
            try self.fillExpr(expr);
        }
    }

    fn fillExpr(self: *Generator, expr: *Expr) !void {
        log.debug("filling Expr: {{ i: {} }}", .{expr.cst});

        expr.kind = switch (self.file.cst.getNodeKind(expr.cst)) {
            .path => .{ .path = try self.resolveExprPath(try self.genPath(expr.cst)) },

            .string => .{ .literal = .{ .string = util.todo("string literal", .{}) } },
            .int => .{
                .literal = .{
                    .integer = blk: {
                        const str = self.file.getLexeme(self.file.cst.getFirstTokenIndex(expr.cst));
                        // 0 radix to let it infer based on prefix; 0x, 0b or 0o
                        const int = std.fmt.parseUnsigned(u64, str, 0) catch
                            util.bug("failed int convert: '{s}'", .{str});
                        log.debug(" > integer: '{}'", .{int});
                        break :blk int;
                    },
                },
            },
            .float => .{
                .literal = .{
                    .float = blk: {
                        const str = self.file.getLexeme(self.file.cst.getFirstNodeIndex(expr.cst));
                        const float = std.fmt.parseFloat(f64, str) catch
                            util.bug("failed float convert: '{s}'", .{str});
                        log.debug(" > float: '{}'", .{float});
                        break :blk float;
                    },
                },
            },
            .boolean => .{ .literal = .{ .boolean = util.todo("boolean literal", .{}) } },

            .expr_let => .{ .let = try self.genExprLet(expr.cst) },

            // .expr_set => {},

            .expr_block => .{ .block = try self.genExprBlock(expr.cst) },

            // .expr_prefix = {},

            .expr_infix => .{ .infix = try self.genExprInfix(expr.cst) },

            .expr_call => .{ .call = try self.genExprCall(expr.cst) },

            // .expr_return => {},

            else => util.todo("more exprs", .{}),
        };
    }

    fn resolveExprPath(self: *Generator, path: Path) !Expr.ExprPath {
        log.debug("resolving Expr.ExprPath, Path: {{ i: {} }}", .{path.cst});

        var to_resolve = self.ast.path_segments.getRange(path.segments);
        std.debug.assert(to_resolve.len != 0);
        var found: Expr.ExprPath.Kind = undefined;
        var search_scope = self.current_scope;

        while (to_resolve.len != 0) : (to_resolve = to_resolve[0..(to_resolve.len - 1)]) {
            const segment = to_resolve[to_resolve.len - 1];
            var scope = self.scopes.get(search_scope);

            switch (segment) {
                .ident => |tok_i| {
                    const slice = self.file.getLexeme(tok_i);

                    while (true) {
                        // If find decl in current scope
                        if (scope.decls.get(slice)) |ref| {
                            found = .{ .decl = ref };
                            break;
                        } else {
                            // else if the current scope is the root then we did not find the item
                            // else if the current scope is a block then search up
                            switch (scope.kind) {
                                .root => util.todo("did not find path item", .{}),
                                .block => scope = self.scopes.get(scope.parent),
                            }
                        }
                    }
                },
            }
        }

        return Expr.ExprPath{ .path = path, .kind = found };
    }

    fn genExprLet(self: *Generator, n_idx: CstIndex) !Expr.Let {
        log.debug("generating Expr.Let: {{ i: {} }}", .{n_idx});

        const name = self.file.cst.getNthTokenIndex(n_idx, 2);

        const ty = if (self.file.cst.findChildNode(n_idx, .expr_let_ty)) |ty_idx| blk: {
            const ty = try self.genTy(self.file.cst.getFirstNodeIndex(ty_idx));
            break :blk try self.ast.tys.append(self.alloc, ty);
        } else null;

        const expr_idx = self.file.cst.getLastNodeIndex(n_idx);
        const expr = try self.ast.exprs.append(self.alloc, try self.genExpr(expr_idx));

        // const local = Local{ .name = name, .expr = expr };
        // try self.locals.append(self.alloc, local);

        return .{ .name = name, .ty = ty, .expr = expr };
    }

    fn genExprBlock(self: *Generator, n_idx: CstIndex) Allocator.Error!Expr.Block.Ref {
        log.debug("generating Expr.Block: {{ i: {} }}", .{n_idx});

        var decls = std.ArrayList(Decl).init(self.alloc);
        defer decls.deinit();
        var exprs = std.ArrayList(Expr).init(self.alloc);
        defer exprs.deinit();

        const elems = self.file.cst.getNodeElems(n_idx);
        var end_i: ?CstIndex = null;

        log.debug(" > priming decls and exprs for block", .{});
        for (elems) |elem| {
            switch (elem) {
                .token => {},
                .node => |i| switch (self.file.cst.getNodeKind(i)) {
                    .expr_block_end => {
                        end_i = self.file.cst.getFirstNodeIndex(i);
                        break; // expr_block_end can only appear at the end
                    },
                    .decl => {
                        const decl = self.primeDecl(i);
                        try decls.append(decl);
                    },
                    else => { // else it is an expr
                        const e = self.primeExpr(i);
                        try exprs.append(e);
                    },
                },
            }
        }

        const ref = try self.ast.blocks.append(self.alloc, .{
            .cst = n_idx,
            .decls = undefined,
            .exprs = undefined,
            .end = null,
        });
        const block = self.ast.blocks.getMut(ref);

        const old_scope = self.current_scope;
        const scope = Scope{
            .kind = .{ .block = ref },
            .parent = old_scope,
        };
        const scope_ref = try self.scopes.append(self.alloc, scope);

        {
            const current_scope = self.scopes.getMut(self.current_scope);
            try current_scope.children.append(self.alloc, scope_ref);
        }

        self.current_scope = scope_ref;

        const decls_range = try self.allocDecls(decls.items);
        block.decls = decls_range;
        try self.fillDecls(decls_range);

        const exprs_range = try self.ast.exprs.appendRange(self.alloc, exprs.items);
        block.exprs = exprs_range;
        try self.fillExprs(exprs_range);

        if (end_i) |i| {
            const e = try self.genExpr(self.file.cst.getFirstNodeIndex(i));
            block.end = try self.ast.exprs.append(self.alloc, e);
        }

        self.current_scope = old_scope;

        return ref;
    }

    fn genExprInfix(self: *Generator, n_idx: CstIndex) !Expr.Infix {
        log.debug("generating Expr.Infix: {{ i: {} }}", .{n_idx});

        const lhs_i = self.file.cst.getFirstNodeIndex(n_idx);
        const rhs_i = self.file.cst.getLastNodeIndex(n_idx);
        const op_i = self.file.cst.getNthNodeIndex(n_idx, 2);

        const lhs = try self.genExpr(lhs_i);
        const rhs = try self.genExpr(rhs_i);

        const lhs_ref = try self.ast.exprs.append(self.alloc, lhs);
        const rhs_ref = try self.ast.exprs.append(self.alloc, rhs);

        // @TODO: Multiple operators
        const op = self.file.cst.getFirstTokenIndex(op_i);

        return Expr.Infix{ .lhs = lhs_ref, .rhs = rhs_ref, .op = op };
    }

    fn genExprCall(self: *Generator, n_idx: CstIndex) !Expr.Call {
        log.debug("generating Expr.Call: {{ i: {} }}", .{n_idx});

        const callee_expr = try self.genExpr(self.file.cst.getFirstNodeIndex(n_idx));
        const callee = try self.ast.exprs.append(self.alloc, callee_expr);

        const args_node = self.file.cst.findChildNode(n_idx, .expr_call_args) orelse
            util.bug("no args node within call, i={}", .{n_idx});

        var args = std.ArrayList(Expr).init(self.alloc);
        defer args.deinit();

        for (self.file.cst.getNodeElems(args_node)) |elem| {
            switch (elem) {
                .token => {},
                .node => |i| {
                    const arg = try self.genExpr(i);
                    try args.append(arg);
                },
            }
        }

        const args_range = try self.ast.exprs.appendRange(self.alloc, args.items);

        return Expr.Call{ .callee = callee, .args = args_range };
    }
};
