nodes: std.MultiArrayList(Node).Slice,
// @TODO...
// Would it be reasonable to change the alignment of this as an
// element is effectively a bit.
elements: []const Element,

const std = @import("std");
const Allocator = std.mem.Allocator;

const super = @import("!mod.zig");

const Cst = @This();

pub fn deinit(self: *Cst, allocator: Allocator) void {
    self.nodes.deinit(allocator);
    allocator.free(self.elements);
    self.* = undefined;
}

// pub fn getNodeChildren(self: Cst, index: u32) void {
//     var accum = 0;
//     var count = 0;
//     for (self.nodes.items(.count)) |c, i| {
//         if (i == index) {
//             count = c;
//             break;
//         }
//         accum += c;
//     }

//     self.elements[accum..(accum + count)];
// }

pub const Node = struct {
    kind: Kind,
    count: u24,
    // 4 bytes in size, no waste

    /// You may see the pattern `T ( ',' T )* ','?` a bit in this,
    /// it can be read a comma separated list of `T` with an optional trailing comma.
    ///
    /// # Syntax "class"
    ///
    /// The prefix before a `Node.Kind`.
    /// If a 'class' is valid where another is expected it is a 'sub-class'.
    /// ie. a `path` is a valid `ty` therefore `path` is a 'sub-class' of a `ty`.
    /// This is denoted by an `isa` in its documentation.
    ///
    /// ## no prefix
    ///
    /// Things like `path` and `string`
    /// These are not specific to any area of syntax and thus don't have a class
    ///
    /// ## `decl` - declaration
    ///
    /// isa `stmt`
    ///
    /// Top-level items, such as function and type declaration
    ///
    /// ## `stmt` - Statement
    ///
    /// A piece of executable code that does not have a resulting value.
    /// An `expr` isa `stmt`.
    ///
    /// ## `ty` - Type
    ///
    /// A type.
    /// An plain path is a valid type.
    ///
    /// ## 'expr'
    ///
    pub const Kind = enum(u8) {
        root,
        skipped,

        /// isa `ty` & 'expr'
        /// `'::'? ident ( '::' ident )*`
        path,

        /// isa 'expr'
        /// '"' (char | '\' escape )* '"'
        string,

        func_signature,
        func_signature_arg,
        func_signature_ret,

        /// isa 'stmt'
        decl_func,
        decl_func_body, // : expr

        decl_const,

        /// 'let' ident ( ':' ty )? '=' expr ';'
        stmt_let,
        stmt_let_ty, // : ty
        stmt_let_expr, // : expr

        // 'return' expr? ';'
        stmt_return,
        stmt_return_expr,

        literal_int,
        literal_float,

        /// '{' stmt* '}'
        expr_block, // []stmt

        /// `expr operator expr`
        expr_infix,
        expr_infix_lhs, // : expr
        expr_infix_rhs, // : expr
        expr_infix_op,

        /// '()'
        expr_unit,
        /// '(' expr ')'
        expr_grouping, // : expr
        /// '(' expr ( ',' expr )* ','? ')'
        expr_tuple, // : []expr

        /// expr tuple
        expr_call,
        expr_call_callee, // : expr
        expr_call_args, // : []expr
    };
};

pub const NodeKind = Node.Kind;

pub const Element = enum(u1) { token, node };

/// Type used by the `Parser` to construct a `Cst`
pub const NodeBuilder = struct {
    kind: Node.Kind,
    elements: std.ArrayListUnmanaged(Element) = .{},
    child_nodes: std.ArrayListUnmanaged(NodeBuilder) = .{},

    pub fn deinit(self: *NodeBuilder, allocator: Allocator) void {
        self.elements.deinit(allocator);
        for (self.child_nodes.items) |*node| {
            node.deinit(allocator);
        }
        self.child_nodes.deinit(allocator);
    }

    pub fn appendToken(self: *NodeBuilder, allocator: Allocator) !void {
        try self.elements.append(allocator, .token);
    }

    pub fn appendNode(self: *NodeBuilder, allocator: Allocator, node: NodeBuilder) !void {
        try self.elements.append(allocator, .node);
        try self.child_nodes.append(allocator, node);
    }
};

/// Takes a root node and produces a `Cst`
pub fn buildCst(allocator: Allocator, root: NodeBuilder) !Cst {
    var nodes = std.MultiArrayList(Node){};
    var elements = std.ArrayListUnmanaged(Element){};

    try buildCstNode(allocator, &nodes, &elements, root);

    return Cst{
        .nodes = nodes.toOwnedSlice(),
        .elements = elements.toOwnedSlice(allocator),
    };
}

// recursive
fn buildCstNode(
    allocator: Allocator,
    nodes: *std.MultiArrayList(Node),
    elements: *std.ArrayListUnmanaged(Element),
    node: NodeBuilder,
) Allocator.Error!void {
    const node_index = nodes.len;
    try nodes.append(allocator, .{ .kind = node.kind, .count = undefined });

    const count_start = elements.items.len;

    for (node.elements.items) |elem| {
        switch (elem) {
            .token => try elements.append(allocator, .token),
            .node => try elements.append(allocator, .node),
        }
    }

    const count = elements.items.len - count_start;
    nodes.items(.count)[node_index] = @intCast(u24, count);

    for (node.child_nodes.items) |n| {
        // recurse
        try buildCstNode(allocator, nodes, elements, n);
    }
}

/// Helper struct to print the tree structure of a `Cst`
pub const Printer = struct {
    allocator: Allocator,
    cst: Cst,
    tokens: []const super.Token.Kind,
    spans: []const super.SpanData,
    src: []const u8,

    node_count: usize = 1, // the first node is the root, so we ignore it
    token_count: usize = 0,
    elem_count: usize = 0,
    level: u8 = 0,

    const Self = @This();
    const term = @import("../term.zig");
    const debug = @import("../debug.zig");

    pub fn print(self: *Self) !void {
        try self.printElements(0);
    }

    fn printElements(self: *Self, index: usize) anyerror!void {
        const node_count = self.cst.nodes.items(.count)[index];

        const elem_start = self.elem_count;
        self.elem_count += node_count;
        const elems = self.cst.elements[elem_start..self.elem_count];

        for (elems) |elem| {
            term.print("\x1b[38;2;100;100;100m", .{}); // grey color
            var i: usize = 0;
            while (i < self.level) : (i += 1) {
                term.print("│ ", .{});
            }
            term.print("{}", .{term.Attr{ .reset = true }});
            switch (elem) {
                .token => {
                    const kind = self.tokens[self.token_count];
                    const span = self.spans[self.token_count];
                    self.token_count += 1;
                    try debug.printToken(self.allocator, kind, span, self.src);
                    term.println("", .{});
                },
                .node => {
                    const kind = self.cst.nodes.items(.kind)[self.node_count];

                    const name = try std.fmt.allocPrint(self.allocator, "{?}", .{kind});
                    defer self.allocator.free(name);

                    term.printAttr("{s}", .{name[5..]}, .{ .col = .magenta });
                    term.println("", .{});

                    self.level += 1;
                    self.node_count += 1;
                    // recurse
                    try self.printElements(self.node_count - 1);
                    self.level -= 1;
                },
            }
        }
    }
};
