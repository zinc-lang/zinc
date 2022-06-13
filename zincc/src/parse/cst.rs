#[derive(Debug, Clone)]
pub enum Element {
    Node(Node),
    Token(usize),
}

#[derive(Debug, Clone)]
pub struct Node {
    pub kind: NodeKind,
    pub children: Vec<Element>,
}

impl Node {
    pub fn new(kind: NodeKind) -> Self {
        Self {
            kind,
            children: vec![],
        }
    }

    pub fn append_node(&mut self, node: Node) {
        self.children.push(Element::Node(node));
    }
}

#[derive(Debug, Clone, Copy)]
#[allow(non_camel_case_types)]
#[repr(u8)]
pub enum NodeKind {
    root,
    skipped,

    path,
    string,
    block,

    func_proto,
    func_proto_arg,
    func_proto_ret,

    decl_func,
    decl_func_body,

    decl_const,

    stmt_let,

    literal_int,
    literal_float,

    expr_infix,
    expr_infix_op,

    expr_unit,
    expr_grouping,
    expr_tuple,

    expr_call,
    // expr_call_arg,
    expr_return,
}
