#[derive(Debug, Clone)]
pub enum Element {
    Node(Node),
    Token(u32),
}

// #[derive(Debug, Clone)]
#[derive(Clone)]
pub struct Node {
    pub kind: NodeKind,
    pub children: Vec<Element>,
}

impl std::fmt::Debug for Node {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Node").finish()
    }
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

    pub fn tokens(&self) -> Vec<u32> {
        self.children
            .iter()
            .filter_map(|s| match s {
                Element::Token(t) => Some(*t),
                _ => None,
            })
            .collect()
    }

    pub fn nodes<'c>(&'c self) -> Vec<&'c Node> {
        self.children
            .iter()
            .filter_map(|s| match s {
                Element::Node(n) => Some(n),
                _ => None,
            })
            .collect()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[allow(non_camel_case_types)]
#[repr(u8)]
pub enum NodeKind {
    root,
    skipped,

    path,
    string,
    block,

    /// For let and const
    binding_ty,

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
