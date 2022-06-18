use crate::util::index_vec::{self, IndexVec};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct NodeId(usize);

impl index_vec::Idx for NodeId {
    fn new(idx: usize) -> Self {
        Self(idx)
    }

    fn index(self) -> usize {
        self.0
    }
}

#[derive(Debug)]
pub struct Cst {
    pub root: Node,
    pub node_map: IndexVec<Node, NodeId>,
}

#[derive(Debug, Clone)]
pub enum Element {
    Token,
    Node,
}

#[derive(Debug, Clone)]
pub struct Node {
    pub kind: NodeKind,
    pub elements: Vec<Element>,
    pub nodes: Vec<NodeId>,
    pub token_offset: u32,
}

impl Node {
    pub fn new(kind: NodeKind, token_offset: u32) -> Self {
        Self {
            kind,
            elements: vec![],
            nodes: vec![],
            token_offset,
        }
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
