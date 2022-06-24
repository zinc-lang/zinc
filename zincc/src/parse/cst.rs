//! CST - Concrete Syntax Tree
//! Adds nothing more than structure to the existing tokens

use crate::util::index::{self, IndexVec};
use std::{fmt, num::NonZeroU32};

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct NodeId {
    pub kind: NodeKind, // u8
    pub raw: RawNodeId, // NonZeroU32
}

impl fmt::Debug for NodeId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "NodeId({:?}, {})",
            self.kind,
            index::Idx::index(self.raw)
        )
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct RawNodeId(NonZeroU32);

impl index::Idx for RawNodeId {
    fn new(idx: usize) -> Self {
        Self(NonZeroU32::new((idx + 1).try_into().unwrap()).unwrap())
    }

    fn index(self) -> usize {
        (self.0.get() - 1) as usize
    }
}

#[derive(Debug)]
pub struct Cst {
    pub root: NodeId,
    pub map: IndexVec<Node, RawNodeId>,
}

impl Cst {
    pub fn root(&self) -> &Node {
        self.get(self.root)
    }

    #[track_caller]
    pub fn get(&self, id: NodeId) -> &Node {
        self.map.get(id.raw).unwrap()
    }
}

#[derive(Debug, Clone)]
pub enum Element {
    Token(usize),
    Node(NodeId),
}

#[derive(Debug, Clone, Default)]
pub struct Node {
    pub elements: Vec<Element>,
}

impl Node {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn tokens(&self) -> Vec<usize> {
        self.elements
            .iter()
            .filter_map(|e| match e {
                Element::Token(i) => Some(i),
                _ => None,
            })
            .cloned()
            .collect()
    }

    pub fn nodes(&self) -> Vec<NodeId> {
        self.elements
            .iter()
            .filter_map(|e| match e {
                Element::Node(id) => Some(id),
                _ => None,
            })
            .cloned()
            .collect()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
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
    func_proto_param,
    func_proto_ret,

    decl_func,
    decl_func_body,

    decl_const,

    stmt_let,
    stmt_expr,
    stmt_decl,

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

pub type NK = NodeKind;
