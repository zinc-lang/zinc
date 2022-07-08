//! CST - Concrete Syntax Tree
//! Adds nothing more than structure to the existing tokens

use crate::util::index::{self, IndexVec};
use std::{fmt, num::NonZeroUsize};

// @FIXME: This might be better implemented as a map of `NodeId`s to their kinds within the `Cst` struct. Best to look into performance implications first.
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct NamedNodeId {
    pub kind: NodeKind, // u8
    pub raw: NodeId,    // NonZeroU32
}

impl fmt::Debug for NamedNodeId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "NodeId({:?}, {})",
            self.kind,
            index::Idx::index(self.raw)
        )
    }
}

impl From<NamedNodeId> for NodeId {
    fn from(id: NamedNodeId) -> Self {
        id.raw
    }
}

index::define_non_zero_u32_idx!(NodeId);

#[derive(Debug)]
pub struct Cst {
    pub root: NamedNodeId,
    pub map: IndexVec<Node, NodeId>,
}

impl Cst {
    pub fn root(&self) -> &Node {
        self.get(self.root.raw)
    }

    #[track_caller]
    pub fn get(&self, raw: impl Into<NodeId>) -> &Node {
        self.map.get(raw.into()).unwrap()
    }
}

#[derive(Debug, Clone)]
pub enum Element {
    Token(TokenIndex),
    Node(NamedNodeId),
}

#[derive(Debug, Clone, Default)]
pub struct Node {
    pub elements: Vec<Element>,
}

impl Node {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn tokens(&self) -> Vec<TokenIndex> {
        self.elements
            .iter()
            .filter_map(|e| match e {
                Element::Token(i) => Some(i),
                _ => None,
            })
            .cloned()
            .collect()
    }

    pub fn nodes(&self) -> Vec<NamedNodeId> {
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
pub struct TokenIndex(NonZeroUsize);

impl TokenIndex {
    #[inline(always)]
    pub fn new(idx: usize) -> Self {
        TokenIndex(NonZeroUsize::new(idx + 1).unwrap())
    }

    #[inline(always)]
    pub fn get(self) -> usize {
        self.0.get() - 1
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[allow(non_camel_case_types)]
#[repr(u8)]
pub enum NodeKind {
    /// Only one of these should exist, for a source file
    /// `decl*`
    root,
    /// When the parser is 'panicking' skipped tokens are put under this.
    skipped,

    /// Since all tokens need to be given a node parent, int and float literals have these nodes.
    literal_int,
    literal_float,

    /// `'::'? ident ( '::' ident )*`
    path,
    /// '"' ( char | '\' char | '\u' '{' 4xHexNumber '}' | '\x' 2xHexNumber )* '"'
    string,
    /// `'{' stmt* '}'`
    block,

    /// A 'let' or 'const' may or may not have this depending on if the user has written a type.
    /// `( ':' ty )?`
    binding_ty,

    /// The 'fn' is present in all cases except for function declarations, in which case it is before the name of the function.
    /// `'fn'? '(' param ( ',' param )* ','? ')' ret`
    func_proto,
    /// `( ident ':' )? ty`
    func_proto_param,
    /// `( ':' ty )?`
    func_proto_ret,

    /// `'fn' ident proto body`
    decl_func,
    /// `( block | '=>' expr ';' )`
    decl_func_body,

    /// `'const' ident binding_ty '=' expr ';'`
    decl_const,

    /// `'let' ident binding_ty '=' expr ';'`
    stmt_let,
    /// `expr ';'`
    stmt_expr,
    /// decl
    stmt_decl,

    /// `expr op expr`
    expr_infix,
    /// `punct*`
    expr_infix_op,

    /// `'()'`
    expr_unit,
    /// `'(' expr ')'`
    expr_grouping,
    /// `'(' expr ( ',' expr )* ','? ')'`
    expr_tuple,

    /// 'false'
    expr_false,
    /// 'true'
    expr_true,

    /// `expr tuple`
    expr_call,
    // expr_call_arg,
    /// `'return' expr`
    expr_return,

    /// `'[]' ty`
    ty_slice,
    /// `'?' ty`
    ty_nullable,
}

pub type NK = NodeKind;
