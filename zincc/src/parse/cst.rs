use crate::util::index::{self, IndexVec};
use std::{fmt, num::NonZeroUsize};

// @FIXME:...
// This would be better implemented as a map of `NodeId`s to their kinds within the `Cst` struct.
// Best to look into performance implications first.
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

index::define_idx! { pub struct NodeId: u32 != 0 }

pub type NodeMap = IndexVec<NodeId, Node>;

#[derive(Debug)]
pub struct Cst {
    pub root: NamedNodeId,
    pub map: NodeMap,
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

    // pub fn tokens(&self) -> Vec<TokenIndex> {
    //     self.elements
    //         .iter()
    //         .filter_map(|e| match e {
    //             Element::Token(i) => Some(i),
    //             _ => None,
    //         })
    //         .cloned()
    //         .collect()
    // }

    // pub fn nodes(&self) -> Vec<NamedNodeId> {
    //     self.elements
    //         .iter()
    //         .filter_map(|e| match e {
    //             Element::Node(id) => Some(id),
    //             _ => None,
    //         })
    //         .cloned()
    //         .collect()
    // }
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
    root,
    err,

    //
    integer,
    float,
    string,
    boolean,

    /// `path := ident ( '::' ident )*`
    path,

    /// `'::' ident | func | module | struct | enum | union`
    decl,

    /// `func := sig ( ';' | body )`
    decl_func,
    // /// `body := ( '=>' expr ) | block`
    // decl_func_body,

    //
    /// `'fn' genericsList? paramsList? ty?`
    func_sig,

    /// `'(' param ( ',' param )* ','? ')'`
    paramsList,
    /// `ident ty default?`
    param_param,
    /// `'=' expr`
    param_param_default,
    // @TODO: Named

    //
    /// `'[' genericParam ( ',' genericParam )* ','? ']'`
    genericsList,
    /// `ident ( constraint ( '+' constraint )* )? default?`
    genericParam_type,
    /// `ty`
    genericParam_type_constraint,
    /// '=' ty`
    genericParam_type_default,
    /// `ident! ty default?`
    genericParam_value,
    /// `'=' expr`
    genericParam_value_default,
    // @TODO: Named

    //
    /// `'ref'? 'mut'? ident`
    pattern_ident,
    /// `path`
    pattern_path,
    // /// `'_'`
    // pattern_wildcard,
    // /// `'(' pattern ( ',' pattern )* ','? ')'`
    // pattern_tuple,

    //
    /// `'{' ( expr ';' )* end? '}'
    expr_block,
    /// `expr`
    expr_block_end,

    /// `op expr`
    expr_prefix,
    /// `'not' | '!' | '-' | '&'`
    expr_prefix_op,

    /// `'let' pattern ty? '=' expr`
    expr_let,
    /// `ty`
    expr_let_ty,

    /// `'set' expr '=' expr`
    expr_set,

    /// `expr op expr`
    ///
    expr_infix,
    /// ```text
    /// | '+' | '-' | '*' | '/' | '='
    /// | '==' | '!='
    /// | '>' | '<' | '>=' | '<='
    /// | 'and' | 'or'
    /// ```
    // @TODO: Should we have a separate `op` for each operator?
    expr_infix_op,

    /// `expr '(' args? ')'`
    expr_call,
    // /// `expr (',' expr) ','?`
    // expr_call_args,

    //
    /// `'[]' ty`
    ty_slice,
    /// `'[' expr ']' ty`
    ty_array,
    /// `'?' ty`
    ty_nullable,
}

pub type NK = NodeKind;
