use crate::{define_idx, util::index::IndexVec2};

use super::{TokenIndex, TokenKind, Tokens};

define_idx! { pub struct NodeId: u32 != 0 }

#[derive(Debug)]
pub struct Cst {
    nodes: IndexVec2<NodeId, NodeKind, Vec<Element>>,
    tokens: Tokens,
}

impl Cst {
    pub fn new(tokens: Tokens) -> Self {
        let mut nodes = IndexVec2::new();
        let _ = nodes.push(NodeKind::root, Vec::new());
        Self { nodes, tokens }
    }

    pub fn root(&self) -> NodeId {
        crate::util::index::Idx::new(0)
    }

    pub fn tokens(&self) -> &Tokens {
        &self.tokens
    }

    pub fn set_kind(&mut self, node: NodeId, kind: NodeKind) {
        *self.nodes.get_a_mut(node).unwrap() = kind;
    }

    pub fn kind(&self, node: NodeId) -> NodeKind {
        *self.nodes.get_a(node).unwrap()
    }

    pub fn elements(&self, node: NodeId) -> impl Iterator<Item = &Element> {
        self.nodes.get_b(node).unwrap().iter()
    }

    pub fn token_indices(&self, node: NodeId) -> impl Iterator<Item = &TokenIndex> {
        self.elements(node).filter_map(|e| e.as_token())
    }

    pub fn tokens_with_indices(
        &self,
        node: NodeId,
    ) -> impl Iterator<Item = (TokenKind, TokenIndex)> + '_ {
        self.token_indices(node)
            .map(|&idx| (*self.tokens.get_a(idx).unwrap(), idx))
    }

    pub fn node_indices(&self, node: NodeId) -> impl Iterator<Item = &NodeId> {
        self.elements(node).filter_map(|e| e.as_node())
    }

    pub fn nodes_with_indices(
        &self,
        node: NodeId,
    ) -> impl Iterator<Item = (NodeKind, NodeId)> + '_ {
        self.node_indices(node)
            .map(|&idx| (*self.nodes.get_a(idx).unwrap(), idx))
    }

    pub fn alloc(&mut self) -> NodeId {
        self.nodes.push(NodeKind::unknown, Vec::new())
    }

    fn push_element(&mut self, node: NodeId, element: Element) {
        self.nodes.get_b_mut(node).unwrap().push(element)
    }

    pub fn push_child_node(&mut self, node: NodeId, child: NodeId) {
        self.push_element(node, Element::Node(child))
    }

    pub fn push_child_token(&mut self, node: NodeId, token: TokenIndex) {
        self.push_element(node, Element::Token(token))
    }
}

#[derive(Debug, Clone)]
pub enum Element {
    Token(TokenIndex),
    Node(NodeId),
}

impl Element {
    pub fn as_token(&self) -> Option<&TokenIndex> {
        match self {
            Element::Token(token) => Some(token),
            Element::Node(_) => None,
        }
    }

    pub fn as_node(&self) -> Option<&NodeId> {
        match self {
            Element::Node(node) => Some(node),
            Element::Token(_) => None,
        }
    }
}

#[allow(unused)] // @TODO: Remove
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[allow(non_camel_case_types)]
#[repr(u8)]
pub enum NodeKind {
    root,
    err,
    unknown, // used in parsing for nodes for which the type is not yet known

    //
    lit_integer,
    lit_float,
    lit_boolean,
    lit_string,

    /// `path := ident ( '::' ident )*`
    path,

    /// `'::' (ident func | module | struct | enum | union) | typealias | import`
    decl,

    /// `func := sig ( ';' | body )`
    decl_func,
    /// `'fn' genericsList? paramsList? ret?`
    decl_func_sig,
    /// `ty`
    decl_func_sig_ret,
    /// `body := ( '=>' expr ) | block`
    decl_func_body,

    /// `'typealias' ident '=' ty`
    decl_typealias,

    // /// `'import' tree`
    // decl_import,
    // decl_import_tree,

    //
    /// `'fn' genericsList? paramsList? ty?`
    #[deprecated]
    func_sig,
    #[deprecated]
    func_sig_ret,

    /// `'(' param ( ( ',' | {NL} ) param )* ','? ')'`
    paramsList,
    /// `ident? ident ':'? ty default?`
    param_param,
    param_param_named,
    param_param_namedInternal,
    // /// `ident ':' ty default?`
    // param_paramNamed,
    // /// `ident ident ':' ty default?`
    // param_paramNamedExternal,
    /// `ty`
    param_param_ty,
    /// `'=' expr`
    param_param_default,
    /// `self`
    param_self,

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
    // /// `'(' pattern ( ( ',' | {NL} ) pattern )* ','? ')'`
    // pattern_tuple,

    //
    /// `'{' ( expr ( ';' | {NL} ) )* end? '}'
    expr_block,
    /// `expr`
    expr_block_end,

    /// `'(' expr ')'`
    expr_grouping,

    /// `op expr`
    expr_prefix,
    /// `'!' | '-' | '&'`
    expr_prefix_op,

    /// `'let' ident ty? '=' expr`
    #[deprecated]
    expr_let_basic,
    /// `'let' pattern ty? '=' expr`
    #[deprecated]
    expr_let_pattern,
    /// `ty`
    #[deprecated]
    expr_let_ty,

    /// `'set' expr '=' expr`
    #[deprecated]
    expr_set,

    //
    /// `expr op expr`
    ///
    expr_infix,
    /// ```text
    /// | '+' | '-' | '*' | '/' | '='
    /// | '==' | '!='
    /// | '>' | '<' | '>=' | '<='
    /// | '||' | '&&'
    /// ```
    expr_infix_op,

    /// `expr '(' args? ')'`
    expr_call,
    /// `expr ( ( ',' | {NL} ) expr ) ','?`
    expr_call_args,
    /// `ident = expr`
    expr_call_arg_named,

    //
    /// `expr '.' expr`
    expr_field,

    /// `'[]' ty`
    ty_slice,
    /// `'[' expr ']' ty `
    ty_array,
    /// `'?' ty`
    ty_nullable,
}
