use std::num::{NonZeroU32, NonZeroUsize};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct NodeId(NonZeroU32);

impl NodeId {
    fn new(index: usize) -> Self {
        Self(NonZeroU32::new(index as u32 + 1).unwrap())
    }

    pub fn index(self) -> usize {
        self.0.get() as usize - 1
    }
}

#[derive(Debug)]
pub struct Cst {
    pub root: NodeId,
    pub elements: Vec<Vec<Element>>,
    pub kinds: Vec<NodeKind>,
}

impl Default for Cst {
    fn default() -> Self {
        Self::new()
    }
}

impl Cst {
    pub fn new() -> Self {
        Self {
            root: NodeId::new(0),
            elements: vec![Vec::new()],
            kinds: vec![NodeKind::root],
        }
    }

    pub fn alloc(&mut self) -> NodeId {
        let id = NodeId::new(self.elements.len());
        self.elements.push(vec![]);
        self.kinds.push(NodeKind::err);
        id
    }

    // pub fn tokens(&self, node: NodeId) -> impl Iterator<Item = &TokenIndex> {
    //     self.elements[node.index()].iter().filter_map(|e| match e {
    //         Element::Token(i) => Some(i),
    //         _ => None,
    //     })
    // }

    // pub fn nodes(&self, node: NodeId) -> impl Iterator<Item = &NodeId> {
    //     self.elements[node.index()].iter().filter_map(|e| match e {
    //         Element::Node(id) => Some(id),
    //         _ => None,
    //     })
    // }
}

#[derive(Debug, Clone)]
pub enum Element {
    Token(TokenIndex),
    Node(NodeId),
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
