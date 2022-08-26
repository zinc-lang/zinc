use super::{
    cst::{self, NamedNodeId, NodeId, NodeKind, NK},
    TokenKind, TK,
};
use crate::source_map::{SourceFileId, SourceMap};
use std::ops::Deref;

mod decl;
mod expr;
mod parse;
mod pattern;
mod ty;

pub fn parse(map: &mut SourceMap, file_id: SourceFileId) -> Vec<ParseError> {
    // let tokens = &map.lex_data[&file_id].tokens;
    // let tokens = tokens
    //     .iter()
    //     .filter(|t| !t.is_trivia())
    //     .cloned()
    //     .collect::<Vec<_>>()
    //     .into_boxed_slice();

    let mut parser = Parser {
        // tokens: &*tokens,
        tokens: &map.lex_data[&file_id].tokens,

        // panicking: false,
        cursor: 0,

        errors: Vec::new(),

        node_map: Default::default(),
    };

    let root = parser.parse_top_level();

    let cst = cst::Cst {
        root: root.into(),
        map: parser.node_map,
    };
    map.csts.insert(file_id, cst);

    parser.errors
}

struct Parser<'s> {
    tokens: &'s [TokenKind],

    // panicking: bool,
    cursor: usize,

    errors: Vec<ParseError>,

    node_map: cst::NodeMap,
}

#[derive(Debug)]
pub enum ParseError {
    _TODO,
    // Expected(ParseErrorExpected),
}

// #[derive(Debug)]
// pub struct ParseErrorExpected {
//     pub what: ParseErrorExpectedWhat,
//     pub at: usize,
//     pub found: usize,
//     pub context: ParseContext,
// }

// #[derive(Debug)]
// pub enum ParseErrorExpectedWhat {
//     Item(ParseErrorItem),
//     Token(TK),
//     // OneOf(Vec<ParseErrorExpectedWhat>),
// }

// #[derive(Debug)]
// pub enum ParseErrorItem {
//     Decl,
//     Expr,
//     Ty,
// }

// #[derive(Debug, Clone, Copy)]
// pub enum ParseContext {
//     TopLevel,
//     // Block,
//     Decl,
//     DeclFunc,
//     // Stmt,
//     // // StmtLet,
//     // // StmtReturn,
//     // ExprStart,
//     // // ExprParen,
//     // ExprCall,
//     String,
//     Path,
//     // TyFunc,
//     // Ty,
//     // TySlice,
// }

#[derive(Debug, Clone)]
struct PNode {
    kind: NodeKind,
    node: NodeId,
    parser: *const std::os::raw::c_void,
    // this is needed as sometimes the parent is not known create time
    parent: Option<NodeId>,
}

impl Deref for PNode {
    type Target = NodeId;

    fn deref(&self) -> &Self::Target {
        &self.node
    }
}

impl PNode {
    pub fn push_token(&mut self) {
        let parser = unsafe { &mut *(self.parser as *const Parser as *mut Parser) };
        let node = &mut parser.node_map[self.node];
        node.elements
            .push(cst::Element::Token(cst::TokenIndex::new(parser.cursor)));
        parser.cursor += 1;
    }

    pub fn peek(&mut self) -> TK {
        self.peek_n(0)
    }

    pub fn peek_next(&mut self) -> TK {
        self.peek_n(1)
    }

    fn peek_n(&mut self, n: usize) -> TK {
        let parser = unsafe { &mut *(self.parser as *const Parser as *mut Parser) };
        loop {
            let tok = parser.tokens[parser.cursor + n];
            if tok.is_trivia() {
                self.push_token();
            } else {
                trace!(?tok, cursor = parser.cursor, "Peek");
                return tok;
            }
        }
    }

    pub fn at(&mut self, tk: TK) -> bool {
        self.peek() == tk
    }

    #[inline]
    pub fn at_end(&mut self) -> bool {
        self.at(TK::eof)
    }

    pub fn at_one_of(&mut self, set: &[TK]) -> bool {
        for &kind in set {
            if self.at(kind) {
                return true;
            }
        }
        false
    }

    pub fn eat(&mut self, tk: TK) -> bool {
        let at = self.at(tk);
        if at {
            self.push_token();
        }
        at
    }

    pub fn eat_one_of(&mut self, set: &[TK]) -> bool {
        let at = self.at_one_of(set);
        if at {
            self.push_token();
        }
        at
    }

    #[track_caller]
    pub fn expect(&mut self, tk: TK) {
        trace!(?tk, "Expecting");
        if !self.eat(tk) {
            todo!("expected: {:?}", tk)
        }
    }

    #[track_caller]
    pub fn expect_one_of(&mut self, set: &[TK]) {
        trace!(?set, "Expecting one of");
        if !self.eat_one_of(set) {
            todo!("expected: {:?}", set)
        }
    }
}

impl From<PNode> for NamedNodeId {
    fn from(val: PNode) -> Self {
        NamedNodeId {
            raw: val.node,
            kind: val.kind,
        }
    }
}

impl Drop for PNode {
    fn drop(&mut self) {
        if let Some(parent) = self.parent {
            assert!(parent != self.node);

            let parser = unsafe { &mut *(self.parser as *const Parser as *mut Parser) };
            let parent = &mut parser.node_map[parent];

            parent.elements.push(cst::Element::Node(NamedNodeId {
                kind: self.kind,
                raw: self.node,
            }));
        } else {
            unreachable!()
        }
    }
}

/// Token ops
impl Parser<'_> {
    fn peek(&mut self) -> TK {
        let mut seek = 0;
        loop {
            let tok = self.tokens[self.cursor + seek];
            if tok.is_trivia() {
                seek += 1;
            } else {
                trace!(?tok, index = self.cursor + seek, "Node-less peek");
                return tok;
            }
        }
    }
}

/// Node ops
impl Parser<'_> {
    fn pnode(&mut self, kind: NK, parent: NodeId) -> PNode {
        let node = self.node_map.push(cst::Node::new());
        PNode {
            kind,
            node,
            parser: self as *const Self as *const _,
            parent: Some(parent),
        }
    }

    // pnode_no_parent
    fn pnode_np(&mut self, kind: NK) -> PNode {
        let node = self.node_map.push(cst::Node::new());
        PNode {
            kind,
            node,
            parser: self as *const Self as *const _,
            parent: None,
        }
    }

    // fn node_bump(&mut self, nk: NK) -> PNode {
    //     let mut n = self.pnode_np(nk);
    //     n.push_token();
    //     n
    // }
}

/// Error ops
impl Parser<'_> {
    // fn report(&mut self, parent: &mut PNode, err: ParseError) {
    //     todo!()
    //     // if !self.panicking {
    //     //     self.panicking = true;

    //     //     self.errors.push(err);
    //     //     return;
    //     // }

    //     // if self.at_set(&[TK::brkt_brace_close, TK::eof, TK::kw_let]) {
    //     //     self.bump(parent);
    //     //     self.panicking = false;
    //     // } else {
    //     //     let mut node = self.node(NK::err);
    //     //     self.bump(&mut node);
    //     //     self.append_node(node, parent);
    //     // }
    // }

    // fn expect(&mut self, what: TK, context: ParseContext, parent: &mut PNode) {
    //     if !self.eat(what, parent) {
    //         self.report(
    //             parent,
    //             ParseError::Expected(ParseErrorExpected {
    //                 what: ParseErrorExpectedWhat::Token(what),
    //                 at: self.cursor - 1,
    //                 found: self.cursor,
    //                 context,
    //             }),
    //         )
    //     }
    // }
}
