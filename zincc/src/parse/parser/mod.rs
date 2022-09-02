use super::{
    cst::{self, NodeId, NodeKind, NK},
    TokenKind, TK,
};
use crate::source_map::{SourceFileId, SourceMap};
use std::ops::Deref;

mod decl;
mod expr;
mod parse;
mod pattern;
mod ty;

use {std::cell::RefCell, std::os::raw::c_void, std::ptr::null};

thread_local! {
    static PARSER_PTR: RefCell<*const c_void> = RefCell::new(null());
}

unsafe fn get_thread_parser<'s>() -> &'static mut Parser<'s> {
    &mut *(PARSER_PTR.with(|ptr| *ptr.borrow() as *const Parser as *mut Parser))
}

pub fn parse(map: &mut SourceMap, file_id: SourceFileId) -> Vec<ParseError> {
    debug_assert!(map.lex_data.contains_key(&file_id));
    debug_assert!(!map.csts.contains_key(&file_id));

    let mut parser = Parser {
        tokens: &map.lex_data[&file_id].tokens,

        // panicking: false,
        cursor: 0,

        errors: Vec::new(),

        // node_map: Default::default(),
        cst: Default::default(),
    };

    PARSER_PTR.with(|ptr| {
        assert!(
            ptr.borrow().is_null(),
            "There can only be one parser per thread"
        );

        let mut ptr = ptr.borrow_mut();
        *ptr = &parser as *const Parser as *const _;
    });

    // let root = parser.parse_top_level();
    parser.parse_top_level();

    PARSER_PTR.with(|ptr| {
        let mut ptr = ptr.borrow_mut();
        *ptr = null();
    });

    // let cst = cst::Cst {
    //     // root,
    //     // map: parser.node_map,
    // };
    // map.csts.insert(file_id, cst);
    map.csts.insert(file_id, parser.cst);

    parser.errors
}

struct Parser<'s> {
    tokens: &'s [TokenKind],

    // panicking: bool,
    cursor: usize,

    errors: Vec<ParseError>,

    // node_map: cst::NodeMap,
    cst: cst::Cst,
}

#[derive(Debug)]
pub enum ParseError {
    _TODO,
}

#[derive(Debug, Clone)]
struct PNode {
    kind: NodeKind,
    node: NodeId,
    // Sometimes the parent is not known at time of creation
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
        let parser = unsafe { get_thread_parser() };
        parser.cst.elements[self.node.index()]
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
        let parser = unsafe { get_thread_parser() };
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

impl Drop for PNode {
    fn drop(&mut self) {
        let parent = self
            .parent
            .expect("PNode does not have parent at time of drop");
        assert!(parent != self.node);

        let parser = unsafe { get_thread_parser() };
        parser.cst.elements[parent.index()].push(cst::Element::Node(self.node));
        parser.cst.kinds[parent.index()] = self.kind;
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
        let node = self.cst.alloc();
        PNode {
            kind,
            node,
            parent: Some(parent),
        }
    }

    // pnode_no_parent
    fn pnode_np(&mut self, kind: NK) -> PNode {
        let node = self.cst.alloc();
        PNode {
            kind,
            node,
            parent: None,
        }
    }
}

// /// Error ops
// impl Parser<'_> {
//     fn report(&mut self,) {}
// }
