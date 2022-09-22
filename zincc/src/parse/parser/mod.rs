use super::{
    cst::{self, NodeId, NodeKind, NK},
    TokenKind, TK,
};
use crate::{
    report::{self, Report},
    source_map::{SourceFileId, SourceMap},
};
use std::{
    cell::RefCell,
    ops::{Deref, Range},
    os::raw::c_void,
    ptr::null,
};

mod decl;
mod expr;
mod parse;
mod pattern;
mod ty;

thread_local! {
    static PARSER_PTR: RefCell<*const c_void> = RefCell::new(null());
}

unsafe fn get_thread_parser<'s>() -> &'static mut Parser<'s> {
    &mut *(PARSER_PTR.with(|ptr| *ptr.borrow() as *const Parser as *mut Parser))
}

pub fn parse(map: &mut SourceMap, file_id: SourceFileId) -> Vec<Report> {
    debug_assert!(map.lex_data.contains_key(&file_id));
    debug_assert!(!map.csts.contains_key(&file_id));

    let lex_data = &map.lex_data[&file_id];

    let tokens = lex_data
        .tokens
        .iter()
        .filter(|tok| !tok.is_trivia())
        .cloned()
        .collect::<Box<[_]>>();

    let token_indicies = lex_data
        .tokens
        .iter()
        .enumerate()
        .filter(|&(i, _)| !lex_data.tokens[i].is_trivia())
        .map(|(i, _)| cst::TokenIndex::new(i))
        .collect::<Box<[_]>>();

    let ranges = lex_data
        .ranges
        .iter()
        .enumerate()
        .filter(|&(i, _)| !lex_data.tokens[i].is_trivia())
        .map(|(_, range)| range)
        .cloned()
        .collect::<Box<[_]>>();

    // @FIXME: This would be a better way of getting the tokens but it causes rustc to crash
    // let ((token_indicies, tokens), ranges) = lex_data
    //     .tokens
    //     .iter()
    //     .enumerate()
    //     .map(|(i, kind)| (cst::TokenIndex::new(i), kind))
    //     .zip(lex_data.ranges.iter())
    //     .filter(|((_, kind), _)| !kind.is_trivia());

    let mut parser = Parser {
        token_indices: &token_indicies,
        tokens: &tokens,
        ranges: &ranges,

        cursor: 0,
        cst: Default::default(),

        panicking: false,
        reports: Vec::new(),
    };

    PARSER_PTR.with(|ptr| {
        assert!(
            ptr.borrow().is_null(),
            "There can only be one parser per thread"
        );

        let mut ptr = ptr.borrow_mut();
        *ptr = &parser as *const Parser as *const _;
    });

    parser.parse_top_level();

    PARSER_PTR.with(|ptr| {
        let mut ptr = ptr.borrow_mut();
        *ptr = null();
    });

    map.csts.insert(file_id, parser.cst);

    parser
        .reports
        .into_iter()
        .map(|rep| rep.file(file_id).build())
        .collect::<Vec<_>>()
}

struct Parser<'s> {
    token_indices: &'s [cst::TokenIndex],
    tokens: &'s [TokenKind],
    ranges: &'s [Range<usize>],

    cursor: usize,
    cst: cst::Cst,

    panicking: bool,
    reports: Vec<report::Builder>,
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
        parser
            .cst
            .push_child_token(self.node, parser.token_indices[parser.cursor]);
        parser.cursor += 1;
    }

    pub fn parent(mut self, parent: NodeId) -> Self {
        self.parent = Some(parent);
        self
    }

    pub fn peek(&mut self) -> TK {
        self.peek_n(0)
    }

    pub fn peek_index(&mut self) -> usize {
        self.peek_index_n(0)
    }

    pub fn peek_next(&mut self) -> TK {
        self.peek_n(1)
    }

    fn peek_n(&mut self, n: usize) -> TK {
        let idx = self.peek_index_n(n);
        let parser = unsafe { get_thread_parser() };
        parser.tokens[idx]
    }

    fn peek_index_n(&mut self, n: usize) -> usize {
        let parser = unsafe { get_thread_parser() };
        parser.cursor + n
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

    pub fn expect(&mut self, tk: TK) {
        trace!(?tk, "Expecting");
        if !self.eat(tk) {
            let parser = unsafe { get_thread_parser() };
            parser.report(|| {
                let peek_idx = self.peek_index();
                let peek = parser.tokens[peek_idx];
                Report::builder()
                    .error()
                    .span(parser.ranges[peek_idx - 1].clone())
                    .message(format!("expected {}, but got {}", tk.name(), peek.name()))
                    .short(format!("help: add {} after this", tk.name()))
            });
        }
    }

    pub fn expect_one_of(&mut self, set: &[TK]) {
        trace!(?set, "Expecting one of");
        if !self.eat_one_of(set) {
            let parser = unsafe { get_thread_parser() };
            parser.report(|| {
                let peek_idx = self.peek_index();
                let peek = parser.tokens[peek_idx];
                Report::builder()
                    .error()
                    .span(parser.ranges[peek_idx - 1].clone())
                    .message(format!(
                        // @TODO: format the set better
                        "expected one of '{:?}', but got {}",
                        set,
                        peek.name()
                    ))
            });
        }
    }
}

impl Drop for PNode {
    fn drop(&mut self) {
        if let Some(parent) = self.parent {
            assert!(parent != self.node);

            let parser = unsafe { get_thread_parser() };
            parser.cst.push_child_node(parent, self.node);
            parser.cst.set_kind(self.node, self.kind);
        } else if self.kind != NK::err {
            error!("PNode does not have parent at time of drop");
            // panic!();
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
    fn pnode(&mut self, kind: NK) -> PNode {
        let node = self.cst.alloc();
        PNode {
            kind,
            node,
            parent: None,
        }
    }
}

/// Error ops
impl Parser<'_> {
    fn report(&mut self, mut func: impl FnMut() -> report::Builder) {
        if !self.panicking {
            self.panicking = true;
            self.reports.push(func().maybe_set_span(|| {
                let parser = unsafe { get_thread_parser() };
                parser.ranges[parser.cursor].clone()
            }));
        }

        // @TODO: Add more recovery options
        if matches!(self.peek(), TK::punct_doubleColon | TK::eof) {
            self.panicking = false;
        } else {
            let mut err = self.pnode(NK::err);
            err.push_token();
        }
    }

    #[track_caller]
    pub fn report_unimpl(&mut self, mut func: impl FnMut() -> report::Builder) -> PNode {
        self.report(|| func().unimpl().short("not implemented"));
        let mut node = self.pnode(NK::err);
        node.push_token();
        node
    }
}
