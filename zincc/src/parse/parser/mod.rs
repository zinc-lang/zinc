use self::parse::parse_file;
use super::{
    cst::{Cst, NodeId, NodeKind},
    TokenIndex, TokenKind, Tokens,
};
use crate::{
    report::{Label, Report},
    util::index::Idx,
    TK,
};
use std::ops::{Deref, Range};

mod decl;
mod expr;
mod parse;
mod ty;

pub fn parse(tokens: Tokens) -> (Cst, Vec<Report>) {
    let mut parser = Parser::new(tokens);
    parse_file(&mut parser);
    (parser.cst, parser.reports)
}

// pub fn parse(map: &mut SourceMap, file_id: SourceFileId, lex_data: LexData) -> Vec<Report> {
//     let (cst, reports) = {
//         let mut parser = Parser::new(lex_data);
//         parse_file(&mut parser);
//         (parser.cst, parser.reports)
//     };

//     let file = &mut map[file_id];
//     file.set_cst(cst);

//     reports.into_iter().map(|r| r.file(file_id)).collect()
// }

#[derive(Debug)]
pub struct Parser<'s> {
    cursor: usize,
    cst: Cst,

    // In panic mode when this is some
    error_node: Option<ParseNode<'s>>,
    reports: Vec<Report>,
}

// @TODO: Add more recovery options
const RECOVERY_TOKENS: &[TokenKind] = &[TK![::], TK![eof]];

impl<'s> Parser<'s> {
    fn new(tokens: Tokens) -> Self {
        Self {
            cursor: 0,
            cst: Cst::new(tokens),
            error_node: None,
            reports: Vec::new(),
        }
    }

    #[track_caller]
    pub fn node(&mut self, kind: NodeKind) -> ParseNode<'s> {
        let creation_site = std::panic::Location::caller();

        let node = self.cst.alloc();
        ParseNode {
            parser: self as *mut _,
            kind,
            node,
            parent: None,
            creation_site,
        }
    }

    fn add_current_token(&mut self, node: NodeId) {
        self.cst
            .push_child_token(node, TokenIndex::new(self.cursor));
        self.cursor += 1;
    }

    pub fn push_token(&mut self, node: NodeId) {
        while self
            .cst
            .tokens()
            .get(TokenIndex::new(self.cursor))
            .unwrap()
            .0
            .is_trivia()
        {
            self.add_current_token(node);
        }
        self.add_current_token(node);
    }

    pub fn prev_token_range(&self) -> &Range<usize> {
        self.cst
            .tokens()
            .get(TokenIndex::new(self.cursor - 1))
            .unwrap()
            .1
    }

    #[track_caller]
    #[allow(unused_mut)]
    pub fn report(&mut self, mut func: impl FnMut(Report) -> Report) {
        let node = if let Some(node) = self.error_node.as_ref() {
            node
        } else {
            // If there is no node already, report and create an err node
            // self.reports.push(func(Report::new()).maybe_set_span(|| {
            //     // self.cst.ranges[self.cursor - 1].clone()
            //     self.cst
            //         .tokens
            //         .get(TokenIndex::new(self.cursor - 1))
            //         .unwrap()
            //         .1
            //         .clone()
            // }));
            let range = self.prev_token_range();
            self.reports.push(func(Report::new().offset(range.start)));

            let node = self.node(NodeKind::err).parent(self.cst.root());
            self.error_node = Some(node);
            self.error_node.as_ref().unwrap()
        };

        if self.at_one_of(RECOVERY_TOKENS) {
            // Once we meet a recovery token complete the error node
            drop(self.error_node.take());
        } else {
            // Until we meet a recovery option we add the tokens to the error node
            node.eat_current();
        }
    }
}

impl Parser<'_> {
    fn peek_with_predicate(
        &self,
        mut skip_if: impl FnMut(TokenKind) -> bool,
    ) -> (usize, TokenKind) {
        let mut seek = 0;
        loop {
            // trace!(index = self.cursor + seek);
            // let tk = self.cst.tokens[self.cursor + seek];
            let tk = *self
                .cst
                .tokens()
                .get(TokenIndex::new(self.cursor + seek))
                .unwrap()
                .0;
            if skip_if(tk) {
                seek += 1;
            } else {
                return (self.cursor + seek, tk);
            }
        }
    }

    #[inline]
    pub fn peek(&self) -> TokenKind {
        self.peek_with_predicate(|tk| tk.is_trivia()).1
    }

    pub fn peek_next(&self) -> TokenKind {
        let mut seen = false;
        self.peek_with_predicate(|tk| {
            let trivia = tk.is_trivia();
            if !trivia {
                if seen {
                    return false;
                } else {
                    seen = true;
                    return true;
                }
            }
            trivia
        })
        .1
    }

    #[inline]
    pub fn peek_skipping_newlines(&self) -> TokenKind {
        self.peek_with_predicate(|tk| tk.is_trivia() || tk == TK![newline])
            .1
    }

    #[inline]
    pub fn at(&self, tk: TokenKind) -> bool {
        self.peek() == tk
    }

    pub fn at_next(&self, tk: TokenKind) -> bool {
        self.peek_next() == tk
    }

    #[inline]
    pub fn at_skipping_newlines(&self, tk: TokenKind) -> bool {
        self.peek_skipping_newlines() == tk
    }

    pub fn at_one_of(&self, one_of: &[TokenKind]) -> bool {
        for tk in one_of {
            if self.at(*tk) {
                return true;
            }
        }
        false
    }

    #[inline]
    pub fn eat(&mut self, tk: TokenKind, node: NodeId) -> bool {
        if self.at(tk) {
            self.push_token(node);
            true
        } else {
            false
        }
    }

    #[inline]
    pub fn eat_newlines(&mut self, node: NodeId) {
        while self.eat(TK![newline], node) {}
    }
}

#[derive(Debug)]
pub struct ParseNode<'s> {
    parser: *mut Parser<'s>,
    kind: NodeKind,
    node: NodeId,
    parent: Option<NodeId>,
    creation_site: &'static std::panic::Location<'static>,
}

impl Deref for ParseNode<'_> {
    type Target = NodeId;

    fn deref(&self) -> &Self::Target {
        &self.node
    }
}

impl Drop for ParseNode<'_> {
    fn drop(&mut self) {
        if let Some(parent) = self.parent {
            assert!(parent != self.node);
            debug_assert!(!self.parser.is_null());
            let parser = unsafe { &mut *self.parser };
            parser.cst.push_child_node(parent, self.node);
            parser.cst.set_kind(self.node, self.kind);
        } else {
            error!(
                kind = ?self.kind,
                "Fatal: ParseNode has no parent at the time of it's drop",
            );
            error!(
                "Created at {}:{}:{}",
                self.creation_site.file(),
                self.creation_site.line(),
                self.creation_site.column()
            );

            if std::thread::panicking() {
                error!("Note: Drop triggered during a panic");
            } else {
                panic!("Node has no parent at time of drop");
            }
        }
    }
}

impl ParseNode<'_> {
    #[inline]
    #[must_use]
    pub fn parent(mut self, parent: NodeId) -> Self {
        debug_assert!(self.parent.is_none());
        self.parent = Some(parent);
        self
    }

    #[inline]
    pub fn eat_current(&self) {
        let p = unsafe { &mut *self.parser };
        p.push_token(self.node);
    }

    #[inline]
    pub fn eat(&self, tk: TokenKind) -> bool {
        let p = unsafe { &mut *self.parser };
        p.eat(tk, self.node)
    }

    #[inline]
    pub fn eat_newlines(&self) {
        let p = unsafe { &mut *self.parser };
        p.eat_newlines(self.node)
    }

    #[track_caller]
    pub fn expect(&self, tk: TokenKind) {
        if !self.eat(tk) {
            let p = unsafe { &mut *self.parser };
            let found = p.peek();

            let range = p.prev_token_range().clone();
            p.report(|r| {
                r.error()
                    .message(format!("Expected '{:?}' but got '{:?}'", tk, found))
                    .label(Label::new().range(range.clone()).message("after here"))
            });
        }
    }
}
