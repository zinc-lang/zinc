use super::{decl::parse_decl, ParseNode, Parser};
use crate::{parse::cst::NodeKind, TK};

pub fn parse_file(p: &mut Parser) {
    let root = p.cst.root();
    p.eat_newlines(root);

    while !p.at(TK![eof]) {
        parse_decl(p, root);
        p.eat_newlines(root);
    }
}

pub fn parse_path<'p>(p: &mut Parser<'p>) -> ParseNode<'p> {
    let path = p.node(NodeKind::path);

    path.expect(TK![ident]);

    while path.eat(TK![::]) {
        path.expect(TK![ident]);
    }

    path
}
