use super::{expr::parse_expr, parse::parse_path, Parser};
use crate::{
    parse::cst::{NodeId, NodeKind},
    report::Label,
    TK,
};

pub fn parse_ty(p: &mut Parser, parent: NodeId) {
    match p.peek() {
        TK![ident] => {
            let _ = parse_path(p).parent(parent);
        }
        TK![?] => {
            let nullable = p.node(NodeKind::ty_nullable).parent(parent);

            nullable.eat_current();
            nullable.eat_newlines();

            parse_ty(p, *nullable);
        }
        TK!['['] => {
            if p.at_next(TK![']']) {
                let slice = p.node(NodeKind::ty_slice).parent(parent);

                slice.eat_current();
                slice.eat_current();
                slice.eat_newlines();

                parse_ty(p, *slice);
            } else {
                let array = p.node(NodeKind::ty_array).parent(parent);

                array.eat_current();
                array.eat_newlines();

                parse_expr(p, *array);

                array.eat_newlines();
                array.expect(TK![']']);
                array.eat_newlines();

                parse_ty(p, *array);
            }
        }
        _ => {
            let range = p.prev_token_range().clone();
            p.report(|r| {
                r.error()
                    .message("expected type")
                    .label(Label::new().range(range.clone()).message("after here"))
            })
        }
    }
}
