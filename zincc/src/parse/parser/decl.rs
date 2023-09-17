use super::{
    expr::{parse_expr, parse_expr_block},
    ty::parse_ty,
    Parser,
};
use crate::{
    parse::cst::{NodeId, NodeKind},
    TK,
};

pub fn parse_decl(p: &mut Parser, parent: NodeId) {
    let decl = p.node(NodeKind::decl).parent(parent);

    decl.expect(TK![::]);
    decl.eat_newlines();

    match p.peek() {
        TK![ident] => {
            decl.eat_current();
            decl.eat_newlines();

            match p.peek() {
                TK![fn] => {
                    parse_decl_func(p, *decl);
                }
                _ => p.report(|r| r.unimpl().message("TODO other decl types")),
            }
        }
        _ => p.report(|r| r.error().message("expected ident after '::'")),
    }
}

fn parse_decl_func(p: &mut Parser, parent: NodeId) {
    let func = p.node(NodeKind::decl_func).parent(parent);

    func.expect(TK![fn]);
    func.eat_newlines();

    {
        let sig = p.node(NodeKind::decl_func_sig).parent(*func);

        if sig.eat(TK!['(']) {
            let params = p.node(NodeKind::paramsList).parent(*sig);
            params.eat_newlines();

            while !p.at(TK![')']) {
                {
                    match p.peek() {
                        TK![ident] => {
                            let param = p.node(NodeKind::param_param).parent(*params);

                            param.expect(TK![ident]);
                            param.eat_newlines();

                            if p.at(TK![ident]) && p.at_next(TK![:]) {
                                let internal =
                                    p.node(NodeKind::param_param_namedInternal).parent(*param);
                                internal.eat_current();
                                internal.eat_current();
                            }

                            if p.at(TK![:]) {
                                let named = p.node(NodeKind::param_param_named).parent(*param);
                                named.eat_current();
                            }

                            {
                                let ty = p.node(NodeKind::param_param_ty).parent(*param);
                                parse_ty(p, *ty);
                            }

                            if p.at_skipping_newlines(TK![=]) {
                                let default = p.node(NodeKind::param_param_default).parent(*param);

                                default.eat_newlines();
                                default.eat_current();
                                default.eat_newlines();

                                parse_expr(p, *default);
                            }
                        }
                        TK![self] => {
                            let param = p.node(NodeKind::param_self).parent(*params);
                            param.eat_current();
                        }
                        _ => todo!(),
                    }
                }

                if params.eat(TK![,]) || params.eat(TK![newline]) {
                    params.eat_newlines();
                } else {
                    break;
                }
            }

            params.expect(TK![')']);
            params.eat_newlines();
        }

        if !p.at_one_of(&[TK!['{'], TK![=>], TK![;]]) {
            let ret = p.node(NodeKind::decl_func_sig_ret).parent(*sig);
            parse_ty(p, *ret)
        }
    }

    match p.peek() {
        TK![;] => {
            func.eat_current();
        }
        TK![=>] => {
            let body = p.node(NodeKind::decl_func_body).parent(*func);

            body.eat_current();
            body.eat_newlines();

            parse_expr(p, *body);
            body.expect(TK![;]);
        }
        TK!['{'] => {
            let body = p.node(NodeKind::decl_func_body).parent(*func);
            let _ = parse_expr_block(p).parent(*body);
        }
        _ => todo!(),
    }
}
