use super::{
    AstFile, Decl, DeclFunctionParam, Expr, ExprBlock, ExprCall, ExprKind, Ident, Path,
    PathSegment, Ty, TyKind, P,
};
use crate::{
    ast::{DeclFunction, DeclKind, ExprCallArg, ExprInfix, ExprPrefix, Literal, LiteralKind},
    parse::{
        cst::{Cst, NodeId, NodeKind},
        TokenIndex,
        TokenKind,
    },
    source_map::{SourceFileId, SourceMap},
    util::index::StringInterningVec,
    TK,
};
use thin_vec::ThinVec;

pub struct File<'s> {
    pub source: &'s str,
    pub cst: &'s Cst,
}

impl<'s> File<'s> {
    pub fn of(file_id: SourceFileId, source_map: &'s SourceMap) -> Self {
        let file = &source_map[file_id];
        let source = file.source();
        let cst = file.cst();
        Self { source, cst }
    }

    pub fn find_node(&self, node: NodeId, nk: NodeKind) -> Option<NodeId> {
        let find = nk;
        self.cst
            .nodes_with_indices(node)
            .find_map(|(nk, idx)| if nk == find { Some(idx) } else { None })
    }

    pub fn find_token(&self, node: NodeId, tk: TokenKind) -> Option<TokenIndex> {
        let find = tk;
        self.cst
            .tokens_with_indices(node)
            .find_map(|(tk, idx)| if tk == find { Some(idx) } else { None })
    }

    pub fn find_ident(&self, node: NodeId) -> Option<(&'s str, TokenIndex)> {
        self.find_token(node, TokenKind::ident).map(|idx| {
            let str = &self.source[self.cst.tokens().get_b(idx).unwrap().clone()];
            (str, idx)
        })
    }

    pub fn get_only_child_node(&self, node: NodeId) -> Option<NodeId> {
        let mut nodes = self.cst.node_indices(node);
        let ret = nodes.next();
        if nodes.next().is_some() {
            return None;
        }
        ret.cloned()
    }

    pub fn get_only_child_token(&self, node: NodeId) -> Option<TokenIndex> {
        let mut tokens = self.cst.tokens_with_indices(node).filter_map(|(tk, idx)| {
            if tk.is_trivia() || tk == TK![newline] {
                None
            } else {
                Some(idx)
            }
        });
        let ret = tokens.next();
        if tokens.next().is_some() {
            return None;
        }
        ret
    }
}

pub struct AstGen {
    pub strings: StringInterningVec,
}

impl AstGen {
    pub fn new(strings: StringInterningVec) -> Self {
        Self { strings }
    }

    fn find_ident(&mut self, file: &File, node: NodeId) -> Option<Ident> {
        file.find_ident(node).map(|(str, idx)| Ident {
            name: self.strings.str_get_or_intern(str),
            token_index: idx,
        })
    }

    fn gen_ident(&mut self, file: &File, index: TokenIndex) -> Ident {
        let str = &file.source[file.cst.tokens().get_b(index).unwrap().clone()];
        let name = self.strings.str_get_or_intern(str);
        Ident {
            name,
            token_index: index,
        }
    }

    pub fn generate_ast_file(&mut self, file_id: SourceFileId, source_map: &SourceMap) -> AstFile {
        let file = File::of(file_id, source_map);

        let decls = self.gen_decls(&file, file.cst.root());

        AstFile { file_id, decls }
    }

    fn gen_decls(&mut self, file: &File, node: NodeId) -> ThinVec<P<Decl>> {
        let mut decls = ThinVec::new();

        for (kind, node_id) in file.cst.nodes_with_indices(node) {
            match kind {
                NodeKind::decl => {
                    let decl = self.gen_decl(file, node_id);
                    decls.push(decl);
                }
                _ => {
                    warn!("seeing node {kind:?} when looking for decls in gen_decls");
                }
            }
        }

        decls
    }

    fn gen_decl(&mut self, file: &File, node: NodeId) -> P<Decl> {
        let decl_node_id = node;

        let (kind, node_id) = file
            .cst
            .nodes_with_indices(decl_node_id)
            .next()
            .expect("decl should have node child");

        let kind = match kind {
            NodeKind::decl_func => {
                let func_node_id = node_id;
                let ident = self
                    .find_ident(file, decl_node_id)
                    .expect("decl should have ident");

                let (params, return_ty) = file
                    .find_node(func_node_id, NodeKind::decl_func_sig)
                    .map(|sig| {
                        let params = file.find_node(sig, NodeKind::paramsList).map(|node| {
                            file.cst
                                .nodes_with_indices(node)
                                .map(|(kind, node)| match kind {
                                    NodeKind::param_param => {
                                        let ident = self
                                            .find_ident(file, node)
                                            .expect("param should have ident");

                                        // let named = file.find_node(node, NodeKind::param_param_named).is_some();
                                        let (named, internal_name) = if let Some(node) = file.find_node(node, NodeKind::param_param_namedInternal) {
                                            let name = self.find_ident(file, node).expect("");
                                            (true, Some(name))
                                        } else {
                                            (file.find_node(node, NodeKind::param_param_named).is_some(), None)
                                        };

                                        let ty = self.gen_ty(
                                            file,
                                            file.get_only_child_node(
                                                file.find_node(node, NodeKind::param_param_ty)
                                                .expect("param should have type node"),
                                            )
                                            .expect("param type should have single child node")
                                        );

                                        let default = file
                                            .find_node(node, NodeKind::param_param_default)
                                            .map(|node| {
                                                self.gen_expr(
                                                    file,
                                                    file.get_only_child_node(node)
                                                        .expect("param default should have single child node")
                                                )
                                            });
                                        
                                        let (ident, external_name) = internal_name
                                            .map(|internal| (internal, Some(ident.clone())))
                                            .unwrap_or_else(|| (ident, None));

                                        P(DeclFunctionParam {
                                            ident,
                                            named,
                                            external_name,
                                            ty,
                                            default,
                                        })
                                    }
                                    NodeKind::param_self => todo!(),
                                    _ => todo!(),
                                })
                                .collect::<ThinVec<_>>()
                        });

                        let ret_ty = file
                            .find_node(sig, NodeKind::decl_func_sig_ret)
                            .map(|node| self.gen_ty(file, node));

                        (params, ret_ty)
                    })
                    .unwrap_or((None, None));

                let body = file
                    .find_node(func_node_id, NodeKind::decl_func_body)
                    .map(|node| {
                        self.gen_expr(
                            file,
                            file.get_only_child_node(node)
                                .expect("body of function should have one child"),
                        )
                    });

                DeclKind::Function(DeclFunction {
                    ident,
                    params,
                    return_ty,
                    body,
                })
            }
            _ => {
                todo!()
            }
        };

        P(Decl { node, kind })
    }

    fn gen_expr(&mut self, file: &File, node: NodeId) -> P<Expr> {
        let kind = match file.cst.kind(node) {
            NodeKind::path => {
                let path = self.gen_path(file, node);
                ExprKind::Path(path)
            }

            NodeKind::expr_block => {
                let mut decls = ThinVec::new();
                let mut exprs = ThinVec::new();

                for (kind, node_id) in file.cst.nodes_with_indices(node) {
                    match kind {
                        NodeKind::expr_block_end => break,
                        NodeKind::decl => decls.push(self.gen_decl(file, node_id)),
                        _ => exprs.push(self.gen_expr(file, node_id)),
                    }
                }

                let end = file.find_node(node, NodeKind::expr_block_end).map(|node| {
                    self.gen_expr(
                        file,
                        file.get_only_child_node(node)
                            .expect("end of block should have one child"),
                    )
                });

                ExprKind::Block(ExprBlock {
                    node,
                    exprs,
                    decls,
                    end,
                })
            }

            NodeKind::expr_grouping => {
                let node = file
                    .get_only_child_node(node)
                    .expect("grouping should have one child");

                ExprKind::Grouping(self.gen_expr(file, node))
            }

            NodeKind::expr_prefix => {
                let mut nodes = file.cst.node_indices(node);

                let op = *nodes.next().expect("prefix expr should have op");
                let rhs = *nodes.next().expect("prefix expr should have rhs");

                debug_assert!(nodes.next().is_none());
                debug_assert!(file.cst.kind(op) == NodeKind::expr_prefix_op);

                let rhs = self.gen_expr(file, rhs);

                let op = file
                    .cst
                    .tokens_with_indices(op)
                    .filter_map(|(tk, idx)| match tk {
                        TK![newline] | TK![string open] | TK![string close] => None,
                        _ if tk.is_trivia() => None,
                        _ => Some(idx),
                    })
                    .collect::<ThinVec<_>>();

                ExprKind::Prefix(ExprPrefix { node, op, rhs })
            }

            NodeKind::expr_infix => {
                let mut nodes = file.cst.node_indices(node);

                let lhs = *nodes.next().expect("infix expr should have lhs");
                let op = *nodes.next().expect("infix expr should have op");
                let rhs = *nodes.next().expect("infix expr should have rhs");

                debug_assert!(nodes.next().is_none());
                debug_assert!(file.cst.kind(op) == NodeKind::expr_infix_op);

                let lhs = self.gen_expr(file, lhs);
                let rhs = self.gen_expr(file, rhs);

                let op = file
                    .cst
                    .tokens_with_indices(op)
                    .filter_map(|(tk, idx)| {
                        if tk.is_trivia() || tk == TK![newline] {
                            None
                        } else {
                            Some(idx)
                        }
                    })
                    .collect::<ThinVec<_>>();

                ExprKind::Infix(ExprInfix { node, lhs, rhs, op })
            }

            NodeKind::expr_call => {
                let callee = file
                    .cst
                    .node_indices(node)
                    .next()
                    .cloned()
                    .expect("call expr should have callee node");
                let callee = self.gen_expr(file, callee);

                let args = file.find_node(node, NodeKind::expr_call_args).map(|node| {
                    file.cst
                        .nodes_with_indices(node)
                        .map(|(kind, node)| {
                            let (expr, named) = if kind == NodeKind::expr_call_arg_named {
                                let name = self
                                    .find_ident(file, node)
                                    .expect("named should have ident");

                                let expr = self.gen_expr(
                                    file,
                                    file.get_only_child_node(node)
                                        .expect("named should have one child node"),
                                );

                                (expr, Some(name))
                            } else {
                                let expr = self.gen_expr(file, node);
                                (expr, None)
                            };
                            P(ExprCallArg { node, expr, named })
                        })
                        .collect::<ThinVec<_>>()
                });

                ExprKind::Call(ExprCall { node, callee, args })
            }

            NodeKind::lit_integer => {
                let lit_tk = file
                    .get_only_child_token(node)
                    .expect("integer literal should have single token child");

                let str = &file.source[file.cst.tokens().get_b(lit_tk).cloned().unwrap()];
                let symbol = self.strings.str_get_or_intern(str);

                ExprKind::Literal(Literal {
                    node,
                    kind: LiteralKind::Integer(symbol),
                })
            }
            NodeKind::lit_float => {
                let lit_tk = file
                    .get_only_child_token(node)
                    .expect("float literal should have single token child");

                let str = &file.source[file.cst.tokens().get_b(lit_tk).cloned().unwrap()];
                let symbol = self.strings.str_get_or_intern(str);

                ExprKind::Literal(Literal {
                    node,
                    kind: LiteralKind::Float(symbol),
                })
            }
            NodeKind::lit_boolean => {
                let lit_tk = file
                    .get_only_child_token(node)
                    .expect("float literal should have single token child");

                let bool = match file.cst.tokens().get_a(lit_tk).unwrap() {
                    TK![true] => true,
                    TK![false] => false,
                    _ => todo!(),
                };

                ExprKind::Literal(Literal {
                    node,
                    kind: LiteralKind::Bool(bool),
                })
            }
            NodeKind::lit_string => {
                let tokens = file
                    .cst
                    .tokens_with_indices(node)
                    .filter_map(|(tk, idx)| {
                        if tk.is_trivia() || tk == TK![newline] {
                            None
                        } else {
                            Some(idx)
                        }
                    })
                    .collect::<ThinVec<_>>();

                ExprKind::Literal(Literal {
                    node,
                    kind: LiteralKind::String(tokens),
                })
            }

            _ => ExprKind::Err,
        };

        P(Expr { node, kind })
    }

    fn gen_ty(&mut self, file: &File, node: NodeId) -> P<Ty> {
        let kind = match file.cst.kind(node) {
            NodeKind::path => TyKind::Path(self.gen_path(file, node)),
            NodeKind::ty_slice => {
                let ty = self.gen_ty(
                    file,
                    file.get_only_child_node(node)
                        .expect("slice ty should have single node child"),
                );
                TyKind::Slice(ty)
            }
            NodeKind::ty_array => {
                let mut nodes = file.cst.node_indices(node);

                let expr = *nodes.next().expect("array ty should have expr node");
                let ty = *nodes.next().expect("array ty should have ty node");

                debug_assert!(nodes.next().is_none());

                let expr = self.gen_expr(file, expr);
                let ty = self.gen_ty(file, ty);

                TyKind::Array(ty, expr)
            }
            NodeKind::ty_nullable => {
                let ty = self.gen_ty(
                    file,
                    file.get_only_child_node(node)
                        .expect("slice ty should have single node child"),
                );
                TyKind::Nullable(ty)
            }
            _ => TyKind::Err,
        };

        P(Ty { node, kind })
    }

    fn gen_path(&mut self, file: &File, node: NodeId) -> Path {
        debug_assert!(file.cst.kind(node) == NodeKind::path);

        let mut segments = ThinVec::new();

        for (kind, index) in file.cst.tokens_with_indices(node) {
            if kind.is_trivia() || kind == TK![newline] {
                continue;
            }

            match kind {
                TK![ident] => {
                    let ident = self.gen_ident(file, index);
                    let segment = PathSegment { ident };
                    segments.push(segment);
                }
                TK![::] => {}
                _ => todo!(),
            }
        }

        Path { node, segments }
    }
}
