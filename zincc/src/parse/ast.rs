use super::cst::NodeId;
use crate::util::index::{self, InterningIndexVec};
use std::{fmt, num::NonZeroUsize};

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct StrSym(NonZeroUsize);

impl index::Idx for StrSym {
    fn new(idx: usize) -> Self {
        Self(NonZeroUsize::new(idx + 1).unwrap())
    }

    fn index(self) -> usize {
        self.0.get() - 1
    }
}

impl fmt::Debug for StrSym {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "StrSym({})", index::Idx::index(*self))
    }
}

#[derive(Clone, Copy)]
pub struct TokId(NonZeroUsize);

impl TokId {
    pub fn new(i: usize) -> Self {
        Self(NonZeroUsize::new(i + 1).unwrap())
    }
}

impl fmt::Debug for TokId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "TokId({})", self.0.get() - 1)
    }
}

#[derive(Debug)]
pub struct AstMap {
    pub strings: InterningIndexVec<String, StrSym>,
    pub root: Root,
}

#[derive(Debug)]
pub struct Integer {
    pub tok: TokId,
    pub int: u64,
}

#[derive(Debug)]
pub struct Float {
    pub tok: TokId,
    pub float: f64,
}

#[derive(Debug)]
pub struct Ident {
    pub tok: TokId,
    pub sym: StrSym,
}

#[derive(Debug)]
pub struct AstString {
    pub id: NodeId,
    pub sym: StrSym,
}

#[derive(Debug)]
pub struct Path {
    pub id: NodeId,
    pub segments: Vec<StrSym>,
}

#[derive(Debug)]
pub struct Block {
    pub id: NodeId,
    pub stmts: Vec<Stmt>,
}

#[derive(Debug)]
pub struct Binding {
    pub id: NodeId,
    // pub mutable: bool,
    pub name: Ident,
    pub ty: Option<Ty>,
    pub expr: Box<Expr>,
}

#[derive(Debug)]
pub struct Root {
    pub id: NodeId,
    pub decls: Vec<Decl>,
}

#[derive(Debug)]
pub enum Decl {
    Func(DeclFunc),
    Const(Binding),
}

#[derive(Debug)]
pub struct DeclFunc {
    pub id: NodeId,
    pub name: Ident,
    pub ty: TyFunc,
    pub body: Expr,
}

#[derive(Debug)]
pub enum Ty {
    Named(Path),
    Func(TyFunc),
}

#[derive(Debug)]
pub struct TyFunc {
    pub id: NodeId,
    pub params: Vec<Param>,
    pub ret: Option<Box<Ty>>,
}

#[derive(Debug)]
pub struct Param {
    pub id: NodeId,
    pub ident: Option<Ident>,
    pub ty: Ty,
}

#[derive(Debug)]
pub enum Expr {
    String(AstString),
    Integer(Integer),
    Float(Float),
    Path(Path),
    Block(Block),
    Infix(ExprInfix),
    Ret(ExprRet),
}

#[derive(Debug)]
pub struct ExprInfix {
    pub id: NodeId,
    pub op: Vec<TokId>,
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
}

#[derive(Debug)]
pub struct ExprRet {
    pub id: NodeId,
    pub value: Option<Box<Expr>>,
}

#[derive(Debug)]
pub enum Stmt {
    Let(Binding),
    Expr(Expr),
    Decl(Decl),
}

pub mod gen {
    use super::super::{
        cst::{self, NK},
        TokenKind, TK,
    };
    use super::*;

    pub fn gen(
        cst: &cst::Cst,
        source: &str,
        tokens: &[TokenKind],
        ranges: &[std::ops::Range<usize>],
    ) -> AstMap {
        let mut gener = Generator::new(cst, source, tokens, ranges);
        let root = gener.gen_root();
        AstMap {
            strings: gener.strings,
            root,
        }
    }

    pub struct Generator<'s> {
        cst: &'s cst::Cst,
        source: &'s str,
        tokens: &'s [TokenKind],
        ranges: &'s [std::ops::Range<usize>],

        strings: InterningIndexVec<String, StrSym>,
    }

    impl<'s> Generator<'s> {
        fn new(
            cst: &'s cst::Cst,
            source: &'s str,
            tokens: &'s [TokenKind],
            ranges: &'s [std::ops::Range<usize>],
        ) -> Self {
            Self {
                cst,
                source,
                tokens,
                ranges,
                strings: InterningIndexVec::new(),
            }
        }

        fn gen_root(&mut self) -> Root {
            let id = self.cst.root;
            let decls = self
                .cst
                .get(id)
                .nodes()
                .iter()
                .map(|&id| self.gen_decl(id))
                .collect();

            Root { id, decls }
        }

        fn gen_ident(&mut self, i: usize) -> Ident {
            debug_assert_eq!(self.tokens[i], TK::ident);
            let tok = TokId::new(i);
            let range = &self.ranges[i as usize];
            let str = &self.source[range.start as usize..range.end as usize];
            let sym = self.strings.get_or_intern(str.to_string());
            Ident { tok, sym }
        }

        fn gen_string(&mut self, id: NodeId) -> AstString {
            debug_assert_eq!(id.kind, NK::string);

            let node = self.cst.get(id);
            debug_assert_eq!(node.nodes().len(), 0);

            let str = node.tokens()[1..][..1]
                .iter()
                .map(|&i| (i, self.tokens[i]))
                .map(|(i, tk)| match tk {
                    TokenKind::string_literal => &self.source[self.ranges[i].clone()],
                    TokenKind::esc_char => todo!(),
                    TokenKind::esc_asciicode => todo!(),
                    TokenKind::esc_unicode => todo!(),
                    TokenKind::string_open | TokenKind::string_close => unreachable!(),
                    _ => unreachable!(),
                })
                .collect();

            let sym = self.strings.get_or_intern(str);

            AstString { id, sym }
        }

        fn gen_path(&mut self, id: NodeId) -> Path {
            let node = self.cst.get(id);
            let segments = node
                .tokens()
                .iter()
                .filter_map(|&i| {
                    if self.tokens[i as usize] == TK::ident {
                        Some(i)
                    } else {
                        None
                    }
                })
                .map(|i| &self.ranges[i as usize])
                .map(|range| &self.source[range.start as usize..range.end as usize])
                .map(|str| self.strings.get_or_intern(str.to_string()))
                .collect();

            Path { id, segments }
        }

        fn gen_int(&mut self, id: NodeId) -> Integer {
            let node = self.cst.get(id);
            debug_assert_eq!(node.nodes().len(), 0);
            debug_assert_eq!(node.tokens().len(), 1);

            let tk_i = node.tokens()[0];
            let tk = self.tokens[tk_i];

            let slice = &self.source[self.ranges[tk_i].clone()];
            let str = slice.replace('_', "");

            let int = match tk {
                TokenKind::int_dec => str.parse::<u64>(),
                TokenKind::int_hex => u64::from_str_radix(&str[2..], 16),
                TokenKind::int_oct => u64::from_str_radix(&str[2..], 8),
                TokenKind::int_bin => u64::from_str_radix(&str[2..], 2),
                _ => unreachable!(),
            }
            .unwrap();

            let tok = TokId::new(tk_i);

            Integer { tok, int }
        }

        fn gen_float(&mut self, id: NodeId) -> Float {
            let node = self.cst.get(id);
            debug_assert_eq!(node.nodes().len(), 0);
            debug_assert_eq!(node.tokens().len(), 1);

            let tk_i = node.tokens()[0];
            debug_assert_eq!(self.tokens[tk_i], TK::float);

            let slice = &self.source[self.ranges[tk_i].clone()];
            let str = slice.replace('_', "");

            let float = str.parse::<f64>().unwrap();

            let tok = TokId::new(tk_i);

            Float { tok, float }
        }

        fn gen_block(&mut self, id: NodeId) -> Block {
            let stmts = self
                .cst
                .get(id)
                .nodes()
                .iter()
                .map(|&id| self.gen_stmt(id))
                .collect();
            Block { id, stmts }
        }

        fn gen_binding(&mut self, id: NodeId) -> Binding {
            debug_assert!(matches!(id.kind, NK::stmt_let | NK::decl_const));
            let node = self.cst.get(id);
            let tokens = node.tokens();

            debug_assert_eq!(tokens.len(), 4);
            debug_assert!(matches!(self.tokens[tokens[0]], TK::kw_let | TK::kw_const));
            debug_assert_eq!(self.tokens[tokens[1]], TK::ident);
            debug_assert_eq!(self.tokens[tokens[2]], TK::punct_eq);
            debug_assert_eq!(self.tokens[tokens[3]], TK::punct_semiColon);

            let name = self.gen_ident(tokens[1]);

            let nodes = node.nodes();
            debug_assert!(!nodes.is_empty() && nodes.len() <= 2);
            let ty = if nodes[0].kind == NK::binding_ty {
                Some(self.gen_ty(self.cst.get(nodes[0]).nodes()[0]))
            } else {
                None
            };

            let expr = Box::new(self.gen_expr(*nodes.last().unwrap()));

            Binding { id, name, ty, expr }
        }

        fn gen_decl(&mut self, id: NodeId) -> Decl {
            match id.kind {
                NK::decl_func => Decl::Func(self.gen_decl_func(id)),
                NK::decl_const => Decl::Const(self.gen_binding(id)),
                _ => unreachable!(),
            }
        }

        fn gen_decl_func(&mut self, id: NodeId) -> DeclFunc {
            debug_assert_eq!(id.kind, NK::decl_func);

            let node = self.cst.get(id);
            debug_assert_eq!(node.nodes().len(), 2);

            let name = self.gen_ident(node.tokens()[0]);

            let proto = node.nodes()[0];
            let body = node.nodes()[1];

            debug_assert_eq!(proto.kind, NK::func_proto);
            debug_assert_eq!(body.kind, NK::decl_func_body);

            let ty = self.gen_decl_proto(proto);
            let body = self.gen_expr(self.cst.get(body).nodes()[0]);

            DeclFunc { id, ty, name, body }
        }

        fn gen_ty(&mut self, id: NodeId) -> Ty {
            match id.kind {
                NK::path => Ty::Named(self.gen_path(id)),
                NK::func_proto => Ty::Func(self.gen_decl_proto(id)),
                _ => unreachable!(),
            }
        }

        fn gen_decl_proto(&mut self, id: NodeId) -> TyFunc {
            debug_assert_eq!(id.kind, NK::func_proto);
            let node = self.cst.get(id);

            let params = node
                .nodes()
                .iter()
                .filter(|id| id.kind == NK::func_proto_param)
                .map(|&id| {
                    let node = self.cst.get(id);
                    let ident = node.tokens().get(0).map(|s| self.gen_ident(*s));
                    let ty = self.gen_ty(node.nodes()[0]);
                    Param { id, ident, ty }
                })
                .collect();

            let ret = node
                .nodes()
                .iter()
                .find(|id| id.kind == NK::func_proto_ret)
                .map(|&id| self.cst.get(id).nodes()[0])
                .map(|id| Box::new(self.gen_ty(id)));

            TyFunc { id, params, ret }
        }

        fn gen_expr(&mut self, id: NodeId) -> Expr {
            match id.kind {
                NK::path => Expr::Path(self.gen_path(id)),
                NK::string => Expr::String(self.gen_string(id)),
                NK::block => Expr::Block(self.gen_block(id)),
                NK::literal_int => Expr::Integer(self.gen_int(id)),
                NK::literal_float => Expr::Float(self.gen_float(id)),
                NK::expr_infix => Expr::Infix(self.gen_expr_infix(id)),
                NK::expr_unit => todo!(),
                NK::expr_grouping => todo!(),
                NK::expr_tuple => todo!(),
                NK::expr_call => todo!(),
                NK::expr_return => Expr::Ret(self.gen_expr_ret(id)),
                _ => unreachable!(),
            }
        }

        fn gen_expr_ret(&mut self, id: NodeId) -> ExprRet {
            debug_assert_eq!(id.kind, NK::expr_return);
            let nodes = self.cst.get(id).nodes();

            let value = if !nodes.is_empty() {
                debug_assert_eq!(nodes.len(), 1);
                let v = self.gen_expr(nodes[0]);
                Some(Box::new(v))
            } else {
                None
            };

            ExprRet { id, value }
        }

        fn gen_expr_infix(&mut self, id: NodeId) -> ExprInfix {
            debug_assert_eq!(id.kind, NK::expr_infix);
            let nodes = self.cst.get(id).nodes();

            assert_eq!(nodes.len(), 3);

            let lhs = Box::new(self.gen_expr(nodes[0]));
            let rhs = Box::new(self.gen_expr(nodes[2]));

            debug_assert_eq!(nodes[1].kind, NK::expr_infix_op);
            let op = self
                .cst
                .get(nodes[1])
                .tokens()
                .iter()
                .cloned()
                .map(TokId::new)
                .collect();

            ExprInfix { id, op, lhs, rhs }
        }

        fn gen_stmt(&mut self, id: NodeId) -> Stmt {
            match id.kind {
                NK::stmt_let => Stmt::Let(self.gen_binding(id)),
                NK::stmt_expr => Stmt::Expr(self.gen_expr(self.cst.get(id).nodes()[0])),
                NK::stmt_decl => Stmt::Decl(self.gen_decl(self.cst.get(id).nodes()[0])),

                _ => unreachable!(),
            }
        }
    }
}
