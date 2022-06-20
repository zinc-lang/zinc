use super::cst::NodeId;
use crate::util::index_vec::{self, InterningIndexVec};
use std::{fmt, num::NonZeroUsize};

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct StrSym(NonZeroUsize);

impl index_vec::Idx for StrSym {
    fn new(idx: usize) -> Self {
        Self(NonZeroUsize::new(idx + 1).unwrap())
    }

    fn index(self) -> usize {
        self.0.get() - 1
    }
}

impl fmt::Debug for StrSym {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "StrSym({})", index_vec::Idx::index(self.clone()))
    }
}

#[derive(Clone, Copy)]
pub struct TokId(NonZeroUsize);

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
    pub int: f64,
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
    Int(TyInt),
    Func(TyFunc),
}

#[derive(Debug)]
pub enum TyInt {
    Sint,
    Uint,
    UintPtr,
    // @TODO: Arbitrary sizes
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
    Infix(ExprInfix),
    Ret(ExprRet),

    None,
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

        fn gen_ident(&mut self, i: usize) -> Ident {
            let tok = TokId(NonZeroUsize::new(i + 1).unwrap());
            let range = &self.ranges[i as usize];
            let str = &self.source[range.start as usize..range.end as usize];
            let sym = self.strings.get_or_intern(str.to_string());
            Ident { tok, sym }
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

        fn gen_root(&mut self) -> Root {
            let id = self.cst.root;
            let node = self.cst.get(id);
            let decls = node.nodes().iter().map(|&id| self.gen_decl(id)).collect();

            Root { id, decls }
        }

        fn gen_decl(&mut self, id: NodeId) -> Decl {
            match id.kind {
                NK::decl_func => Decl::Func(self.gen_decl_func(id)),
                NK::decl_const => todo!(),
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

        fn gen_expr(&mut self, _id: NodeId) -> Expr {
            Expr::None
        }
    }
}
