use super::cst::NodeId;
use crate::util::index::{self, IndexVec, InterningIndexVec};
use std::{fmt, num::NonZeroU32};

pub type StrSym = index::NonZeroU32IdxRef<String>;

#[derive(Clone, Copy)]
pub struct TokId(NonZeroU32);

impl TokId {
    pub fn new(i: usize) -> Self {
        Self(NonZeroU32::new((i + 1).try_into().unwrap()).unwrap())
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
    pub decls: IndexVec<Decl, DeclId>,
    pub tys: IndexVec<Ty, TyId>,
    pub stmts: IndexVec<Stmt, StmtId>,
    pub exprs: IndexVec<Expr, ExprId>,
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
    pub stmts: Vec<StmtId>,
}

#[derive(Debug)]
pub struct Binding {
    pub id: NodeId,
    // pub mutable: bool,
    pub name: Ident,
    pub ty: Option<TyId>,
    pub expr: ExprId,
}

#[derive(Debug)]
pub struct Root {
    pub id: NodeId,
    pub decls: Vec<DeclId>,
}

pub type DeclId = index::U32IdxRef<Decl>;

#[derive(Debug)]
pub enum Decl {
    Func(DeclFunc),
    Const(Binding),
}

#[derive(Debug)]
pub struct DeclFunc {
    pub id: NodeId,
    pub name: Ident,
    pub ty: TyId, // TyFunc
    pub body: ExprId,
}

pub type TyId = index::U32IdxRef<Ty>;

#[derive(Debug)]
pub enum Ty {
    Named(Path),
    Func(TyFunc),
    Slice(NodeId, TyId),
    Nullable(NodeId, TyId),
}

#[derive(Debug)]
pub struct TyFunc {
    pub id: NodeId,
    pub params: Vec<Param>,
    pub ret: Option<TyId>,
}

#[derive(Debug)]
pub struct Param {
    pub id: NodeId,
    pub ident: Option<Ident>,
    pub ty: TyId,
}

pub type ExprId = index::U32IdxRef<Expr>;

#[derive(Debug)]
pub enum Expr {
    String(AstString),
    Integer(Integer),
    Float(Float),
    Path(Path),
    Block(Block),
    Infix(ExprInfix),
    Call(ExprCall),
    Ret(ExprRet),
    True(TokId),
    False(TokId),
}

#[derive(Debug)]
pub struct ExprInfix {
    pub id: NodeId,
    pub op: Vec<TokId>,
    pub lhs: ExprId,
    pub rhs: ExprId,
}

#[derive(Debug)]
pub struct ExprCall {
    pub id: NodeId,
    pub callee: ExprId,
    pub args: Vec<ExprId>,
}

#[derive(Debug)]
pub struct ExprRet {
    pub id: NodeId,
    pub value: Option<ExprId>,
}

pub type StmtId = index::U32IdxRef<Stmt>;

#[derive(Debug)]
pub enum Stmt {
    Let(Binding),
    Expr(ExprId),
    Decl(DeclId),
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
    ) -> (AstMap, Root) {
        let mut gen = AstGen::new(cst, source, tokens, ranges);
        let root = gen.gen_root();
        (gen.map, root)
    }

    pub struct AstGen<'s> {
        cst: &'s cst::Cst,
        source: &'s str,
        tokens: &'s [TokenKind],
        ranges: &'s [std::ops::Range<usize>],

        map: AstMap,
    }

    impl<'s> AstGen<'s> {
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
                map: AstMap {
                    strings: Default::default(),
                    decls: Default::default(),
                    tys: Default::default(),
                    stmts: Default::default(),
                    exprs: Default::default(),
                },
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
            let sym = self.map.strings.get_or_intern(str.to_string());
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

            let sym = self.map.strings.get_or_intern(str);

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
                .map(|str| self.map.strings.get_or_intern(str.to_string()))
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

            debug_assert!(matches!(self.tokens[tokens[0]], TK::kw_let | TK::kw_const));
            debug_assert_eq!(self.tokens[tokens[1]], TK::ident);

            let name = self.gen_ident(tokens[1]);

            let nodes = node.nodes();
            debug_assert!(!nodes.is_empty() && nodes.len() <= 2);
            let ty = if nodes[0].kind == NK::binding_ty {
                Some(self.gen_ty(self.cst.get(nodes[0]).nodes()[0]))
            } else {
                None
            };

            let expr = self.gen_expr(*nodes.last().unwrap());

            Binding { id, name, ty, expr }
        }

        fn gen_decl(&mut self, id: NodeId) -> DeclId {
            let decl = match id.kind {
                NK::decl_func => Decl::Func(self.gen_decl_func(id)),
                NK::decl_const => Decl::Const(self.gen_binding(id)),
                _ => unreachable!(),
            };
            self.map.decls.push(decl)
        }

        fn gen_decl_func(&mut self, id: NodeId) -> DeclFunc {
            debug_assert_eq!(id.kind, NK::decl_func);

            let node = self.cst.get(id);
            debug_assert_eq!(node.nodes().len(), 2);

            debug_assert_eq!(*self.tokens.get(node.tokens()[0]).unwrap(), TK::kw_fn);
            let name = self.gen_ident(node.tokens()[1]);

            let proto = node.nodes()[0];
            let body = node.nodes()[1];

            debug_assert_eq!(proto.kind, NK::func_proto);
            debug_assert_eq!(body.kind, NK::decl_func_body);

            let ty = self.gen_decl_proto(proto);
            let ty = self.map.tys.push(Ty::Func(ty));
            let body = self.gen_expr(self.cst.get(body).nodes()[0]);

            DeclFunc { id, ty, name, body }
        }

        fn gen_ty(&mut self, id: NodeId) -> TyId {
            let ty = match id.kind {
                NK::path => Ty::Named(self.gen_path(id)),
                NK::func_proto => Ty::Func(self.gen_decl_proto(id)),
                NK::ty_slice => Ty::Slice(id, self.gen_ty(self.cst.get(id).nodes()[0])),
                NK::ty_nullable => Ty::Nullable(id, self.gen_ty(self.cst.get(id).nodes()[0])),
                _ => unreachable!(),
            };
            self.map.tys.push(ty)
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
                .map(|id| self.gen_ty(id));

            TyFunc { id, params, ret }
        }

        fn gen_expr(&mut self, id: NodeId) -> ExprId {
            let expr = match id.kind {
                NK::path => Expr::Path(self.gen_path(id)),
                NK::string => Expr::String(self.gen_string(id)),
                NK::block => Expr::Block(self.gen_block(id)),
                NK::literal_int => Expr::Integer(self.gen_int(id)),
                NK::literal_float => Expr::Float(self.gen_float(id)),
                NK::expr_infix => Expr::Infix(self.gen_expr_infix(id)),
                NK::expr_unit => todo!(),
                NK::expr_grouping => todo!(),
                NK::expr_tuple => todo!(),
                NK::expr_call => Expr::Call(self.gen_expr_call(id)),
                NK::expr_return => Expr::Ret(self.gen_expr_ret(id)),
                NK::expr_true => Expr::True(TokId::new(self.cst.get(id).tokens()[0])),
                NK::expr_false => Expr::False(TokId::new(self.cst.get(id).tokens()[0])),
                _ => unreachable!(),
            };
            self.map.exprs.push(expr)
        }

        fn gen_expr_infix(&mut self, id: NodeId) -> ExprInfix {
            debug_assert_eq!(id.kind, NK::expr_infix);
            let nodes = self.cst.get(id).nodes();

            assert_eq!(nodes.len(), 3);

            let lhs = self.gen_expr(nodes[0]);
            let rhs = self.gen_expr(nodes[2]);

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

        fn gen_expr_call(&mut self, id: NodeId) -> ExprCall {
            debug_assert_eq!(id.kind, NK::expr_call);
            let nodes = self.cst.get(id).nodes();

            let callee = self.gen_expr(nodes[0]);
            let args = nodes[1..].iter().map(|&id| self.gen_expr(id)).collect();

            ExprCall { id, callee, args }
        }

        fn gen_expr_ret(&mut self, id: NodeId) -> ExprRet {
            debug_assert_eq!(id.kind, NK::expr_return);
            let nodes = self.cst.get(id).nodes();

            let value = if !nodes.is_empty() {
                debug_assert_eq!(nodes.len(), 1);
                Some(self.gen_expr(nodes[0]))
            } else {
                None
            };

            ExprRet { id, value }
        }

        fn gen_stmt(&mut self, id: NodeId) -> StmtId {
            let stmt = match id.kind {
                NK::stmt_let => Stmt::Let(self.gen_binding(id)),
                NK::stmt_expr => Stmt::Expr(self.gen_expr(self.cst.get(id).nodes()[0])),
                NK::stmt_decl => Stmt::Decl(self.gen_decl(self.cst.get(id).nodes()[0])),

                _ => unreachable!(),
            };
            self.map.stmts.push(stmt)
        }
    }
}
