use crate::parse::cst::TokenIndex;
use crate::util::index::{self, IndexVec};

pub type CstId = crate::parse::cst::NodeId;

pub use gen::gen;
use smallvec::SmallVec;

index::define_idx! { pub struct DeclId: u32 }
index::define_idx! { pub struct StmtId: u32 }
index::define_idx! { pub struct ExprId: u32 != 0 }

index::define_idx! { pub struct TyId: u32 != 0 }
index::define_idx! { pub struct TyFuncId: u32  }

#[derive(Debug)]
pub struct AstMap {
    pub root: Root,

    pub decls: IndexVec<DeclId, Decl>,
    pub stmts: IndexVec<StmtId, Stmt>,
    pub exprs: IndexVec<ExprId, Expr>,

    pub tys: IndexVec<TyId, Ty>,
    pub func_tys: IndexVec<TyFuncId, TyFunc>,
}

#[derive(Debug)]
pub struct Root {
    pub cst: CstId,
    pub decls: Vec<DeclId>,
}

impl Root {
    fn nil() -> Self {
        Self {
            cst: <crate::parse::cst::NodeId as index::Idx>::new(0),
            decls: Vec::new(),
        }
    }
}

#[derive(Debug)]
pub struct Decl {
    pub cst: CstId,
    pub kind: DeclKind,
}

#[derive(Debug)]
pub enum DeclKind {
    Func(DeclFunc),
    Const(Binding),
}

#[derive(Debug)]
pub struct DeclFunc {
    pub name: TokenIndex,
    pub ty: TyFuncId,
    pub body: ExprId,
}

#[derive(Debug)]
pub struct Ty {
    pub cst: CstId,
    pub kind: TyKind,
}

#[derive(Debug)]
pub enum TyKind {
    Path(Path),
    Func(TyFuncId),
    Slice(TyId),
    Nullable(TyId),
}

#[derive(Debug)]
pub struct TyFunc {
    // @FIXME: Optimize size based on real usage metrics
    pub params: SmallVec<[FuncParam; 4]>,
    pub ret: Option<TyId>,
}

#[derive(Debug)]
pub struct FuncParam {
    pub cst: CstId,
    pub name: Option<TokenIndex>,
    pub ty: TyId,
}

#[derive(Debug)]
pub struct Expr {
    pub cst: CstId,
    pub kind: ExprKind,
}

#[derive(Debug, Clone)]
pub enum ExprKind {
    Path(Path),
    Block(Block),
    Literal(ExprLiteral),
    Infix(ExprInfix),
    Call(ExprCall),
    Return(Option<ExprId>),
}

#[derive(Debug, Clone)]
pub enum ExprLiteral {
    String(AstString),
    Integer(u64),
    Float(f64),
    Bool(bool),
}

#[derive(Debug, Clone)]
pub struct ExprInfix {
    pub lhs: ExprId,
    pub rhs: ExprId,
    pub op: ExprInfixOp,
}

#[derive(Debug, Clone)]
pub enum ExprInfixOp {
    Equal,
    Add,
    Sub,
    Mul,
    Div,
    // @TODO: Add more
}

#[derive(Debug, Clone)]
pub struct ExprCall {
    pub callee: ExprId,
    // @FIXME: Optimize size based on real usage metrics
    pub args: SmallVec<[ExprId; 4]>,
}

#[derive(Debug)]
pub struct Stmt {
    pub cst: CstId,
    pub kind: StmtKind,
}

#[derive(Debug)]
pub enum StmtKind {
    Let(Binding),
    Expr(ExprId),
    Decl(DeclId),
}

#[derive(Debug)]
pub struct Binding {
    pub name: TokenIndex,
    pub ty: Option<TyId>,
    pub expr: ExprId,
}

/// invariants:
/// ```rust
/// # let segments: Vec<Segment> = vec![];
/// #
/// !segments.is_empty();
///
/// if segments.first().unwrap() == PathSegment::Sep {
///     segments.len() >= 2;
/// }
/// ```
#[derive(Debug, Clone)]
pub struct Path {
    pub cst: CstId,
    // @FIXME: Optimize size based on real usage metrics
    pub segments: SmallVec<[PathSegment; 4]>,
}

#[derive(Debug, Clone)]
pub enum PathSegment {
    Sep,
    Ident(TokenIndex),
    // @TODO: Add more, self, Self, etc.
}

impl PathSegment {
    #[must_use]
    pub fn is_sep(&self) -> bool {
        matches!(self, Self::Sep)
    }
}

#[derive(Debug, Clone)]
pub struct AstString {
    pub cst: CstId,
    pub baked: String,
}

#[derive(Debug, Clone)]
pub struct Block {
    pub cst: CstId,
    pub stmts: Vec<StmtId>,
}

pub mod gen {
    use smallvec::SmallVec;

    use crate::{
        ast::{self, AstMap},
        parse::{
            cst::{self, NamedNodeId, NK},
            TokenKind, TK,
        },
    };

    pub fn gen(
        cst: &cst::Cst,
        source: &str,
        tokens: &[TokenKind],
        ranges: &[std::ops::Range<usize>],
    ) -> AstMap {
        let mut gen = AstGen::new(cst, source, tokens, ranges);
        let root = gen.gen_root();
        gen.map.root = root;
        gen.map
    }

    struct AstGen<'s> {
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
                    root: ast::Root::nil(),
                    decls: Default::default(),
                    stmts: Default::default(),
                    exprs: Default::default(),
                    tys: Default::default(),
                    func_tys: Default::default(),
                },
            }
        }

        fn gen_root(&mut self) -> ast::Root {
            let cst = self.cst.root.raw;
            let decls = self
                .cst
                .get(cst)
                .nodes()
                .iter()
                .map(|&id| self.gen_decl(id))
                .collect();

            ast::Root { cst, decls }
        }

        fn gen_decl(&mut self, id: NamedNodeId) -> ast::DeclId {
            let kind = match id.kind {
                NK::decl_func => ast::DeclKind::Func(self.gen_decl_func(id)),
                NK::decl_const => ast::DeclKind::Const(self.gen_binding(id)),
                _ => unreachable!(),
            };
            self.map.decls.push(ast::Decl { cst: id.raw, kind })
        }

        fn gen_decl_func(&mut self, id: NamedNodeId) -> ast::DeclFunc {
            debug_assert_eq!(id.kind, NK::decl_func);

            let node = self.cst.get(id);
            debug_assert_eq!(node.nodes().len(), 2);

            debug_assert_eq!(self.tokens[node.tokens()[0].get()], TK::kw_fn);
            let name = node.tokens()[1];

            let proto = node.nodes()[0];
            let body = node.nodes()[1];

            debug_assert_eq!(proto.kind, NK::func_proto);
            debug_assert_eq!(body.kind, NK::decl_func_body);

            let ty = self.gen_ty_func(proto);
            let body = self.gen_expr(self.cst.get(body).nodes()[0]);

            ast::DeclFunc { ty, name, body }
        }

        fn gen_binding(&mut self, id: NamedNodeId) -> ast::Binding {
            debug_assert!(matches!(id.kind, NK::stmt_let | NK::decl_const));
            let node = self.cst.get(id);
            let tokens = node.tokens();

            debug_assert!(matches!(
                self.tokens[tokens[0].get()],
                TK::kw_let | TK::kw_const
            ));
            debug_assert_eq!(self.tokens[tokens[1].get()], TK::ident);

            let name = tokens[1];

            let nodes = node.nodes();
            debug_assert!(!nodes.is_empty() && nodes.len() <= 2);
            let ty = if nodes[0].kind == NK::binding_ty {
                Some(self.gen_ty(self.cst.get(nodes[0]).nodes()[0]))
            } else {
                None
            };

            let expr = self.gen_expr(*nodes.last().unwrap());

            ast::Binding { name, ty, expr }
        }

        fn gen_ty(&mut self, id: NamedNodeId) -> ast::TyId {
            let kind = match id.kind {
                NK::path => ast::TyKind::Path(self.gen_path(id)),
                NK::func_proto => ast::TyKind::Func(self.gen_ty_func(id)),
                NK::ty_slice => ast::TyKind::Slice(self.gen_ty(self.cst.get(id).nodes()[0])),
                NK::ty_nullable => ast::TyKind::Nullable(self.gen_ty(self.cst.get(id).nodes()[0])),
                _ => unreachable!(),
            };
            self.map.tys.push(ast::Ty { cst: id.raw, kind })
        }

        fn gen_ty_func(&mut self, id: NamedNodeId) -> ast::TyFuncId {
            debug_assert_eq!(id.kind, NK::func_proto);
            let node = self.cst.get(id);

            let params = node
                .nodes()
                .iter()
                .filter(|id| id.kind == NK::func_proto_param)
                .map(|&id| {
                    let node = self.cst.get(id);
                    let name = node.tokens().get(0).cloned();
                    let ty = self.gen_ty(node.nodes()[0]);
                    ast::FuncParam {
                        cst: id.raw,
                        name,
                        ty,
                    }
                })
                .collect();

            let ret = node
                .nodes()
                .iter()
                .find(|id| id.kind == NK::func_proto_ret)
                .map(|&id| self.cst.get(id).nodes()[0])
                .map(|id| self.gen_ty(id));

            let func = ast::TyFunc { params, ret };
            self.map.func_tys.push(func)
        }

        fn gen_expr(&mut self, id: NamedNodeId) -> ast::ExprId {
            let kind = match id.kind {
                NK::path => ast::ExprKind::Path(self.gen_path(id)),
                NK::block => ast::ExprKind::Block(self.gen_block(id)),
                NK::string => ast::ExprKind::Literal(ast::ExprLiteral::String(self.gen_string(id))),
                NK::literal_int => {
                    ast::ExprKind::Literal(ast::ExprLiteral::Integer(self.gen_int(id)))
                }
                NK::literal_float => {
                    ast::ExprKind::Literal(ast::ExprLiteral::Float(self.gen_float(id)))
                }
                NK::expr_true => ast::ExprKind::Literal(ast::ExprLiteral::Bool(true)),
                NK::expr_false => ast::ExprKind::Literal(ast::ExprLiteral::Bool(false)),
                NK::expr_infix => ast::ExprKind::Infix(self.gen_expr_infix(id)),
                // NK::expr_unit => todo!(),
                // NK::expr_grouping => todo!(),
                // NK::expr_tuple => todo!(),
                NK::expr_call => ast::ExprKind::Call(self.gen_expr_call(id)),
                NK::expr_return => ast::ExprKind::Return(self.gen_expr_return(id)),
                _ => unreachable!(),
            };
            self.map.exprs.push(ast::Expr { cst: id.raw, kind })
        }

        fn gen_expr_infix(&mut self, id: NamedNodeId) -> ast::ExprInfix {
            debug_assert_eq!(id.kind, NK::expr_infix);
            let nodes = self.cst.get(id).nodes();

            assert_eq!(nodes.len(), 3);

            let lhs = self.gen_expr(nodes[0]);
            let rhs = self.gen_expr(nodes[2]);

            debug_assert_eq!(nodes[1].kind, NK::expr_infix_op);
            let op_tokens = self.cst.get(nodes[1]).tokens();
            assert_eq!(op_tokens.len(), 1, "TODO: More infix operator types");
            let op = match self.tokens[op_tokens[0].get()] {
                TK::punct_eq => ast::ExprInfixOp::Equal,
                TK::punct_plus => ast::ExprInfixOp::Add,
                TK::punct_minus => ast::ExprInfixOp::Sub,
                TK::punct_star => ast::ExprInfixOp::Mul,
                TK::punct_slash => ast::ExprInfixOp::Div,
                _ => todo!("More infix operator types"),
            };

            ast::ExprInfix { lhs, rhs, op }
        }

        fn gen_expr_call(&mut self, id: NamedNodeId) -> ast::ExprCall {
            debug_assert_eq!(id.kind, NK::expr_call);
            let nodes = self.cst.get(id).nodes();

            let callee = self.gen_expr(nodes[0]);
            let args = nodes[1..].iter().map(|&id| self.gen_expr(id)).collect();

            ast::ExprCall { callee, args }
        }

        fn gen_expr_return(&mut self, id: NamedNodeId) -> Option<ast::ExprId> {
            debug_assert_eq!(id.kind, NK::expr_return);
            let nodes = self.cst.get(id).nodes();

            if !nodes.is_empty() {
                debug_assert_eq!(nodes.len(), 1);
                Some(self.gen_expr(nodes[0]))
            } else {
                None
            }
        }

        fn gen_stmt(&mut self, id: NamedNodeId) -> ast::StmtId {
            let kind = match id.kind {
                NK::stmt_let => ast::StmtKind::Let(self.gen_binding(id)),
                NK::stmt_expr => ast::StmtKind::Expr(self.gen_expr(self.cst.get(id).nodes()[0])),
                NK::stmt_decl => ast::StmtKind::Decl(self.gen_decl(self.cst.get(id).nodes()[0])),
                _ => unreachable!(),
            };
            self.map.stmts.push(ast::Stmt { cst: id.raw, kind })
        }

        fn gen_path(&mut self, id: NamedNodeId) -> ast::Path {
            let node = self.cst.get(id);
            let segments = node
                .tokens()
                .iter()
                .map(|&i| match self.tokens[i.get()] {
                    TK::ident => ast::PathSegment::Ident(i),
                    TK::punct_dblColon => ast::PathSegment::Sep,
                    _ => todo!("More path segment types"),
                })
                .collect::<SmallVec<_>>();
            assert!(!segments.is_empty());

            ast::Path {
                cst: id.raw,
                segments,
            }
        }

        fn gen_string(&mut self, id: NamedNodeId) -> ast::AstString {
            debug_assert_eq!(id.kind, NK::string);
            let node = self.cst.get(id);
            debug_assert_eq!(node.nodes().len(), 0);

            let len = node.tokens().len();
            let baked = node.tokens()[1..len - 1]
                .iter()
                .map(|&i| {
                    (
                        self.tokens[i.get()],
                        &self.source[self.ranges[i.get()].clone()],
                    )
                })
                .map(|(tk, slice)| match tk {
                    TokenKind::string_literal => slice.to_string(),
                    TokenKind::esc_asciicode => {
                        let code = slice[1..].parse::<u8>().unwrap();
                        String::from_utf8(vec![code]).unwrap()
                    }
                    TokenKind::esc_unicode => {
                        let code = slice[1..].parse::<u32>().unwrap();
                        char::from_u32(code).unwrap().to_string()
                    }
                    TokenKind::esc_char_newline => "\n".to_string(),
                    TokenKind::esc_char_return => "\r".to_string(),
                    TokenKind::esc_char_tab => "\t".to_string(),
                    TokenKind::esc_char_backslash => "\\".to_string(),
                    TokenKind::esc_char_doublequote => "\"".to_string(),
                    TokenKind::esc_char_singlequote => "'".to_string(),
                    TokenKind::esc_char_other => slice.chars().nth(1).unwrap().to_string(),

                    TokenKind::string_open | TokenKind::string_close => unreachable!(),
                    _ => unreachable!(),
                })
                .collect();

            ast::AstString { cst: id.raw, baked }
        }

        fn gen_block(&mut self, id: NamedNodeId) -> ast::Block {
            let stmts = self
                .cst
                .get(id)
                .nodes()
                .iter()
                .map(|&id| self.gen_stmt(id))
                .collect();
            ast::Block { cst: id.raw, stmts }
        }

        fn gen_int(&mut self, id: NamedNodeId) -> u64 {
            let node = self.cst.get(id);
            debug_assert_eq!(node.nodes().len(), 0);
            debug_assert_eq!(node.tokens().len(), 1);

            let tk_i = node.tokens()[0];
            let tk = self.tokens[tk_i.get()];

            let slice = &self.source[self.ranges[tk_i.get()].clone()];
            let str = slice.replace('_', "");

            match tk {
                TokenKind::int_dec => str.parse::<u64>(),
                TokenKind::int_hex => u64::from_str_radix(&str[2..], 16),
                TokenKind::int_oct => u64::from_str_radix(&str[2..], 8),
                TokenKind::int_bin => u64::from_str_radix(&str[2..], 2),
                _ => unreachable!(),
            }
            // If this fails the lexer failed to report the error
            .expect("internal lexing error")
        }

        fn gen_float(&mut self, id: NamedNodeId) -> f64 {
            let node = self.cst.get(id);
            debug_assert_eq!(node.nodes().len(), 0);
            debug_assert_eq!(node.tokens().len(), 1);

            let tk_i = node.tokens()[0];
            debug_assert_eq!(self.tokens[tk_i.get()], TK::float);

            let slice = &self.source[self.ranges[tk_i.get()].clone()];
            let str = slice.replace('_', "");

            // If this fails the lexer failed to report the error
            str.parse().expect("internal lexing error")
        }
    }
}
