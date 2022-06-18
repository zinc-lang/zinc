use crate::parse::cst;

pub type TokRef = u32;

#[derive(Debug, Clone)]
pub struct Root<'c> {
    pub node: &'c cst::Node,
    pub decls: Vec<Decl<'c>>,
}

#[derive(Debug, Clone)]
pub struct Ident(TokRef);

#[derive(Debug, Clone)]
pub struct Path<'c> {
    pub node: &'c cst::Node,
}

#[derive(Debug, Clone)]
pub struct AstString<'c> {
    pub node: &'c cst::Node,
}

#[derive(Debug, Clone)]
pub struct Binding<'c> {
    pub node: &'c cst::Node,
    pub name: Ident,
    pub ty: Option<Ty<'c>>,
    pub expr: Expr<'c>,
}

#[derive(Debug, Clone)]
pub enum Decl<'c> {
    Func(DeclFunc<'c>),
    Const(Binding<'c>),
}

#[derive(Debug, Clone)]
pub struct DeclFunc<'c> {
    pub node: &'c cst::Node,
    pub name: Ident,
    pub ty: TyFunc<'c>,
    pub body: Expr<'c>,
}

#[derive(Debug, Clone)]
pub enum Ty<'c> {
    /// Includes primitives and user defined, since we have no access to tokens during ast-gen
    Named(Path<'c>),
    Func(TyFunc<'c>),
}

#[derive(Debug, Clone)]
pub struct TyFunc<'c> {
    pub node: &'c cst::Node,
    pub args: Vec<(Option<TokRef>, Ty<'c>)>,
    pub ret: Option<Box<Ty<'c>>>,
}

#[derive(Debug, Clone)]
pub enum Expr<'c> {
    Path(Path<'c>),
    String(AstString<'c>),
    Block(Vec<Stmt<'c>>),
    Integer(TokRef),
    Float(TokRef),
    Infix(ExprInfix<'c>),
    // Unit
    // Grouping
    // Call
    Ret(Option<Box<Expr<'c>>>),
}

#[derive(Debug, Clone)]
pub struct ExprInfix<'c> {
    pub node: &'c cst::Node,
    pub op: Vec<TokRef>,
    pub lhs: Box<Expr<'c>>,
    pub rhs: Box<Expr<'c>>,
}

#[derive(Debug, Clone)]
pub enum Stmt<'c> {
    Let(Binding<'c>),
    Decl(Decl<'c>),
    Expr(Expr<'c>),
}

pub mod gen {
    //! TODO: More assertions
    use super::*;
    use crate::parse::cst::{self, NodeKind as NK};

    pub fn root(node: &cst::Node) -> Root {
        let decls = node.nodes().iter().map(|s| decl(s)).collect();
        Root { node, decls }
    }

    pub fn decl(node: &cst::Node) -> Decl {
        match node.kind {
            NK::decl_func => {
                let nodes = node.nodes();
                let name = Ident(node.tokens()[0]);
                let ty = func_ty(nodes.iter().find(|s| s.kind == NK::func_proto).unwrap());
                let body = expr(
                    nodes
                        .iter()
                        .find(|s| s.kind == NK::decl_func_body)
                        .map(|s| {
                            debug_assert_eq!(s.nodes().len(), 1);
                            s.nodes()[0]
                        })
                        .unwrap(),
                );

                Decl::Func(DeclFunc {
                    node,
                    name,
                    ty,
                    body,
                })
            }
            NK::decl_const => Decl::Const(binding(node)),
            _ => unreachable!(),
        }
    }

    /// 'let's and 'const's
    pub fn binding(node: &cst::Node) -> Binding {
        debug_assert!(matches!(node.kind, NK::stmt_let | NK::decl_const));

        let tokens = node.tokens();
        let nodes = node.nodes();

        debug_assert!(tokens.len() >= 4 && tokens.len() <= 5);
        debug_assert!(!nodes.is_empty() && nodes.len() <= 2);

        let name = Ident(tokens[1]);

        let ty = nodes
            .iter()
            .find(|s| s.kind == NK::binding_ty)
            .map(|s| {
                debug_assert_eq!(s.nodes().len(), 1);
                s.nodes()[0]
            })
            .map(ty);

        let expr = expr(nodes.last().unwrap());

        Binding {
            node,
            name,
            ty,
            expr,
        }
    }

    pub fn ty(node: &cst::Node) -> Ty {
        match node.kind {
            NK::path => Ty::Named(Path { node }),
            NK::func_proto => Ty::Func(func_ty(node)),
            _ => unreachable!(),
        }
    }

    pub fn func_ty(node: &cst::Node) -> TyFunc {
        assert_eq!(node.kind, NK::func_proto);
        let nodes = node.nodes();

        let args = nodes
            .iter()
            .filter(|s| s.kind == NK::func_proto_arg)
            .map(|n| {
                debug_assert_eq!(n.nodes().len(), 1);

                let name = if let cst::Element::Token(i) = n.children[0] {
                    Some(i)
                } else {
                    None
                };
                let ty = ty(n.nodes()[0]);
                (name, ty)
            })
            .collect();

        let ret = nodes
            .iter()
            .find(|s| s.kind == NK::func_proto_ret)
            .map(|s| {
                debug_assert_eq!(s.nodes().len(), 1);
                s.nodes()[0]
            })
            .map(ty)
            .map(Box::new);

        TyFunc { node, args, ret }
    }

    pub fn expr(node: &cst::Node) -> Expr {
        match node.kind {
            NK::path => Expr::Path(Path { node }),
            NK::string => Expr::String(AstString { node }),
            NK::block => Expr::Block(node.nodes().iter().map(|s| stmt(s)).collect()),
            NK::literal_int => Expr::Integer({
                debug_assert_eq!(node.tokens().len(), 1);
                node.tokens()[0]
            }),
            NK::literal_float => Expr::Float({
                debug_assert_eq!(node.tokens().len(), 1);
                node.tokens()[0]
            }),
            NK::expr_infix => {
                let nodes = node.nodes();
                debug_assert_eq!(node.nodes().len(), 3);

                let lhs = Box::new(expr(nodes.get(0).unwrap()));
                let op = nodes.get(1).unwrap().tokens();
                let rhs = Box::new(expr(nodes.get(2).unwrap()));
                Expr::Infix(ExprInfix { node, op, lhs, rhs })
            }
            NK::expr_unit => todo!(),
            NK::expr_grouping => todo!(),
            NK::expr_tuple => todo!(),
            NK::expr_call => todo!(),
            NK::expr_return => {
                Expr::Ret(if let Some(cst::Element::Node(n)) = node.children.get(1) {
                    Some(Box::new(expr(n)))
                } else {
                    None
                })
            }
            _ => unreachable!(),
        }
    }

    pub fn stmt(node: &cst::Node) -> Stmt {
        match node.kind {
            NK::stmt_let => Stmt::Let(binding(node)),

            NK::path
            | NK::string
            | NK::block
            | NK::literal_int
            | NK::literal_float
            | NK::expr_infix
            | NK::expr_infix_op
            | NK::expr_unit
            | NK::expr_grouping
            | NK::expr_tuple
            | NK::expr_call
            | NK::expr_return => Stmt::Expr(expr(node)),

            NK::decl_func | NK::decl_const => Stmt::Decl(decl(node)),

            _ => unreachable!(),
        }
    }
}
