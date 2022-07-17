use crate::{
    ast, nameres as nr,
    util::index::{self, InterningIndexVec},
};
use fnv::FnvHashMap as HashMap;
use smallvec::SmallVec;
use std::num::NonZeroU8;

pub fn resolve(nr: &nr::NameResolutionMap) -> TypeMap {
    let mut typer = Typer::new(nr);
    typer.resolve();
    typer.map
}

index::define_idx! { pub struct TyId: u32 }

#[derive(Debug, PartialEq, Eq)]
pub enum Ty {
    Prim(TyPrim),
    Func(TyFuncId),
    Slice(TyId),
    Nullable(TyId),
}

impl Ty {
    pub fn as_prim(&self) -> Option<&TyPrim> {
        if let Self::Prim(v) = self {
            Some(v)
        } else {
            None
        }
    }

    pub fn as_func(&self) -> Option<&TyFuncId> {
        if let Self::Func(v) = self {
            Some(v)
        } else {
            None
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum TyPrim {
    Integer {
        signed: bool,
        size: Option<NonZeroU8>, // None => pointer sized
    },
    Bool,
    Void,
    Never,
}

#[derive(Debug, Default)]
pub struct TypeMap {
    // pub locals: HashMap<nr::LocalId, TyId>,
    pub exprs: HashMap<nr::ExprId, TyId>,
    pub locals: HashMap<nr::LocalId, TyId>,

    pub tys: InterningIndexVec<TyId, Ty>,
    pub func_tys: InterningIndexVec<TyFuncId, TyFunc>,
}

index::define_idx! { pub struct TyFuncId: u32 }

#[derive(Debug, PartialEq, Eq)]
pub struct TyFunc {
    pub params: SmallVec<[TyId; 4]>,
    pub ret: TyId,
}

#[derive(Debug)]
struct Typer<'nr> {
    nr: &'nr nr::NameResolutionMap,
    map: TypeMap,
}

impl<'nr> Typer<'nr> {
    fn new(nr: &'nr nr::NameResolutionMap) -> Self {
        Self {
            nr,
            map: Default::default(),
        }
    }

    fn resolve(&mut self) {
        self.nr
            .decls
            .keys()
            .cloned()
            .for_each(|decl_id| self.resolve_decl(decl_id));

        // assert that all exprs have a type
        self.nr.exprs.indices().iter().for_each(|id| {
            assert!(
                self.map.exprs.contains_key(id),
                "expr id({:?}), does not have a type",
                id
            )
        });
    }

    fn resolve_decl(&mut self, decl_id: nr::DeclId) {
        let decl = &self.nr.decls[&decl_id];
        match decl {
            nr::DeclKind::Func(func) => {
                let _ = self.nr_ty_func_id_to_ty_func_id(func.ty);

                self.resolve_expr(func.body);
            }
        }
    }

    fn nr_uty_id_to_ty_id(&mut self, uty: nr::UTyId) -> TyId {
        let uty = &self.nr.utys[uty];
        let ty = match uty {
            nr::UTy::Res(res) => match res {
                nr::TyPathResolution::PrimTy(prim) => {
                    let prim = match prim {
                        nr::PrimTy::Integer { signed, size } => TyPrim::Integer {
                            signed: *signed,
                            size: *size,
                        },
                        nr::PrimTy::Void => TyPrim::Void,
                        nr::PrimTy::Bool => TyPrim::Bool,
                        nr::PrimTy::Never => TyPrim::Never,
                    };
                    Ty::Prim(prim)
                }
            },
            nr::UTy::Func(id) => Ty::Func(self.nr_ty_func_id_to_ty_func_id(*id)),
            nr::UTy::Slice(id) => Ty::Slice(self.nr_uty_id_to_ty_id(self.nr.tys[*id].id)),
            nr::UTy::Nullable(id) => Ty::Nullable(self.nr_uty_id_to_ty_id(self.nr.tys[*id].id)),
        };
        self.map.tys.get_or_intern(ty)
    }

    fn nr_ty_func_id_to_ty_func_id(&mut self, ty_func: nr::TyFuncId) -> TyFuncId {
        let func = &self.nr.func_tys[ty_func];
        let params = func
            .params
            .iter()
            .map(|&id| self.nr_uty_id_to_ty_id(self.nr.tys[id].id))
            .collect();
        let ret = func
            .ret
            .map_or(self.map.tys.get_or_intern(Ty::Prim(TyPrim::Void)), |id| {
                self.nr_uty_id_to_ty_id(self.nr.tys[id].id)
            });

        let func = TyFunc { params, ret };
        self.map.func_tys.get_or_intern(func)
    }

    fn resolve_stmt(&mut self, stmt_id: nr::StmtId) {
        let stmt = &self.nr.stmts[stmt_id];
        match &stmt.kind {
            nr::StmtKind::Let(local_id) => {
                let local = &self.nr.locals[*local_id];
                if let Some(_ty) = local.ty {
                    // @TODO: check type matches expr
                    todo!()
                } else {
                    let id = self.resolve_expr(local.expr);
                    self.map.locals.insert(*local_id, id);
                }
            }
            nr::StmtKind::Expr(expr_id) => {
                self.resolve_expr(*expr_id);
            }
            nr::StmtKind::Decl(decl_id) => {
                self.resolve_decl(*decl_id);
            }
        }
    }

    #[inline]
    #[track_caller]
    fn map_expr_ty(&mut self, expr_id: nr::ExprId, ty: TyId) -> TyId {
        if let Some(other_ty) = self.map.exprs.get(&expr_id) {
            assert_eq!(ty, *other_ty);
        } else {
            self.map.exprs.insert(expr_id, ty);
        }
        ty
    }

    fn resolve_expr(&mut self, expr_id: nr::ExprId) -> TyId {
        let expr = &self.nr.exprs[expr_id];
        let ty = match &expr.kind {
            nr::ExprKind::Res(res) => {
                let id = match res {
                    nr::ExprPathResolution::Decl(id) => {
                        let decl = &self.nr.decls[id];
                        match decl {
                            nr::DeclKind::Func(func) => {
                                let ty = Ty::Func(self.nr_ty_func_id_to_ty_func_id(func.ty));
                                self.map.tys.get_or_intern(ty)
                            }
                        }
                    }
                    nr::ExprPathResolution::Local(id) => self.map.locals[id],
                    nr::ExprPathResolution::Arg(id) => {
                        let arg = &self.nr.func_args[*id];
                        let ty = &self.nr.tys[arg.ty];

                        self.nr_uty_id_to_ty_id(ty.id)
                    }
                };
                return self.map_expr_ty(expr_id, id);
            }
            nr::ExprKind::Block(block) => {
                self.nr.blocks[*block]
                    .stmts
                    .iter()
                    .cloned()
                    .for_each(|stmt_id| self.resolve_stmt(stmt_id));

                Ty::Prim(TyPrim::Void)
            }
            nr::ExprKind::Literal(literal) => match literal {
                nr::ExprLiteral::String(_) => todo!(),
                // @TODO: Properly handle integer type discerning
                nr::ExprLiteral::Integer(_) => Ty::Prim(TyPrim::Integer {
                    signed: true,
                    size: None,
                }),
                nr::ExprLiteral::Float(_) => todo!(),
                nr::ExprLiteral::Bool(_) => Ty::Prim(TyPrim::Bool),
            },
            nr::ExprKind::Infix(infix) => {
                let lhs = self.resolve_expr(infix.lhs);
                let rhs = self.resolve_expr(infix.rhs);

                match infix.op {
                    ast::ExprInfixOp::Equal => {
                        if lhs != rhs {
                            todo!("err: types don't match")
                        } else {
                            return lhs;
                        }
                    }
                    ast::ExprInfixOp::Add
                    | ast::ExprInfixOp::Sub
                    | ast::ExprInfixOp::Mul
                    | ast::ExprInfixOp::Div => {
                        let id = self.check_arithmetic_infix_op(lhs, rhs);
                        return self.map_expr_ty(expr_id, id);
                    }
                }
            }
            nr::ExprKind::Call(call) => {
                for arg in &call.args {
                    self.resolve_expr(*arg);
                }

                let callee_ty = self.resolve_expr(call.callee);
                let callee_ty = &self.map.tys[callee_ty];

                if let Some(id) = callee_ty.as_func() {
                    let func = &self.map.func_tys[*id];
                    // @TODO: Check arg types

                    return self.map_expr_ty(expr_id, func.ret);
                } else {
                    todo!("err: callee is not a function")
                }
            }
            nr::ExprKind::Return(expr_id) => {
                // @TODO: Check that type matches with function return type
                let _ = expr_id.map(|id| self.resolve_expr(id));
                Ty::Prim(TyPrim::Void)
            }
        };
        let id = self.map.tys.get_or_intern(ty);
        self.map_expr_ty(expr_id, id)
    }

    fn check_arithmetic_infix_op(&mut self, lhs: TyId, rhs: TyId) -> TyId {
        let lhs_ty = &self.map.tys[lhs];
        match lhs_ty {
            Ty::Prim(prim) => match prim {
                TyPrim::Integer { signed, size } => {
                    let rhs_ty = &self.map.tys[rhs];
                    if let Some(prim) = rhs_ty.as_prim() {
                        match prim.as_integer() {
                            Some((rhs_signed, rhs_size))
                                if signed == rhs_signed && size == rhs_size =>
                            {
                                lhs
                            }
                            _ => todo!("err: integer type mismatch"),
                        }
                    } else {
                        todo!("err: integer type mismatch")
                    }
                }
                TyPrim::Bool => todo!("err: cannot use bool as operand"),
                TyPrim::Void => todo!("err: cannot use void as operand"),
                TyPrim::Never => self.map.tys.get_or_intern(Ty::Prim(TyPrim::Never)),
            },
            Ty::Func { .. } => todo!("err: cannot use function as operand"),
            Ty::Slice(_) => todo!("err: cannot use slice as operand"),
            Ty::Nullable(_) => todo!("err: cannot use nullable as operand"),
        }
    }
}

impl TyPrim {
    fn as_integer(&self) -> Option<(&bool, &Option<NonZeroU8>)> {
        match self {
            TyPrim::Integer { signed, size } => Some((signed, size)),
            _ => None,
        }
    }
}
