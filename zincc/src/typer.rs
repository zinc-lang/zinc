use crate::{
    nameres::{self as nr, NameResolutionResult},
    util::index::{self, InterningIndexVec},
};
use smallvec::SmallVec;
use std::{collections::HashMap, num::NonZeroU8};

pub fn resolve(nr: &NameResolutionResult) -> TypeMap {
    let mut typer = Typer::new(nr);
    typer.resolve();
    typer.map
}

index::define_idx! { pub struct TyId: u32 }

#[derive(Debug, PartialEq, Eq)]
pub enum Ty {
    Prim(TyPrim),
    Func {
        args: SmallVec<[TyId; 4]>,
        ret: TyId,
    },
    Slice(TyId),
    Nullable(TyId),
}

#[derive(Debug, PartialEq, Eq)]
pub enum TyPrim {
    Sint(TyIntSize),
    Uint(TyIntSize),
    Bool,
    Void,
    Never,
}

#[derive(Debug, PartialEq, Eq)]
pub enum TyIntSize {
    PtrSized,
    BitSize(NonZeroU8),
}

#[derive(Debug)]
struct Typer<'nr> {
    nr: &'nr NameResolutionResult,
    map: TypeMap,
}

#[derive(Debug, Default)]
pub struct TypeMap {
    pub exprs: HashMap<nr::ExprId, TyId>,
    pub tys: InterningIndexVec<TyId, Ty>,
}

impl<'nr> Typer<'nr> {
    fn new(nr: &'nr NameResolutionResult) -> Self {
        Self {
            nr,
            map: Default::default(),
        }
    }

    fn resolve(&mut self) {
        self.nr
            .map
            .exprs
            .indices()
            .iter()
            .cloned()
            .for_each(|expr_id| {
                let id = self.resolve_expr(expr_id);
                self.map.exprs.insert(expr_id, id);
            });
    }

    fn resolve_expr(&mut self, expr_id: nr::ExprId) -> TyId {
        let expr = self.nr.map.exprs.get(expr_id).unwrap();
        let ty = match &expr.kind {
            nr::ExprKind::Res(_) => todo!(),
            nr::ExprKind::Block(_) => {
                // @TODO: If block contains a return it should be of type never
                Ty::Prim(TyPrim::Void)
            }
            nr::ExprKind::Literal(literal) => match literal {
                nr::ExprLiteral::String(_) => todo!(),
                nr::ExprLiteral::Integer(_) => Ty::Prim(TyPrim::Sint(TyIntSize::PtrSized)),
                nr::ExprLiteral::Float(_) => todo!(),
                nr::ExprLiteral::Bool(_) => Ty::Prim(TyPrim::Bool),
            },
            nr::ExprKind::Infix(_) => todo!(),
            nr::ExprKind::Call(_) => todo!(),
            nr::ExprKind::Return(_) => Ty::Prim(TyPrim::Never),
        };
        self.map.tys.get_or_intern(ty)
    }
}
