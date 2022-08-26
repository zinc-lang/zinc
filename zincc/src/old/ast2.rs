use crate::parse::cst::TokenIndex;
use crate::util::index::{self, IndexVec, StringSymbol};
use std::ops::Range;

pub type CstId = crate::parse::cst::NodeId;

index::define_idx! { pub struct DeclId: u32 }
index::define_idx! { pub struct StmtId: u32 }
index::define_idx! { pub struct ExprId: u32 != 0 }
index::define_idx! { pub struct BlockId: u32 }

index::define_idx! { pub struct TyId: u32 != 0 }
index::define_idx! { pub struct TyFuncId: u32  }
index::define_idx! { pub struct TyFuncParamId: u32  }

index::define_idx! { pub struct PathSegmentId: u32  }

#[derive(Debug)]
pub struct Ast {
    pub root: Root,

    pub decls: IndexVec<DeclId, Decl>,

    pub tys: IndexVec<TyId, Ty>,
    // pub ty_funcs: IndexVec<TyFuncId, TyFunc>,
    // pub ty_func_params: IndexVec<TyFuncParamId, TyFuncParam>,

    // pub stmts: IndexVec<StmtId, Stmt>,
    // pub exprs: IndexVec<ExprId, Expr>,
    // pub blocks: IndexVec<BlockId, Block>,

    // pub path_segments: IndexVec<PathSegmentId, PathSegment>,
}

#[derive(Debug)]
pub struct StringToken {
    str: StringSymbol,
    tok: TokenIndex,
}

#[derive(Debug)]
pub struct Root {
    pub cst: CstId,
    pub decls: Range<DeclId>,
}

#[derive(Debug)]
pub struct Decl {
    pub cst: CstId,
    pub kind: DeclKind,
}

#[derive(Debug)]
pub enum DeclKind {
    Func(DeclFunc),
}

#[derive(Debug)]
pub struct DeclFunc {
    pub name: StringToken,
    pub ty: TyFuncId,
    pub body: ExprId,
}

#[derive(Debug)]
pub struct Ty {
    pub cst: CstId,
    pub kind: TyKind,
}

#[derive(Debug)]
pub struct TyPath {
    raw: Path,
    kind: TyPathKind,
}

#[derive(Debug)]
pub enum TyPathKind {
    Primitive(TyPrimitive),
}

#[derive(Debug)]
pub enum TyPrimitive {}

#[derive(Debug)]
pub enum TyKind {
    // Path(Path),
    Func(TyFuncId),
    // Slice(TyId),
    // Nullable(TyId),
}
