use smallvec::SmallVec;
use std::{cell::RefCell, collections::HashMap};

use crate::{
    ast,
    util::index::{self, IndexVec, InterningIndexVec, StringInterningVec, StringSymbol},
};

#[derive(Debug)]
pub struct SharedData<'s> {
    pub source: &'s str,
    pub ranges: &'s [std::ops::Range<usize>],
    pub ast: &'s ast::AstMap,
    pub ast_root: &'s ast::Root,
    pub strings: RefCell<StringInterningVec>,
}

impl<'s> SharedData<'s> {
    pub fn new(
        source: &'s str,
        ranges: &'s [std::ops::Range<usize>],
        ast: &'s ast::AstMap,
        root: &'s ast::Root,
    ) -> Self {
        Self {
            source,
            ranges,
            ast,
            ast_root: root,
            strings: StringInterningVec::new().into(),
        }
    }

    #[inline]
    fn get_tok_slice(&self, idx: usize) -> &str {
        &self.source[self.ranges[idx].clone()]
    }

    fn get_tok_sym(&self, idx: usize) -> StringSymbol {
        let str = self.get_tok_slice(idx);
        self.str_intern(str)
    }

    fn str_intern(&self, str: &str) -> StringSymbol {
        let mut strings = self.strings.borrow_mut();
        if !strings.is_str_interned(str) {
            strings.intern(str.to_string())
        } else {
            strings.get_from_str_value(str).unwrap()
        }
    }
}

#[derive(Debug)]
pub struct TypeData {
    interned_utys: RefCell<InterningIndexVec<UTy, UTyId>>,
    tys: RefCell<IndexVec<Ty, TyId>>,
    void_ty: TyId,
}

impl TypeData {
    pub fn new() -> Self {
        let mut interned_utys = InterningIndexVec::new();
        let mut tys = IndexVec::new();
        let void_uty = interned_utys.intern(UTy::Res(TyPathResoution::PrimTy(PrimTy::Void)));
        let void_ty = tys.push(Ty {
            ast: None,
            id: void_uty,
        });

        Self {
            interned_utys: RefCell::new(interned_utys),
            tys: RefCell::new(tys),
            void_ty,
        }
    }
}

pub fn stage1<'s>(sd: &'s SharedData<'s>) -> IndexVec<ScopeDesc, ScopeDescId> {
    let mut gen = stage1::Stage1Gen::<'s>::new(sd);
    gen.seed();
    assert!(gen.scopes.raw.get(0).unwrap().kind == ScopeKind::Root);
    gen.scopes
}

pub fn stage2<'s>(
    sd: &'s SharedData<'s>,
    td: &'s TypeData,
    scopes: &'s mut IndexVec<ScopeDesc, ScopeDescId>,
) {
    let mut gen = stage2::Stage2::<'s>::new(sd, td, scopes);
    gen.do_resolution();
}

mod stage1 {
    use super::*;

    #[derive(Debug)]
    pub(super) struct Stage1Gen<'s> {
        sd: &'s SharedData<'s>,
        pub(super) scopes: IndexVec<ScopeDesc, ScopeDescId>,
    }

    impl<'s> Stage1Gen<'s> {
        pub fn new(sd: &'s SharedData<'s>) -> Self {
            Self {
                sd,
                scopes: IndexVec::new(),
            }
        }

        pub fn seed(&mut self) {
            let root_scope = self.scopes.push(ScopeDesc::new(ScopeKind::Root, None));

            self.sd.ast_root.decls.iter().for_each(|decl| {
                self.seed_decl(*decl, root_scope);
            });
        }

        fn seed_decl(&mut self, id: ast::DeclId, parent_id: ScopeDescId) {
            let decl = self.sd.ast.decls.get(id).unwrap();
            match &decl.kind {
                ast::DeclKind::Func(func) => {
                    let sym = self.sd.get_tok_sym(func.name);
                    let kind = DeclDescKind::Func(DeclFuncDesc {});
                    let scope = self.scopes.get_mut(parent_id).unwrap();
                    scope.decls.insert(sym, DeclDesc { id, kind });

                    self.seed_expr(func.body, parent_id);
                }
                ast::DeclKind::Const(_) => todo!(),
            }
        }

        fn seed_expr(&mut self, id: ast::ExprId, parent_id: ScopeDescId) {
            let expr = self.sd.ast.exprs.get(id).unwrap();
            match &expr.kind {
                ast::ExprKind::Path(_) => {}
                ast::ExprKind::Block(blk) => {
                    let block_scope = self
                        .scopes
                        .push(ScopeDesc::new(ScopeKind::Block(id), Some(parent_id)));
                    let parent = self.scopes.get_mut(parent_id).unwrap();
                    parent.children.push(block_scope);

                    blk.stmts
                        .iter()
                        .for_each(|stmt| self.seed_stmt(*stmt, block_scope));
                }
                ast::ExprKind::Literal(_) => {}
                ast::ExprKind::Infix(infix) => {
                    self.seed_expr(infix.lhs, parent_id);
                    self.seed_expr(infix.rhs, parent_id);
                }
                ast::ExprKind::Call(call) => {
                    self.seed_expr(call.callee, parent_id);
                    call.args
                        .iter()
                        .for_each(|arg| self.seed_expr(*arg, parent_id));
                }
                ast::ExprKind::Return(ret) => {
                    ret.map(|expr| self.seed_expr(expr, parent_id));
                }
            }
        }

        fn seed_stmt(&mut self, id: ast::StmtId, parent_id: ScopeDescId) {
            let stmt = self.sd.ast.stmts.get(id).unwrap();
            match &stmt.kind {
                ast::StmtKind::Decl(decl) => self.seed_decl(*decl, parent_id),
                ast::StmtKind::Expr(expr) => self.seed_expr(*expr, parent_id),
                ast::StmtKind::Let(l) => self.seed_expr(l.expr, parent_id),
            }
        }
    }
}

mod stage2 {
    use super::*;

    pub(crate) struct Stage2<'s> {
        sd: &'s SharedData<'s>,
        td: &'s TypeData,
        scopes: RefCell<&'s mut IndexVec<ScopeDesc, ScopeDescId>>,
    }

    impl<'s> Stage2<'s> {
        pub(crate) fn new(
            sd: &'s SharedData<'s>,
            td: &'s TypeData,
            scopes: &'s mut IndexVec<ScopeDesc, ScopeDescId>,
        ) -> Self {
            Self {
                sd,
                td,
                scopes: RefCell::new(scopes),
            }
        }

        pub(crate) fn do_resolution(&mut self) {
            let mut scopes = self.scopes.borrow_mut();
            let root_scope = scopes.raw.get_mut(0).unwrap();

            self.sd.ast_root.decls.iter().for_each(|decl| {
                let kind = &self.sd.ast.decls.get(*decl).unwrap().kind;
                match kind {
                    ast::DeclKind::Func(func) => {
                        let sym = self.sd.get_tok_sym(func.name);
                        let decl_desc = root_scope.decls.get_mut(&sym).unwrap();
                        assert_eq!(decl_desc.id, *decl);
                        let _func_desc = decl_desc.kind.as_func().unwrap();

                        let _ty = self.get_ty(func.ty);
                        // func_desc.ty = Some(ty);
                    }
                    ast::DeclKind::Const(_) => todo!(),
                }
            });
        }

        fn get_ty(&self, ty_id: ast::TyId) -> TyId {
            let ty = &self.sd.ast.tys.get(ty_id).unwrap().kind;
            let uty = match ty {
                ast::TyKind::Path(path) => UTy::Res(self.resolve_ty_path(path)),
                ast::TyKind::Func(ty) => UTy::Func {
                    args: ty
                        .params
                        .iter()
                        .map(|param| self.get_ty(param.ty))
                        .collect(),
                    ret: ty.ret.map_or(self.td.void_ty, |ret| self.get_ty(ret)),
                },
                ast::TyKind::Slice(ty) => UTy::Slice(self.get_ty(*ty)),
                ast::TyKind::Nullable(ty) => UTy::Nullable(self.get_ty(*ty)),
            };

            let mut interned_utys = self.td.interned_utys.borrow_mut();
            let id = interned_utys.get_or_intern(uty);

            let mut tys = self.td.tys.borrow_mut();
            tys.push(Ty {
                ast: Some(ty_id),
                id,
            })
        }

        fn resolve_ty_path(&self, path: &ast::Path) -> TyPathResoution {
            if path.segments.len() == 1 {
                if let Some(ast::PathSegment::Ident(idx)) = path.segments.get(0) {
                    fn chk_int_num(str: &str) -> bool {
                        str.chars().map(|c| c.is_ascii_digit()).all(|b| b)
                    }

                    let str = self.sd.get_tok_slice(*idx);

                    let prim_ty = match str {
                        "sint" => PrimTy::Sint(IntSize::Unspecified),
                        "uint" => PrimTy::Uint(IntSize::Unspecified),
                        "sintptr" => PrimTy::Sint(IntSize::PtrSized),
                        "uintptr" => PrimTy::Uint(IntSize::PtrSized),
                        "bool" => PrimTy::Bool,
                        "void" => PrimTy::Void,
                        // if it starts with 's' and the rest of the string is a number
                        _ if str.starts_with('s') && chk_int_num(&str[1..]) => {
                            PrimTy::Sint(IntSize::BitSized(str[1..].parse().unwrap()))
                        }
                        // if it starts with 'u' and the rest of the string is a number
                        _ if str.starts_with('u') && chk_int_num(&str[1..]) => {
                            PrimTy::Uint(IntSize::BitSized(str[1..].parse().unwrap()))
                        }
                        _ => todo!(),
                    };
                    TyPathResoution::PrimTy(prim_ty)
                } else {
                    todo!()
                }
            } else {
                todo!()
            }
        }
    }
}

index::define_u32_idx!(ScopeDescId);

/// A Description of a Scope.
#[derive(Debug)]
pub struct ScopeDesc {
    pub kind: ScopeKind,
    pub decls: HashMap<StringSymbol, DeclDesc>,
    pub children: SmallVec<[ScopeDescId; 4]>,

    // @FIXME: Is this needed?
    pub parent: Option<ScopeDescId>,
}

impl ScopeDesc {
    pub fn new(kind: ScopeKind, parent: Option<ScopeDescId>) -> Self {
        Self {
            kind,
            parent,
            decls: HashMap::new(),
            children: SmallVec::new(),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ScopeKind {
    Root,
    Module(StringSymbol),
    Block(ast::ExprId),
}

/// A description of a declaration.
#[derive(Debug)]
pub struct DeclDesc {
    pub id: ast::DeclId,
    pub kind: DeclDescKind,
}

#[derive(Debug)]
pub enum DeclDescKind {
    Func(DeclFuncDesc),
    // @TODO: Add more
    Temp,
}

impl DeclDescKind {
    pub fn as_func(&self) -> Option<&DeclFuncDesc> {
        match self {
            Self::Func(v) => Some(v),
            _ => None,
        }
    }
}

/// A description of a function declaration.
#[derive(Debug)]
pub struct DeclFuncDesc {
    // @TODO: Overloads, types
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TyPathResoution {
    PrimTy(PrimTy),
    // @TODO: Add more
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PrimTy {
    Sint(IntSize),
    Uint(IntSize),
    Void,
    Bool,
    // @TODO: Add more
}

/// 1 byte
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum IntSize {
    Unspecified,
    PtrSized,
    BitSized(u8),
}

index::define_u32_idx!(TyId);

/// A type holding a reference to the ast, and the unique type
#[derive(Debug)]
pub struct Ty {
    pub ast: Option<ast::TyId>,
    pub id: UTyId,
}

index::define_u32_idx!(UTyId);

/// A unique type, if two `UTypeId`s are equal, they represent the same type.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum UTy {
    Res(TyPathResoution),
    Func {
        args: SmallVec<[TyId; 4]>,
        ret: TyId,
    },
    Slice(TyId),
    Nullable(TyId),
}
