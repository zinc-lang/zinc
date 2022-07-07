use bimap::BiHashMap;
use fnv::FnvHashMap;
use smallvec::SmallVec;
use std::cell::RefCell;

use crate::{
    ast,
    parse::cst::TokenIndex,
    util::index::{self, IndexVec, InterningIndexVec, StringInterningVec, StringSymbol},
};

pub fn stage1<'s>(
    sd: &'s SharedData<'s>,
) -> (
    IndexVec<ScopeDesc, ScopeDescId>,
    BiHashMap<ScopeKind, ScopeDescId>,
) {
    let mut stage = stage1::Stage1Gen::<'s>::new(sd);
    stage.seed();
    (stage.scopes, stage.scope_kind_map)
}

pub fn stage2<'s>(
    sd: &'s SharedData<'s>,
    td: &'s TypeData,
    scopes: &'s IndexVec<ScopeDesc, ScopeDescId>,
    scope_kind_map: &'s BiHashMap<ScopeKind, ScopeDescId>,
) -> Map {
    let mut stage = stage2::Stage2::<'s>::new(sd, td, scopes, scope_kind_map);
    stage.do_resolution();
    stage.map
}

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
    fn get_tok_slice(&self, idx: TokenIndex) -> &str {
        &self.source[self.ranges[idx.get()].clone()]
    }

    fn get_tok_sym(&self, idx: TokenIndex) -> StringSymbol {
        let str = self.get_tok_slice(idx);
        self.str_intern(str)
    }

    /// Allows for checking if a string is interned from only a [`&str`],
    /// without needing to create a [`String`] first.
    ///
    /// If you already have a [`String`], use [`Self::string_intern`].
    fn str_intern(&self, str: &str) -> StringSymbol {
        let mut strings = self.strings.borrow_mut();
        if !strings.is_str_interned(str) {
            strings.intern(str.to_string())
        } else {
            strings.get_from_str_value(str).unwrap()
        }
    }

    /// In the case that there is already a [`String`] allocated,
    /// instead of creating a new one from [`&str`] in [`Self::str_intern`].
    ///
    /// If you have a [`&str`], use [`Self::str_intern`].
    fn string_intern(&self, string: String) -> StringSymbol {
        let mut strings = self.strings.borrow_mut();
        strings.get_or_intern(string)
    }
}

#[derive(Debug)]
pub struct TypeData {
    interned_utys: RefCell<InterningIndexVec<UTy, UTyId>>,
    tys: RefCell<IndexVec<Ty, TyId>>,
    void_ty: TyId,
}

impl Default for TypeData {
    fn default() -> Self {
        Self::new()
    }
}

impl TypeData {
    pub fn new() -> Self {
        let mut interned_utys = InterningIndexVec::new();
        let mut tys = IndexVec::new();
        let void_uty = interned_utys.intern(UTy::Res(TyPathResolution::PrimTy(PrimTy::Void)));
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

mod stage1 {
    use bimap::BiHashMap;

    use super::*;

    #[derive(Debug)]
    pub(super) struct Stage1Gen<'s> {
        sd: &'s SharedData<'s>,
        pub(super) scopes: IndexVec<ScopeDesc, ScopeDescId>,
        pub(super) scope_kind_map: BiHashMap<ScopeKind, ScopeDescId>,
    }

    impl<'s> Stage1Gen<'s> {
        pub fn new(sd: &'s SharedData<'s>) -> Self {
            Self {
                sd,
                scopes: Default::default(),
                scope_kind_map: Default::default(),
            }
        }

        pub fn seed(&mut self) {
            let root_scope = self.scopes.push(ScopeDesc::new(None));

            self.sd.ast_root.decls.iter().for_each(|decl| {
                self.seed_decl(*decl, root_scope);
            });
        }

        fn seed_decl(&mut self, id: ast::DeclId, parent_id: ScopeDescId) {
            let decl = self.sd.ast.decls.get(id).unwrap();
            match &decl.kind {
                ast::DeclKind::Func(func) => {
                    let sym = self.sd.get_tok_sym(func.name);
                    let scope = self.scopes.get_mut(parent_id).unwrap();

                    // @TODO: Handle error
                    assert!(
                        !scope.decls_name_map.contains_key(&sym),
                        "todo!: Handle error, multiple decls with the same name"
                    );

                    let desc = DeclDesc {
                        ast_id: id,
                        tag: DeclDescTag::Func,
                    };
                    let id = scope.decls.push(desc);
                    scope.decls_name_map.insert(sym, id);

                    self.seed_expr(func.body, parent_id);
                }
                ast::DeclKind::Const(_) => todo!(),
            }
        }

        fn seed_expr(&mut self, id: ast::ExprId, parent_id: ScopeDescId) {
            let expr = self.sd.ast.exprs.get(id).unwrap();
            match &expr.kind {
                ast::ExprKind::Path(_) => {}
                ast::ExprKind::Literal(_) => {}

                ast::ExprKind::Block(blk) => {
                    let block_scope = self.scopes.push(ScopeDesc::new(Some(parent_id)));
                    let parent = self.scopes.get_mut(parent_id).unwrap();
                    parent.children.push(block_scope);
                    self.scope_kind_map
                        .insert(ScopeKind::Block(id), block_scope);

                    blk.stmts
                        .iter()
                        .for_each(|stmt| self.seed_stmt(*stmt, block_scope));
                }
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

        scopes: &'s IndexVec<ScopeDesc, ScopeDescId>,
        scope_kind_map: &'s BiHashMap<ScopeKind, ScopeDescId>,
        current_scope: ScopeDescId,

        args: Vec<FuncArgId>,
        locals: Vec<LocalId>,
        locals_bread_crumbs: Vec<u32>,

        pub(crate) map: Map,
    }

    impl<'s> Stage2<'s> {
        pub(super) fn new(
            sd: &'s SharedData<'s>,
            td: &'s TypeData,
            scopes: &'s IndexVec<ScopeDesc, ScopeDescId>,
            scope_kind_map: &'s BiHashMap<ScopeKind, ScopeDescId>,
        ) -> Self {
            Self {
                sd,
                td,
                scopes,
                scope_kind_map,
                current_scope: *scopes.indices().get(0).unwrap(),
                args: Default::default(),
                locals: Default::default(),
                locals_bread_crumbs: Default::default(),
                map: Default::default(),
            }
        }

        pub(super) fn do_resolution(&mut self) {
            self.sd.ast_root.decls.iter().cloned().for_each(|decl_id| {
                self.resolve_decl(decl_id);
            });
        }

        fn resolve_decl(&mut self, decl_id: ast::DeclId) -> DeclDescId {
            let scope = self.scopes.get(self.current_scope).unwrap();

            let old_args = std::mem::take(&mut self.args);
            let old_locals = std::mem::take(&mut self.locals);
            let old_locals_crumbs = std::mem::take(&mut self.locals_bread_crumbs);

            let kind = &self.sd.ast.decls.get(decl_id).unwrap().kind;
            let ret = match kind {
                ast::DeclKind::Func(func) => {
                    let sym = self.sd.get_tok_sym(func.name);
                    let decl_desc_id = *scope.decls_name_map.get(&sym).unwrap();
                    let decl_desc = scope.decls.get(decl_desc_id).unwrap();
                    assert_eq!(decl_desc.ast_id, decl_id);
                    assert_eq!(decl_desc.tag, DeclDescTag::Func);

                    let ty = self.get_ty(func.ty);

                    debug_assert!(
                        {
                            let tys = self.td.tys.borrow();
                            let utys = self.td.interned_utys.borrow();
                            utys.get(tys.get(ty).unwrap().id).unwrap().is_func()
                        },
                        "internal compiler error; function does not have function type"
                    );

                    // setup args
                    match &self.sd.ast.tys.get(func.ty).unwrap().kind {
                        ast::TyKind::Func(ast::TyFunc { params, .. }) => {
                            params.iter().for_each(|param| {
                                if let Some(name) = param.name {
                                    let name = self.sd.get_tok_sym(name);
                                    let ty = self.get_ty(param.ty);
                                    let id = self.map.func_args.push(FuncArg { name, ty });
                                    self.args.push(id);
                                }
                            })
                        }
                        _ => unreachable!(),
                    }

                    let body = self.resolve_expr(func.body);
                    self.args.clear();

                    let func = DeclFunc { ty, body };
                    self.map.decls.insert(decl_desc_id, DeclKind::Func(func));

                    decl_desc_id
                }
                ast::DeclKind::Const(_) => todo!(),
            };

            self.args = old_args;
            self.locals = old_locals;
            self.locals_bread_crumbs = old_locals_crumbs;

            ret
        }

        fn resolve_expr(&mut self, expr_id: ast::ExprId) -> ExprId {
            let expr = self.sd.ast.exprs.get(expr_id).unwrap();
            let kind = match expr.kind.clone() {
                ast::ExprKind::Path(path) => ExprKind::Res(self.resolve_expr_path(&path)),
                ast::ExprKind::Block(blk) => {
                    let blk_scope = {
                        let scope = self
                            .scope_kind_map
                            .get_by_left(&ScopeKind::Block(expr_id))
                            .unwrap();
                        let current_scope = self.scopes.get(self.current_scope).unwrap();
                        assert!(current_scope.children.contains(scope));
                        scope
                    };
                    let old_scope = self.current_scope;
                    self.current_scope = *blk_scope;

                    self.push_locals_scope();

                    let stmts = blk
                        .stmts
                        .iter()
                        .map(|stmt_id| {
                            let stmt = self.sd.ast.stmts.get(*stmt_id).unwrap();
                            let kind = match stmt.kind {
                                ast::StmtKind::Let(ref l) => {
                                    let name = self.sd.get_tok_sym(l.name);
                                    let expr = self.resolve_expr(l.expr);
                                    let local_id = self.map.locals.push(Local {
                                        scope: self.current_scope,
                                        name,
                                        expr,
                                        ty: l.ty.map(|id| self.get_ty(id)),
                                    });
                                    self.locals.push(local_id);
                                    StmtKind::Let(local_id)
                                }
                                ast::StmtKind::Expr(id) => StmtKind::Expr(self.resolve_expr(id)),
                                ast::StmtKind::Decl(id) => StmtKind::Decl(self.resolve_decl(id)),
                            };
                            self.map.stmts.push(Stmt {
                                ast_id: *stmt_id,
                                kind,
                            })
                        })
                        .collect();

                    self.pop_locals_scope();

                    self.current_scope = old_scope;
                    let block_id = self.map.blocks.push(Block {
                        cst: blk.cst,
                        stmts,
                    });
                    self.map.block_scope_map.insert(*blk_scope, block_id);
                    ExprKind::Block(block_id)
                }
                ast::ExprKind::Literal(literal) => ExprKind::Literal(match literal {
                    ast::ExprLiteral::String(str) => {
                        ExprLiteral::String(self.sd.string_intern(str.baked))
                    }
                    ast::ExprLiteral::Integer(int) => ExprLiteral::Integer(int),
                    ast::ExprLiteral::Float(float) => ExprLiteral::Float(float),
                    ast::ExprLiteral::Bool(bool) => ExprLiteral::Bool(bool),
                }),
                ast::ExprKind::Infix(infix) => ExprKind::Infix(ExprInfix {
                    lhs: self.resolve_expr(infix.lhs),
                    rhs: self.resolve_expr(infix.rhs),
                    op: infix.op,
                }),
                ast::ExprKind::Call(call) => ExprKind::Call(ExprCall {
                    callee: self.resolve_expr(call.callee),
                    args: call
                        .args
                        .iter()
                        .cloned()
                        .map(|id| self.resolve_expr(id))
                        .collect(),
                }),
                ast::ExprKind::Return(ret) => ExprKind::Return(ret.map(|id| self.resolve_expr(id))),
            };
            let expr = Expr {
                ast_id: expr_id,
                kind,
            };
            self.map.exprs.push(expr)
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

        fn resolve_expr_path(&self, path: &ast::Path) -> ExprPathResolution {
            // let scope = self.scopes.get(self.current_scope).unwrap();

            // @TODO: Resolve paths longer that one element
            assert!(path.segments.len() == 1, "todo!");
            for segment in path.segments.iter() {
                match segment {
                    ast::PathSegment::Sep => {}
                    ast::PathSegment::Ident(idx) => {
                        let sym = self.sd.get_tok_sym(*idx);

                        if let Some(local) = self
                            .locals
                            .iter()
                            .rev()
                            .find(|&&id| self.map.locals.get(id).unwrap().name == sym)
                            .map(|&id| ExprPathResolution::Local(id))
                        {
                            return local;
                        }

                        if let Some(arg) = self
                            .args
                            .iter()
                            .find(|&&id| self.map.func_args.get(id).unwrap().name == sym)
                            .map(|&id| ExprPathResolution::Arg(id))
                        {
                            return arg;
                        }

                        return ExprPathResolution::Decl(
                            self.search_for_decl_in_scope_looking_up(self.current_scope, sym),
                        );
                    }
                }
            }

            unreachable!()
        }

        fn search_for_decl_in_scope_looking_up(
            &self,
            scope_id: ScopeDescId,
            sym: StringSymbol,
        ) -> DeclDescId {
            let scope = self.scopes.get(scope_id).unwrap();

            match scope.decls_name_map.get(&sym) {
                Some(id) => *id,
                None if scope.parent.is_some() && {
                    let kind = self.scope_kind_map.get_by_right(&scope_id).unwrap();
                    !kind.is_module()
                } =>
                {
                    let parent = scope.parent.unwrap();
                    self.search_for_decl_in_scope_looking_up(parent, sym)
                }
                None => {
                    let strings = self.sd.strings.borrow();
                    todo!(
                        "could not find value `{}` in scope",
                        strings.get(sym).unwrap()
                    );
                }
            }
        }

        fn resolve_ty_path(&self, path: &ast::Path) -> TyPathResolution {
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
                    TyPathResolution::PrimTy(prim_ty)
                } else {
                    todo!()
                }
            } else {
                todo!()
            }
        }

        fn push_locals_scope(&mut self) {
            self.locals_bread_crumbs.push(self.locals.len() as u32)
        }

        fn pop_locals_scope(&mut self) {
            self.locals
                .truncate(self.locals_bread_crumbs.pop().unwrap() as usize)
        }
    }
}

#[derive(Debug, Default)]
pub struct Map {
    decls: FnvHashMap<DeclDescId, DeclKind>,
    exprs: IndexVec<Expr, ExprId>,
    stmts: IndexVec<Stmt, StmtId>,
    blocks: IndexVec<Block, BlockId>,
    block_scope_map: FnvHashMap<ScopeDescId, BlockId>,
    locals: IndexVec<Local, LocalId>,
    func_args: IndexVec<FuncArg, FuncArgId>,
}

index::define_u32_idx!(ScopeDescId);

/// A Description of a Scope. Such as a module, or a block.
#[derive(Debug)]
pub struct ScopeDesc {
    pub decls: IndexVec<DeclDesc, DeclDescId>,
    pub decls_name_map: FnvHashMap<StringSymbol, DeclDescId>,

    pub children: SmallVec<[ScopeDescId; 4]>,

    // @FIXME: Is this needed?
    pub parent: Option<ScopeDescId>,
}

impl ScopeDesc {
    pub fn new(parent: Option<ScopeDescId>) -> Self {
        Self {
            parent,
            decls: Default::default(),
            decls_name_map: Default::default(),
            children: Default::default(),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ScopeKind {
    Block(ast::ExprId),
    Module(StringSymbol),
}

impl ScopeKind {
    /// Returns `true` if the scope kind is [`Module`].
    ///
    /// [`Module`]: ScopeKind::Module
    #[must_use]
    pub fn is_module(&self) -> bool {
        matches!(self, Self::Module(..))
    }
}

index::define_u32_idx!(DeclDescId);

/// A description of a declaration.
#[derive(Debug)]
pub struct DeclDesc {
    pub ast_id: ast::DeclId,
    pub tag: DeclDescTag,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DeclDescTag {
    Func,
    // @TODO: Add more
    Temp,
}

#[derive(Debug)]
pub enum DeclKind {
    Func(DeclFunc),
}

#[derive(Debug)]
pub struct DeclFunc {
    pub ty: TyId, // Func type
    pub body: ExprId,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TyPathResolution {
    PrimTy(PrimTy),
    // @TODO: Add more, like a struct, enum, etc.
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

/// Holds a reference to where the type is mentioned in the ast,
/// and the actual referenced unique type
#[derive(Debug)]
pub struct Ty {
    pub ast: Option<ast::TyId>,
    pub id: UTyId,
}

index::define_u32_idx!(UTyId);

/// A unique type, if two `UTypeId`s are equal, they represent the same type.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum UTy {
    Res(TyPathResolution),
    Func {
        args: SmallVec<[TyId; 4]>,
        ret: TyId,
    },
    Slice(TyId),
    Nullable(TyId),
}

impl UTy {
    #[must_use]
    pub fn is_func(&self) -> bool {
        matches!(self, Self::Func { .. })
    }
}

#[derive(Debug, Clone)]
pub enum ExprPathResolution {
    Decl(DeclDescId),
    Local(LocalId),
    Arg(FuncArgId),
}

index::define_non_zero_u32_idx!(ExprId);

#[derive(Debug)]
pub struct Expr {
    pub ast_id: ast::ExprId,
    pub kind: ExprKind,
}

#[derive(Debug)]
pub enum ExprKind {
    Res(ExprPathResolution),
    Block(BlockId),
    Literal(ExprLiteral),
    Infix(ExprInfix),
    Call(ExprCall),
    Return(Option<ExprId>),
}

#[derive(Debug)]
pub enum ExprLiteral {
    String(StringSymbol),
    Integer(u64),
    Float(f64),
    Bool(bool),
}

#[derive(Debug)]
pub struct ExprInfix {
    pub lhs: ExprId,
    pub rhs: ExprId,
    pub op: ast::ExprInfixOp,
}

#[derive(Debug)]
pub struct ExprCall {
    pub callee: ExprId,
    // @FIXME: Optimize size based on real usage metrics
    pub args: SmallVec<[ExprId; 4]>,
}

index::define_non_zero_u32_idx!(BlockId);

#[derive(Debug)]
pub struct Block {
    pub cst: ast::CstId,
    pub stmts: Vec<StmtId>,
}

index::define_non_zero_u32_idx!(StmtId);

#[derive(Debug)]
pub struct Stmt {
    pub ast_id: ast::StmtId,
    pub kind: StmtKind,
}

#[derive(Debug)]
pub enum StmtKind {
    Let(LocalId),
    Expr(ExprId),
    Decl(DeclDescId),
}

index::define_non_zero_u32_idx!(LocalId);

#[derive(Debug)]
pub struct Local {
    pub scope: ScopeDescId,
    pub name: StringSymbol,
    pub expr: ExprId,
    pub ty: Option<TyId>,
}

index::define_non_zero_u32_idx!(FuncArgId);

#[derive(Debug)]
pub struct FuncArg {
    pub name: StringSymbol,
    pub ty: TyId,
}
