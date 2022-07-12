//! The name resolver, split into stage 1 and stage 2.
//! Stage 1 seeds all declarations, so that we know that they exist and so that their order does not matter.
//! Next in stage 2 we actually resolve the bodies of the declarations, such as the expression(s) of a function.
//! Or the types in a type declaration.
//!
//! @FIXME:...
//! This module is kind of a mess, we really need to clean it up.
//! Some organization is needed. And possibly some restructuring.
//! We could probably also combine stage 1 and 2 into a single struct
//! to avoid the disorganisation that occurs with having to share some
//! state between them.
//!
//! @NOTE:...
//! Something to note is that it seems that is might be reasonable to be able to skip the astgen step.
//! We could do it during stage 1 and 2 of the name resolver, whether this would be faster or not is unknown.
//! It's possible that it would be slower as we would effectively be doing 2 passes over the cst, but that
//! does not mean it would be inherently slower.

use bimap::BiHashMap;
use smallvec::SmallVec;
use std::{cell::RefCell, num::NonZeroU8};

// @TODO: Continue to test if this makes any speed difference. (2022-07-11)
use fnv::FnvHashMap;
// use std::collections::HashMap as FnvHashMap;

use crate::{
    ast,
    parse::cst::TokenIndex,
    util::index::{self, IndexVec, InterningIndexVec, StringInterningVec, StringSymbol},
};

pub fn resolve(
    source: &str,
    spans: &[std::ops::Range<usize>],
    ast_map: &ast::AstMap,
) -> NameResolutionResult {
    let sd = SharedData::new(source, spans, ast_map);
    let scope_map = stage1(&sd);
    stage2(sd, scope_map)
}

fn stage1<'s>(sd: &'s SharedData<'s>) -> ScopeMap {
    let mut stage = stage1::Stage1Gen::<'s>::new(sd);
    stage.seed();
    stage.map
}

fn stage2(sd: SharedData, scope_map: ScopeMap) -> NameResolutionResult {
    let map = {
        let mut stage = stage2::Stage2::new(&sd, &scope_map);
        stage.do_resolution();
        stage.map
    };

    NameResolutionResult {
        scope_map,
        strings: sd.strings.into_inner(),
        map,
    }
}

#[derive(Debug)]
pub struct NameResolutionResult {
    pub strings: StringInterningVec,
    pub scope_map: ScopeMap,
    pub map: IdMap,
}

#[derive(Debug, Default)]
pub struct ScopeMap {
    pub descs: IndexVec<ScopeDescId, ScopeDesc>,
    pub kinds: BiHashMap<ScopeKind, ScopeDescId>,
}

#[derive(Debug, Default)]
pub struct IdMap {
    pub utys: InterningIndexVec<UTyId, UTy>,
    pub tys: IndexVec<TyId, Ty>,

    pub decls: FnvHashMap<DeclId, DeclKind>,

    pub blocks: IndexVec<BlockId, Block>,
    pub block_scope_map: FnvHashMap<ScopeDescId, BlockId>,

    pub exprs: IndexVec<ExprId, Expr>,
    pub stmts: IndexVec<StmtId, Stmt>,

    pub locals: IndexVec<LocalId, Local>,
    pub func_args: IndexVec<FuncArgId, FuncArg>,
}

index::define_idx! { pub struct DeclId: u32 }
index::define_idx! { pub struct ScopeDescId: u32 != 0 }

index::define_idx! { pub struct TyId: u32 != 0 }
index::define_idx! { pub struct UTyId: u32 }

index::define_idx! { pub struct ExprId: u32 != 0 }
index::define_idx! { pub struct StmtId: u32 }
index::define_idx! { pub struct BlockId: u32 }

index::define_idx! { pub struct LocalId: u32 }
index::define_idx! { pub struct FuncArgId: u32 }

#[derive(Debug)]
pub struct SharedData<'s> {
    pub source: &'s str,
    pub ranges: &'s [std::ops::Range<usize>],
    pub ast: &'s ast::AstMap,
    // @TODO: Could we move this out; take it as a &mut parameter for each stage?
    pub strings: RefCell<StringInterningVec>,
}

impl<'s> SharedData<'s> {
    pub fn new(
        source: &'s str,
        ranges: &'s [std::ops::Range<usize>],
        ast: &'s ast::AstMap,
    ) -> Self {
        Self {
            source,
            ranges,
            ast,
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

mod stage1 {
    use super::*;

    #[derive(Debug)]
    pub(super) struct Stage1Gen<'s> {
        sd: &'s SharedData<'s>,
        pub(super) map: ScopeMap,
    }

    impl<'s> Stage1Gen<'s> {
        pub fn new(sd: &'s SharedData<'s>) -> Self {
            Self {
                sd,
                map: ScopeMap::default(),
            }
        }

        pub fn seed(&mut self) {
            let root_scope = self.map.descs.push(ScopeDesc::new(None));

            self.sd.ast.root.decls.iter().for_each(|decl| {
                self.seed_decl(*decl, root_scope);
            });
        }

        fn seed_decl(&mut self, id: ast::DeclId, parent_id: ScopeDescId) {
            let decl = &self.sd.ast.decls[id];
            match &decl.kind {
                ast::DeclKind::Func(func) => {
                    let sym = self.sd.get_tok_sym(func.name);
                    let scope = self.map.descs.get_mut(parent_id).unwrap();

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
            let expr = &self.sd.ast.exprs[id];
            match &expr.kind {
                ast::ExprKind::Path(_) => {}
                ast::ExprKind::Literal(_) => {}

                ast::ExprKind::Block(blk) => {
                    let block_scope = self.map.descs.push(ScopeDesc::new(Some(parent_id)));
                    let parent = self.map.descs.get_mut(parent_id).unwrap();
                    parent.children.push(block_scope);
                    self.map.kinds.insert(ScopeKind::Block(id), block_scope);

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
            let stmt = &self.sd.ast.stmts[id];
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

        scope_map: &'s ScopeMap,
        current_scope: ScopeDescId,

        /// Before resolving the body of a function we set this up to the function's args.
        /// Then clear it after.
        args: Vec<FuncArgId>,

        /// The way this works is whenever we encounter a local, we add it to this list.
        /// When we go into a new scope, we push the length of `locals` to `locals_bread_crumbs`
        /// so that when we leave the scope, we can truncate `locals` to what it was before.
        /// This system allows for shadowing, as we search though the list in the reverse order.
        locals: Vec<LocalId>,
        locals_bread_crumbs: Vec<u32>,

        pub(crate) map: IdMap,
    }

    impl<'s> Stage2<'s> {
        pub(super) fn new(sd: &'s SharedData<'s>, scope_map: &'s ScopeMap) -> Self {
            Self {
                sd,
                scope_map,
                current_scope: scope_map.descs.indices()[0],
                args: Default::default(),
                locals: Default::default(),
                locals_bread_crumbs: Default::default(),
                map: Default::default(),
            }
        }

        pub(super) fn do_resolution(&mut self) {
            self.sd.ast.root.decls.iter().cloned().for_each(|decl_id| {
                self.resolve_decl(decl_id);
            });
        }

        fn resolve_decl(&mut self, decl_id: ast::DeclId) -> DeclId {
            let scope = &self.scope_map.descs[self.current_scope];

            let old_args = std::mem::take(&mut self.args);
            let old_locals = std::mem::take(&mut self.locals);
            let old_locals_crumbs = std::mem::take(&mut self.locals_bread_crumbs);

            let kind = &self.sd.ast.decls[decl_id].kind;
            let ret = match kind {
                ast::DeclKind::Func(func) => {
                    let sym = self.sd.get_tok_sym(func.name);
                    let decl_desc_id = scope.decls_name_map[&sym];
                    let decl_desc = &scope.decls[decl_desc_id];
                    assert_eq!(decl_desc.ast_id, decl_id);
                    assert_eq!(decl_desc.tag, DeclDescTag::Func);

                    let ty = self.get_ty(func.ty);

                    debug_assert!(
                        self.map.utys[self.map.tys[ty].id].is_func(),
                        "internal compiler error; function does not have function type"
                    );

                    // setup args
                    assert!(self.args.is_empty());
                    match &self.sd.ast.tys[func.ty].kind {
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
            let expr = &self.sd.ast.exprs[expr_id];
            let kind = match expr.kind.clone() {
                ast::ExprKind::Path(path) => ExprKind::Res(self.resolve_expr_path(&path)),
                ast::ExprKind::Block(blk) => {
                    let blk_scope = {
                        let scope = self
                            .scope_map
                            .kinds
                            .get_by_left(&ScopeKind::Block(expr_id))
                            .unwrap();
                        let current_scope = &self.scope_map.descs[self.current_scope];
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
                            let stmt = &self.sd.ast.stmts[*stmt_id];
                            let kind = match stmt.kind {
                                ast::StmtKind::Let(ref l) => {
                                    let name = self.sd.get_tok_sym(l.name);
                                    let expr = self.resolve_expr(l.expr);
                                    let ty = l.ty.map(|id| self.get_ty(id));
                                    let local = Local {
                                        scope: self.current_scope,
                                        name,
                                        expr,
                                        ty,
                                    };
                                    let local_id = self.map.locals.push(local);
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

        fn get_ty(&mut self, ty_id: ast::TyId) -> TyId {
            let ty = &self.sd.ast.tys[ty_id].kind;
            let uty = match ty {
                ast::TyKind::Path(path) => UTy::Res(self.resolve_ty_path(path)),
                ast::TyKind::Func(ty) => UTy::Func {
                    args: ty
                        .params
                        .iter()
                        .map(|param| self.get_ty(param.ty))
                        .collect(),
                    ret: ty.ret.map(|ret| self.get_ty(ret)),
                },
                ast::TyKind::Slice(ty) => UTy::Slice(self.get_ty(*ty)),
                ast::TyKind::Nullable(ty) => UTy::Nullable(self.get_ty(*ty)),
            };

            let id = self.map.utys.get_or_intern(uty);

            self.map.tys.push(Ty {
                ast: Some(ty_id),
                id,
            })
        }

        fn resolve_expr_path(&self, path: &ast::Path) -> ExprPathResolution {
            // @TODO: Resolve paths longer that one element
            assert!(path.segments.len() == 1, "todo!");

            let res = path
                .segments
                .iter()
                .filter(|&e| !e.is_sep())
                .map(|segment| match segment {
                    ast::PathSegment::Sep => unreachable!(),
                    ast::PathSegment::Ident(idx) => {
                        let sym = self.sd.get_tok_sym(*idx);

                        if let Some(local) = self
                            .locals
                            .iter()
                            .rev()
                            .find(|&&id| self.map.locals[id].name == sym)
                            .map(|&id| ExprPathResolution::Local(id))
                        {
                            local
                        } else if let Some(arg) = self
                            .args
                            .iter()
                            .find(|&&id| self.map.func_args[id].name == sym)
                            .map(|&id| ExprPathResolution::Arg(id))
                        {
                            arg
                        } else {
                            ExprPathResolution::Decl(
                                self.search_for_decl_in_scope_looking_up(self.current_scope, sym),
                            )
                        }
                    }
                })
                .collect::<Vec<_>>();

            assert!(res.len() == 1, "todo!");
            res[0].clone()
        }

        fn search_for_decl_in_scope_looking_up(
            &self,
            scope_id: ScopeDescId,
            sym: StringSymbol,
        ) -> DeclId {
            let scope = &self.scope_map.descs[scope_id];

            match scope.decls_name_map.get(&sym) {
                Some(id) => *id,
                None if scope.parent.is_some() && {
                    let kind = self.scope_map.kinds.get_by_right(&scope_id).unwrap();
                    !kind.is_module()
                } =>
                {
                    let parent = scope.parent.unwrap();
                    self.search_for_decl_in_scope_looking_up(parent, sym)
                }
                None => {
                    let strings = self.sd.strings.borrow();
                    todo!("could not find value `{}` in scope", strings[sym]);
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
                        "sint" | "sintptr" => PrimTy::Integer {
                            signed: true,
                            size: None,
                        },
                        "uint" | "uintptr" => PrimTy::Integer {
                            signed: false,
                            size: None,
                        },
                        "bool" => PrimTy::Bool,
                        "void" => PrimTy::Void,
                        // if it starts with 's' and the rest of the string is a number
                        _ if str.starts_with('s') && chk_int_num(&str[1..]) => PrimTy::Integer {
                            signed: true,
                            size: Some(str[1..].parse().unwrap()),
                        },
                        // if it starts with 'u' and the rest of the string is a number
                        _ if str.starts_with('u') && chk_int_num(&str[1..]) => PrimTy::Integer {
                            signed: false,
                            size: Some(str[1..].parse().unwrap()),
                        },
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
            self.locals_bread_crumbs
                .push(self.locals.len().try_into().unwrap());
        }

        fn pop_locals_scope(&mut self) {
            self.locals
                .truncate(self.locals_bread_crumbs.pop().unwrap() as usize)
        }
    }
}

// pub type ScopeDescMap = IndexVec<ScopeDescId, ScopeDesc>;

/// A Description of a Scope. Such as a module, or a block.
#[derive(Debug)]
pub struct ScopeDesc {
    pub decls: IndexVec<DeclId, DeclDesc>,
    pub decls_name_map: FnvHashMap<StringSymbol, DeclId>,

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
    Integer {
        signed: bool,
        size: Option<NonZeroU8>,
    },
    Void,
    Bool,
    // @TODO: Add more
}

/// Holds a reference to where the type is mentioned in the ast,
/// and the actual referenced unique type
#[derive(Debug)]
pub struct Ty {
    pub ast: Option<ast::TyId>,
    pub id: UTyId,
}

/// A unique type, if two `UTyId`s are equal, they represent the same type.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum UTy {
    Res(TyPathResolution),
    Func {
        args: SmallVec<[TyId; 4]>,
        ret: Option<TyId>,
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
    Decl(DeclId),
    Local(LocalId),
    Arg(FuncArgId),
}

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

#[derive(Debug)]
pub struct Block {
    pub cst: ast::CstId,
    pub stmts: Vec<StmtId>,
}

#[derive(Debug)]
pub struct Stmt {
    pub ast_id: ast::StmtId,
    pub kind: StmtKind,
}

#[derive(Debug)]
pub enum StmtKind {
    Let(LocalId),
    Expr(ExprId),
    Decl(DeclId),
}

#[derive(Debug)]
pub struct Local {
    pub scope: ScopeDescId,
    pub name: StringSymbol,
    pub expr: ExprId,
    pub ty: Option<TyId>,
}

#[derive(Debug)]
pub struct FuncArg {
    pub name: StringSymbol,
    pub ty: TyId,
}
