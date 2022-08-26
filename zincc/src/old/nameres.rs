//! The name resolver.
//!
//! @NOTE:...
//! Something to note is that it seems that is might be reasonable to be able to skip the astgen step.
//! We could do it during stage 1 and 2 of the name resolver, whether this would be faster or not is unknown.
//! It's possible that it would be slower as we would effectively be doing 2 passes over the cst, but that
//! does not mean it would be inherently slower.

use fnv::FnvHashMap as HashMap;
use smallvec::SmallVec;
use std::num::NonZeroU8;

type BiHashMap<K, V> = bimap::BiHashMap<K, V, fnv::FnvBuildHasher, fnv::FnvBuildHasher>;

use crate::{
    ast,
    util::index::{self, IndexVec, InterningIndexVec, StringInterningVec, StringSymbol},
};

pub fn resolve(
    source: &str,
    ranges: &[std::ops::Range<usize>],
    ast_map: &ast::AstMap,
) -> (NameResolutionMap, StringInterningVec) {
    let mut resolver = resolver::NameResolver::new(source, ranges, ast_map);
    resolver.resolve();
    (resolver.map, resolver.strings.into_inner())
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

index::define_idx! { pub struct TyFuncId: u32 }

index::define_idx! { pub struct DeclFuncId: u32 }

#[derive(Debug, Default)]
pub struct NameResolutionMap {
    pub scope_descs: IndexVec<ScopeDescId, ScopeDesc>,
    pub scope_kinds: BiHashMap<ScopeKind, ScopeDescId>,

    pub tys: IndexVec<TyId, Ty>,
    pub utys: InterningIndexVec<UTyId, UTy>,
    pub func_tys: InterningIndexVec<TyFuncId, TyFunc>,

    pub decls: HashMap<DeclId, DeclKind>,
    pub decl_funcs: IndexVec<DeclFuncId, DeclFunc>,

    pub blocks: IndexVec<BlockId, Block>,
    pub block_scope_map: HashMap<ScopeDescId, BlockId>,

    pub exprs: IndexVec<ExprId, Expr>,
    pub stmts: IndexVec<StmtId, Stmt>,

    pub locals: IndexVec<LocalId, Local>,
    pub func_args: IndexVec<FuncArgId, FuncArg>,
}

impl NameResolutionMap {
    pub fn root_scope_id() -> ScopeDescId {
        use crate::util::index::Idx;
        ScopeDescId::new(0)
    }

    pub fn root_scope(&self) -> &ScopeDesc {
        &self.scope_descs[Self::root_scope_id()]
    }
}

/// A Description of a Scope. Such as a module, or a block.
#[derive(Debug)]
pub struct ScopeDesc {
    pub decls: IndexVec<DeclId, DeclDesc>,
    pub decls_name_map: BiHashMap<StringSymbol, DeclId>,

    pub children: SmallVec<[ScopeDescId; 4]>,

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
    // Module(StringSymbol),
}

impl ScopeKind {
    /// Returns `true` if the scope kind is [`Module`].
    ///
    /// [`Module`]: ScopeKind::Module
    #[must_use]
    pub fn is_module(&self) -> bool {
        // matches!(self, Self::Module(..))
        false
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
}

#[derive(Debug)]
pub enum DeclKind {
    Func(DeclFuncId),
}

#[derive(Debug)]
pub struct DeclFunc {
    pub ty: TyFuncId,
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
    Never,
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
    Func(TyFuncId),
    Slice(TyId),
    Nullable(TyId),
}

#[derive(Debug, PartialEq, Eq)]
pub struct TyFunc {
    pub params: SmallVec<[TyId; 4]>,
    pub ret: Option<TyId>,
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

mod resolver {
    use crate::{
        ast, nameres as nr,
        parse::cst::TokenIndex,
        util::index::{StringInterningVec, StringSymbol},
    };
    use std::{cell::RefCell, num::NonZeroU8};

    use super::NameResolutionMap;

    pub(super) struct NameResolver<'s> {
        source: &'s str,
        ranges: &'s [std::ops::Range<usize>],
        ast: &'s ast::AstMap,

        current_scope: nr::ScopeDescId,

        /// Before resolving the body of a function we set this up to the function's args.
        /// Then clear it after.
        args: Vec<nr::FuncArgId>,

        /// The way this works is whenever we encounter a local, we add it to this list.
        /// When we go into a new scope, we push the length of `locals` to `locals_bread_crumbs`
        /// so that when we leave the scope, we can truncate `locals` to what it was before.
        /// This system allows for shadowing, as we search though the list in the reverse order.
        locals: Vec<nr::LocalId>,
        locals_bread_crumbs: Vec<u32>,

        pub map: nr::NameResolutionMap,
        pub strings: RefCell<StringInterningVec>,
    }

    impl<'s> NameResolver<'s> {
        pub(super) fn new(
            source: &'s str,
            ranges: &'s [std::ops::Range<usize>],
            ast: &'s ast::AstMap,
        ) -> Self {
            Self {
                source,
                ranges,
                ast,
                current_scope: NameResolutionMap::root_scope_id(),
                strings: RefCell::new(StringInterningVec::new()),
                args: Default::default(),
                locals: Default::default(),
                locals_bread_crumbs: Default::default(),
                map: Default::default(),
            }
        }
    }

    impl NameResolver<'_> {
        pub fn seed(&mut self) {
            let root_scope = self.map.scope_descs.push(nr::ScopeDesc::new(None));

            self.ast.root.decls.iter().for_each(|decl| {
                self.seed_decl(*decl, root_scope);
            });
        }

        fn seed_decl(&mut self, id: ast::DeclId, parent_id: nr::ScopeDescId) {
            let decl = &self.ast.decls[id];
            match &decl.kind {
                ast::DeclKind::Func(func) => {
                    let sym = self.get_tok_sym(func.name);
                    let scope = self.map.scope_descs.get_mut(parent_id).unwrap();

                    // @TODO: Handle error
                    assert!(
                        !scope.decls_name_map.contains_left(&sym),
                        "todo!: Handle error, multiple decls with the same name"
                    );

                    let desc = nr::DeclDesc {
                        ast_id: id,
                        tag: nr::DeclDescTag::Func,
                    };
                    let id = scope.decls.push(desc);
                    scope.decls_name_map.insert(sym, id);

                    self.seed_expr(func.body, parent_id);
                }
                ast::DeclKind::Const(_) => todo!(),
            }
        }

        fn seed_expr(&mut self, id: ast::ExprId, parent_id: nr::ScopeDescId) {
            let expr = &self.ast.exprs[id];
            match &expr.kind {
                ast::ExprKind::Path(_) => {}
                ast::ExprKind::Literal(_) => {}

                ast::ExprKind::Block(blk) => {
                    let block_scope = self
                        .map
                        .scope_descs
                        .push(nr::ScopeDesc::new(Some(parent_id)));
                    let parent = self.map.scope_descs.get_mut(parent_id).unwrap();
                    parent.children.push(block_scope);
                    self.map
                        .scope_kinds
                        .insert(nr::ScopeKind::Block(id), block_scope);

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

        fn seed_stmt(&mut self, id: ast::StmtId, parent_id: nr::ScopeDescId) {
            let stmt = &self.ast.stmts[id];
            match &stmt.kind {
                ast::StmtKind::Decl(decl) => self.seed_decl(*decl, parent_id),
                ast::StmtKind::Expr(expr) => self.seed_expr(*expr, parent_id),
                ast::StmtKind::Let(l) => self.seed_expr(l.expr, parent_id),
            }
        }
    }

    impl NameResolver<'_> {
        pub fn resolve(&mut self) {
            self.seed();

            self.ast.root.decls.iter().cloned().for_each(|decl_id| {
                self.resolve_decl(decl_id);
            });
        }

        fn resolve_decl(&mut self, decl_id: ast::DeclId) -> nr::DeclId {
            let scope = &self.map.scope_descs[self.current_scope];

            let old_args = std::mem::take(&mut self.args);
            let old_locals = std::mem::take(&mut self.locals);
            let old_locals_crumbs = std::mem::take(&mut self.locals_bread_crumbs);

            let kind = &self.ast.decls[decl_id].kind;
            let ret = match kind {
                ast::DeclKind::Func(func) => {
                    let sym = self.get_tok_sym(func.name);
                    let decl_desc_id = *scope.decls_name_map.get_by_left(&sym).unwrap();
                    let decl_desc = &scope.decls[decl_desc_id];
                    assert_eq!(decl_desc.ast_id, decl_id);
                    assert_eq!(decl_desc.tag, nr::DeclDescTag::Func);

                    let ty = self.get_ty_func(func.ty);

                    // setup args
                    assert!(self.args.is_empty());
                    {
                        let ty = &self.ast.func_tys[func.ty];
                        ty.params.iter().for_each(|param| {
                            if let Some(name) = param.name {
                                let name = self.get_tok_sym(name);
                                let ty = self.get_ty(param.ty);
                                let id = self.map.func_args.push(nr::FuncArg { name, ty });
                                self.args.push(id);
                            }
                        })
                    }

                    let body = self.resolve_expr(func.body);
                    self.args.clear();

                    let func = nr::DeclFunc { ty, body };
                    let id = self.map.decl_funcs.push(func);
                    self.map.decls.insert(decl_desc_id, nr::DeclKind::Func(id));

                    decl_desc_id
                }
                ast::DeclKind::Const(_) => todo!(),
            };

            self.args = old_args;
            self.locals = old_locals;
            self.locals_bread_crumbs = old_locals_crumbs;

            ret
        }

        fn resolve_expr(&mut self, expr_id: ast::ExprId) -> nr::ExprId {
            let expr = &self.ast.exprs[expr_id];
            let kind = match expr.kind.clone() {
                ast::ExprKind::Path(path) => nr::ExprKind::Res(self.resolve_expr_path(&path)),
                ast::ExprKind::Block(blk) => {
                    let blk_scope = {
                        let scope = self
                            .map
                            .scope_kinds
                            .get_by_left(&nr::ScopeKind::Block(expr_id))
                            .unwrap();
                        let current_scope = &self.map.scope_descs[self.current_scope];
                        assert!(current_scope.children.contains(scope));
                        *scope
                    };
                    let old_scope = self.current_scope;
                    self.current_scope = blk_scope;

                    self.push_locals_scope();

                    let stmts = blk
                        .stmts
                        .iter()
                        .cloned()
                        .map(|stmt_id| {
                            let stmt = &self.ast.stmts[stmt_id];
                            let kind = match stmt.kind {
                                ast::StmtKind::Let(ref l) => {
                                    let name = self.get_tok_sym(l.name);
                                    let expr = self.resolve_expr(l.expr);
                                    let ty = l.ty.map(|id| self.get_ty(id));
                                    let local = nr::Local {
                                        scope: self.current_scope,
                                        name,
                                        expr,
                                        ty,
                                    };
                                    let local_id = self.map.locals.push(local);
                                    self.locals.push(local_id);
                                    nr::StmtKind::Let(local_id)
                                }
                                ast::StmtKind::Expr(id) => {
                                    nr::StmtKind::Expr(self.resolve_expr(id))
                                }
                                ast::StmtKind::Decl(id) => {
                                    nr::StmtKind::Decl(self.resolve_decl(id))
                                }
                            };
                            self.map.stmts.push(nr::Stmt {
                                ast_id: stmt_id,
                                kind,
                            })
                        })
                        .collect();

                    self.pop_locals_scope();

                    self.current_scope = old_scope;
                    let block_id = self.map.blocks.push(nr::Block {
                        cst: blk.cst,
                        stmts,
                    });
                    self.map.block_scope_map.insert(blk_scope, block_id);
                    nr::ExprKind::Block(block_id)
                }
                ast::ExprKind::Literal(literal) => nr::ExprKind::Literal(match literal {
                    ast::ExprLiteral::String(str) => {
                        nr::ExprLiteral::String(self.string_intern(str.baked))
                    }
                    ast::ExprLiteral::Integer(int) => nr::ExprLiteral::Integer(int),
                    ast::ExprLiteral::Float(float) => nr::ExprLiteral::Float(float),
                    ast::ExprLiteral::Bool(bool) => nr::ExprLiteral::Bool(bool),
                }),
                ast::ExprKind::Infix(infix) => nr::ExprKind::Infix(nr::ExprInfix {
                    lhs: self.resolve_expr(infix.lhs),
                    rhs: self.resolve_expr(infix.rhs),
                    op: infix.op,
                }),
                ast::ExprKind::Call(call) => nr::ExprKind::Call(nr::ExprCall {
                    callee: self.resolve_expr(call.callee),
                    args: call
                        .args
                        .iter()
                        .cloned()
                        .map(|id| self.resolve_expr(id))
                        .collect(),
                }),
                ast::ExprKind::Return(ret) => {
                    nr::ExprKind::Return(ret.map(|id| self.resolve_expr(id)))
                }
            };
            let expr = nr::Expr {
                ast_id: expr_id,
                kind,
            };
            self.map.exprs.push(expr)
        }

        fn get_ty(&mut self, ty_id: ast::TyId) -> nr::TyId {
            let ty = &self.ast.tys[ty_id].kind;
            let uty = match ty {
                ast::TyKind::Path(path) => nr::UTy::Res(self.resolve_ty_path(path)),
                ast::TyKind::Func(id) => nr::UTy::Func(self.get_ty_func(*id)),
                ast::TyKind::Slice(ty) => nr::UTy::Slice(self.get_ty(*ty)),
                ast::TyKind::Nullable(ty) => nr::UTy::Nullable(self.get_ty(*ty)),
            };

            let id = self.map.utys.get_or_intern(uty);

            self.map.tys.push(nr::Ty {
                ast: Some(ty_id),
                id,
            })
        }

        fn get_ty_func(&mut self, ty_func_id: ast::TyFuncId) -> nr::TyFuncId {
            let func = &self.ast.func_tys[ty_func_id];
            let params = func
                .params
                .iter()
                .map(|param| self.get_ty(param.ty))
                .collect();
            let ret = func.ret.map(|id| self.get_ty(id));

            let func = nr::TyFunc { params, ret };
            self.map.func_tys.get_or_intern(func)
        }

        fn resolve_expr_path(&self, path: &ast::Path) -> nr::ExprPathResolution {
            // @TODO: Resolve paths longer that one element
            assert!(path.segments.len() == 1, "todo!");

            let res = path
                .segments
                .iter()
                .filter(|&e| !e.is_sep())
                .map(|segment| match segment {
                    ast::PathSegment::Sep => unreachable!(),
                    ast::PathSegment::Ident(idx) => {
                        let sym = self.get_tok_sym(*idx);

                        if let Some(local) = self
                            .locals
                            .iter()
                            .rev()
                            .find(|&&id| self.map.locals[id].name == sym)
                            .map(|&id| nr::ExprPathResolution::Local(id))
                        {
                            local
                        } else if let Some(arg) = self
                            .args
                            .iter()
                            .find(|&&id| self.map.func_args[id].name == sym)
                            .map(|&id| nr::ExprPathResolution::Arg(id))
                        {
                            arg
                        } else {
                            nr::ExprPathResolution::Decl(
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
            scope_id: nr::ScopeDescId,
            sym: nr::StringSymbol,
        ) -> nr::DeclId {
            let scope = &self.map.scope_descs[scope_id];

            match scope.decls_name_map.get_by_left(&sym) {
                Some(id) => *id,
                None if scope.parent.is_some() && {
                    let kind = self.map.scope_kinds.get_by_right(&scope_id).unwrap();
                    !kind.is_module()
                } =>
                {
                    let parent = scope.parent.unwrap();
                    self.search_for_decl_in_scope_looking_up(parent, sym)
                }
                None => {
                    let strings = self.strings.borrow();
                    todo!("could not find value `{}` in scope", strings[sym]);
                }
            }
        }

        fn resolve_ty_path(&self, path: &ast::Path) -> nr::TyPathResolution {
            if path.segments.len() == 1 {
                if let Some(ast::PathSegment::Ident(idx)) = path.segments.get(0) {
                    fn chk_int_num(str: &str) -> bool {
                        str.chars().map(|c| c.is_ascii_digit()).all(|b| b)
                    }

                    let str = self.get_tok_slice(*idx);

                    let prim_ty = match str {
                        "sint" | "sintptr" => nr::PrimTy::Integer {
                            signed: true,
                            size: None,
                        },
                        "uint" | "uintptr" => nr::PrimTy::Integer {
                            signed: false,
                            size: None,
                        },

                        "byte" => nr::PrimTy::Integer {
                            signed: false,
                            size: Some(NonZeroU8::new(8).unwrap()),
                        },
                        "sbyte" => nr::PrimTy::Integer {
                            signed: true,
                            size: Some(NonZeroU8::new(8).unwrap()),
                        },

                        "bool" => nr::PrimTy::Bool,
                        "void" => nr::PrimTy::Void,

                        "never" => nr::PrimTy::Never,

                        // if it starts with 's' and the rest of the string is a number
                        _ if str.starts_with('s') && chk_int_num(&str[1..]) => {
                            nr::PrimTy::Integer {
                                signed: true,
                                size: Some(str[1..].parse().unwrap()),
                            }
                        }
                        // if it starts with 'u' and the rest of the string is a number
                        _ if str.starts_with('u') && chk_int_num(&str[1..]) => {
                            nr::PrimTy::Integer {
                                signed: false,
                                size: Some(str[1..].parse().unwrap()),
                            }
                        }
                        _ => todo!(),
                    };
                    nr::TyPathResolution::PrimTy(prim_ty)
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
}
