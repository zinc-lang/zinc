use std::ops::Range;

use crate::{
    parse::cst::{NodeId, TokenIndex},
    source_map::SourceFileId,
    util::index::{define_idx, IndexVec},
};

#[derive(Debug)]
pub struct Ast {
    pub map: AstMap,
    pub root_file: AstFile,
}

#[derive(Debug, Default)]
pub struct AstMap {
    // pub strings: StringInterningVec,
    pub decls: IndexVec<decl::Id, decl::Decl>,
    pub decl_funcs: IndexVec<decl::FuncId, decl::Func>,

    pub tys: IndexVec<ty::Id, ty::Ty>,
    pub ty_funcs: IndexVec<ty::FuncId, ty::Func>,
    pub ty_func_params: IndexVec<ty::FuncParamId, ty::FuncParam>,

    pub exprs: IndexVec<expr::Id, expr::Expr>,
    pub expr_blocks: IndexVec<expr::BlockId, expr::Block>,

    pub scope_map: scope::Map,
}

#[derive(Debug)]
pub struct AstFile {
    pub node: NodeId,
    pub scope: scope::Id,
    pub file: SourceFileId,

    pub decls: Range<decl::Id>,
}

pub mod decl {
    use super::*;

    define_idx! { pub struct Id: u32 != 0 }
    define_idx! { pub struct FuncId: u32 }

    #[derive(Debug)]
    pub struct Decl {
        pub node: NodeId,
        // // pub name: TokenIndex,
        // pub desc: scope::DeclDescId,
        pub kind: Kind,
    }

    #[derive(Debug)]
    pub enum Kind {
        Func(FuncId),
    }

    #[derive(Debug)]
    pub struct Func {
        pub sig: ty::FuncId,
        pub body: Option<expr::Id>,
    }
}

pub mod ty {
    use super::*;

    define_idx! { pub struct Id: u32 != 0 }
    define_idx! { pub struct FuncId: u32 }
    define_idx! { pub struct FuncParamId: u32 != 0 }

    #[derive(Debug)]
    pub struct Ty {
        pub node: NodeId,
        pub kind: Kind,
    }

    #[derive(Debug)]
    pub enum Kind {
        Res(res::Resolution),
        Func(FuncId),
    }

    pub mod res {
        // use super::*;

        #[derive(Debug)]
        pub enum Resolution {
            Primitive(Primitive),
        }

        #[derive(Debug)]
        pub enum Primitive {
            Integer(IntegerPrimitive),
            Bool,
            Void,
        }

        #[derive(Debug, Clone)]
        pub enum IntegerPrimitive {
            Sint,
            Uint,
            SintSized(bool, u8),
            UintSized(bool, u8),
        }
    }

    #[derive(Debug)]
    pub struct Func {
        pub params: Option<Range<FuncParamId>>,
        pub ret: Option<ty::Id>,
    }

    #[derive(Debug)]
    pub struct FuncParam {
        pub name: TokenIndex,
        pub ty: ty::Id,
    }
}

pub mod expr {
    use super::*;

    define_idx! { pub struct Id: u32 != 0 }
    define_idx! { pub struct BlockId: u32 }

    #[derive(Debug)]
    pub struct Expr {
        pub node: NodeId,
        pub kind: Kind,
    }

    #[derive(Debug)]
    pub enum Kind {
        Res(res::Resolution),
        Literal(Literal),
        Let(Let),
        Block(BlockId),
        Call(Call),
        Infix(Infix),
    }

    pub mod res {
        use super::*;

        #[derive(Debug)]
        pub enum Resolution {
            Decl(scope::DeclDescId),
            // @TODO: Local/Arg
        }
    }

    #[derive(Debug)]
    pub enum Literal {
        // String(StringSymbol),
        Integer(u64),
        Float(f64),
        Boolean(bool),
    }

    #[derive(Debug)]
    pub struct Let {
        pub pattern: pattern::Pattern,
        pub ty: Option<ty::Id>,
        pub expr: expr::Id,
    }

    // #[derive(Debug)]
    // pub struct Set {}

    #[derive(Debug)]
    pub struct Block {
        pub node: NodeId,
        pub scope: scope::Id,
        pub decls: Range<decl::Id>,
        pub exprs: Range<expr::Id>,
        pub end: Option<expr::Id>,
    }

    #[derive(Debug)]
    pub struct Call {
        pub callee: expr::Id,
        pub args: Range<expr::Id>,
    }

    #[derive(Debug)]
    pub struct Infix {
        pub lhs: expr::Id,
        pub rhs: expr::Id,
        pub op: TokenIndex, // @TODO: Multiple tokens
    }
}

pub mod pattern {
    use super::*;

    #[derive(Debug)]
    pub struct Pattern {
        pub node: NodeId,
        pub kind: Kind,
    }

    #[derive(Debug)]
    pub enum Kind {
        Ident(NodeId),
        // @TODO: More
    }
}

pub mod scope {
    use super::*;
    use std::collections::HashMap;

    define_idx! { pub struct Id: u32 != 0 }
    define_idx! { pub struct DeclDescId: u32 }
    define_idx! { pub struct BlockId: u32 }

    #[derive(Debug)]
    pub struct Map {
        pub root: scope::Id,
        pub scopes: IndexVec<scope::Id, Scope>,
        pub parents: HashMap<scope::Id, scope::Id>,

        pub decls: IndexVec<DeclDescId, DeclDesc>,
        pub decls_map: HashMap<DeclDescId, decl::Id>,
        // pub blocks: IndexSet<BlockId>,
    }

    impl Default for Map {
        fn default() -> Self {
            Self::new()
        }
    }

    impl Map {
        pub fn new() -> Self {
            let mut scopes = IndexVec::new();
            let root = scopes.push(Scope::new(Kind::Root));
            Self {
                root,
                scopes,
                parents: HashMap::new(),

                decls: IndexVec::new(),
                decls_map: HashMap::new(),
                // blocks: IndexSet::new(),
            }
        }
    }

    #[derive(Debug)]
    pub struct Scope {
        pub kind: Kind,
        // Due to how and when children get added to this, we cannot use a range
        pub children: Vec<scope::Id>,
        // @FIXME: Use a range
        pub decls: Vec<DeclDescId>,
    }

    impl Scope {
        pub fn new(kind: Kind) -> Self {
            Self {
                kind,
                children: Vec::new(),
                decls: Vec::new(),
            }
        }

        // pub fn get_decls<'a>(&'a self, map: &'a Map) -> impl Iterator<Item = &'a DeclDesc> {
        //     map.decls
        //         .iter()
        //         .zip({
        //             let ref this = map.decls;
        //             this.raw.iter().enumerate().map(|(idx, _)| I::new(idx))
        //         })
        //         .filter(|(_, id)| self.decls.contains(id))
        //         .map(|(desc, _)| desc)
        // }
    }

    #[derive(Debug)]
    pub enum Kind {
        Root,
        Block,
    }

    #[derive(Debug)]
    pub struct DeclDesc {
        pub name: TokenIndex,
        pub tag: DeclDescTag,
        pub node: NodeId,
    }

    #[derive(Debug, PartialEq, Eq)]
    pub enum DeclDescTag {
        Func,
        // @TODO: More
    }
}

pub mod gen {
    use super::*;
    use crate::parse::cst::{Cst, NK};
    use crate::parse::TK;
    use crate::source_map::{SourceFileId, SourceMap};
    use crate::util;

    mod mut_cell {
        use std::cell::RefCell;

        #[derive(Debug)]
        pub struct MutCell<T> {
            value: RefCell<T>,
        }

        impl<T> MutCell<T> {
            pub fn new(value: T) -> Self {
                let value = RefCell::new(value);
                Self { value }
            }

            pub fn with<R>(&self, func: impl FnOnce(&mut T) -> R) -> R {
                let value = &mut *self.value.borrow_mut();
                func(value)
            }

            pub fn into_inner(self) -> T {
                self.value.into_inner()
            }
        }

        impl<T> std::ops::Deref for MutCell<T> {
            type Target = T;

            fn deref(&self) -> &Self::Target {
                unsafe { &*self.value.as_ptr() as &Self::Target }
            }
        }
    }

    use mut_cell::MutCell;

    #[derive(Debug)]
    pub struct Generator<'s> {
        source_map: &'s mut SourceMap,
        map: MutCell<AstMap>,
        current: MutCell<Current>,
    }

    #[derive(Debug)]
    struct Current {
        file: SourceFileId,
        scope: scope::Id,
    }

    impl<'s> Generator<'s> {
        pub fn new(source_map: &'s mut SourceMap, file: SourceFileId) -> Self {
            let map = AstMap::default();
            let scope = map.scope_map.root;

            Self {
                source_map,

                map: MutCell::new(map),
                current: MutCell::new(Current { file, scope }),
            }
        }

        pub fn generate(self) -> Ast {
            let _s = trace_span!("Generate").entered();
            self.seed_file();
            let file = self.gen_file();

            Ast {
                map: self.map.into_inner(),
                root_file: file,
            }
        }

        fn current_cst_no_trivia(&self) -> Cst {
            let tokens = &self.source_map.lex_data[&self.current.file].tokens;
            self.source_map.csts[&self.current.file].trivia_removed(tokens)
        }

        fn seed_file(&self) {
            let _s = trace_span!("Seed file").entered();
            trace!(file = ?self.current.file);

            let cst = self.current_cst_no_trivia();
            self.seed_decls_from_node(cst.root());
        }

        /// Iterating over the `decl`s in the node found within `self.current_file`
        /// get its name and kind then append it to `self.current_scope`
        fn seed_decls_from_node(&self, node: NodeId) {
            let _s = trace_span!("Seed decls from node").entered();
            trace!(scope = ?self.current.scope, ?node);

            let cst = &self.current_cst_no_trivia();

            for &node in cst.nodes(node) {
                if cst.kind(node) == NK::decl {
                    let mut nodes_iter = cst.nodes(node);

                    let name = self.find_token(cst, node, |tk| tk == TK::ident).unwrap();

                    let decl_kind = *nodes_iter.next().unwrap();
                    debug_assert!(nodes_iter.next().is_none());

                    let tag = match cst.kind(decl_kind) {
                        NK::decl_func => scope::DeclDescTag::Func,
                        _ => todo!(),
                    };
                    trace!(?name, ?tag, "Seeded decl");

                    let id = self.map.with(|map| {
                        map.scope_map
                            .decls
                            .push(scope::DeclDesc { name, tag, node })
                    });
                    self.map
                        .with(|map| map.scope_map.scopes[self.current.scope].decls.push(id))
                }
            }
        }

        fn gen_file(&self) -> AstFile {
            let _s = trace_span!("Gen file").entered();
            trace!(file = ?self.current.file);

            let cst = &self.current_cst_no_trivia();

            let mut decls = Vec::new();
            for &node in cst.nodes(cst.root()) {
                match cst.kind(node) {
                    NK::decl => decls.push(self.gen_decl(cst, node)),
                    _ => todo!(),
                }
            }

            let decls = self.alloc_decls_and_descs(decls);

            AstFile {
                node: cst.root(),
                scope: self.current.scope,
                file: self.current.file,
                decls,
            }
        }

        fn find_token(
            &self,
            cst: &Cst,
            node: NodeId,
            mut predicate: impl FnMut(TK) -> bool,
        ) -> Option<TokenIndex> {
            let mut tokens_iter = cst.tokens(node);
            let file_tokens = &self.source_map.lex_data[&self.current.file].tokens;
            tokens_iter
                .find(|&&i| predicate(file_tokens[i.get()]))
                .copied()
        }

        fn gen_decl(&self, cst: &Cst, node: NodeId) -> (decl::Decl, scope::DeclDescId) {
            let _s = trace_span!("Gen decl").entered();
            trace!(?node);

            let mut nodes_iter = cst.nodes(node);

            let decl = *nodes_iter.next().unwrap();
            debug_assert!(nodes_iter.next().is_none());

            let name = self.find_token(cst, node, |tk| tk == TK::ident).unwrap();

            let desc = *self.map.scope_map.scopes[self.current.scope]
                .decls
                .iter()
                .find(|&&s| self.map.scope_map.decls[s].name == name)
                .unwrap();

            let kind = match cst.kind(decl) {
                NK::decl_func => {
                    let mut nodes_iter = cst.nodes(decl);

                    let sig = *nodes_iter.next().unwrap();
                    debug_assert!(cst.kind(sig) == NK::func_sig);
                    let sig = self.gen_ty_func(cst, sig);
                    let sig = self.map.with(|map| map.ty_funcs.push(sig));

                    let body = nodes_iter
                        .find(|&&e| cst.kind(e) == NK::decl_func_body)
                        .map(|&body| {
                            let expr = *cst.nodes(body).next().unwrap();
                            let expr = self.gen_expr(cst, expr);
                            self.map.with(|map| map.exprs.push(expr))
                        });

                    let func = decl::Func { sig, body };
                    let func = self.map.with(|map| map.decl_funcs.push(func));
                    decl::Kind::Func(func)
                }
                _ => todo!(),
            };

            (decl::Decl { node, kind }, desc)
        }

        fn alloc_decls_and_descs(
            &self,
            vec: Vec<(decl::Decl, scope::DeclDescId)>,
        ) -> Range<decl::Id> {
            let (decls, decl_descs): (Vec<_>, Vec<_>) = vec.into_iter().unzip();

            let decls = self.map.with(|map| map.decls.push_range(decls.into_iter()));

            for (decl, desc) in util::index::indicies_of_range(decls.clone())
                .into_iter()
                .zip(decl_descs)
            {
                self.map
                    .with(|map| map.scope_map.decls_map.insert(desc, decl));
            }

            decls
        }

        fn gen_expr(&self, cst: &Cst, node: NodeId) -> expr::Expr {
            let _s = trace_span!("Gen expr").entered();
            trace!(?node);

            let kind = match cst.kind(node) {
                NK::expr_block => {
                    let scope = scope::Scope::new(scope::Kind::Block);
                    let scope = self.map.with(|map| map.scope_map.scopes.push(scope));

                    self.map.with(|map| {
                        let parent = &mut map.scope_map.scopes[self.current.scope];
                        parent.children.push(scope);

                        map.scope_map.parents.insert(scope, self.current.scope);
                    });

                    let old_scope = self.current.scope;
                    self.current.with(|current| current.scope = scope);

                    self.seed_decls_from_node(node);

                    let mut decls = Vec::new();
                    let mut exprs = Vec::new();
                    let mut end = None;
                    for &node in cst.nodes(node) {
                        match cst.kind(node) {
                            NK::decl => decls.push(self.gen_decl(cst, node)),
                            NK::expr_block_end => {
                                end = Some(node);
                                break;
                            }
                            _ => exprs.push(node),
                        }
                    }

                    let decls = self.alloc_decls_and_descs(decls);

                    let exprs = exprs
                        .into_iter()
                        .map(|node| self.gen_expr(cst, node))
                        .collect::<Vec<_>>();
                    let end = end.map(|node| self.gen_expr(cst, node));

                    let exprs = self.map.with(|map| map.exprs.push_range(exprs.into_iter()));
                    let end = end.map(|end| self.map.with(|map| map.exprs.push(end)));

                    let block = expr::Block {
                        node,
                        scope,
                        decls,
                        exprs,
                        end,
                    };

                    self.current.with(|current| current.scope = old_scope);

                    let block = self.map.with(|map| map.expr_blocks.push(block));
                    expr::Kind::Block(block)
                }

                // NK::path => {
                //     let tokens = &self.source_map.lex_data[&self.current.file].tokens;
                //     let tokens = cst.tokens(node).map(|i| tokens[i.get()]);

                //     todo!()
                // }

                //
                NK::integer => {
                    let index = self
                        .find_token(cst, node, |tk| {
                            matches!(tk, TK::int_dec | TK::int_bin | TK::int_oct | TK::int_hex)
                        })
                        .unwrap();
                    let lex_data = &self.source_map.lex_data[&self.current.file];
                    let range = &lex_data.ranges[index.get()];
                    let source = &self.source_map.sources[&self.current.file];
                    let str = &source[range.clone()];

                    let integer = str.parse::<u64>().unwrap();

                    expr::Kind::Literal(expr::Literal::Integer(integer))
                }

                NK::expr_infix => {
                    let mut nodes_iter = cst.nodes(node);

                    let lhs = *nodes_iter.next().unwrap();
                    let op = *nodes_iter.next().unwrap();
                    let rhs = *nodes_iter.next().unwrap();

                    let lhs = self.gen_expr(cst, lhs);
                    let rhs = self.gen_expr(cst, rhs);

                    let (lhs, rhs) = self
                        .map
                        .with(|map| (map.exprs.push(lhs), map.exprs.push(rhs)));

                    let op = *cst.tokens(op).next().unwrap();

                    expr::Kind::Infix(expr::Infix { lhs, rhs, op })
                }

                NK::expr_call => {
                    let mut nodes_iter = cst.nodes(node);

                    let callee = *nodes_iter.next().unwrap();

                    let callee = if cst.kind(callee) == NK::path {
                        let file_tokens = &self.source_map.lex_data[&self.current.file].tokens;
                        let mut tokens = cst.tokens(callee);
                        let name = *tokens.find(|i| file_tokens[i.get()] == TK::ident).unwrap();

                        // debug!(vec = ?cst.tokens(callee).collect::<Vec<_>>());

                        let decl = self.expr_resolve_decl(self.current.scope, name).unwrap();

                        expr::Expr {
                            node: callee,
                            kind: expr::Kind::Res(expr::res::Resolution::Decl(decl)),
                        }
                    } else {
                        self.gen_expr(cst, callee)
                    };
                    let callee = self.map.with(|map| map.exprs.push(callee));

                    let args = nodes_iter.map(|&id| self.gen_expr(cst, id));
                    let args = self.map.with(|map| map.exprs.push_range(args));

                    expr::Kind::Call(expr::Call { callee, args })
                }

                _ => todo!("more expr"),
            };

            expr::Expr { node, kind }
        }

        fn expr_resolve_decl(
            &self,
            scope_id: scope::Id,
            name_token: TokenIndex,
        ) -> Option<scope::DeclDescId> {
            let scope = &self.map.scope_map.scopes[scope_id];

            let ranges = &self.source_map.lex_data[&self.current.file].ranges;
            let source = &self.source_map.sources[&self.current.file];

            let name = &source[ranges[name_token.get()].clone()];

            // @FIXME:...
            // This is a bit cluncky, maybe we could store functions in thier own vec within scope
            if let Some(func) = scope
                .decls
                .iter()
                .map(|&id| (id, &self.map.scope_map.decls[id]))
                .find(|(_, decl)| &source[ranges[decl.name.get()].clone()] == name)
                .map(|(id, _)| id)
            {
                Some(func)
            } else {
                match scope.kind {
                    scope::Kind::Root => None,
                    scope::Kind::Block => {
                        let parent = self.map.scope_map.parents[&scope_id];
                        self.expr_resolve_decl(parent, name_token)
                    }
                }
            }
        }

        fn gen_ty(&self, cst: &Cst, node: NodeId) -> ty::Ty {
            let _s = trace_span!("Gen ty").entered();
            trace!(?node);

            let kind = match cst.kind(node) {
                NK::path => {
                    // @TODO: Mulitiple path segments
                    let ident = self.find_token(cst, node, |tk| tk == TK::ident).unwrap();
                    let ident = &self.source_map.sources[&self.current.file]
                        [self.source_map.lex_data[&self.current.file].ranges[ident.get()].clone()];

                    let res = match ident {
                        "sint" => ty::res::Resolution::Primitive(ty::res::Primitive::Integer(
                            ty::res::IntegerPrimitive::Sint,
                        )),
                        "uint" => ty::res::Resolution::Primitive(ty::res::Primitive::Integer(
                            ty::res::IntegerPrimitive::Uint,
                        )),
                        "void" => ty::res::Resolution::Primitive(ty::res::Primitive::Void),
                        _ => todo!(),
                    };
                    ty::Kind::Res(res)
                }
                NK::func_sig => {
                    let func = self.gen_ty_func(cst, node);
                    let func = self.map.with(|map| map.ty_funcs.push(func));
                    ty::Kind::Func(func)
                }
                _ => todo!(),
            };

            ty::Ty { node, kind }
        }

        fn gen_ty_func(&self, cst: &Cst, node: NodeId) -> ty::Func {
            let _s = trace_span!("Gen ty func").entered();
            trace!(?node);

            let mut nodes_iter = cst.nodes(node);

            let params = nodes_iter
                .find(|&&node| cst.kind(node) == NK::paramsList)
                .map(|&node| {
                    let mut params = Vec::new();
                    for &node in cst.nodes(node) {
                        let param = match cst.kind(node) {
                            NK::param_param => {
                                let name =
                                    self.find_token(cst, node, |tk| tk == TK::ident).unwrap();
                                let ty = *cst.nodes(node).next().unwrap();
                                let ty = self.gen_ty(cst, ty);
                                let ty = self.map.with(|map| map.tys.push(ty));
                                ty::FuncParam { name, ty }
                            }
                            _ => todo!(),
                        };
                        params.push(param);
                    }
                    self.map
                        .with(|map| map.ty_func_params.push_range(params.into_iter()))
                });

            let ret = nodes_iter.next().map(|&node| {
                let ty = self.gen_ty(cst, node);
                self.map.with(|map| map.tys.push(ty))
            });

            ty::Func { params, ret }
        }
    }
}
