use super::*;
use crate::{
    parse::{
        cst::{Cst, NK},
        TK,
    },
    source_map::{SourceFileId, SourceMap},
    util::index::{self, StringSymbol},
};

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

        #[track_caller]
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
            // unsafe { &*(self.value.borrow().deref() as *const T) }
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
        let scope = map.scope.root;

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

    fn get_token_string(&self, token: TokenIndex) -> StringSymbol {
        let str = &self.source_map.sources[&self.current.file]
            [self.source_map.lex_data[&self.current.file].ranges[token.get()].clone()];
        self.map.with(|map| map.strings.str_get_or_intern(str))
    }

    /// Iterating over the `decl`s in the node found within `self.current_file`
    /// get its name and kind then append it to `self.current_scope`
    fn seed_decls_from_node(&self, node: NodeId) {
        let _s = trace_span!("Seed decls from node").entered();
        trace!(scope = ?self.current.scope, ?node);

        let cst = &self.current_cst_no_trivia();

        let mut decls = Vec::new();
        for &node in cst.nodes(node) {
            if cst.kind(node) == NK::decl {
                let mut nodes_iter = cst.nodes(node);

                let name = self.find_token(cst, node, |tk| tk == TK::ident).unwrap();
                let name = self.get_token_string(name);

                let decl_kind = *nodes_iter.next().unwrap();
                debug_assert!(nodes_iter.next().is_none());

                let tag = match cst.kind(decl_kind) {
                    NK::decl_func => scope::DeclDescTag::Func,
                    _ => todo!(),
                };
                trace!(?name, ?tag, "Seeded decl");

                decls.push(scope::DeclDesc { name, tag, node });
                // let id = self
                //     .map
                //     .with(|map| map.scope.decls.push(scope::DeclDesc { name, tag, node }));
                // self.map
                //     .with(|map| map.scope.scopes[self.current.scope].decls.push(id))
            }
        }

        let decls = self
            .map
            .with(|map| map.scope.decls.push_range(decls.into_iter()));
        self.map
            .with(|map| map.scope.scopes[self.current.scope].decls = decls);
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

    fn gen_decl(&self, cst: &Cst, node: NodeId) -> (decl::Decl, scope::DeclDescId) {
        let _s = trace_span!("Gen decl").entered();
        trace!(?node);

        let mut nodes_iter = cst.nodes(node);

        let decl = *nodes_iter.next().unwrap();
        debug_assert!(nodes_iter.next().is_none());

        let name = self.find_token(cst, node, |tk| tk == TK::ident).unwrap();
        let name = self.get_token_string(name);

        let desc =
            index::indicies_of_range(self.map.scope.scopes[self.current.scope].decls.clone())
                .find(|&s| self.map.scope.decls[s].name == name)
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

    fn alloc_decls_and_descs(&self, vec: Vec<(decl::Decl, scope::DeclDescId)>) -> Range<decl::Id> {
        let (decls, decl_descs): (Vec<_>, Vec<_>) = vec.into_iter().unzip();

        let decls = self.map.with(|map| map.decls.push_range(decls.into_iter()));

        for (decl, desc) in index::indicies_of_range(decls.clone())
            .into_iter()
            .zip(decl_descs)
        {
            self.map.with(|map| map.scope.decls_map.insert(desc, decl));
        }

        decls
    }

    fn gen_expr(&self, cst: &Cst, node: NodeId) -> expr::Expr {
        let _s = trace_span!("Gen expr").entered();
        trace!(?node);

        let kind = match cst.kind(node) {
            NK::expr_block => {
                let scope = scope::Scope::new(scope::Kind::Block);
                let scope = self.map.with(|map| map.scope.scopes.push(scope));

                self.map.with(|map| {
                    let parent = &mut map.scope.scopes[self.current.scope];
                    parent.children.push(scope);

                    map.scope.parents.insert(scope, self.current.scope);
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
                let end = end.map(|node| self.gen_expr(cst, *cst.nodes(node).next().unwrap()));

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

            NK::path => {
                let file_tokens = &self.source_map.lex_data[&self.current.file].tokens;
                let mut tokens = cst.tokens(node);
                let name = *tokens.find(|i| file_tokens[i.get()] == TK::ident).unwrap();
                let name = self.get_token_string(name);

                let res = self.expr_resolve(self.current.scope, name).unwrap();
                expr::Kind::Res(res)
            }

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
                    let name = self.get_token_string(name);

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
                let args = args.collect::<Vec<_>>(); // just passing args was trigering the RefCell
                let args = self.map.with(|map| map.exprs.push_range(args.into_iter()));

                expr::Kind::Call(expr::Call { callee, args })
            }

            NK::expr_let_basic => {
                let mut nodes_iter = cst.nodes(node);

                let ident = *nodes_iter.next().unwrap();
                let ident = *cst.tokens(ident).next().unwrap();
                let ident = self.get_token_string(ident);

                let next = *nodes_iter.next().unwrap();

                let (ty, expr) = if cst.kind(next) == NK::expr_let_ty {
                    let ty = self.gen_ty(cst, next);
                    let ty = self.map.with(|map| map.tys.push(ty));

                    let expr = *nodes_iter.next().unwrap();
                    let expr = self.gen_expr(cst, expr);

                    (Some(ty), expr)
                } else {
                    let expr = self.gen_expr(cst, next);
                    (None, expr)
                };
                let expr = self.map.with(|map| map.exprs.push(expr));

                let binding = expr::LetBasic { ident, ty, expr };
                let binding = self.map.with(|map| map.expr_basic_lets.push(binding));

                self.map
                    .with(|map| map.scope.scopes[self.current.scope].locals.push(binding));

                expr::Kind::LetBasic(binding)
            }

            _ => todo!("more expr '{:?}'", cst.kind(node)),
        };

        expr::Expr { node, kind }
    }

    fn expr_resolve(
        &self,
        scope_id: scope::Id,
        name: StringSymbol,
    ) -> Option<expr::res::Resolution> {
        let scope = &self.map.scope.scopes[scope_id];

        // search locals first, then args, then the parent scope
        scope
            .locals
            .iter()
            .find(|&&id| self.map.expr_basic_lets[id].ident == name)
            .cloned()
            .map(expr::res::Resolution::Local)
            .or_else(|| {
                index::indicies_of_range(scope.args.clone())
                    .find(|&id| self.map.ty_func_params[id].name == name)
                    .map(expr::res::Resolution::Arg)
                    .or_else(|| {
                        index::indicies_of_range(scope.decls.clone())
                            .find(|&id| self.map.scope.decls[id].name == name)
                            .map(expr::res::Resolution::Decl)
                    })
                    .or_else(|| match scope.kind {
                        scope::Kind::Root => None,
                        scope::Kind::Block => {
                            let parent = self.map.scope.parents[&scope_id];
                            self.expr_resolve(parent, name)
                        }
                    })
            })
    }

    fn expr_resolve_decl(
        &self,
        scope_id: scope::Id,
        name: StringSymbol,
    ) -> Option<scope::DeclDescId> {
        let scope = &self.map.scope.scopes[scope_id];

        index::indicies_of_range(scope.decls.clone())
            .map(|id| (id, &self.map.scope.decls[id]))
            .find(|(_, decl)| decl.name == name)
            .map(|(id, _)| id)
            .or_else(|| match scope.kind {
                scope::Kind::Root => None,
                scope::Kind::Block => {
                    let parent = self.map.scope.parents[&scope_id];
                    self.expr_resolve_decl(parent, name)
                }
            })
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
                            let name = self.find_token(cst, node, |tk| tk == TK::ident).unwrap();
                            let name = self.get_token_string(name);

                            let ty = *cst.nodes(node).next().unwrap();
                            let ty = self.gen_ty(cst, ty);
                            let ty = self.map.with(|map| map.tys.push(ty));

                            ty::FuncParam { name, ty }
                        }
                        _ => todo!(),
                    };
                    params.push(param);
                }

                let params = self
                    .map
                    .with(|map| map.ty_func_params.push_range(params.into_iter()));

                self.map
                    .with(|map| map.scope.scopes[self.current.scope].args = params.clone());

                params
            });

        let ret = nodes_iter.next().map(|&node| {
            let ty = self.gen_ty(cst, node);
            self.map.with(|map| map.tys.push(ty))
        });

        ty::Func { params, ret }
    }
}
