use crate::{
    parse::cst::{NodeId, TokenIndex},
    source_map::SourceFileId,
    util::index::{define_idx, IndexVec, StringInterningVec, StringSymbol},
};
use std::ops::Range;

pub mod gen;

pub use {decl::DeclId, expr::ExprId, scope::ScopeId, ty::TyId};

#[derive(Debug)]
pub struct Ast {
    pub map: AstMap,
    pub root_file: AstFile,
}

#[derive(Debug, Default)]
pub struct AstMap {
    pub strings: StringInterningVec,

    pub decls: IndexVec<DeclId, decl::Decl>,
    pub decl_funcs: IndexVec<decl::FuncId, decl::Func>,

    pub tys: IndexVec<TyId, ty::Ty>,
    pub ty_funcs: IndexVec<ty::FuncId, ty::Func>,
    pub ty_func_params: IndexVec<ty::FuncParamId, ty::FuncParam>,

    pub exprs: IndexVec<ExprId, expr::Expr>,
    pub expr_basic_lets: IndexVec<expr::LetBasicId, expr::LetBasic>,
    pub expr_blocks: IndexVec<expr::BlockId, expr::Block>,

    pub scope: scope::Map,
}

#[derive(Debug)]
pub struct AstFile {
    pub node: NodeId,
    pub scope: ScopeId,
    pub file: SourceFileId,

    pub decls: Range<DeclId>,
}

pub mod decl {
    use super::*;

    define_idx! { pub struct DeclId: u32 != 0 }
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
        pub body: Option<ExprId>,
    }
}

pub mod ty {
    use super::*;

    define_idx! { pub struct TyId: u32 != 0 }
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
            Err,
        }

        #[derive(Debug)]
        pub enum Primitive {
            Integer(IntegerPrimitive),
            // Bool,
            Void,
        }

        #[derive(Debug, Clone)]
        pub enum IntegerPrimitive {
            Sint,
            Uint,
            // SintSized(bool, u8),
            // UintSized(bool, u8),
        }
    }

    #[derive(Debug)]
    pub struct Func {
        pub params: Option<Range<FuncParamId>>,
        pub ret: Option<TyId>,
    }

    #[derive(Debug)]
    pub struct FuncParam {
        pub name: StringSymbol,
        pub ty: TyId,
    }
}

pub mod expr {
    use super::*;

    define_idx! { pub struct ExprId: u32 != 0 }
    define_idx! { pub struct BlockId: u32 }
    define_idx! { pub struct LetBasicId: u32 }

    #[derive(Debug)]
    pub struct Expr {
        pub node: NodeId,
        pub kind: Kind,
    }

    #[derive(Debug)]
    pub enum Kind {
        Res(res::Resolution),
        Literal(Literal),
        LetBasic(LetBasicId),
        // LetPattern(LetPattern),
        Block(BlockId),
        Call(Call),
        Infix(Infix),
    }

    pub mod res {
        use super::*;

        #[derive(Debug)]
        pub enum Resolution {
            Decl(scope::DeclDescId),
            Arg(ty::FuncParamId),
            Local(LetBasicId),
            Err,
        }
    }

    #[derive(Debug)]
    pub enum Literal {
        // String(StringSymbol),
        Integer(u64),
        // Float(f64),
        // Boolean(bool),
    }

    #[derive(Debug)]
    pub struct LetBasic {
        pub ident: StringSymbol,
        pub ty: Option<TyId>,
        pub expr: ExprId,
    }

    #[derive(Debug)]
    pub struct LetPattern {
        pub pattern: pattern::Pattern,
        pub ty: Option<TyId>,
        pub expr: ExprId,
    }

    // #[derive(Debug)]
    // pub struct Set {}

    #[derive(Debug)]
    pub struct Block {
        pub node: NodeId,
        pub scope: ScopeId,
        pub decls: Range<DeclId>,
        pub exprs: Range<ExprId>,
        pub end: Option<ExprId>,
    }

    #[derive(Debug)]
    pub struct Call {
        pub callee: ExprId,
        pub args: Range<ExprId>,
    }

    #[derive(Debug)]
    pub struct Infix {
        pub lhs: ExprId,
        pub rhs: ExprId,
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
        // Ident(NodeId),
        // @TODO: More
    }
}

pub mod scope {
    use super::*;
    use crate::util::index::{self, StringSymbol};
    use std::collections::HashMap;

    define_idx! { pub struct ScopeId: u32 != 0 }
    define_idx! { pub struct DeclDescId: u32 }
    // define_idx! { pub struct BlockId: u32 }
    define_idx! { pub struct ArgId: u32 }
    define_idx! { pub struct LocalId: u32 }

    #[derive(Debug)]
    pub struct Map {
        pub root: ScopeId,
        pub scopes: IndexVec<ScopeId, Scope>,
        pub parents: HashMap<ScopeId, ScopeId>,

        pub decls: IndexVec<DeclDescId, DeclDesc>,
        // pub decls_map: HashMap<DeclDescId, DeclId>,
        pub decls_map: HashMap<DeclId, DeclDescId>,
        // pub blocks: IndexSet<BlockId>,

        //
        // pub locals: IndexVec<LocalId, Local>,
    }

    impl Default for Map {
        fn default() -> Self {
            Self::new()
        }
    }

    impl Map {
        pub fn new() -> Self {
            let mut scopes = IndexVec::new();
            // let root = scopes.push(Scope::new(Kind::Root));
            let root = scopes.push(Scope::new_root());
            Self {
                root,
                scopes,
                parents: HashMap::new(),

                decls: IndexVec::new(),
                decls_map: HashMap::new(),
                // locals: IndexVec::new(),
            }
        }
    }

    // #[derive(Debug)]
    // pub struct Scope {
    //     pub kind: Kind,
    //     // Due to how and when children get added to this, we cannot use a range, same for locals
    //     pub children: Vec<scope::Id>,
    //     pub decls: Range<DeclDescId>,
    //     pub args: Range<ty::FuncParamId>,
    //     pub locals: Vec<expr::LetBasicId>,
    // }

    // impl Scope {
    //     pub fn new(kind: Kind) -> Self {
    //         Self {
    //             kind,
    //             children: Vec::new(),
    //             decls: util::index::empty_range(),
    //             args: util::index::empty_range(),
    //             locals: Vec::new(),
    //         }
    //     }
    // }

    // #[derive(Debug)]
    // pub enum Kind {
    //     Root,
    //     Block,
    // }

    #[derive(Debug)]
    #[non_exhaustive]
    pub enum Scope {
        Root(RootScope),
        Block(BlockScope),
        Func(FuncScope),
    }

    impl Scope {
        pub(self) fn new_root() -> Self {
            Self::Root(RootScope {
                children: Vec::new(),
                decls: index::empty_range(),
            })
        }

        pub fn new_block() -> Self {
            Self::Block(BlockScope {
                children: Vec::new(),
                decls: index::empty_range(),
                locals: Vec::new(),
            })
        }

        pub fn new_func() -> Self {
            Self::Func(FuncScope {
                child: None,
                args: index::empty_range(),
            })
        }

        pub fn set_decls(&mut self, decls: Range<DeclDescId>) {
            match self {
                Scope::Root(root) => {
                    debug_assert!(root.decls == index::empty_range());
                    root.decls = decls;
                }
                Scope::Block(block) => {
                    debug_assert!(block.decls == index::empty_range());
                    block.decls = decls;
                }
                Scope::Func(_) => unreachable!("Cannot add decls to a function scope"),
            }
        }

        pub fn set_args(&mut self, args: Range<ty::FuncParamId>) {
            match self {
                Scope::Root(_) => unreachable!("Cannot set args in root scope"),
                Scope::Block(_) => unreachable!("Cannot set arsg in block scope"),
                Scope::Func(func) => {
                    debug_assert!(func.args == index::empty_range());
                    func.args = args;
                }
            }
        }

        pub fn push_child(&mut self, child: ScopeId) {
            match self {
                Scope::Root(root) => root.children.push(child),
                Scope::Block(block) => block.children.push(child),
                Scope::Func(func) => {
                    if func.child.is_none() {
                        func.child = Some(child)
                    } else {
                        unreachable!("Cannot append more than one child to a function scope")
                    }
                }
            }
        }

        pub fn push_local(&mut self, local: expr::LetBasicId) {
            match self {
                Scope::Block(block) => block.locals.push(local),
                Scope::Root(_) => unreachable!("Cannot push local to root scope"),
                Scope::Func(_) => unreachable!("Cannot push local to funciton scope"),
            }
        }

        pub fn get_decls(&self) -> &Range<DeclDescId> {
            match self {
                Scope::Root(root) => &root.decls,
                Scope::Block(block) => &block.decls,
                Scope::Func(_) => unreachable!(),
            }
        }

        pub fn get_locals(&self) -> &[expr::LetBasicId] {
            match self {
                Scope::Block(block) => block.locals.as_slice(),
                Scope::Root(_) => &[],
                Scope::Func(_) => &[],
            }
        }

        pub fn get_args(&self) -> &Range<ty::FuncParamId> {
            match self {
                Scope::Func(func) => &func.args,
                Scope::Root(_) => unreachable!(),
                Scope::Block(_) => unimplemented!(),
            }
        }

        /// Returns `true` if the scope is [`Root`].
        ///
        /// [`Root`]: Scope::Root
        #[must_use]
        pub fn is_root(&self) -> bool {
            matches!(self, Self::Root(..))
        }

        /// Returns `true` if the scope is [`Block`].
        ///
        /// [`Block`]: Scope::Block
        #[must_use]
        pub fn is_block(&self) -> bool {
            matches!(self, Self::Block(..))
        }

        /// Returns `true` if the scope is [`Func`].
        ///
        /// [`Func`]: Scope::Func
        #[must_use]
        pub fn is_func(&self) -> bool {
            matches!(self, Self::Func(..))
        }

        pub fn can_search_up(&self) -> bool {
            !self.is_root()
            // @TODO: && !self.is_module()
        }
    }

    #[derive(Debug)]
    pub struct RootScope {
        children: Vec<ScopeId>,
        decls: Range<DeclDescId>,
    }

    #[derive(Debug)]
    pub struct BlockScope {
        children: Vec<ScopeId>,
        decls: Range<DeclDescId>,
        locals: Vec<expr::LetBasicId>,
    }

    #[derive(Debug)]
    pub struct FuncScope {
        child: Option<ScopeId>,
        args: Range<ty::FuncParamId>,
    }

    #[derive(Debug)]
    pub struct DeclDesc {
        pub name: StringSymbol,
        pub tag: DeclDescTag,
        pub node: NodeId,
    }

    #[derive(Debug, PartialEq, Eq)]
    pub enum DeclDescTag {
        Func,
        // @TODO: More
    }
}
