use crate::{
    parse::cst::{NodeId, TokenIndex},
    source_map::SourceFileId,
    util::index::{define_idx, IndexVec, StringInterningVec},
};
use std::ops::Range;

pub mod gen;

#[derive(Debug)]
pub struct Ast {
    pub map: AstMap,
    pub root_file: AstFile,
}

#[derive(Debug, Default)]
pub struct AstMap {
    pub strings: StringInterningVec,

    pub decls: IndexVec<decl::Id, decl::Decl>,
    pub decl_funcs: IndexVec<decl::FuncId, decl::Func>,

    pub tys: IndexVec<ty::Id, ty::Ty>,
    pub ty_funcs: IndexVec<ty::FuncId, ty::Func>,
    pub ty_func_params: IndexVec<ty::FuncParamId, ty::FuncParam>,

    pub exprs: IndexVec<expr::Id, expr::Expr>,
    pub expr_basic_lets: IndexVec<expr::LetBasicId, expr::LetBasic>,
    pub expr_blocks: IndexVec<expr::BlockId, expr::Block>,

    pub scope: scope::Map,
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
    use crate::util::index::StringSymbol;

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
        pub ret: Option<ty::Id>,
    }

    #[derive(Debug)]
    pub struct FuncParam {
        pub name: StringSymbol,
        pub ty: ty::Id,
    }
}

pub mod expr {
    use crate::util::index::StringSymbol;

    use super::*;

    define_idx! { pub struct Id: u32 != 0 }
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
        pub ty: Option<ty::Id>,
        pub expr: expr::Id,
    }

    #[derive(Debug)]
    pub struct LetPattern {
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
        // Ident(NodeId),
        // @TODO: More
    }
}

pub mod scope {
    use crate::util::{self, index::StringSymbol};

    use super::*;
    use std::collections::HashMap;

    define_idx! { pub struct Id: u32 != 0 }
    define_idx! { pub struct DeclDescId: u32 }
    // define_idx! { pub struct BlockId: u32 }
    define_idx! { pub struct ArgId: u32 }
    define_idx! { pub struct LocalId: u32 }

    #[derive(Debug)]
    pub struct Map {
        pub root: scope::Id,
        pub scopes: IndexVec<scope::Id, Scope>,
        pub parents: HashMap<scope::Id, scope::Id>,

        pub decls: IndexVec<DeclDescId, DeclDesc>,
        pub decls_map: HashMap<DeclDescId, decl::Id>,
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
            let root = scopes.push(Scope::new(Kind::Root));
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

    #[derive(Debug)]
    pub struct Scope {
        pub kind: Kind,
        // Due to how and when children get added to this, we cannot use a range, same for locals
        pub children: Vec<scope::Id>,
        pub decls: Range<DeclDescId>,
        pub args: Range<ty::FuncParamId>,
        pub locals: Vec<expr::LetBasicId>,
    }

    impl Scope {
        pub fn new(kind: Kind) -> Self {
            Self {
                kind,
                children: Vec::new(),
                decls: util::index::empty_range(),
                args: util::index::empty_range(),
                locals: Vec::new(),
            }
        }
    }

    #[derive(Debug)]
    pub enum Kind {
        Root,
        Block,
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
