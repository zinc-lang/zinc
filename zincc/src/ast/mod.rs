//! # AST
//!
//! ## TODO
//!
//! Documentation
//!
//! ## Idea - lazy ast
//!
//! With the way the cst works it could make sense to create an ast of the same
//! structure as this one but with lazy evaluation of the properties and
//! children items.

use crate::{
    parse::{cst::NodeId, TokenIndex},
    source_map::SourceFileId,
    util::index::StringSymbol,
};
use thin_vec::ThinVec;

pub mod gen;

pub struct P<T: ?Sized> {
    ptr: Box<T>,
}

#[allow(non_snake_case)]
pub fn P<T: 'static>(value: T) -> P<T> {
    P {
        ptr: Box::new(value),
    }
}

impl<T: std::fmt::Debug> std::fmt::Debug for P<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // f.debug_struct("P").field("ptr", &self.ptr).finish()
        write!(f, "P@")?;
        std::fmt::Debug::fmt(&self.ptr, f)
    }
}

#[allow(unused)] // @TODO: Remove
impl<T: 'static> P<T> {
    /// Move out of the pointer
    pub fn and_then<U>(self, f: impl FnOnce(T) -> U) -> U {
        f(*self.ptr)
    }

    /// Equivalent to `and_then(|x| x)`
    pub fn into_inner(self) -> T {
        *self.ptr
    }

    /// Producce a new `P<T>` from `self` without reallocating
    pub fn map(mut self, f: impl FnOnce(T) -> T) -> P<T> {
        let x = f(*self.ptr);
        *self.ptr = x;

        self
    }

    /// Optionally produce a new `P<T>` from `self` without reallocating
    pub fn filter_map(mut self, f: impl FnOnce(T) -> Option<T>) -> Option<P<T>> {
        *self.ptr = f(*self.ptr)?;
        Some(self)
    }
}

#[derive(Debug)]
pub struct AstFile {
    pub file_id: SourceFileId,
    pub decls: ThinVec<P<Decl>>,
}

#[derive(Debug, Clone)]
pub struct Ident {
    pub name: StringSymbol,
    pub token_index: TokenIndex,
}

#[derive(Debug)]
pub struct Path {
    pub node: NodeId,
    pub segments: ThinVec<PathSegment>,
}

#[derive(Debug)]
pub struct PathSegment {
    pub ident: Ident,
    // pub args // @TODO: comptime args
}

#[derive(Debug)]
pub struct Decl {
    pub node: NodeId,
    pub kind: DeclKind,
}

#[derive(Debug)]
pub enum DeclKind {
    Function(DeclFunction),
}

#[derive(Debug)]
pub struct DeclFunction {
    pub ident: Ident,
    // @TODO: comptime arguments
    pub params: Option<ThinVec<P<DeclFunctionParam>>>,
    pub return_ty: Option<P<Ty>>,
    pub body: Option<P<Expr>>,
}

#[derive(Debug)]
pub struct DeclFunctionParam {
    pub ident: Ident,
    pub named: bool,
    pub external_name: Option<Ident>,
    pub ty: P<Ty>,
    pub default: Option<P<Expr>>,
}

/// A type as it appears lexically in the source code
#[derive(Debug)]
pub struct Ty {
    pub node: NodeId,
    pub kind: TyKind,
}

#[derive(Debug)]
pub enum TyKind {
    // Infer, // @TODO: Do we need this?
    Err,
    Path(Path),
    Slice(P<Ty>),
    Array(P<Ty>, P<Expr>),
    Nullable(P<Ty>),
}

#[derive(Debug)]
pub struct Expr {
    pub node: NodeId,
    // @TODO: attributes
    pub kind: ExprKind,
}

#[derive(Debug)]
pub enum ExprKind {
    Err,
    Path(Path),
    Block(ExprBlock),
    Grouping(P<Expr>),
    Prefix(ExprPrefix),
    Infix(ExprInfix),
    Call(ExprCall),
    Literal(Literal),
}

#[derive(Debug)]
pub struct ExprBlock {
    pub node: NodeId,
    pub decls: ThinVec<P<Decl>>,
    pub exprs: ThinVec<P<Expr>>,
    pub end: Option<P<Expr>>,
}

#[derive(Debug)]
pub struct ExprPrefix {
    pub node: NodeId,
    pub op: ThinVec<TokenIndex>,
    pub rhs: P<Expr>,
}

#[derive(Debug)]
pub struct ExprInfix {
    pub node: NodeId,
    pub lhs: P<Expr>,
    pub rhs: P<Expr>,
    pub op: ThinVec<TokenIndex>,
}

#[derive(Debug)]
pub struct ExprCall {
    pub node: NodeId,
    pub callee: P<Expr>,
    pub args: Option<ThinVec<P<ExprCallArg>>>,
}

#[derive(Debug)]
pub struct ExprCallArg {
    pub node: NodeId,
    pub expr: P<Expr>,
    pub named: Option<Ident>,
}

#[derive(Debug)]
pub struct Literal {
    pub node: NodeId,
    pub kind: LiteralKind,
}

#[derive(Debug)]
pub enum LiteralKind {
    Integer(StringSymbol),
    Float(StringSymbol),
    Bool(bool),
    String(ThinVec<TokenIndex>),
    // Err,
}
