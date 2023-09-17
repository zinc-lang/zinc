#![allow(deprecated)]

use crate::{
    parse::{self, cst, TokenIndex, Tokens},
    util::{index::Idx, AutoIndentingWriter},
    TK,
};
use colored::Colorize;
use std::io::{self, Write};

pub fn write_token<W: Write>(
    out: &mut W,
    use_color: bool,
    source: &str,
    tk: parse::TokenKind,
    range: &std::ops::Range<usize>,
) -> io::Result<()> {
    let slice = &source[range.start..range.end];

    if use_color {
        write!(
            out,
            "{}@{}  {}",
            format!("{tk:?}").cyan(),
            format!("{range:?}").white(),
            format!("{slice:?}").green()
        )
    } else {
        write!(out, "{tk:?}@{range:?}  {slice:?}")
    }
}

pub fn write_tokens<W: Write>(
    out: &mut W,
    use_color: bool,
    skip_trivia: bool,
    skip_newlines: bool,
    source: &str,
    tokens: &Tokens,
) -> io::Result<()> {
    for (&tk, range) in tokens.vec().iter() {
        if (skip_trivia && tk.is_trivia()) || (skip_newlines && tk == TK![newline]) {
            continue;
        }
        write_token(out, use_color, source, tk, range)?;
        writeln!(out)?;
    }
    Ok(())
}

pub fn verify_tokens(tokens: &Tokens) {
    let mut last_offset = 0;
    for (_, range) in tokens.vec().iter() {
        assert!(range.start == last_offset);
        last_offset = range.end;
    }
}

pub fn write_cst_from_root<W: Write>(
    writer: &mut W,
    use_color: bool,
    skip_trivia: bool,
    skip_newlines: bool,
    source: &str,
    cst: &cst::Cst,
) -> io::Result<()> {
    CstWriter::new(writer, use_color, skip_trivia, skip_newlines, source, cst)
        .write_node_children(cst.root())
}

// pub fn write_cst_node<W: Write>(
//     writer: &mut W,
//     cst: &cst::Cst,
//     node: cst::NodeId,
//     source: &str,
//     use_color: bool,
// ) -> io::Result<()> {
//     CstWriter::new(writer, cst, source, use_color).write_node_with_children(node)
// }

pub struct CstWriter<'w, 's, W: Write> {
    f: AutoIndentingWriter<&'w mut W>,
    use_color: bool,
    skip_trivia: bool,
    skip_newlines: bool,
    source: &'s str,
    cst: &'s cst::Cst,
}

impl<'w, 's, W: Write> CstWriter<'w, 's, W> {
    pub fn new(
        writer: &'w mut W,
        use_color: bool,
        skip_trivia: bool,
        skip_newlines: bool,
        source: &'s str,
        cst: &'s cst::Cst,
    ) -> Self {
        Self {
            f: AutoIndentingWriter::new(writer, 4),
            use_color,
            skip_trivia,
            skip_newlines,
            source,
            cst,
        }
    }

    pub fn write_node(&mut self, node: cst::NodeId) -> io::Result<()> {
        let kind = self.cst.kind(node);
        if self.use_color {
            write!(self.f, "{}", format!("{kind:?}").magenta())?;
        } else {
            write!(self.f, "{:?}", kind)?;
        }

        write!(
            self.f,
            " {}",
            format!("[{}]", format!("{}", node.index()).magenta()).white()
        )?;
        writeln!(self.f)?;

        Ok(())
    }

    pub fn write_token(&mut self, token: TokenIndex) -> io::Result<()> {
        let (&tk, range) = self.cst.tokens().get(token).unwrap();

        if (self.skip_trivia && tk.is_trivia()) || (self.skip_newlines && tk == TK![newline]) {
            return Ok(());
        }

        write_token(&mut self.f, self.use_color, self.source, tk, range)?;

        write!(
            self.f,
            " {}",
            format!("[{}]", format!("{}", token.index()).cyan()).white()
        )?;
        writeln!(self.f)?;

        Ok(())
    }

    pub fn write_node_children(&mut self, node: cst::NodeId) -> io::Result<()> {
        for elem in self.cst.elements(node) {
            match elem {
                cst::Element::Token(token) => {
                    self.write_token(*token)?;
                }
                cst::Element::Node(node) => {
                    self.write_node_with_children(*node)?;
                }
            }
        }
        self.f.flush()
    }

    pub fn write_node_with_children(&mut self, node: cst::NodeId) -> io::Result<()> {
        self.write_node(node)?;

        self.f.push_indent();
        self.write_node_children(node)?;
        self.f.pop_indent();

        Ok(())
    }
}

/// Currently this just makes sure that all tokens are in the correct order.
/// It is possible, if one is not careful, to add tokens in the wrong order in
/// the parser.
///
/// # Panics
///
/// When verification fails
pub fn verify_cst(cst: &cst::Cst) {
    let mut verifier = CstVerifier::new(cst);
    verifier.verify();
}

struct CstVerifier<'c> {
    cst: &'c cst::Cst,
    last_offset: usize,
}

impl<'c> CstVerifier<'c> {
    fn new(cst: &'c cst::Cst) -> Self {
        Self {
            cst,
            last_offset: 0,
        }
    }

    fn verify(&mut self) {
        self.verify_node(self.cst.root());
    }

    fn verify_node(&mut self, node: cst::NodeId) {
        for elem in self.cst.elements(node) {
            match elem {
                cst::Element::Token(token) => {
                    let range = self.cst.tokens().get_b(*token).unwrap();
                    assert!(
                        range.start == self.last_offset,
                        "tokens not in correct order in cst - index: {token:?}"
                    );
                    self.last_offset = range.end;
                }
                cst::Element::Node(node) => {
                    self.verify_node(*node);
                }
            }
        }
    }
}

// pub struct DebugAstScopeWriter<'w, W: Write> {
//     f: AutoIndentingWriter<&'w mut W>,
//     map: &'w ast::scope::Map,
// }

// pub struct DebugAstWriter<'w, W: Write> {
//     f: AutoIndentingWriter<&'w mut W>,
//     map: &'w ast::AstMap,
//     // ast: &'w ast::AstFile,
//     // source: &'w str,
// }

// impl<'w, W: Write> DebugAstWriter<'w, W> {
//     pub fn new(
//         writer: &'w mut W,
//         map: &'w ast::AstMap,
//         // source_map: &'w SourceMap,
//         // file_id: SourceFileId,
//     ) -> Self {
//         // let file = &source_map.files[file_id];
//         // let ast = file.ast();
//         // let source = file.source();

//         Self {
//             f: AutoIndentingWriter::new(writer, 4),
//             map,
//             // source,
//         }
//     }

//     pub fn write_file(&mut self, file: &ast::AstFile) -> io::Result<()> {
//         // file.decls.
//         // for decl in file.decls {
//         //     // self.write_decl(decl);
//         // }
//         // for decl in &self.map.decls[file.decls.clone()] {
//         //     self.write_decl(decl)?
//         // }

//         for id in file.decls.clone() {
//             self.write_decl(id)?;
//             writeln!(self.f)?;
//         }

//         Ok(())
//     }

//     // fn write_id(&mut self, id: impl Idx) -> io::Result<()> {
//     //     write!(self.f, "{} ", format!("[#{}]", id.index()).white())?;
//     //     Ok(())
//     // }

//     // fn write_id_with_cst(&mut self, id)

//     fn write_decl(&mut self, id: ast::DeclId) -> io::Result<()> {
//         // let decl = self.ast.decls[]
//         // writeln!(self.f,)
//         // decl.node
//         let decl = &self.map.decls[id];

//         // write!(
//         //     self.f,
//         //     ":: {} ",
//         //     format!("[#{}]", decl.node.index()).white()
//         // )?;

//         write!(
//             self.f,
//             ":: {} ",
//             format!("[#{}|{}]", id.index(), decl.node.index()).white()
//         )?;

//         let desc = self.map.scope.decls_map[&id];
//         let desc = &self.map.scope.decls[desc];

//         let name = &self.map.strings[desc.name];

//         match decl.kind {
//             ast::decl::Kind::Func(func_id) => {
//                 let func = &self.map.decl_funcs[func_id];

//                 write!(self.f, "{} ", name.green())?;

//                 let sig = &self.map.ty_funcs[func.sig];
//                 if let Some(params) = &sig.params {
//                     write!(self.f, "(")?;
//                     for param_id in params.clone() {
//                         let param = &self.map.ty_func_params[param_id];
//                         let name = &self.map.strings[param.name];
//                         write!(self.f, "{name} ")?;
//                         self.write_ty(param.ty)?;
//                         write!(self.f, ", ")?;
//                     }
//                     write!(self.f, ") ")?;
//                 }

//                 write!(self.f, "{} ", "fn".red())?;

//                 if let Some(ty_id) = sig.ret {
//                     self.write_ty(ty_id)?;
//                     write!(self.f, " ")?;
//                 }

//                 // let sig = &self.map.ty_funcs[func.sig];
//                 // sig.
//                 // self.write_expr(func.)
//                 if let Some(body) = func.body {
//                     self.write_expr(body)?;
//                     // writeln!(self.f)?;
//                 } else {
//                     write!(self.f, ";")?;
//                 }
//             }
//         }

//         Ok(())
//     }

//     fn write_expr(&mut self, id: ast::ExprId) -> io::Result<()> {
//         let expr = &self.map.exprs[id];

//         write!(
//             self.f,
//             "{} ",
//             format!("[#{}|{}]", id.index(), expr.node.index()).white()
//         )?;

//         match &expr.kind {
//             ast::expr::Kind::Res(res) => match res {
//                 ast::expr::res::Resolution::Decl(id) => {
//                     let desc = &self.map.scope.decls[*id];
//                     let name = &self.map.strings[desc.name];
//                     write!(self.f, "{name}")?;
//                 }
//                 ast::expr::res::Resolution::Arg(_) => todo!(),
//                 ast::expr::res::Resolution::Local(_) => todo!(),
//                 ast::expr::res::Resolution::Err => todo!(),
//             },
//             ast::expr::Kind::Literal(literal) => match literal {
//                 ast::expr::Literal::Integer(int) => {
//                     write!(self.f, "{}", format!("{int}").purple())?;
//                 }
//             },
//             ast::expr::Kind::LetBasic(_) => todo!(),
//             ast::expr::Kind::Block(block_id) => {
//                 let block = &self.map.expr_blocks[*block_id];

//                 writeln!(self.f, "{{")?;
//                 self.f.push_indent();
//                 {
//                     for id in block.decls.clone() {
//                         self.write_decl(id)?;
//                         writeln!(self.f)?;
//                     }

//                     for id in block.exprs.clone() {
//                         self.write_expr(id)?;
//                         writeln!(self.f, ";")?;
//                     }

//                     if let Some(id) = block.end {
//                         self.write_expr(id)?;
//                         writeln!(self.f)?;
//                     }
//                 }
//                 self.f.pop_indent();
//                 writeln!(self.f, "}}")?;
//             }
//             ast::expr::Kind::Call(call) => {
//                 self.write_expr(call.callee)?;

//                 write!(self.f, "(")?;
//                 for id in call.args.clone() {
//                     self.write_expr(id)?;
//                     write!(self.f, ", ")?;
//                 }
//                 write!(self.f, ")")?;
//             }
//             ast::expr::Kind::Infix(infix) => {
//                 self.write_expr(infix.lhs)?;

//                 // @TODO
//                 write!(self.f, " {:?} ", infix.op)?;

//                 self.write_expr(infix.rhs)?;
//             }
//         }

//         Ok(())
//     }

//     fn write_ty(&mut self, id: ast::TyId) -> io::Result<()> {
//         let ty = &self.map.tys[id];

//         write!(
//             self.f,
//             "{} ",
//             format!("[#{}|{}]", id.index(), ty.node.index()).white()
//         )?;

//         match &ty.kind {
//             ast::ty::Kind::Res(res) => match res {
//                 ast::ty::res::Resolution::Primitive(prim) => match prim {
//                     ast::ty::res::Primitive::Integer(int) => match int {
//                         ast::ty::res::IntegerPrimitive::Sint => {
//                             write!(self.f, "{}", "sint".cyan())?
//                         }
//                         ast::ty::res::IntegerPrimitive::Uint => {
//                             write!(self.f, "{}", "uint".cyan())?
//                         }
//                     },
//                     ast::ty::res::Primitive::Void => write!(self.f, "{}", "void".cyan())?,
//                 },
//                 ast::ty::res::Resolution::Err => {
//                     write!(self.f, "{}", "##Resolution Error##".bright_red())?
//                 }
//             },
//             ast::ty::Kind::Func(id) => {
//                 let func = &self.map.ty_funcs[*id];

//                 write!(self.f, "{} ", "fn".red())?;

//                 if let Some(params) = &func.params {
//                     write!(self.f, "(")?;
//                     for param in params.clone() {
//                         let param = &self.map.ty_func_params[param];
//                         let name = &self.map.strings[param.name];
//                         write!(self.f, "{name} ")?;
//                         self.write_ty(param.ty)?;
//                         write!(self.f, ", ")?;
//                     }
//                     write!(self.f, ") ")?;
//                 }

//                 if let Some(ret) = &func.ret {
//                     self.write_ty(*ret)?;
//                 }
//             }
//         }

//         Ok(())
//     }
// }
