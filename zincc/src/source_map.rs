use crate::{ast, parse, util};
use std::{
    collections::HashMap,
    ops::{Deref, Range},
    path::{Path, PathBuf},
};

#[derive(Debug)]
pub struct SourceMap {
    pub root: SourceFileId,
    pub files: util::index::IndexVec<SourceFileId, SourceFile>,
    pub sources: HashMap<SourceFileId, String>,
    pub lex_data: HashMap<SourceFileId, LexData>,
    pub csts: HashMap<SourceFileId, parse::cst::Cst>,
    pub ast_files: HashMap<SourceFileId, ast::AstFile>,
}

impl SourceMap {
    pub fn new(root: SourceFile) -> Self {
        let mut files = util::index::IndexVec::new();
        let root = files.push(root);
        Self {
            root,
            files,
            sources: Default::default(),
            lex_data: Default::default(),
            csts: Default::default(),
            ast_files: Default::default(),
        }
    }

    pub fn set_ast_files(&mut self, ast_files: HashMap<SourceFileId, ast::AstFile>) {
        self.ast_files = ast_files;
    }
}

util::index::define_idx! { pub struct SourceFileId: u32 }

#[derive(Debug)]
pub struct SourceFile {
    path: PathBuf,
}

impl SourceFile {
    pub fn new(path: impl AsRef<Path>) -> std::io::Result<Self> {
        let path = path.as_ref();
        let path = std::fs::canonicalize(path)?;

        Ok(Self { path })
    }

    pub fn path(&self) -> &Path {
        self.path.as_path()
    }

    pub fn path_relative(&self) -> PathBuf {
        pathdiff::diff_paths(&self.path, std::env::current_dir().unwrap()).unwrap()
    }
}

#[derive(Debug)]
pub struct LexData {
    pub tokens: Box<[parse::TokenKind]>,
    pub ranges: Box<[Range<usize>]>,
    pub line_offsets: Box<[usize]>,
}

impl LexData {
    pub fn zip(&self) -> impl Iterator<Item = (&parse::TokenKind, &Range<usize>)> {
        let tokens = self.tokens.deref().iter();
        let ranges = self.ranges.deref().iter();
        tokens.zip(ranges)
    }
}
