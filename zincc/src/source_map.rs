use crate::{ast::AstFile, parse::cst::Cst, util};
use std::path::{Path, PathBuf};

util::index::define_idx! { pub struct SourceFileId: u32 }

pub struct SourceMap {
    root: SourceFileId,
    files: util::index::IndexVec<SourceFileId, SourceFile>,
}

impl SourceMap {
    pub fn new(root: SourceFile) -> Self {
        let mut files = util::index::IndexVec::new();
        let root = files.push(root);
        Self { root, files }
    }

    #[inline]
    pub fn root(&self) -> SourceFileId {
        self.root
    }
}

impl std::ops::Index<SourceFileId> for SourceMap {
    type Output = SourceFile;

    fn index(&self, index: SourceFileId) -> &Self::Output {
        &self.files[index]
    }
}

impl std::ops::IndexMut<SourceFileId> for SourceMap {
    fn index_mut(&mut self, index: SourceFileId) -> &mut Self::Output {
        &mut self.files[index]
    }
}

#[derive(Debug)]
pub struct SourceFile {
    path: PathBuf,
    source: Option<String>,
    line_offsets: Option<Box<[usize]>>,
    cst: Option<Cst>,
    ast_file: Option<AstFile>,
}

impl SourceFile {
    pub fn new(path: impl AsRef<Path>) -> std::io::Result<Self> {
        let path = path.as_ref();
        let path = std::fs::canonicalize(path)?;

        Ok(Self {
            path,
            source: None,
            line_offsets: None,
            cst: None,
            ast_file: None,
        })
    }

    pub fn path(&self) -> &Path {
        self.path.as_path()
    }

    pub fn path_relative(&self) -> PathBuf {
        pathdiff::diff_paths(&self.path, std::env::current_dir().unwrap()).unwrap()
    }

    pub fn read_source(&mut self) -> std::io::Result<()> {
        let source = util::read_file_to_string(self.path()).map(|s| s + "\n\0")?;
        self.set_source(source);
        Ok(())
    }

    pub fn source(&self) -> &String {
        self.source.as_ref().expect("file should have source")
    }

    pub fn set_source(&mut self, source: String) {
        debug_assert!(self.source.is_none());
        self.source = Some(source);
    }

    pub fn line_offsets(&self) -> &[usize] {
        self.line_offsets
            .as_ref()
            .expect("file should have line_offsets")
    }

    pub fn set_line_offsets(&mut self, line_offsets: Box<[usize]>) {
        debug_assert!(self.line_offsets.is_none());
        self.line_offsets = Some(line_offsets);
    }

    pub fn cst(&self) -> &Cst {
        self.cst.as_ref().expect("file should have cst")
    }

    pub fn set_cst(&mut self, cst: Cst) {
        debug_assert!(self.cst.is_none());
        self.cst = Some(cst);
    }

    pub fn ast(&self) -> &AstFile {
        self.ast_file.as_ref().expect("file should have ast")
    }

    pub fn set_ast(&mut self, ast: AstFile) {
        debug_assert!(self.ast_file.is_none());
        self.ast_file = Some(ast)
    }
}

// pub fn read_file_source(map: &mut SourceMap, file_id: SourceFileId) -> std::io::Result<()> {
//     let file = &mut map[file_id];

//     let source = util::read_file_to_string(file.path()).map(|s| s + "\n\0")?;
//     file.set_source(source);

//     Ok(())
// }
