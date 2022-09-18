pub mod index;
pub mod mut_cell;
pub mod progress;
pub mod time;

mod aiw;

#[doc(inline)]
pub use aiw::AutoIndentingWriter;

/// Given a path to a file, returns the file read to a [`Result<String>`].
///
/// [`Result<String>`]: std::io::Result<String>
pub fn read_file_to_string(path: impl AsRef<std::path::Path>) -> std::io::Result<String> {
    use std::io::Read;
    let mut file = std::fs::File::open(path)?;
    let mut str = String::new();
    let len = file.read_to_string(&mut str)?;
    assert_eq!(len, str.len());
    Ok(str)
}

/// A structure representing a location in a file as a line and column,
/// instead of an offset from a file.
#[derive(Debug, Clone, Copy)]
pub struct FileLocation {
    pub line: usize,
    pub column: usize,
}

impl FileLocation {
    /// Calculate a [`FileLocation`] from a [`&str`] and an `offset`.
    pub fn from_offset(str: &str, offset: usize) -> Option<Self> {
        if offset > str.len() {
            return None;
        }

        let mut line: usize = 1;
        let mut column: usize = 1;
        for (i, ch) in str.chars().into_iter().enumerate() {
            match ch {
                '\n' => {
                    line += 1;
                    column = 1;
                }
                _ => column += 1,
            }
            if offset == i {
                break;
            }
        }

        Some(Self { line, column })
    }

    /// Calculate a [`FileLocation`] range from a [`&str`] and an `offset` range.
    #[allow(unused)] // @FIXME: Remove this attribute when we do actually use this.
    pub fn from_range(
        str: &str,
        range: &std::ops::Range<usize>,
    ) -> Option<std::ops::Range<FileLocation>> {
        Some(
            FileLocation::from_offset(str, range.start)?
                ..FileLocation::from_offset(str, range.end)?,
        )
    }
}
