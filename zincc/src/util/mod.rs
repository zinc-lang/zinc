pub mod index;
pub mod time;

mod aiw;

#[doc(inline)]
pub use aiw::AutoIndentingWriter;

/// Given a path to a file, returns the file read to a [`std::io::Result<String>`].
pub fn read_file_to_string(path: impl AsRef<std::path::Path>) -> std::io::Result<String> {
    use std::io::Read;
    let mut file = std::fs::File::open(path)?;
    let mut str = String::new();
    let len = file.read_to_string(&mut str)?;
    assert_eq!(len, str.len());
    Ok(str)
}
