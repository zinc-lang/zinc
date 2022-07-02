pub mod index;
pub mod time;

mod aiw;
pub use aiw::AutoIndentingWriter;

pub fn read_file_to_string<P: AsRef<std::path::Path>>(path: P) -> std::io::Result<String> {
    use std::io::Read;
    let mut file = std::fs::File::open(path)?;
    let mut str = String::new();
    let len = file.read_to_string(&mut str)?;
    assert_eq!(len, str.len());
    Ok(str)
}
