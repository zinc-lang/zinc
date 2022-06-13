pub mod cst;
mod lexer;
mod parser;

pub use lexer::{lex, LexResult};
pub use parser::{
    parse, ParseError, ParseErrorContext, ParseErrorExpected, ParseErrorExpectedWhat, ParseResult,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[allow(non_camel_case_types)]
#[repr(u8)]
pub enum TokenKind {
    EOF,

    brkt_paren_open,   // (
    brkt_paren_close,  // )
    brkt_brace_open,   // {
    brkt_brace_close,  // }
    brkt_square_open,  // [
    brkt_square_close, // ]

    punct_colon,     // :
    punct_dblColon,  // ::
    punct_semiColon, // ;
    punct_comma,     // ,

    punct_eq,    // =
    punct_plus,  // +
    punct_minus, // -
    punct_star,  // *
    punct_slash, // /

    punct_fat_arrow, // =>

    ident,

    float,
    int_dec,
    int_hex, // 0x
    int_oct, // 0o
    int_bin, // 0b

    string_open,
    string_literal,
    string_close,

    esc_char,      // \<X>
    esc_asciicode, // \x<XX>
    esc_unicode,   // \u{XXXX}

    kw_const,
    kw_false,
    kw_fn,
    kw_let,
    kw_mut,
    kw_return,
    kw_true,
}

pub type TK = TokenKind;

#[derive(Debug, Clone, Copy)]
pub struct FileLocation {
    pub line: usize,
    pub column: usize,
}

impl FileLocation {
    pub fn from_offset(offset: usize, str: &str) -> Self {
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
        Self { line, column }
    }

    pub fn from_range(range: std::ops::Range<usize>, str: &str) -> std::ops::Range<FileLocation> {
        FileLocation::from_offset(range.start, str)..FileLocation::from_offset(range.end, str)
    }
}
