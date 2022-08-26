pub mod cst;
mod lexer;

// #[allow(dead_code)]
// mod parser;
mod parser;

pub use lexer::lex;
// pub use parser::{parse, ParseContext, ParseError, ParseErrorExpected, ParseErrorExpectedWhat};
pub use parser::{parse, ParseError};

/// A token represents a piece of source code with semantic meaning.
/// Such as `brkt` being a prefix for all bracket tokens: `(){}[]`.
/// Or `punct` for punctuation tokens: `,;:` etc.
/// Or `kw` for keyword tokens: `fn let if` etc.
/// Or tokens which follow a rule, such as `ident`, `float` or  the `int_*`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[allow(non_camel_case_types)]
#[repr(u8)]
pub enum TokenKind {
    eof,
    err,

    tr_whitespace,
    tr_comment,

    brkt_paren_open,   // (
    brkt_paren_close,  // )
    brkt_brace_open,   // {
    brkt_brace_close,  // }
    brkt_square_open,  // [
    brkt_square_close, // ]

    punct_ampersand,     // &
    punct_bang,          // !
    punct_bangEq,        // !=
    punct_colon,         // :
    punct_doubleColon,   // ::
    punct_semicolon,     // ;
    punct_comma,         // ,
    punct_dot,           // .
    punct_doubleDot,     // ..
    punct_tripleDot,     // ...
    punct_eq,            // =
    punct_eqEq,          // ==
    punct_fatArrow,      // =>
    punct_greaterThan,   // >
    punct_greaterThanEq, // >=
    punct_lessThan,      // <
    punct_lessThanEq,    // <=
    // punct_lThinArrow,    // <-
    punct_minus,      // -
    punct_rThinArrow, // ->
    punct_pipe,       // |
    punct_plus,       // +
    punct_question,   // ?
    punct_slash,      // /
    punct_star,       // *

    ident,

    float,
    int_dec,
    int_hex, // 0x
    int_oct, // 0o
    int_bin, // 0b

    // string_prefix,  // an ident preceding a string without any whitespace
    string_open,    // "
    string_literal, // any normal text which has not been escaped
    string_close,   // "

    // string_num_suffix, // an ident following a string or number without any whitespace
    //
    esc_asciicode,        // \xNN
    esc_unicode,          // \u{NNNN}
    esc_char_newline,     // \n
    esc_char_return,      // \r
    esc_char_tab,         // \t
    esc_char_backslash,   // \\
    esc_char_doubleQuote, // \"
    esc_char_singleQuote, // \'
    esc_char_other,       // \{anything that is not above}

    kw_class,
    kw_const,
    kw_else,
    kw_enum,
    kw_false,
    kw_fn,
    kw_if,
    kw_impl,
    kw_import,
    kw_let,
    kw_mixin,
    kw_module,
    kw_mut,
    kw_pub,
    kw_return,
    kw_set,
    kw_struct,
    kw_trait,
    kw_true,
    kw_union,
}

impl TokenKind {
    pub fn is_trivia(self) -> bool {
        matches!(self, TK::tr_whitespace | TK::tr_comment)
    }
}

pub type TK = TokenKind;

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
        range: std::ops::Range<usize>,
    ) -> Option<std::ops::Range<FileLocation>> {
        Some(
            FileLocation::from_offset(str, range.start)?
                ..FileLocation::from_offset(str, range.end)?,
        )
    }
}
