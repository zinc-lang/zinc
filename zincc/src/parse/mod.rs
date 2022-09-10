pub mod cst;
mod lexer;

mod parser;

pub use lexer::lex;
pub use parser::parse;

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

    pub fn name(self) -> &'static str {
        use TokenKind::*;
        match self {
            eof => todo!(),
            err => todo!(),
            tr_whitespace => todo!(),
            tr_comment => todo!(),
            brkt_paren_open => "'('",
            brkt_paren_close => "')'",
            brkt_brace_open => "'{'",
            brkt_brace_close => "'}'",
            brkt_square_open => "'['",
            brkt_square_close => "']'",
            punct_ampersand => "'&'",
            punct_bang => "'!'",
            punct_bangEq => "'!='",
            punct_colon => "':'",
            punct_doubleColon => "'::'",
            punct_semicolon => "';'",
            punct_comma => "','",
            punct_dot => "'.'",
            punct_doubleDot => "'..'",
            punct_tripleDot => "'...'",
            punct_eq => "'='",
            punct_eqEq => "'=='",
            punct_fatArrow => "'=>'",
            punct_greaterThan => "'>'",
            punct_greaterThanEq => "'>='",
            punct_lessThan => "'<'",
            punct_lessThanEq => "'<='",
            punct_minus => "'-'",
            punct_rThinArrow => "'->'",
            punct_pipe => "'|'",
            punct_plus => "'+'",
            punct_question => "'?'",
            punct_slash => "'/'",
            punct_star => "'*'",
            ident => "an identifier",
            float => todo!(),
            int_dec => todo!(),
            int_hex => todo!(),
            int_oct => todo!(),
            int_bin => todo!(),
            string_open | string_close => "'\"'",
            string_literal => todo!(),
            esc_asciicode => todo!(),
            esc_unicode => todo!(),
            esc_char_newline => todo!(),
            esc_char_return => todo!(),
            esc_char_tab => todo!(),
            esc_char_backslash => todo!(),
            esc_char_doubleQuote => todo!(),
            esc_char_singleQuote => todo!(),
            esc_char_other => todo!(),
            kw_class => todo!(),
            kw_const => todo!(),
            kw_else => todo!(),
            kw_enum => todo!(),
            kw_false => todo!(),
            kw_fn => todo!(),
            kw_if => todo!(),
            kw_impl => todo!(),
            kw_import => todo!(),
            kw_let => todo!(),
            kw_mixin => todo!(),
            kw_module => todo!(),
            kw_mut => todo!(),
            kw_pub => todo!(),
            kw_return => todo!(),
            kw_set => todo!(),
            kw_struct => todo!(),
            kw_trait => todo!(),
            kw_true => todo!(),
            kw_union => todo!(),
        }
    }
}

pub type TK = TokenKind;
