pub mod cst;
mod lexer;
mod parser;

use std::ops::Range;

pub use lexer::lex;
pub use parser::parse;

define_idx! { pub struct TokenIndex: u32 != 0 }

pub type Tokens = IndexVec2<TokenIndex, TokenKind, Range<usize>>;

/// A token represents a piece of source code with semantic meaning.
/// Such as `brkt` being a prefix for all bracket tokens: `(){}[]`.
/// Or `punct` for punctuation tokens: `,;:` etc.
/// Or `kw` for keyword tokens: `fn let if` etc.
/// Or tokens which follow a rule, such as `ident`, `float` or `int_*`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[allow(non_camel_case_types)]
#[repr(u8)]
pub enum TokenKind {
    eof,
    err,

    newline,

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

    int_dec,
    int_hex, // 0x
    int_oct, // 0o
    int_bin, // 0b
    float,

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
    kw_self,
    kw_set,
    kw_struct,
    kw_trait,
    kw_true,
    kw_union,
}

impl TokenKind {
    pub fn is_trivia(self) -> bool {
        matches!(self, TK![tr whitespace] | TK![tr comment])
    }

    // pub fn name(self) -> &'static str {
    //     use TokenKind::*;
    //     match self {
    //         eof => todo!(),
    //         err => todo!(),

    //         newline => "\\n",

    //         tr_whitespace => todo!(),
    //         tr_comment => todo!(),

    //         brkt_paren_open => "'('",
    //         brkt_paren_close => "')'",
    //         brkt_brace_open => "'{'",
    //         brkt_brace_close => "'}'",
    //         brkt_square_open => "'['",
    //         brkt_square_close => "']'",

    //         punct_ampersand => "'&'",
    //         punct_bang => "'!'",
    //         punct_bangEq => "'!='",
    //         punct_colon => "':'",
    //         punct_doubleColon => "'::'",
    //         punct_semicolon => "';'",
    //         punct_comma => "','",
    //         punct_dot => "'.'",
    //         punct_doubleDot => "'..'",
    //         punct_tripleDot => "'...'",
    //         punct_eq => "'='",
    //         punct_eqEq => "'=='",
    //         punct_fatArrow => "'=>'",
    //         punct_greaterThan => "'>'",
    //         punct_greaterThanEq => "'>='",
    //         punct_lessThan => "'<'",
    //         punct_lessThanEq => "'<='",
    //         punct_minus => "'-'",
    //         punct_rThinArrow => "'->'",
    //         punct_pipe => "'|'",
    //         punct_plus => "'+'",
    //         punct_question => "'?'",
    //         punct_slash => "'/'",
    //         punct_star => "'*'",

    //         ident => "an identifier",

    //         float => todo!(),
    //         int_dec => todo!(),
    //         int_hex => todo!(),
    //         int_oct => todo!(),
    //         int_bin => todo!(),

    //         string_open | string_close => "'\"'",
    //         string_literal => todo!(),

    //         esc_asciicode => todo!(),
    //         esc_unicode => todo!(),
    //         esc_char_newline => todo!(),
    //         esc_char_return => todo!(),
    //         esc_char_tab => todo!(),
    //         esc_char_backslash => todo!(),
    //         esc_char_doubleQuote => todo!(),
    //         esc_char_singleQuote => todo!(),
    //         esc_char_other => todo!(),

    //         kw_class => todo!(),
    //         kw_const => todo!(),
    //         kw_else => todo!(),
    //         kw_enum => todo!(),
    //         kw_false => todo!(),
    //         kw_fn => todo!(),
    //         kw_if => todo!(),
    //         kw_impl => todo!(),
    //         kw_import => todo!(),
    //         kw_let => todo!(),
    //         kw_mixin => todo!(),
    //         kw_module => todo!(),
    //         kw_mut => todo!(),
    //         kw_pub => todo!(),
    //         kw_return => todo!(),
    //         kw_self => todo!(),
    //         kw_set => todo!(),
    //         kw_struct => todo!(),
    //         kw_trait => todo!(),
    //         kw_true => todo!(),
    //         kw_union => todo!(),
    //     }
    // }
}

#[macro_export]
macro_rules! TK {
    [eof] => { $crate::parse::TokenKind::eof };
    [err] => { $crate::parse::TokenKind::err };

    [newline]       => { $crate::parse::TokenKind::newline };

    [tr whitespace] => { $crate::parse::TokenKind::tr_whitespace };
    [tr comment]    => { $crate::parse::TokenKind::tr_comment };

    ['('] => { $crate::parse::TokenKind::brkt_paren_open };
    [')'] => { $crate::parse::TokenKind::brkt_paren_close };
    ['{'] => { $crate::parse::TokenKind::brkt_brace_open };
    ['}'] => { $crate::parse::TokenKind::brkt_brace_close };
    ['['] => { $crate::parse::TokenKind::brkt_square_open };
    [']'] => { $crate::parse::TokenKind::brkt_square_close };

    [&]   => { $crate::parse::TokenKind::punct_ampersand };
    [!]   => { $crate::parse::TokenKind::punct_bang };
    [!=]  => { $crate::parse::TokenKind::punct_bangEq };
    [:]   => { $crate::parse::TokenKind::punct_colon };
    [::]  => { $crate::parse::TokenKind::punct_doubleColon };
    [;]   => { $crate::parse::TokenKind::punct_semicolon };
    [,]   => { $crate::parse::TokenKind::punct_comma };
    [.]   => { $crate::parse::TokenKind::punct_dot };
    [..]  => { $crate::parse::TokenKind::punct_doubleDot };
    [...] => { $crate::parse::TokenKind::punct_tripleDot };
    [=]   => { $crate::parse::TokenKind::punct_eq };
    [==]  => { $crate::parse::TokenKind::punct_eqEq };
    [=>]  => { $crate::parse::TokenKind::punct_fatArrow };
    [>]   => { $crate::parse::TokenKind::punct_greaterThan };
    [>=]  => { $crate::parse::TokenKind::punct_greaterThanEq };
    [<]   => { $crate::parse::TokenKind::punct_lessThan };
    [<=]  => { $crate::parse::TokenKind::punct_lessThanEq };
    [-]   => { $crate::parse::TokenKind::punct_minus };
    [->]  => { $crate::parse::TokenKind::punct_rThinArrow };
    [|]   => { $crate::parse::TokenKind::punct_pipe };
    [+]   => { $crate::parse::TokenKind::punct_plus };
    [?]   => { $crate::parse::TokenKind::punct_question };
    [/]   => { $crate::parse::TokenKind::punct_slash };
    [*]   => { $crate::parse::TokenKind::punct_star };

    [ident] => { $crate::parse::TokenKind::ident };

    [int dec] => { $crate::parse::TokenKind::int_dec };
    [int hex] => { $crate::parse::TokenKind::int_hex };
    [int oct] => { $crate::parse::TokenKind::int_oct };
    [int bin] => { $crate::parse::TokenKind::int_bin };
    [float]   => { $crate::parse::TokenKind::float };

    [string open]    => { $crate::parse::TokenKind::string_open };
    [string literal] => { $crate::parse::TokenKind::string_literal };
    [string close]   => { $crate::parse::TokenKind::string_close };

    [esc asciicode]        => { $crate::parse::TokenKind::esc_asciicode };
    [esc unicode]          => { $crate::parse::TokenKind::esc_unicode };
    [esc char newline]     => { $crate::parse::TokenKind::esc_char_newline };
    [esc char return]      => { $crate::parse::TokenKind::esc_char_return };
    [esc char tab]         => { $crate::parse::TokenKind::esc_char_tab };
    [esc char backslash]   => { $crate::parse::TokenKind::esc_char_backslash };
    [esc char doubleQuote] => { $crate::parse::TokenKind::esc_char_doubleQuote };
    [esc char singleQuote] => { $crate::parse::TokenKind::esc_char_singleQuote };
    [esc char other]       => { $crate::parse::TokenKind::esc_char_other };

    [class]  => { $crate::parse::TokenKind::kw_class };
    [const]  => { $crate::parse::TokenKind::kw_const };
    [else]   => { $crate::parse::TokenKind::kw_else };
    [enum]   => { $crate::parse::TokenKind::kw_enum };
    [false]  => { $crate::parse::TokenKind::kw_false };
    [fn]     => { $crate::parse::TokenKind::kw_fn };
    [if]     => { $crate::parse::TokenKind::kw_if };
    [impl]   => { $crate::parse::TokenKind::kw_impl };
    [import] => { $crate::parse::TokenKind::kw_import };
    [let]    => { $crate::parse::TokenKind::kw_let };
    [mixin]  => { $crate::parse::TokenKind::kw_mixin };
    [module] => { $crate::parse::TokenKind::kw_module };
    [mut]    => { $crate::parse::TokenKind::kw_mut };
    [pub]    => { $crate::parse::TokenKind::kw_pub };
    [return] => { $crate::parse::TokenKind::kw_return };
    [self]   => { $crate::parse::TokenKind::kw_self };
    [set]    => { $crate::parse::TokenKind::kw_set };
    [struct] => { $crate::parse::TokenKind::kw_struct };
    [trait]  => { $crate::parse::TokenKind::kw_trait };
    [true]   => { $crate::parse::TokenKind::kw_true };
    [union]  => { $crate::parse::TokenKind::kw_union };
}
pub use TK;

use crate::{define_idx, util::index::IndexVec2};
