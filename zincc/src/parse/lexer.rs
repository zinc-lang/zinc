use super::{TokenKind, Tokens, TK};
use crate::{
    report::{Label, Report},
    util::index::IndexVec2,
};
use std::ops::Range;

// @TODO: Should we move to interning the strings here and now in the lexer,
//        using string references instead of ranges for lexemes.

#[derive(Debug)]
pub struct LexData {
    pub tokens: Tokens,
    pub line_offsets: Box<[usize]>,
}

pub fn lex(source: &str) -> (LexData, Vec<Report>) {
    debug_assert!(source.ends_with("\n\0"));

    let mut lexer = Lexer::new(source);
    lexer.do_lex();

    // For some reason our implementation of shrink_to_fit is broken
    // lexer.tokens.raw_mut().shrink_to_fit();

    let lex_data = LexData {
        tokens: lexer.tokens,
        line_offsets: lexer.line_offsets.into_boxed_slice(),
    };

    (lex_data, lexer.reports)
}

struct Lexer<'s> {
    ascii: &'s [u8],
    range: Range<usize>,

    tokens: Tokens,
    line_offsets: Vec<usize>,

    reports: Vec<Report>,
}

impl<'s> Lexer<'s> {
    fn new(source: &'s str) -> Self {
        Self {
            ascii: source.as_bytes(),
            range: 0..0,
            tokens: IndexVec2::new(),
            line_offsets: Vec::new(),
            reports: Vec::new(),
        }
    }
}

impl Lexer<'_> {
    fn do_lex(&mut self) {
        'lex_loop: loop {
            let ch = self.advance();

            match ch {
                b'\n' => {
                    self.tok(TK![newline]);
                    self.line_offsets.push(self.range.start);
                }
                b' ' | b'\r' => {
                    // self.advance_anz();
                    while matches!(self.peek(), b' ' | b'\r') {
                        self.advance_anz();
                    }
                    self.tok(TK![tr whitespace]);
                }

                b'\t' => {
                    todo!()
                }

                b'(' => self.tok(TK!['(']),
                b')' => self.tok(TK![')']),
                b'{' => self.tok(TK!['{']),
                b'}' => self.tok(TK!['}']),
                b'[' => self.tok(TK!['[']),
                b']' => self.tok(TK![']']),

                b'&' => self.tok(TK![&]),
                b'!' => self.tok_if_match(b'=', TK![!=], TK![!]),
                b':' => self.tok_if_match(b':', TK![::], TK![:]),
                b';' => self.tok(TK![;]),
                b',' => self.tok(TK![,]),

                b'.' => {
                    if self.eat(b'.') {
                        if self.eat(b'.') {
                            self.tok(TK![...]);
                        } else {
                            self.tok(TK![..]);
                        }
                    } else {
                        self.tok(TK![.]);
                    }
                }

                b'=' => {
                    if self.eat(b'=') {
                        self.tok(TK![==]);
                    } else if self.eat(b'>') {
                        self.tok(TK![=>]);
                    } else {
                        self.tok(TK![=]);
                    }
                }

                b'>' => self.tok_if_match(b'=', TK![>=], TK![>]),
                b'<' => self.tok_if_match(b'=', TK![<=], TK![<]),

                b'-' => self.tok_if_match(b'>', TK![->], TK![-]),
                b'|' => self.tok(TK![|]),
                b'+' => self.tok(TK![+]),
                b'?' => self.tok(TK![?]),
                b'*' => self.tok(TK![*]),

                b'/' => {
                    if self.eat(b'/') {
                        self.advance_anz();
                        self.advance_anz();
                        while self.peek() != b'\n' {
                            self.advance_anz();
                        }
                        // self.advance_anz();
                        self.tok(TK![tr comment]);
                    } else {
                        self.tok(TK![/])
                    }
                }

                b'"' => {
                    // // string_prefix
                    // if self.ws_len == 0 {
                    //     let prev = self.tokens.last_mut().unwrap();
                    //     if *prev == TK::ident {
                    //         let _ = std::mem::replace(prev, TK::string_prefix);
                    //     }
                    // }

                    self.lex_string();

                    // self.lex_literal_suffix();
                }

                b'0' => {
                    match self.peek() {
                        b'x' => self.lex_number_generic(TK![int hex], is_char::number_hex),
                        b'b' => self.lex_number_generic(TK![int bin], is_char::number_binary),
                        b'o' => self.lex_number_generic(TK![int oct], is_char::number_octal),
                        _ => self.lex_number(),
                    }
                    // self.lex_literal_suffix();
                }

                0 => break 'lex_loop,

                ch => {
                    match () {
                        _ if is_char::ident_start(ch) => self.lex_ident(),
                        _ if is_char::number_start(ch) => {
                            self.lex_number();
                            // self.lex_literal_suffix();
                        }
                        _ => {
                            let range = self.range.end - 1..self.range.end;
                            self.report(|r| {
                                r.message("unexpected character").label(
                                    Label::new()
                                        .range(range.clone())
                                        .message("unknown character"),
                                )
                            });
                            self.tok(TK![err]);
                        }
                    }
                }
            }
        }

        // self.range.start -= 1;
        // self.range.end -= 1;
        // self.range = 0..0;
        self.tok(TK![eof]);
        self.line_offsets
            .push(self.line_offsets[self.line_offsets.len() - 1]);
    }

    fn tok(&mut self, kind: TokenKind) {
        let range = self.range.start..self.range.end;
        self.range.start = self.range.end;
        self.tokens.push(kind, range);
    }

    fn report(&mut self, mut func: impl FnMut(Report) -> Report) {
        let report = func(Report::new().error().offset(self.range.end - 1));
        self.reports.push(report);
    }

    #[must_use]
    #[inline]
    fn advance(&mut self) -> u8 {
        let ch = self.ascii[self.range.end];
        self.range.end += 1;
        // self.ascii[self.range.end - 1]
        ch
    }

    /// Advance Assert No Zero
    #[inline]
    fn advance_anz(&mut self) {
        assert!(self.advance() != 0);
    }

    #[inline]
    fn peek_nth(&self, n: usize) -> u8 {
        self.ascii[self.range.end + n]
    }

    #[inline]
    fn peek(&self) -> u8 {
        self.peek_nth(0)
    }

    #[inline]
    fn peek_next(&self) -> u8 {
        self.peek_nth(1)
    }

    #[inline]
    fn at(&self, ch: u8) -> bool {
        self.peek() == ch
    }

    #[inline]
    fn eat(&mut self, expect: u8) -> bool {
        let cond = self.at(expect);
        if cond {
            let _ = self.advance();
        }
        cond
    }

    #[inline]
    fn tok_if_match(&mut self, expect: u8, matched: TokenKind, unmatched: TokenKind) {
        let m = self.eat(expect);
        if m {
            self.tok(matched);
        } else {
            self.tok(unmatched);
        }
    }

    #[inline]
    fn lex_number_generic(&mut self, kind: TokenKind, condition: fn(u8) -> bool) {
        self.advance_anz();
        self.lex_while_condition(condition);
        self.tok(kind);
    }

    #[inline]
    fn lex_while_condition(&mut self, condition: fn(u8) -> bool) {
        while condition(self.peek()) {
            let _ = self.advance();
        }
    }

    fn lex_string(&mut self) {
        self.tok(TK![string open]);
        let start_range = self.range.start - 1..self.range.start;

        let mut p = self.peek();
        while p != b'\"' && p != b'\0' && p != b'\n' {
            if p == b'\\' {
                if self.range.start != self.range.end {
                    self.tok(TK![string literal]);
                }

                self.advance_anz();
                self.lex_escape();
            } else {
                self.advance_anz();
            }
            p = self.peek();
        }
        if self.range.start != self.range.end {
            self.tok(TK![string literal]);
        }
        if !self.eat(b'\"') {
            // self.report(
            //     Report::new()
            //         .error()
            //         .span(start_range)
            //         .message("unterminated string")
            //         .short("starts here"),
            // );
            self.report(|r| {
                r.message("unterminated string")
                    .offset(start_range.start)
                    .label(
                        Label::new()
                            .range(start_range.clone())
                            .message("starts here"),
                    )
            })
        }

        self.tok(TK![string close]);
    }

    fn lex_escape(&mut self) {
        let kind = match self.advance() {
            b'n' => TK![esc char newline],
            b'r' => TK![esc char return],
            b't' => TK![esc char tab],
            b'\\' => TK![esc char backslash],
            b'\"' => TK![esc char doubleQuote],
            b'\'' => TK![esc char singleQuote],
            b'x' => {
                let mut lex_char = || {
                    if !is_char::number_hex(self.advance()) {
                        // self.report(Report::new().error().message(
                        //     "expected hexidecimal character in ascii escape starting with '\\x'",
                        // ))
                        let range = self.range.end - 1..self.range.end;
                        self.report(|r| {
                            r.message("expected hexadecimal character in ascii escape")
                                .label(Label::new().range(range.clone()).message("not hexadecimal"))
                        })
                    }
                };
                lex_char();
                lex_char();

                TK![esc asciicode]
            }
            b'u' => {
                if self.advance() != b'{' {
                    // self.report(
                    //     Report::new()
                    //         .error()
                    //         .message("expected '{' in unicode escape after '\\u'"),
                    // )
                    let range = self.range.end - 1..self.range.end;
                    self.report(|r| {
                        r.message("expected a '{' after 'u' in unicode escape ")
                            .label(
                                Label::new()
                                    .range(range.clone())
                                    .message("expected '{' here"),
                            )
                    })
                }

                let mut count = 0;
                loop {
                    let p = self.peek();
                    if p == b'}' || p == b' ' {
                        break;
                    }

                    count += 1;

                    if !is_char::number_hex(self.advance()) {
                        // self.report(
                        //     Report::new()
                        //         .error()
                        //         .message("expected hexideciaml charactor in unicode escape"),
                        // );
                        let range = self.range.end - 1..self.range.end;
                        self.report(|r| {
                            r.message("expected hexadecimal character in unicode escape")
                                .label(
                                    Label::new()
                                        .range(range.clone())
                                        .message("expected hexadecimal character here"),
                                )
                        });
                        break;
                    }

                    if count > 6 || p == b'\0' {
                        break;
                    }
                }

                if !self.eat(b'}') {
                    // self.report(
                    //     Report::new()
                    //         .error()
                    //         .message("expected '}' after unicode escape"),
                    // )
                    let range = self.range.end - 1..self.range.end;
                    self.report(|r| {
                        r.message("expected '}' at the end of unicode escape")
                            .label(
                                Label::new()
                                    .range(range.clone())
                                    .message("expected '}' after here"),
                            )
                    })
                }

                TK![esc unicode]
            }
            _ => TK![esc char other],
        };
        self.tok(kind);
    }

    fn lex_number(&mut self) {
        let mut kind = TK![int dec];

        self.lex_while_condition(is_char::number_mid);

        if self.at(b'.') && is_char::number_mid(self.peek_next()) {
            kind = TK![float];
            self.advance_anz();

            self.lex_while_condition(is_char::number_mid);
            if self.eat(b'E') {
                match self.peek() {
                    b'+' | b'-' => self.advance_anz(),
                    // _ => self.report(
                    //     Report::new()
                    //         .error()
                    //         .message("expected '+' or '-' after 'E' in float literal")
                    //         .short("help: place a '+' or '-' after here"),
                    // ),
                    _ => {
                        let range = self.range.end - 1..self.range.end;
                        self.report(|r| {
                            r.message("expected '+' or '-' after 'E' in exponential float literal")
                                .label(
                                    Label::new()
                                        .range(range.clone())
                                        .message("expected '+' or '-' after here"),
                                )
                        })
                    }
                }
                self.lex_while_condition(is_char::number_mid);
            }
        }

        self.tok(kind);
    }

    fn lex_ident(&mut self) {
        self.lex_while_condition(is_char::ident_mid);

        let slice = &self.ascii[self.range.clone()];

        let kind = match slice[0] {
            // class
            // const
            b'c' if slice.len() > 1 => match slice[1] {
                b'l' if &slice[2..] == b"ass" => TK![class],
                b'o' if &slice[2..] == b"nst" => TK![const],
                _ => TK![ident],
            },

            // else
            // enum
            b'e' if slice.len() > 1 => match slice[1] {
                b'l' if &slice[2..] == b"se" => TK![else],
                b'n' if &slice[2..] == b"um" => TK![enum],
                _ => TK![ident],
            },

            // false
            // fn
            b'f' if slice.len() > 1 => match slice[1] {
                b'a' if &slice[2..] == b"lse" => TK![false],
                b'n' if slice.len() == 2 => TK![fn],
                _ => TK![ident],
            },

            // if
            // impl
            // import
            b'i' if slice.len() > 1 => match slice[1] {
                b'f' if slice.len() == 2 => TK![if],
                b'm' if slice.len() > 2 && slice[2] == b'p' => match slice[3] {
                    b'l' if slice.len() == 4 => TK![impl],
                    b'o' if slice.len() > 4 && &slice[4..] == b"rt" => TK![import],
                    _ => TK![ident],
                },
                _ => TK![ident],
            },

            // let
            b'l' if slice.len() > 1 && &slice[1..] == b"et" => TK![let],

            // mixin
            // module
            // mut
            b'm' if slice.len() > 1 => match slice[1] {
                b'i' if &slice[2..] == b"xin" => TK![mixin],
                b'o' if &slice[2..] == b"dule" => TK![module],
                b'u' if &slice[2..] == b"t" => TK![mut],
                _ => TK![ident],
            },

            // pub
            b'p' if slice.len() > 1 && &slice[1..] == b"ub" => TK![pub],

            // return
            b'r' if slice.len() > 1 && &slice[1..] == b"eturn" => TK![return],

            // self
            // set
            // struct
            b's' if slice.len() > 1 => match slice[1] {
                b'e' if slice.len() > 2 => match slice[2] {
                    b'l' if slice.len() == 4 && slice[3] == b'f' => TK![self],
                    b't' if slice.len() == 3 => TK![set],
                    _ => TK![ident],
                },
                b't' if &slice[2..] == b"ruct" => TK![struct],
                _ => TK![ident],
            },

            // trait
            // true
            b't' if slice.len() > 1 && slice[1] == b'r' => match slice[2] {
                b'a' if slice.len() > 3 && &slice[3..] == b"it" => TK![trait],
                b'u' if slice.len() > 3 && &slice[3..] == b"e" => TK![true],
                _ => TK![ident],
            },

            // union
            b'u' if slice.len() > 1 && &slice[1..] == b"nion" => TK![union],

            _ => TK![ident],
        };

        self.tok(kind);
    }

    // #[cfg(test)]
    // fn has_reports(&self) -> bool {
    //     !self.reports.is_empty()
    // }

    // fn lex_literal_suffix(&mut self) {
    //     debug_assert!(self.ws_len == 0);

    //     if is_char::ident_start(self.peek()) {
    //         self.lex_while_condition(is_char::ident_mid);
    //         self.tok(TK::string_num_suffix);
    //     }
    // }
}

mod is_char {
    #[inline(always)]
    pub fn ident_start(ch: u8) -> bool {
        matches!(ch, b'a'..=b'z' | b'A'..=b'Z' | b'_')
    }

    #[inline(always)]
    pub fn ident_mid(ch: u8) -> bool {
        ident_start(ch) || number_start(ch)
    }

    #[inline(always)]
    pub fn number_start(ch: u8) -> bool {
        ch.is_ascii_digit()
    }

    #[inline(always)]
    pub fn number_mid(ch: u8) -> bool {
        number_start(ch) || ch == b'_'
    }

    #[inline(always)]
    pub fn number_hex(ch: u8) -> bool {
        matches!(ch, b'0'..=b'9' | b'A'..=b'F' | b'a'..=b'f' | b'_')
    }

    #[inline(always)]
    pub fn number_binary(ch: u8) -> bool {
        matches!(ch, b'0' | b'1' | b'_')
    }

    #[inline(always)]
    pub fn number_octal(ch: u8) -> bool {
        matches!(ch, b'0'..=b'7' | b'_')
    }
}

// #[cfg(test)]
// mod tests {
//     use super::*;

//     fn lex(src: impl ToString) -> Result<LexData, ()> {
//         let mut source = src.to_string();
//         source.push_str("\n\0");

//         let mut lexer = Lexer::new(&source);
//         lexer.do_lex();

//         if lexer.has_reports() {
//             return Err(());
//         }

//         let lex_data = LexData {
//             tokens: lexer.tokens.into_boxed_slice(),
//             ranges: lexer.ranges.into_boxed_slice(),
//             line_offsets: lexer.line_offsets.into_boxed_slice(),
//         };

//         Ok(lex_data)
//     }

//     fn ensure_tokens(src: impl ToString, tokens: &[TokenKind]) -> Result<(), ()> {
//         let src_str = src.to_string();
//         let result = lex(src)?;

//         let result = &result.tokens.as_ref()[..1];
//         eprintln!("--------------------------------------");
//         eprintln!("src: {:?}", src_str);
//         eprintln!("result: {result:?}");
//         eprintln!("tokens: {tokens:?}");

//         // ignore last inherit newline
//         if result == tokens {
//             Ok(())
//         } else {
//             Err(())
//         }
//     }

//     #[test]
//     fn whitespace() -> Result<(), ()> {
//         ensure_tokens(" ", &[TK![tr whitespace]])?;
//         ensure_tokens("  ", &[TK![tr whitespace]])?;
//         ensure_tokens("   ", &[TK![tr whitespace]])?;
//         ensure_tokens("\n", &[TK![tr whitespace]])?;
//         ensure_tokens("\n\n", &[TK![tr whitespace]])?;
//         ensure_tokens("\n\n\n", &[TK![tr whitespace]])?;
//         ensure_tokens(" \n", &[TK![tr whitespace]])?;
//         ensure_tokens(" \n \n", &[TK![tr whitespace]])?;
//         ensure_tokens(" \n \n \n", &[TK![tr whitespace]])?;

//         Ok(())
//     }
// }
