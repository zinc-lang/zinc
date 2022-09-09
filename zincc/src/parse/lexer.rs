use super::TK;
use crate::{
    report::{self, Report},
    source_map::{self, SourceFileId, SourceMap},
};
use std::ops::Range;

pub fn lex(map: &mut SourceMap, file_id: SourceFileId) -> Vec<Report> {
    debug_assert!(map.sources.contains_key(&file_id));
    debug_assert!(!map.lex_data.contains_key(&file_id));

    let source = &map.sources[&file_id];
    debug_assert!(source.ends_with("\n\0"));

    let mut lexer = Lexer::new(source);
    lexer.do_lex();

    let lex_data = source_map::LexData {
        tokens: lexer.tokens.into_boxed_slice(),
        ranges: lexer.ranges.into_boxed_slice(),
        line_offsets: lexer.line_offsets.into_boxed_slice(),
    };
    map.lex_data.insert(file_id, lex_data);

    let reports = lexer
        .reports
        .into_iter()
        .map(|rep| rep.file(file_id).build())
        .collect::<Vec<_>>();

    reports
}

struct Lexer<'s> {
    ascii: &'s [u8],
    range: Range<usize>,

    tokens: Vec<TK>,
    ranges: Vec<Range<usize>>,
    line_offsets: Vec<usize>,

    // errors: Vec<LexError>,
    reports: Vec<report::Builder>,
}

impl<'s> Lexer<'s> {
    fn new(source: &'s str) -> Self {
        Self {
            ascii: source.as_bytes(),
            range: 0..0,
            tokens: Vec::new(),
            ranges: Vec::new(),
            line_offsets: Vec::new(),
            // errors: Vec::new(),
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
                    self.tok(TK::tr_whitespace);
                    self.line_offsets.push(self.range.start);
                }
                b' ' | b'\r' => {
                    // self.advance_anz();
                    while matches!(self.peek(), b' ' | b'\r') {
                        self.advance_anz();
                    }
                    self.tok(TK::tr_whitespace);
                }

                b'\t' => {
                    todo!()
                }

                b'(' => self.tok(TK::brkt_paren_open),
                b')' => self.tok(TK::brkt_paren_close),
                b'{' => self.tok(TK::brkt_brace_open),
                b'}' => self.tok(TK::brkt_brace_close),
                b'[' => self.tok(TK::brkt_square_open),
                b']' => self.tok(TK::brkt_square_close),

                b'&' => self.tok(TK::punct_ampersand),
                b'!' => self.tok_if_match(b'=', TK::punct_bangEq, TK::punct_bang),
                b':' => self.tok_if_match(b':', TK::punct_doubleColon, TK::punct_colon),
                b';' => self.tok(TK::punct_semicolon),
                b',' => self.tok(TK::punct_comma),

                b'.' => {
                    if self.eat(b'.') {
                        if self.eat(b'.') {
                            self.tok(TK::punct_tripleDot); // ...
                        } else {
                            self.tok(TK::punct_doubleDot); // ..
                        }
                    } else {
                        self.tok(TK::punct_dot); // .
                    }
                }

                b'=' => {
                    if self.eat(b'=') {
                        self.tok(TK::punct_eqEq);
                    } else if self.eat(b'>') {
                        self.tok(TK::punct_fatArrow);
                    } else {
                        self.tok(TK::punct_eq);
                    }
                }

                b'>' => self.tok_if_match(b'=', TK::punct_greaterThanEq, TK::punct_greaterThan),
                b'<' => self.tok_if_match(b'=', TK::punct_lessThanEq, TK::punct_lessThan),

                b'-' => self.tok_if_match(b'>', TK::punct_rThinArrow, TK::punct_minus),
                b'|' => self.tok(TK::punct_pipe),
                b'+' => self.tok(TK::punct_plus),
                b'?' => self.tok(TK::punct_question),
                b'*' => self.tok(TK::punct_star),

                b'/' => {
                    if self.eat(b'/') {
                        self.advance_anz();
                        self.advance_anz();
                        while self.peek() != b'\n' {
                            self.advance_anz();
                        }
                        self.tok(TK::tr_comment);
                    } else {
                        self.tok(TK::punct_slash)
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
                        b'x' => self.lex_number_generic(TK::int_hex, is_char::number_hex),
                        b'b' => self.lex_number_generic(TK::int_bin, is_char::number_binary),
                        b'o' => self.lex_number_generic(TK::int_oct, is_char::number_octal),
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
                            self.report(Report::builder().error().message("unexpected character"));
                            self.tok(TK::err);
                        }
                    }
                }
            }
        }

        self.range.start -= 1;
        self.range.end -= 1;
        self.tok(TK::eof);
        self.line_offsets
            .push(self.line_offsets[self.line_offsets.len() - 1]);
    }

    fn tok(&mut self, kind: TK) {
        let range = self.range.start..self.range.end;
        self.range.start = self.range.end;

        self.tokens.push(kind);
        self.ranges.push(range);
    }

    fn report(&mut self, report: report::Builder) {
        let report = if report.span.is_none() {
            report.span(self.range.end - 1..self.range.end)
        } else {
            report
        };
        self.reports.push(report)
    }

    #[must_use]
    #[inline]
    fn advance(&mut self) -> u8 {
        self.range.end += 1;
        self.ascii[self.range.end - 1]
    }

    /// Advance Assert No Zero
    #[inline]
    fn advance_anz(&mut self) {
        assert_ne!(self.advance(), 0);
    }

    #[inline]
    fn peek_raw(&self, n: usize) -> u8 {
        self.ascii[self.range.end + n]
    }

    #[inline]
    fn peek(&self) -> u8 {
        self.peek_raw(0)
    }

    #[inline]
    fn peek_next(&self) -> u8 {
        self.peek_raw(1)
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
    fn tok_if_match(&mut self, expect: u8, r#if: TK, r#else: TK) {
        let m = self.eat(expect);
        if m {
            self.tok(r#if);
        } else {
            self.tok(r#else);
        }
    }

    #[inline]
    fn lex_number_generic(&mut self, kind: TK, condition: fn(u8) -> bool) {
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
        self.tok(TK::string_open);
        let start_range = self.range.start - 1..self.range.start;

        let mut p = self.peek();
        while p != b'\"' && p != b'\0' && p != b'\n' {
            if p == b'\\' {
                if self.range.start != self.range.end {
                    self.tok(TK::string_literal);
                }

                self.advance_anz();
                self.lex_escape();
            } else {
                self.advance_anz();
            }
            p = self.peek();
        }
        if self.range.start != self.range.end {
            self.tok(TK::string_literal);
        }
        if !self.eat(b'\"') {
            self.report(
                Report::builder()
                    .error()
                    .span(start_range)
                    .message("unterminated string")
                    .short("starts here"),
            );
        }

        self.tok(TK::string_close);
    }

    fn lex_escape(&mut self) {
        let kind = match self.advance() {
            b'n' => TK::esc_char_newline,
            b'r' => TK::esc_char_return,
            b't' => TK::esc_char_tab,
            b'\\' => TK::esc_char_backslash,
            b'\"' => TK::esc_char_doubleQuote,
            b'\'' => TK::esc_char_singleQuote,
            b'x' => {
                let mut lex_char = || {
                    if !is_char::number_hex(self.advance()) {
                        self.report(Report::builder().error().message(
                            "expected hexidecimal character in ascii escape starting with '\\x'",
                        ))
                    }
                };
                lex_char();
                lex_char();

                TK::esc_asciicode
            }
            b'u' => {
                if self.advance() != b'{' {
                    self.report(
                        Report::builder()
                            .error()
                            .message("expected '{' in unicode escape after '\\u'"),
                    )
                }

                let mut count = 0;
                loop {
                    let p = self.peek();
                    if p == b'}' || p == b' ' {
                        break;
                    }

                    count += 1;

                    if !is_char::number_hex(self.advance()) {
                        self.report(
                            Report::builder()
                                .error()
                                .message("expected hexideciaml charactor in unicode escape"),
                        );
                        break;
                    }

                    if count > 6 || p == b'\0' {
                        break;
                    }
                }

                if !self.eat(b'}') {
                    self.report(
                        Report::builder()
                            .error()
                            .message("expected '}' after unicode escape"),
                    )
                }

                TK::esc_unicode
            }
            _ => TK::esc_char_other,
        };
        self.tok(kind);
    }

    fn lex_number(&mut self) {
        let mut kind = TK::int_dec;

        self.lex_while_condition(is_char::number_mid);

        if self.at(b'.') && is_char::number_mid(self.peek_next()) {
            kind = TK::float;
            self.advance_anz();

            self.lex_while_condition(is_char::number_mid);
            if self.eat(b'E') {
                match self.peek() {
                    b'+' | b'-' => self.advance_anz(),
                    _ => self.report(
                        Report::builder()
                            .error()
                            .message("expected '+' or '-' after 'E' in float literal")
                            .short("help: place a '+' or '-' after here"),
                    ),
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
                b'l' if &slice[2..] == b"ass" => TK::kw_class,
                b'o' if &slice[2..] == b"nst" => TK::kw_const,
                _ => TK::ident,
            },

            // else
            // enum
            b'e' if slice.len() > 1 => match slice[1] {
                b'l' if &slice[2..] == b"se" => TK::kw_else,
                b'n' if &slice[2..] == b"um" => TK::kw_enum,
                _ => TK::ident,
            },

            // false
            // fn
            b'f' if slice.len() > 1 => match slice[1] {
                b'a' if &slice[2..] == b"lse" => TK::kw_false,
                b'n' if slice.len() == 2 => TK::kw_fn,
                _ => TK::ident,
            },

            // if
            // impl
            // import
            b'i' if slice.len() > 1 => match slice[1] {
                b'f' if slice.len() == 2 => TK::kw_if,
                b'm' if slice.len() > 2 && slice[2] == b'p' => match slice[3] {
                    b'l' if slice.len() == 4 => TK::kw_impl,
                    b'o' if slice.len() > 4 && &slice[4..] == b"rt" => TK::kw_import,
                    _ => TK::ident,
                },
                _ => TK::ident,
            },

            // let
            b'l' if slice.len() > 1 && &slice[1..] == b"et" => TK::kw_let,

            // mixin
            // module
            // mut
            b'm' if slice.len() > 1 => match slice[1] {
                b'i' if &slice[2..] == b"xin" => TK::kw_mixin,
                b'o' if &slice[2..] == b"dule" => TK::kw_module,
                b'u' if &slice[2..] == b"t" => TK::kw_mut,
                _ => TK::ident,
            },

            // pub
            b'p' if slice.len() > 1 && &slice[1..] == b"ub" => TK::kw_pub,

            // return
            b'r' if slice.len() > 1 && &slice[1..] == b"eturn" => TK::kw_return,

            // set
            // struct
            b's' if slice.len() > 1 => match slice[1] {
                b'e' if &slice[2..] == b"t" => TK::kw_set,
                b't' if &slice[2..] == b"uct" => TK::kw_struct,
                _ => TK::ident,
            },

            // trait
            // true
            b't' if slice.len() > 1 && slice[1] == b'r' => match slice[2] {
                b'a' if slice.len() > 3 && &slice[3..] == b"it" => TK::kw_trait,
                b'u' if slice.len() > 3 && &slice[3..] == b"e" => TK::kw_true,
                _ => TK::ident,
            },

            // union
            b'u' if slice.len() > 1 && &slice[1..] == b"nion" => TK::kw_union,

            _ => TK::ident,
        };

        self.tok(kind);
    }

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
        matches!(ch, b'0'..=b'9')
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
