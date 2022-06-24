use super::TK;
use std::ops::Range;

pub fn lex(source: &str) -> LexResult {
    let mut lexer = Lexer::new(source);
    lexer.do_lex();
    LexResult {
        tokens: lexer.tokens,
        spans: lexer.spans,
        ws_lens: lexer.ws_lens,
    }
}

#[derive(Debug)]
pub struct LexResult {
    pub tokens: Vec<TK>,
    pub spans: Vec<Range<usize>>,
    pub ws_lens: Vec<u16>,
}

impl LexResult {
    pub fn debug_zip(&self) -> Box<dyn std::iter::Iterator<Item = (TK, Range<usize>, u16)>> {
        Box::new(
            self.tokens
                .clone()
                .into_iter()
                .zip(self.spans.clone().into_iter())
                .zip(self.ws_lens.clone().into_iter())
                .map(|((tk, range), ws)| (tk, range, ws)),
        )
    }
}

struct Lexer<'s> {
    ascii: &'s [u8],

    ws_len: u16,
    tokens: Vec<TK>,
    spans: Vec<Range<usize>>,
    ws_lens: Vec<u16>,

    span: Range<usize>,
}

impl<'s> Lexer<'s> {
    fn new(source: &'s str) -> Self {
        Self {
            ascii: source.as_bytes(),
            ws_len: 0,
            tokens: vec![],
            spans: vec![],
            ws_lens: vec![],
            span: 0..0,
        }
    }
}

impl Lexer<'_> {
    fn do_lex(&mut self) {
        'lex_loop: loop {
            let ch = self.advance();

            match ch {
                b' ' | b'\n' | b'\t' | b'\r' => {
                    self.inc_ws();
                    while matches!(self.peek(), b' ' | b'\n' | b'\t' | b'\r') {
                        assert_ne!(self.advance(), b'\0');
                        self.inc_ws();
                    }
                }

                b'(' => self.tok(TK::brkt_paren_open),
                b')' => self.tok(TK::brkt_paren_close),
                b'{' => self.tok(TK::brkt_brace_open),
                b'}' => self.tok(TK::brkt_brace_close),
                b'[' => self.tok(TK::brkt_square_open),
                b']' => self.tok(TK::brkt_square_close),

                b':' => self.tok_if_match(b':', TK::punct_dblColon, TK::punct_colon),
                b';' => self.tok(TK::punct_semiColon),
                b',' => self.tok(TK::punct_comma),

                b'=' => self.tok_if_match(b'>', TK::punct_fat_arrow, TK::punct_eq),
                b'+' => self.tok(TK::punct_plus),
                b'-' => self.tok(TK::punct_minus),
                b'*' => self.tok(TK::punct_star),

                b'?' => self.tok(TK::punct_question),

                b'/' => {
                    if self.match_next(b'/') {
                        self.inc_ws();
                        self.inc_ws(); // '//'
                        while !matches!(self.peek(), b'\n' | b'\0') {
                            assert_ne!(self.advance(), b'\0');
                            self.inc_ws();
                        }
                    } else {
                        self.tok(TK::punct_slash)
                    }
                }

                b'"' => self.lex_string(),

                b'\0' => break 'lex_loop,

                _ => {
                    if ch == b'0' {
                        match self.peek() {
                            b'x' => {
                                self.lex_number_generic(TK::int_hex, is_char::number_hex);
                                continue 'lex_loop;
                            }
                            b'b' => {
                                self.lex_number_generic(TK::int_bin, is_char::number_binary);
                                continue 'lex_loop;
                            }
                            b'o' => {
                                self.lex_number_generic(TK::int_oct, is_char::number_octal);
                                continue 'lex_loop;
                            }
                            _ => {}
                        }
                    }

                    match ch {
                        _ if is_char::number_start(ch) => self.lex_number(),
                        _ if is_char::ident_start(ch) => self.lex_ident(),
                        _ => todo!(
                            "report error; unknown char {:?}",
                            crate::parse::FileLocation::from_offset(
                                self.span.end,
                                &String::from_utf8(self.ascii.to_vec()).unwrap(),
                            )
                        ),
                    }
                }
            }
        }

        self.span.end -= 1;
        self.tok(TK::EOF);
    }

    fn tok(&mut self, kind: TK) {
        self.tokens.push(kind);
        self.ws_lens.push(self.ws_len);

        let range = self.span.start..self.span.end;
        self.spans.push(range);

        self.ws_len = 0;
        self.span.start = self.span.end;
    }

    #[must_use]
    fn advance(&mut self) -> u8 {
        self.span.end += 1;
        self.ascii[self.span.end - 1]
    }

    fn peek_raw(&self, n: usize) -> u8 {
        self.ascii[self.span.end + n]
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
    fn match_next(&mut self, expect: u8) -> bool {
        let cond = self.at(expect);
        if cond {
            let _ = self.advance();
        }
        cond
    }

    fn tok_if_match(&mut self, expect: u8, r#if: TK, r#else: TK) {
        let m = self.match_next(expect);
        if m {
            self.tok(r#if);
        } else {
            self.tok(r#else);
        }
    }

    #[inline]
    fn inc_ws(&mut self) {
        self.ws_len += 1;
        self.span.start += 1;
    }

    #[inline]
    fn lex_number_generic(&mut self, kind: TK, condition: fn(u8) -> bool) {
        assert_ne!(self.advance(), b'\0');
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

        let mut p = self.peek();
        while p != b'\"' && p != b'\0' {
            if p == b'\\' {
                if self.span.start != self.span.end {
                    self.tok(TK::string_literal);
                }

                assert_ne!(self.advance(), b'\0');
                self.lex_escape();
            } else {
                assert_ne!(self.advance(), b'\0');
            }
            p = self.peek();
        }
        if self.span.start != self.span.end {
            self.tok(TK::string_literal);
        }
        assert_eq!(self.advance(), b'\"');

        self.tok(TK::string_close);
    }

    fn lex_escape(&mut self) {
        let kind = match self.advance() {
            b'x' => {
                let mut lex_char = || {
                    if !is_char::number_hex(self.advance()) {
                        todo!("report error; non hex char as ascii escape")
                    }
                };
                lex_char();
                lex_char();

                TK::esc_asciicode
            }
            b'u' => todo!("unicode escape lexing"),
            b'\0' => todo!("report error; eof when expecting escape"),
            _ => TK::esc_char,
        };
        self.tok(kind);
    }

    fn lex_number(&mut self) {
        let mut kind = TK::int_dec;

        self.lex_while_condition(is_char::number_mid);

        if self.at(b'.') && is_char::number_mid(self.peek_next()) {
            kind = TK::float;
            assert_ne!(self.advance(), b'\0');

            self.lex_while_condition(is_char::number_mid);
            if self.match_next(b'E') {
                match self.peek() {
                    b'+' | b'-' => assert_ne!(self.advance(), b'\0'),
                    _ => todo!("report error; malformed float literal"),
                }
                self.lex_while_condition(is_char::number_mid);
            }
        }

        self.tok(kind);
    }

    fn lex_ident(&mut self) {
        self.lex_while_condition(is_char::ident_mid);

        let slice = &self.ascii[self.span.clone()];

        let kind = match slice[0] {
            // const
            b'c' => self.check_kw(slice, 1, "onst".as_bytes(), TK::kw_const),

            // false
            // fn
            b'f' => match slice[1] {
                b'a' => self.check_kw(slice, 2, "lse".as_bytes(), TK::kw_false),
                b'n' if slice.len() == 2 => TK::kw_fn,
                _ => TK::ident,
            },

            // let
            b'l' => self.check_kw(slice, 1, "et".as_bytes(), TK::kw_let),

            // mut
            b'm' => self.check_kw(slice, 1, "ut".as_bytes(), TK::kw_mut),

            // return
            b'r' => self.check_kw(slice, 1, "eturn".as_bytes(), TK::kw_return),

            // true
            b't' => self.check_kw(slice, 1, "rue".as_bytes(), TK::kw_true),

            _ => TK::ident,
        };
        self.tok(kind);
    }

    fn check_kw(&self, slice: &[u8], start: usize, rest: &[u8], kind: TK) -> TK {
        let checking_slice = &slice[start..];
        if checking_slice == rest {
            kind
        } else {
            TK::ident
        }
    }
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
