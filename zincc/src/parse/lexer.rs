use super::TK;
use std::ops::Range;

/// Given a `source` [`&str`] produce a [`LexResult`].
///
/// ## Asserts
/// `debug_assert!(source.ends_with("\n\0"))`
pub fn lex(source: &str) -> LexResult {
    debug_assert!(source.ends_with("\n\0"));
    let mut lexer = Lexer::new(source);
    lexer.do_lex();
    lexer.out
}

#[derive(Debug)]
pub struct LexResult {
    pub tokens: Vec<TK>,
    pub spans: Vec<Range<usize>>,
    pub ws_lens: Vec<u16>,
    pub errors: Vec<LexError>,
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

#[derive(Debug, Clone)]
pub struct LexError {
    pub kind: LexErrorKind,
    pub offset: usize,
}

#[derive(Debug, Clone)]
pub enum LexErrorKind {
    UnrecognizedChar,
    // UnexpectedEOF,
    ExpectedHexCharInAsciiEscape,
    ExpectedPlusOrMinusAfterEInFloatLiteral,
    ExpectedBraceInUnicodeEsc,
    ExpectedHexCharInUnicodeEsc,
    UnterminatedString,
}

struct Lexer<'s> {
    ascii: &'s [u8],
    span: Range<usize>,
    ws_len: u16,
    out: LexResult,
}

impl<'s> Lexer<'s> {
    fn new(source: &'s str) -> Self {
        Self {
            ascii: source.as_bytes(),
            ws_len: 0,
            span: 0..0,
            out: LexResult {
                tokens: vec![],
                spans: vec![],
                ws_lens: vec![],
                errors: vec![],
            },
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

                b'+' => self.tok(TK::punct_plus),
                b'-' => self.tok(TK::punct_minus),
                b'*' => self.tok(TK::punct_star),

                b'?' => self.tok(TK::punct_question),
                b'!' => self.tok(TK::punct_bang),
                b'&' => self.tok(TK::punct_amp),

                b'=' => self.tok_if_match(b'>', TK::punct_fat_arrow, TK::punct_eq),

                b'/' => {
                    if self.eat(b'/') {
                        self.inc_ws();
                        self.inc_ws(); // '//'
                        while self.peek() != b'\n' {
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
                        _ => {
                            self.report_error(LexErrorKind::UnrecognizedChar);
                            self.tok(TK::err);
                        }
                    }
                }
            }
        }

        self.span.end -= 1;
        self.tok(TK::Eof);
    }

    fn tok(&mut self, kind: TK) {
        let range = self.span.start..self.span.end;
        self.ws_len = 0;
        self.span.start = self.span.end;

        self.out.tokens.push(kind);
        self.out.ws_lens.push(self.ws_len);
        self.out.spans.push(range);
    }

    fn report_error(&mut self, kind: LexErrorKind) {
        self.out.errors.push(LexError {
            kind,
            offset: self.span.end,
        });
    }

    #[must_use]
    #[inline]
    fn advance(&mut self) -> u8 {
        self.span.end += 1;
        self.ascii[self.span.end - 1]
    }

    #[inline]
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
        if !self.eat(b'\"') {
            self.report_error(LexErrorKind::UnterminatedString);
        }

        self.tok(TK::string_close);
    }

    fn lex_escape(&mut self) {
        let kind = match self.advance() {
            b'n' => TK::esc_char_newline,
            b'r' => TK::esc_char_return,
            b't' => TK::esc_char_tab,
            b'\\' => TK::esc_char_backslash,
            b'\"' => TK::esc_char_doublequote,
            b'\'' => TK::esc_char_singlequote,
            b'x' => {
                let mut lex_char = || {
                    if !is_char::number_hex(self.advance()) {
                        self.report_error(LexErrorKind::ExpectedHexCharInAsciiEscape);
                    }
                };
                lex_char();
                lex_char();

                TK::esc_asciicode
            }
            b'u' => {
                if self.advance() != b'{' {
                    self.report_error(LexErrorKind::ExpectedBraceInUnicodeEsc);
                }

                let mut count = 0;
                loop {
                    let p = self.peek();
                    if p == b'}' || p == b' ' {
                        break;
                    }

                    count += 1;

                    if !is_char::number_hex(self.advance()) {
                        self.report_error(LexErrorKind::ExpectedHexCharInUnicodeEsc);
                        break;
                    }

                    if count > 6 || p == b'\0' {
                        break;
                    }
                }

                if !self.eat(b'}') {
                    self.report_error(LexErrorKind::ExpectedBraceInUnicodeEsc);
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
            assert_ne!(self.advance(), b'\0');

            self.lex_while_condition(is_char::number_mid);
            if self.eat(b'E') {
                match self.peek() {
                    b'+' | b'-' => assert_ne!(self.advance(), b'\0'),
                    _ => self.report_error(LexErrorKind::ExpectedPlusOrMinusAfterEInFloatLiteral),
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
            // and
            b'a' => self.check_kw(slice, 1, b"nd", TK::kw_and),

            // const
            b'c' => self.check_kw(slice, 1, b"onst", TK::kw_const),

            // false
            // fn
            b'f' => match slice[1] {
                b'a' => self.check_kw(slice, 2, b"lse", TK::kw_false),
                b'n' if slice.len() == 2 => TK::kw_fn,
                _ => TK::ident,
            },

            // let
            b'l' => self.check_kw(slice, 1, b"et", TK::kw_let),

            // mut
            b'm' => self.check_kw(slice, 1, b"ut", TK::kw_mut),

            // or
            b'o' => self.check_kw(slice, 1, b"r", TK::kw_or),

            // return
            b'r' => self.check_kw(slice, 1, b"eturn", TK::kw_return),

            // true
            b't' => self.check_kw(slice, 1, b"rue", TK::kw_true),

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
