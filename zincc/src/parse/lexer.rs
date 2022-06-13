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
    pub spans: Vec<Range<u32>>,
    pub ws_lens: Vec<u16>,
}

impl LexResult {
    pub fn debug_zip(&self) -> Box<dyn std::iter::Iterator<Item = (TK, Range<u32>, u16)>> {
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
    source: &'s str,

    ws_len: u16,
    tokens: Vec<TK>,
    spans: Vec<Range<u32>>,
    ws_lens: Vec<u16>,

    span: Range<u32>,
}

impl<'s> Lexer<'s> {
    fn new(source: &'s str) -> Self {
        Self {
            source,
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
        while self.next() {}
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

    // fn advance(&mut self) -> Option<char> {
    //     let c = self.source.chars().nth(self.span.end as usize);
    //     self.span.end += 1;
    //     c
    // }
    //
    // fn advance_assume(&mut self) -> char {
    //     self.advance().unwrap()
    // }
    #[must_use]
    fn advance(&mut self) -> char {
        let c = self
            .source
            .chars()
            .nth(self.span.end as usize)
            .unwrap_or('\0');
        self.span.end += 1;
        c
    }

    fn peek_raw(&self, n: usize) -> char {
        self.source
            .chars()
            .nth(self.span.end as usize + n)
            .unwrap_or('\0')
    }

    fn peek(&self) -> char {
        self.peek_raw(0)
    }

    fn peek_next(&self) -> char {
        self.peek_raw(1)
    }

    fn at(&self, ch: char) -> bool {
        self.peek() == ch
    }

    fn match_next(&mut self, expect: char) -> bool {
        let cond = self.at(expect);
        if cond {
            let _ = self.advance();
        }
        cond
    }

    fn tok_if_match(&mut self, expect: char, r#if: TK, r#else: TK) {
        let m = self.match_next(expect);
        if m {
            self.tok(r#if);
        } else {
            self.tok(r#else);
        }
    }

    fn next(&mut self) -> bool {
        // let ch = match self.advance() {
        //     '\0' => return false,
        //     c => c,
        // };
        let ch = self.advance();

        match ch {
            ' ' | '\n' | '\t' | '\r' => {
                self.inc_ws();
                while matches!(self.peek(), ' ' | '\n' | '\t' | '\r') {
                    assert_ne!(self.advance(), '\0');
                    self.inc_ws();
                }
            }

            '(' => self.tok(TK::brkt_paren_open),
            ')' => self.tok(TK::brkt_paren_close),
            '{' => self.tok(TK::brkt_brace_open),
            '}' => self.tok(TK::brkt_brace_close),
            '[' => self.tok(TK::brkt_square_open),
            ']' => self.tok(TK::brkt_square_close),

            ':' => self.tok_if_match(':', TK::punct_dblColon, TK::punct_colon),
            ';' => self.tok(TK::punct_semiColon),
            ',' => self.tok(TK::punct_comma),

            '=' => self.tok_if_match('>', TK::punct_fat_arrow, TK::punct_eq),
            '+' => self.tok(TK::punct_plus),
            '-' => self.tok(TK::punct_minus),
            '*' => self.tok(TK::punct_star),

            '/' => {
                if self.match_next('/') {
                    self.inc_ws();
                    self.inc_ws(); // '//'
                    while !matches!(self.peek(), '\n' | '\0') {
                        assert_ne!(self.advance(), '\0');
                        self.inc_ws();
                    }
                } else {
                    self.tok(TK::punct_slash)
                }
            }

            '"' => self.lex_string(),

            '\0' => return false,

            _ => {
                if ch == '0' {
                    match self.peek() {
                        'x' => {
                            self.lex_number_generic(TK::int_hex, is_char::number_hex);
                            return true;
                        }
                        'b' => {
                            self.lex_number_generic(TK::int_bin, is_char::number_binary);
                            return true;
                        }
                        'o' => {
                            self.lex_number_generic(TK::int_oct, is_char::number_octal);
                            return true;
                        }
                        _ => {}
                    }
                }

                match ch {
                    _ if is_char::number_start(ch) => self.lex_number(),
                    _ if is_char::ident_start(ch) => self.lex_ident(),
                    _ => todo!("report error; unknown char"),
                }
            }
        }

        true
    }

    #[inline]
    fn inc_ws(&mut self) {
        self.ws_len += 1;
        self.span.start += 1;
    }

    fn lex_number_generic(&mut self, kind: TK, condition: fn(char) -> bool) {
        assert_ne!(self.advance(), '\0');
        self.lex_while_condition(condition);
        self.tok(kind);
    }

    #[inline]
    fn lex_while_condition(&mut self, condition: fn(char) -> bool) {
        while condition(self.peek()) {
            let _ = self.advance();
        }
    }

    fn lex_string(&mut self) {
        self.tok(TK::string_open);

        let mut p = self.peek();
        while p != '\"' && p != '\0' {
            if p == '\\' {
                if self.span.start != self.span.end {
                    self.tok(TK::string_literal);
                }

                assert_ne!(self.advance(), '\0');
                self.lex_escape();
            } else {
                assert_ne!(self.advance(), '\0');
            }
            p = self.peek();
        }
        if self.span.start != self.span.end {
            self.tok(TK::string_literal);
        }
        assert_eq!(self.advance(), '\"');

        self.tok(TK::string_close);
    }

    fn lex_escape(&mut self) {
        let kind = match self.advance() {
            'x' => {
                let mut lex_char = || {
                    if !is_char::number_hex(self.advance()) {
                        todo!("report error; non hex char as ascii escape")
                    }
                };
                lex_char();
                lex_char();

                TK::esc_asciicode
            }
            'u' => todo!("unicode escape lexing"),
            '\0' => todo!("report error; eof when expecting escape"),
            _ => TK::esc_char,
        };
        self.tok(kind);
    }

    fn lex_number(&mut self) {
        let mut kind = TK::int_dec;

        self.lex_while_condition(is_char::number_mid);

        if self.at('.') && is_char::number_mid(self.peek_next()) {
            kind = TK::float;
            assert_ne!(self.advance(), '\0');

            self.lex_while_condition(is_char::number_mid);
            if self.match_next('E') {
                match self.peek() {
                    '+' | '-' => assert_ne!(self.advance(), '\0'),
                    _ => todo!("report error; malformed float literal"),
                }
            }
        }

        self.tok(kind);
    }

    fn lex_ident(&mut self) {
        self.lex_while_condition(is_char::ident_mid);

        let slice = self.source.chars().collect::<Vec<_>>();
        let slice = &slice[self.span.start as usize..self.span.end as usize];

        let kind = match slice[0] {
            // const
            'c' => self.check_kw(slice, 1, "onst", TK::kw_const),

            // fn
            // false
            'f' => match slice[1] {
                'a' => self.check_kw(slice, 2, "lse", TK::kw_false),
                'n' => self.check_kw(slice, 2, "", TK::kw_fn), // @Speed
                _ => TK::ident,
            },

            // let
            'l' => self.check_kw(slice, 1, "et", TK::kw_let),

            // mut
            'm' => self.check_kw(slice, 1, "ut", TK::kw_mut),

            // return
            'r' => self.check_kw(slice, 1, "eturn", TK::kw_return),

            // true
            't' => self.check_kw(slice, 1, "rue", TK::kw_true),

            _ => TK::ident,
        };
        self.tok(kind);
    }

    fn check_kw(&self, slice: &[char], start: usize, rest: &str, kind: TK) -> TK {
        let checking_slice = &slice[start..];
        if checking_slice.iter().collect::<String>() == rest {
            kind
        } else {
            TK::ident
        }
    }
}

mod is_char {
    #[inline(always)]
    pub fn ident_start(ch: char) -> bool {
        ch.is_alphabetic() || ch == '_'
    }

    #[inline(always)]
    pub fn ident_mid(ch: char) -> bool {
        ch.is_alphanumeric() || ch == '_'
    }

    #[inline(always)]
    pub fn number_start(ch: char) -> bool {
        ch.is_ascii_digit()
    }

    #[inline(always)]
    pub fn number_mid(ch: char) -> bool {
        number_start(ch) || ch == '_'
    }

    #[inline(always)]
    pub fn number_hex(ch: char) -> bool {
        ch.is_ascii_hexdigit() || ch == '_'
    }

    #[inline(always)]
    pub fn number_binary(ch: char) -> bool {
        matches!(ch, '0' | '1' | '_')
    }

    #[inline(always)]
    pub fn number_octal(ch: char) -> bool {
        matches!(ch, '0'..='7' | '_')
    }
}
