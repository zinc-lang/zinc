use super::{
    cst::{Node, NodeKind as NK},
    TokenKind, TK,
};

pub fn parse(tokens: &[TokenKind]) -> ParseResult {
    let mut parser = Parser {
        tokens,

        panicking: false,
        cursor: 0,

        errors: Vec::new(),
    };
    let cst = parser.parse_top_level();
    ParseResult {
        cst,
        errors: parser.errors,
    }
}

struct Parser<'s> {
    tokens: &'s [TokenKind],

    panicking: bool,
    cursor: u32,

    errors: Vec<ParseError>,
}

#[derive(Debug)]
pub struct ParseResult {
    pub cst: Node,
    pub errors: Vec<ParseError>,
}

#[derive(Debug)]
pub enum ParseError {
    Expected(ParseErrorExpected),
}

#[derive(Debug)]
pub struct ParseErrorExpected {
    pub what: ParseErrorExpectedWhat,
    pub at: u32,
    pub found: u32,
    pub context: ParseContext,
}

#[derive(Debug)]
pub enum ParseErrorExpectedWhat {
    Item(ParseErrorItem),
    Token(TK),
    OneOf(Vec<ParseErrorExpectedWhat>),
}

#[derive(Debug)]
pub enum ParseErrorItem {
    Decl,
    Expr,
}

#[derive(Debug)]
pub enum ParseContext {
    TopLevel,
    Block,
    DeclFunc,
    DeclConst,
    Stmt,
    StmtLet,
    StmtReturn,
    ExprStart,
    ExprParen,
    ExprCall,
    String,
    Path,
}

/// Token ops
impl Parser<'_> {
    fn at_end(&self) -> bool {
        self.at(TK::EOF)
    }

    fn peek_n(&self, n: u32) -> TK {
        if self.cursor + n >= self.tokens.len() as u32 {
            TK::EOF
        } else {
            self.tokens[(self.cursor + n) as usize]
        }
    }
    fn peek(&self) -> TK {
        self.peek_n(0)
    }

    fn at(&self, kind: TK) -> bool {
        self.peek() == kind
    }

    fn at_set(&self, set: &[TK]) -> bool {
        for kind in set {
            if self.at(*kind) {
                return true;
            }
        }
        false
    }

    fn eat(&mut self, kind: TK, parent: &mut Node) -> bool {
        if self.at(kind) {
            self.bump(parent);
            true
        } else {
            false
        }
    }

    fn eat_set(&mut self, set: &[TK], parent: &mut Node) -> bool {
        if self.at_set(set) {
            self.bump(parent);
            true
        } else {
            false
        }
    }
}

/// Error ops
impl Parser<'_> {
    fn report(&mut self, parent: &mut Node, err: ParseError) {
        if !self.panicking {
            self.panicking = true;

            self.errors.push(err);
            return;
        }

        if self.at_set(&[TK::brkt_brace_close, TK::EOF, TK::kw_let]) {
            self.bump(parent);
            self.panicking = false;
        } else {
            let mut node = self.node(NK::skipped);
            self.bump(&mut node);
            parent.append_node(node);
        }
    }

    fn expect(&mut self, what: TK, context: ParseContext, parent: &mut Node) {
        if !self.eat(what, parent) {
            self.report(
                parent,
                ParseError::Expected(ParseErrorExpected {
                    what: ParseErrorExpectedWhat::Token(what),
                    at: self.cursor - 1,
                    found: self.cursor,
                    context,
                }),
            )
        }
    }
}

/// Node ops
impl Parser<'_> {
    fn bump(&mut self, parent: &mut Node) {
        parent.elements.push(super::cst::Element::Token);
        self.cursor += 1;
    }

    fn node(&self, kind: NK) -> Node {
        Node::new(kind, self.cursor)
    }
}

/// Parse
impl Parser<'_> {
    fn parse_top_level(&mut self) -> Node {
        let mut root = self.node(NK::root);

        while !self.at_end() {
            self.parse_decl(ParseContext::TopLevel, &mut root);
        }

        self.bump(&mut root);

        root
    }

    fn parse_path(&mut self) -> Node {
        let mut path = self.node(NK::path);

        self.eat(TK::punct_dblColon, &mut path);
        self.expect(TK::ident, ParseContext::Path, &mut path);

        while self.at(TK::punct_dblColon) {
            self.expect(TK::punct_dblColon, ParseContext::Path, &mut path);
            self.expect(TK::ident, ParseContext::Path, &mut path);
        }

        path
    }

    fn parse_string(&mut self) -> Node {
        assert!(self.at(TK::string_open));

        let mut str = self.node(NK::string);

        self.bump(&mut str);
        while self.eat_set(
            &[
                TK::string_literal,
                TK::esc_char,
                TK::esc_asciicode,
                TK::esc_unicode,
            ],
            &mut str,
        ) {}
        self.expect(TK::string_close, ParseContext::String, &mut str);

        str
    }

    fn parse_literal_int(&mut self) -> Node {
        assert!(self.at_set(&[TK::int_dec, TK::int_bin, TK::int_hex, TK::int_oct]));
        let mut lit = self.node(NK::literal_int);
        self.bump(&mut lit);
        lit
    }

    fn parse_literal_float(&mut self) -> Node {
        assert!(self.at(TK::float));
        let mut lit = self.node(NK::literal_float);
        self.bump(&mut lit);
        lit
    }

    fn parse_block(&mut self) -> Node {
        let mut block = self.node(NK::block);

        self.expect(TK::brkt_brace_open, ParseContext::Block, &mut block);

        while !self.at_set(&[TK::brkt_brace_close, TK::EOF]) {
            self.parse_stmt(&mut block);
        }

        self.expect(TK::brkt_brace_close, ParseContext::Block, &mut block);

        block
    }

    /// 'let's and 'const's
    fn parse_binding(&mut self, binding: &mut Node) {
        // ident
        self.expect(TK::ident, ParseContext::DeclConst, binding);

        // ty?
        if !self.at(TK::punct_eq) {
            let mut ty = self.node(NK::binding_ty);
            self.parse_ty(&mut ty);
            binding.append_node(ty);
        }

        // '='
        self.expect(TK::punct_eq, ParseContext::DeclConst, binding);

        // expr
        self.parse_stmt_expr(binding);
    }
}

/// Parse decl
impl Parser<'_> {
    fn parse_decl(&mut self, context: ParseContext, parent: &mut Node) {
        if let Some(decl) = self.try_parse_decl() {
            parent.append_node(decl);
        } else {
            self.report(
                parent,
                ParseError::Expected(ParseErrorExpected {
                    what: ParseErrorExpectedWhat::Item(ParseErrorItem::Decl),
                    at: self.cursor,
                    found: self.cursor,
                    context,
                }),
            )
        }
    }

    fn try_parse_decl(&mut self) -> Option<Node> {
        let decl = match self.peek() {
            TK::ident => match self.peek_n(1) {
                TK::kw_fn => {
                    let mut func = self.node(NK::decl_func);
                    self.bump(&mut func);
                    self.parse_decl_func(&mut func);
                    func
                }
                _ => return None,
            },
            TK::kw_const => {
                let mut konst = self.node(NK::decl_const);

                self.bump(&mut konst); // 'const'
                self.parse_binding(&mut konst);

                konst
            }
            _ => return None,
        };
        Some(decl)
    }

    fn parse_decl_func(&mut self, func: &mut Node) {
        self.parse_ty_func(func);

        let mut body = self.node(NK::decl_func_body);

        if self.eat(TK::punct_fat_arrow, &mut body) {
            self.parse_stmt_expr(&mut body);
        } else {
            body.append_node(self.parse_block());
        }

        func.append_node(body);
    }
}

/// Parse ty
impl Parser<'_> {
    fn parse_ty(&mut self, parent: &mut Node) {
        parent.append_node(self.parse_path());
    }

    fn parse_ty_func(&mut self, parent: &mut Node) {
        assert!(self.at(TK::kw_fn));

        let mut proto = self.node(NK::func_proto);
        self.bump(&mut proto); // 'fn'

        if self.eat(TK::brkt_paren_open, &mut proto) {
            while !self.at(TK::brkt_paren_close) {
                let mut arg = self.node(NK::func_proto_arg);
                if self.at(TK::ident)
                    && !matches!(self.peek_n(1), TK::punct_comma | TK::brkt_paren_close)
                {
                    self.expect(TK::ident, ParseContext::DeclFunc, &mut arg);
                }
                self.parse_ty(&mut arg);
                proto.append_node(arg);

                if !self.eat(TK::punct_comma, &mut proto) {
                    break;
                }
            }

            self.expect(TK::brkt_paren_close, ParseContext::DeclFunc, &mut proto);
        }

        if !self.at_set(&[TK::punct_comma, TK::punct_fat_arrow, TK::brkt_brace_open]) {
            let mut ret = self.node(NK::func_proto_ret);
            self.parse_ty(&mut ret);
            proto.append_node(ret);
        }

        parent.append_node(proto);
    }
}

/// Parse stmt
impl Parser<'_> {
    fn parse_stmt(&mut self, parent: &mut Node) {
        match self.peek() {
            TK::kw_let => {
                let mut binding = self.node(NK::stmt_let);

                // 'let'
                self.bump(&mut binding); // 'let'

                self.parse_binding(&mut binding);

                parent.append_node(binding);
            }
            _ => {
                if let Some(decl) = self.try_parse_decl() {
                    parent.append_node(decl);
                } else {
                    self.parse_stmt_expr(parent);
                }
            }
        }
    }

    /// Parse an expression in the context of a statement.
    /// ie. with a semicolon if it does not end with a '}'
    fn parse_stmt_expr(&mut self, parent: &mut Node) {
        self.parse_expr(parent);

        if self.tokens[(self.cursor - 1) as usize] != TK::brkt_brace_close {
            self.expect(TK::punct_semiColon, ParseContext::Stmt, parent);
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
#[repr(u8)]
enum Precedence {
    None = 0,
    Assignment,
    // LogicalOr,
    // LogicalAnd,
    Sum,
    Product,
    // Prefix,
    Call,
}

impl Precedence {
    fn of(kind: TK) -> Self {
        match kind {
            TK::punct_eq => Self::Assignment,
            TK::punct_plus | TK::punct_minus => Self::Sum,
            TK::punct_star | TK::punct_slash => Self::Product,
            TK::brkt_paren_open => Self::Call,
            _ => Self::None,
        }
    }
}

/// Parse expr
impl Parser<'_> {
    fn parse_expr(&mut self, parent: &mut Node) {
        self.parse_expr_precedence(Precedence::None, parent);
    }

    fn try_parse_expr(&mut self) -> Option<Node> {
        self.try_parse_expr_precedence(Precedence::None)
    }

    fn parse_expr_precedence(&mut self, prec: Precedence, parent: &mut Node) {
        if let Some(expr) = self.try_parse_expr_precedence(prec) {
            parent.append_node(expr);
        } else {
            self.report(
                parent,
                ParseError::Expected(ParseErrorExpected {
                    what: ParseErrorExpectedWhat::Item(ParseErrorItem::Expr),
                    at: self.cursor,
                    found: self.cursor,
                    context: ParseContext::ExprStart,
                }),
            );
        }
    }

    fn try_parse_expr_precedence(&mut self, prec: Precedence) -> Option<Node> {
        let mut lhs = self.try_parse_expr_start()?;

        let init_prec = prec;
        let mut p = self.peek();
        let mut infix_prec = Precedence::of(p);

        while init_prec < infix_prec {
            if let Some(e) = self.try_parse_expr_infix(p, lhs.clone(), infix_prec) {
                lhs = e;
            } else {
                // @FIXME: Is this ever an error condition?
                // return Some(lhs);
                unreachable!();
            }

            p = self.peek();
            infix_prec = Precedence::of(p);
        }

        Some(lhs)
    }

    fn try_parse_expr_start(&mut self) -> Option<Node> {
        let expr = match self.peek() {
            TK::ident => self.parse_path(),
            TK::string_open => self.parse_string(),

            TK::int_dec | TK::int_hex | TK::int_bin | TK::int_oct => self.parse_literal_int(),
            TK::float => self.parse_literal_float(),

            TK::brkt_brace_open => self.parse_block(),

            TK::kw_return => {
                let mut ret = self.node(NK::expr_return);
                self.bump(&mut ret); // 'return'
                if let Some(expr) = self.try_parse_expr() {
                    ret.append_node(expr);
                }
                ret
            }

            _ => return None,
        };
        Some(expr)
    }

    fn try_parse_expr_infix(&mut self, p: TK, lhs: Node, prec: Precedence) -> Option<Node> {
        let node = match p {
            TK::punct_eq | TK::punct_plus | TK::punct_minus | TK::punct_star | TK::punct_slash => {
                let mut infix = self.node(NK::expr_infix);

                // lhs
                infix.append_node(lhs);

                // op
                {
                    let mut op = self.node(NK::expr_infix_op);
                    // @TODO: parse multiple operators
                    self.bump(&mut op);
                    infix.append_node(op);
                }

                // rhs
                self.parse_expr_precedence(prec, &mut infix);

                infix
            }
            TK::brkt_paren_open => {
                let mut call = self.node(NK::expr_call);

                // callee
                call.append_node(lhs);

                self.bump(&mut call); // '('

                if !self.at(TK::brkt_paren_close) {
                    self.parse_expr(&mut call);
                    while self.eat(TK::punct_comma, &mut call)
                        && !self.at(TokenKind::brkt_paren_close)
                    {
                        self.parse_expr(&mut call);
                    }
                }

                self.expect(TK::brkt_paren_close, ParseContext::ExprCall, &mut call); // ')'

                call
            }
            _ => return None,
        };
        Some(node)
    }
}
