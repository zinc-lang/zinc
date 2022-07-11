use super::{
    cst::{self, NodeKind, NK},
    TokenKind, TK,
};

pub fn parse(tokens: &[TokenKind]) -> ParseResult {
    let mut parser = Parser {
        tokens,

        panicking: false,
        cursor: 0,

        errors: Vec::new(),

        node_map: Default::default(),
    };
    let root = parser.parse_top_level();
    let mut map = parser.node_map;
    let root = map.push(root.node);
    ParseResult {
        cst: cst::Cst {
            root: cst::NamedNodeId {
                raw: root,
                kind: NK::root,
            },
            map,
        },
        errors: parser.errors,
    }
}

struct Parser<'s> {
    tokens: &'s [TokenKind],

    panicking: bool,
    cursor: usize,

    errors: Vec<ParseError>,

    node_map: crate::util::index::IndexVec<cst::Node, cst::NodeId>,
}

#[derive(Debug)]
pub struct ParseResult {
    pub cst: cst::Cst,
    pub errors: Vec<ParseError>,
}

#[derive(Debug)]
pub enum ParseError {
    Expected(ParseErrorExpected),
}

#[derive(Debug)]
pub struct ParseErrorExpected {
    pub what: ParseErrorExpectedWhat,
    pub at: usize,
    pub found: usize,
    pub context: ParseContext,
}

#[derive(Debug)]
pub enum ParseErrorExpectedWhat {
    Item(ParseErrorItem),
    Token(TK),
    // OneOf(Vec<ParseErrorExpectedWhat>),
}

#[derive(Debug)]
pub enum ParseErrorItem {
    Decl,
    Expr,
    Ty,
}

#[derive(Debug, Clone, Copy)]
pub enum ParseContext {
    TopLevel,
    Block,
    DeclFunc,
    DeclConst,
    Stmt,
    // StmtLet,
    // StmtReturn,
    ExprStart,
    // ExprParen,
    ExprCall,
    String,
    Path,
    TyFunc,
    Ty,
    TySlice,
}

#[derive(Clone)]
struct PNode {
    kind: NodeKind,
    node: cst::Node,
}

/// Token ops
impl Parser<'_> {
    fn at_end(&self) -> bool {
        self.at(TK::Eof)
    }

    fn peek_n(&self, n: usize) -> TK {
        if self.cursor + n >= self.tokens.len() {
            TK::Eof
        } else {
            self.tokens[self.cursor + n]
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

    fn eat(&mut self, kind: TK, parent: &mut PNode) -> bool {
        if self.at(kind) {
            self.bump(parent);
            true
        } else {
            false
        }
    }

    fn eat_set(&mut self, set: &[TK], parent: &mut PNode) -> bool {
        if self.at_set(set) {
            self.bump(parent);
            true
        } else {
            false
        }
    }
}

/// Node ops
impl Parser<'_> {
    fn bump(&mut self, parent: &mut PNode) {
        parent
            .node
            .elements
            .push(cst::Element::Token(cst::TokenIndex::new(self.cursor)));
        self.cursor += 1;
    }

    fn node(&self, kind: NK) -> PNode {
        PNode {
            kind,
            node: cst::Node::new(),
        }
    }

    fn append_node(&mut self, what: PNode, to: &mut PNode) {
        let id_raw = self.node_map.push(what.node);
        let id = cst::NamedNodeId {
            raw: id_raw,
            kind: what.kind,
        };
        to.node.elements.push(cst::Element::Node(id));
    }

    fn node_bump(&mut self, nk: NK) -> PNode {
        let mut n = self.node(nk);
        self.bump(&mut n);
        n
    }
}

/// Error ops
impl Parser<'_> {
    fn report(&mut self, parent: &mut PNode, err: ParseError) {
        if !self.panicking {
            self.panicking = true;

            self.errors.push(err);
            return;
        }

        if self.at_set(&[TK::brkt_brace_close, TK::Eof, TK::kw_let]) {
            self.bump(parent);
            self.panicking = false;
        } else {
            let mut node = self.node(NK::skipped);
            self.bump(&mut node);
            self.append_node(node, parent);
        }
    }

    fn expect(&mut self, what: TK, context: ParseContext, parent: &mut PNode) {
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

/// Parse
impl Parser<'_> {
    fn parse_top_level(&mut self) -> PNode {
        let mut root = self.node(NK::root);

        while !self.at_end() {
            self.parse_decl(ParseContext::TopLevel, &mut root);
        }

        self.bump(&mut root);

        root
    }

    fn parse_path(&mut self) -> PNode {
        let mut path = self.node(NK::path);

        self.eat(TK::punct_dblColon, &mut path);
        self.expect(TK::ident, ParseContext::Path, &mut path);

        while self.at(TK::punct_dblColon) {
            self.expect(TK::punct_dblColon, ParseContext::Path, &mut path);
            self.expect(TK::ident, ParseContext::Path, &mut path);
        }

        path
    }

    fn parse_string(&mut self) -> PNode {
        assert!(self.at(TK::string_open));

        let mut str = self.node(NK::string);

        self.bump(&mut str);
        while self.eat_set(
            &[
                TK::string_literal,
                TK::esc_asciicode,
                TK::esc_unicode,
                TK::esc_asciicode,
                TK::esc_unicode,
                TK::esc_char_newline,
                TK::esc_char_return,
                TK::esc_char_tab,
                TK::esc_char_backslash,
                TK::esc_char_doublequote,
                TK::esc_char_singlequote,
                TK::esc_char_other,
            ],
            &mut str,
        ) {}
        self.expect(TK::string_close, ParseContext::String, &mut str);

        str
    }

    fn parse_literal_int(&mut self) -> PNode {
        assert!(self.at_set(&[TK::int_dec, TK::int_bin, TK::int_hex, TK::int_oct]));
        let mut lit = self.node(NK::literal_int);
        self.bump(&mut lit);
        lit
    }

    fn parse_literal_float(&mut self) -> PNode {
        assert!(self.at(TK::float));
        let mut lit = self.node(NK::literal_float);
        self.bump(&mut lit);
        lit
    }

    fn parse_block(&mut self) -> PNode {
        let mut block = self.node(NK::block);

        self.expect(TK::brkt_brace_open, ParseContext::Block, &mut block);

        while !self.at_set(&[TK::brkt_brace_close, TK::Eof]) {
            self.parse_stmt(&mut block);
        }

        self.expect(TK::brkt_brace_close, ParseContext::Block, &mut block);

        block
    }

    /// 'let's and 'const's
    fn parse_binding(&mut self, binding: &mut PNode) {
        // ident
        self.expect(TK::ident, ParseContext::DeclConst, binding);

        // ( ':' ty )?
        if self.eat(TK::punct_colon, binding) {
            let mut ty = self.node(NK::binding_ty);
            self.parse_ty(&mut ty);
            self.append_node(ty, binding);
        }

        // '=' expr
        self.expect(TK::punct_eq, ParseContext::DeclConst, binding);
        self.parse_stmt_expr(binding);
    }
}

/// Parse decl
impl Parser<'_> {
    fn parse_decl(&mut self, context: ParseContext, parent: &mut PNode) {
        if let Some(decl) = self.try_parse_decl() {
            self.append_node(decl, parent);
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

    fn try_parse_decl(&mut self) -> Option<PNode> {
        let decl = match self.peek() {
            TK::kw_fn => {
                let mut func = self.node(NK::decl_func);
                self.bump(&mut func); // kw_fn
                self.bump(&mut func); // ident

                let mut proto = self.node(NK::func_proto);
                self.parse_func_params_and_ret(ParseContext::DeclFunc, &mut proto);
                self.append_node(proto, &mut func);

                let mut body = self.node(NK::decl_func_body);

                if self.eat(TK::punct_fat_arrow, &mut body) {
                    self.parse_stmt_expr(&mut body);
                } else {
                    let block = self.parse_block();
                    self.append_node(block, &mut body);
                }

                self.append_node(body, &mut func);
                func
            }
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
}

/// Parse ty
impl Parser<'_> {
    fn parse_ty(&mut self, parent: &mut PNode) {
        match self.peek() {
            TK::ident | TK::punct_dblColon => {
                let path = self.parse_path();
                self.append_node(path, parent);
            }
            TK::brkt_square_open => {
                let mut slice_ty = self.node(NK::ty_slice);
                self.bump(&mut slice_ty); // '['
                self.expect(TK::brkt_square_close, ParseContext::TySlice, &mut slice_ty); // ']'
                self.parse_ty(&mut slice_ty); // ty
                self.append_node(slice_ty, parent);
            }
            TK::punct_question => {
                let mut nullable_ty = self.node(NK::ty_nullable);
                self.bump(&mut nullable_ty); // '?'
                self.parse_ty(&mut nullable_ty); // ty
                self.append_node(nullable_ty, parent);
            }
            TK::kw_fn => self.parse_ty_func(parent),
            _ => self.report(
                parent,
                ParseError::Expected(ParseErrorExpected {
                    what: ParseErrorExpectedWhat::Item(ParseErrorItem::Ty),
                    at: self.cursor,
                    found: self.cursor,
                    context: ParseContext::Ty,
                }),
            ),
        }
    }

    fn parse_func_params_and_ret(&mut self, ctx: ParseContext, parent: &mut PNode) {
        self.expect(TK::brkt_paren_open, ctx, parent);
        while !self.at(TK::brkt_paren_close) {
            let mut param = self.node(NK::func_proto_param);

            if self.at(TK::ident) && self.peek_n(1) == TK::punct_colon {
                self.expect(TK::ident, ctx, &mut param);
                self.expect(TK::punct_colon, ctx, &mut param);
            }

            self.parse_ty(&mut param);
            self.append_node(param, parent);

            if !self.eat(TK::punct_comma, parent) {
                break;
            }
        }
        self.expect(TK::brkt_paren_close, ctx, parent);

        if self.eat(TK::punct_colon, parent) {
            let mut ret = self.node(NK::func_proto_ret);
            self.parse_ty(&mut ret);
            self.append_node(ret, parent);
        }
    }

    fn parse_ty_func(&mut self, parent: &mut PNode) {
        assert!(self.at(TK::kw_fn));

        let mut proto = self.node(NK::func_proto);
        self.bump(&mut proto); // 'fn'

        self.parse_func_params_and_ret(ParseContext::TyFunc, &mut proto);

        self.append_node(proto, parent);
    }
}

/// Parse stmt
impl Parser<'_> {
    fn parse_stmt(&mut self, parent: &mut PNode) {
        match self.peek() {
            TK::kw_let => {
                let mut binding = self.node(NK::stmt_let);

                self.bump(&mut binding); // 'let'

                self.parse_binding(&mut binding);

                self.append_node(binding, parent);
            }
            _ => {
                if let Some(decl) = self.try_parse_decl() {
                    let mut stmt = self.node(NK::stmt_decl);
                    self.append_node(decl, &mut stmt);
                    self.append_node(stmt, parent);
                } else {
                    let mut stmt = self.node(NK::stmt_expr);
                    self.parse_stmt_expr(&mut stmt);
                    self.append_node(stmt, parent);
                }
            }
        }
    }

    /// Parse an expression in the context of a statement.
    /// ie. with a semicolon if it does not end with a '}'
    fn parse_stmt_expr(&mut self, parent: &mut PNode) {
        self.parse_expr(parent);

        if self.tokens[self.cursor - 1] != TK::brkt_brace_close {
            self.expect(TK::punct_semiColon, ParseContext::Stmt, parent);
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
#[repr(u8)]
enum Precedence {
    None = 0,
    Assignment,
    LogicalOr,
    LogicalAnd,
    Sum,
    Product,
    // Suffix, // ? ! .
    Call,
}

impl Precedence {
    fn of(kind: TK) -> Self {
        match kind {
            TK::punct_eq => Self::Assignment,
            TK::kw_or => Self::LogicalOr,
            TK::kw_and => Self::LogicalAnd,
            TK::punct_plus | TK::punct_minus => Self::Sum,
            TK::punct_star | TK::punct_slash => Self::Product,
            TK::brkt_paren_open => Self::Call,
            _ => Self::None,
        }
    }
}

/// Parse expr
impl Parser<'_> {
    fn parse_expr(&mut self, parent: &mut PNode) {
        self.parse_expr_precedence(Precedence::None, parent);
    }

    fn try_parse_expr(&mut self) -> Option<PNode> {
        self.try_parse_expr_precedence(Precedence::None)
    }

    fn parse_expr_precedence(&mut self, prec: Precedence, parent: &mut PNode) {
        if let Some(expr) = self.try_parse_expr_precedence(prec) {
            self.append_node(expr, parent);
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

    fn try_parse_expr_precedence(&mut self, prec: Precedence) -> Option<PNode> {
        let mut lhs = self.try_parse_expr_start()?;

        let init_prec = prec;
        let mut p = self.peek();
        let mut infix_prec = Precedence::of(p);

        while init_prec < infix_prec {
            if let Some(e) = self.try_parse_expr_infix(p, lhs.clone(), infix_prec) {
                lhs = e;
            } else {
                // @FIXME:...
                //  Is this an error condition?
                //  Is it even reachable?
                // return Some(lhs);
                unreachable!();
            }

            p = self.peek();
            infix_prec = Precedence::of(p);
        }

        Some(lhs)
    }

    fn try_parse_expr_start(&mut self) -> Option<PNode> {
        let expr = match self.peek() {
            TK::ident => self.parse_path(),
            TK::string_open => self.parse_string(),

            TK::int_dec | TK::int_hex | TK::int_bin | TK::int_oct => self.parse_literal_int(),
            TK::float => self.parse_literal_float(),

            TK::kw_false => self.node_bump(NK::expr_false),
            TK::kw_true => self.node_bump(NK::expr_true),

            TK::brkt_brace_open => self.parse_block(),

            TK::brkt_paren_open => todo!("parse paren exprs"),

            TK::kw_return => {
                let mut ret = self.node(NK::expr_return);
                self.bump(&mut ret); // 'return'
                if let Some(expr) = self.try_parse_expr() {
                    self.append_node(expr, &mut ret);
                }
                ret
            }

            _ => return None,
        };
        Some(expr)
    }

    fn try_parse_expr_infix(&mut self, p: TK, lhs: PNode, prec: Precedence) -> Option<PNode> {
        let node = match p {
            TK::punct_eq | TK::punct_plus | TK::punct_minus | TK::punct_star | TK::punct_slash => {
                let mut infix = self.node(NK::expr_infix);

                // lhs
                self.append_node(lhs, &mut infix);

                // op
                {
                    let mut op = self.node(NK::expr_infix_op);
                    // @TODO: parse multiple operators
                    self.bump(&mut op);
                    self.append_node(op, &mut infix);
                }

                // rhs
                self.parse_expr_precedence(prec, &mut infix);

                infix
            }
            TK::brkt_paren_open => {
                let mut call = self.node(NK::expr_call);

                // callee
                self.append_node(lhs, &mut call);

                self.bump(&mut call); // '('

                // args
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
