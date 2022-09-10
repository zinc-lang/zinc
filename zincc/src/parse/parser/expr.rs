use super::*;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
#[repr(u8)]
enum Precedence {
    None = 0,
    SetLet, // Assignment or binding
    // LogicalOr,  // || or
    // LogicalAnd, // && and
    Equality, // == !=
    // Order,      // < <= > >=
    Sum,        // + -
    Product,    // * /
    CastPrefix, // as not !
    Suffix,     // () ? .
}

impl Precedence {
    fn of(kind: TK) -> Self {
        match kind {
            // TK::kw_or => Self::LogicalOr,
            // TK::kw_and => Self::LogicalAnd,

            //
            TK::punct_eqEq | TK::punct_bangEq => Self::Equality,

            TK::punct_plus | TK::punct_minus => Self::Sum,
            TK::punct_star | TK::punct_slash => Self::Product,
            // TK::brkt_paren_open => Self::Call,

            //
            TK::brkt_paren_open | TK::punct_question | TK::punct_dot => Self::Suffix,
            _ => Self::None,
        }
    }
}

/// Parse expr
impl Parser<'_> {
    pub fn parse_expr(&mut self, parent: NodeId) {
        self.parse_expr_precedence(Precedence::None, parent);
    }

    fn try_parse_expr(&mut self) -> Option<PNode> {
        self.try_parse_expr_precedence(Precedence::None)
    }

    fn parse_expr_precedence(&mut self, prec: Precedence, parent: NodeId) {
        if let Some(mut expr) = self.try_parse_expr_precedence(prec) {
            expr.parent = Some(parent)
        } else {
            self.report(report_expr_error)
        }
    }

    fn try_parse_expr_precedence(&mut self, prec: Precedence) -> Option<PNode> {
        let mut lhs = self.try_parse_expr_start()?;

        let init_prec = prec;
        let mut p = self.peek();
        let mut infix_prec = Precedence::of(p);

        while init_prec < infix_prec {
            if let Some(e) = self.try_parse_expr_middle(p, lhs, infix_prec) {
                lhs = e;
            } else {
                // @NOTE:...
                // If we reach this then there is an inconsistency with how this function determines
                // how and when to continue parsing.
                unreachable!("middle of expression failed to parse when precedence allowed it");
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

            TK::int_dec | TK::int_hex | TK::int_bin | TK::int_oct => self.parse_int(),
            TK::float => self.parse_float(),

            TK::kw_false | TK::kw_true => self.parse_bool(),

            TK::brkt_brace_open => self.parse_expr_block(),

            // @TODO
            TK::brkt_paren_open => {
                self.report_unimpl(|| Report::builder().message("paren expression"))
            }

            // @TODO
            TK::kw_return => self.report_unimpl(|| Report::builder().message("return expression")),

            // @TODO
            TK::kw_if => self.report_unimpl(|| Report::builder().message("if expression")),

            // @TODO
            TK::kw_fn => self.report_unimpl(|| Report::builder().message("fn expression")),

            TK::kw_let => {
                let mut bind = self.pnode(NK::expr_let_basic);

                bind.push_token(); // 'let'

                let pattern_ident = self.parse_pattern(*bind);
                if !pattern_ident {
                    bind.kind = NK::expr_let_pattern;
                }

                if !bind.at(TK::punct_eq) {
                    // ty?
                    let ty = self.pnode(NK::expr_let_ty).parent(*bind);
                    self.parse_ty(*ty);
                }

                bind.expect(TK::punct_eq); // '='

                self.parse_expr_precedence(Precedence::SetLet, *bind);

                bind
            }

            TK::kw_set => {
                let mut set = self.pnode(NK::expr_set);

                set.push_token(); // 'set'
                self.parse_expr(*set); // expr
                set.expect(TK::punct_eq); // '='
                self.parse_expr(*set); // expr

                set
            }

            TK::punct_minus | TK::punct_bang | TK::punct_ampersand => {
                let prefix = self.pnode(NK::expr_prefix);

                {
                    let mut op = self.pnode(NK::expr_prefix_op).parent(*prefix);
                    op.push_token();
                }

                self.parse_expr_precedence(Precedence::CastPrefix, *prefix);

                prefix
            }

            _ => return None,
        };
        Some(expr)
    }

    fn try_parse_expr_middle(&mut self, p: TK, mut lhs: PNode, prec: Precedence) -> Option<PNode> {
        let node = match p {
            TK::punct_plus
            | TK::punct_minus
            | TK::punct_star
            | TK::punct_slash
            | TK::punct_eqEq
            | TK::punct_bangEq => {
                let infix = self.pnode(NK::expr_infix);

                // lhs
                lhs.parent = Some(*infix);
                drop(lhs);

                // op
                {
                    let mut op = self.pnode(NK::expr_infix_op).parent(*infix);
                    // @TODO: parse multiple operators
                    op.push_token();
                }

                // rhs
                self.parse_expr_precedence(prec, *infix);

                infix
            }
            TK::brkt_paren_open => {
                let mut call = self.pnode(NK::expr_call);

                // callee
                lhs.parent = Some(*call);
                drop(lhs);

                call.push_token(); // '('

                // args
                while !call.at(TK::brkt_paren_close) {
                    self.parse_expr(*call);
                    if !call.eat(TK::punct_comma) {
                        break;
                    }
                }

                call.expect(TK::brkt_paren_close);

                call
            }
            _ => return None,
        };
        Some(node)
    }

    pub fn parse_expr_block(&mut self) -> PNode {
        let mut block = self.pnode(NK::expr_block);

        block.expect(TK::brkt_brace_open);

        while !block.at(TK::brkt_brace_close) {
            if block.at(TK::punct_doubleColon) {
                self.parse_decl(*block);
            } else if let Some(mut expr) = self.try_parse_expr() {
                if block.at(TK::brkt_brace_close) {
                    let end = self.pnode(NK::expr_block_end).parent(*block);
                    expr.parent = Some(*end);
                    break;
                } else {
                    expr.parent = Some(*block);
                    drop(expr);
                    if self.tokens[self.cursor - 1] != TK::brkt_brace_close {
                        block.expect(TK::punct_semicolon);
                    } else {
                        let _ = block.eat(TK::punct_semicolon);
                    }
                }
            } else {
                self.report(report_expr_error)
            }
        }

        block.expect(TK::brkt_brace_close);

        block
    }
}

fn report_expr_error() -> report::Builder {
    Report::builder().error().message("expected expression")
}
