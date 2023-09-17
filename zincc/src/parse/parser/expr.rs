use std::ops::Range;

use super::{decl::parse_decl, parse::parse_path, ParseNode, Parser, Report};
use crate::{
    parse::{
        cst::{NodeId, NodeKind},
        TokenKind,
    },
    report::Label,
    TK,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
enum Precedence {
    None = 0,
    // SetLet, // Assignment or binding
    // LogicalOr,  // ||
    // LogicalAnd, // &&
    Equality, // == !=
    Order,    // < <= > >=
    Sum,      // + -
    Product,  // * /
    // CastPrefix, // as not !
    // Index, // []
    Suffix, // ?
    Field,  // .
    Call,   // ()
}

impl Precedence {
    fn of(kind: TokenKind) -> Option<Self> {
        let prec = match kind {
            TK![==] | TK![!=] => Self::Equality,

            TK![>] | TK![>=] | TK![<] | TK![<=] => Self::Order,

            TK![+] | TK![-] => Self::Sum,
            TK![*] | TK![/] => Self::Product,

            TK![?] => Self::Suffix,
            TK![.] => Self::Field,
            TK!['('] => Self::Call,

            _ => return None,
        };
        Some(prec)
    }

    fn associativity(self) -> Associativity {
        match self {
            Self::None => Associativity::None,
            Self::Equality
            | Self::Sum
            | Self::Product
            | Self::Order
            | Self::Call
            | Self::Suffix => Associativity::Left,
            Self::Field => Associativity::Right,
        }
    }

    fn binding_power(self) -> (u8, u8) {
        let associativity = self.associativity();
        let int = self as u8 * 2;
        match associativity {
            Associativity::None => (int, int),
            Associativity::Left => (int, int + 1),
            Associativity::Right => (int, int - 1),
        }
    }
}

#[derive(Debug, Clone, Copy)]
enum Associativity {
    None,
    Left,
    Right,
}

#[inline]
pub fn parse_expr(p: &mut Parser, parent: NodeId) {
    parse_expr_binding_power(p, 0, parent);
}

#[inline]
fn try_parse_expr<'p>(p: &mut Parser<'p>) -> Option<ParseNode<'p>> {
    try_parse_expr_binding_power(p, 0)
}

fn parse_expr_binding_power(p: &mut Parser, min_binding_power: u8, parent: NodeId) {
    if let Some(expr) = try_parse_expr_binding_power(p, min_binding_power) {
        let _ = expr.parent(parent);
    } else {
        let range = p.prev_token_range().clone();
        p.report(|r| report_expr_error(r, range.clone()));
    }
}

fn try_parse_expr_binding_power<'p>(
    p: &mut Parser<'p>,
    min_binding_power: u8,
) -> Option<ParseNode<'p>> {
    let mut lhs = try_parse_expr_single(p)?;

    loop {
        let (peeked, skipped_newlines, infix_prec) = get_precedence(p);
        let (left_binding_power, right_binding_power) = infix_prec.binding_power();

        if left_binding_power < min_binding_power {
            break;
        }

        if let Some((node, stop)) =
            try_parse_expr_compound(p, peeked, skipped_newlines, lhs, right_binding_power)
        {
            lhs = node;
            if stop {
                break;
            }
        } else {
            // @NOTE: If we reach this then there is an inconsistency with how this function
            //        determines how and when to continue parsing.
            unreachable!("BUG: Middle of expression failed to parse when precedence allowed it");
        }
    }

    // todo!()
    Some(lhs)
}

fn get_precedence(p: &mut Parser) -> (TokenKind, bool, Precedence) {
    let mut peeked = p.peek();
    let mut skipped_newlines = false;
    let prec = Precedence::of(peeked).unwrap_or_else(|| {
        peeked = p.peek_skipping_newlines();
        skipped_newlines = true;
        Precedence::of(peeked).unwrap_or(Precedence::None)
    });
    (peeked, skipped_newlines, prec)
}

fn try_parse_expr_single<'p>(p: &mut Parser<'p>) -> Option<ParseNode<'p>> {
    let peek = p.peek();
    let node = match peek {
        TK![ident] => parse_path(p),

        TK![int dec] | TK![int hex] | TK![int oct] | TK![int bin] => {
            parse_basic_literal(p, NodeKind::lit_integer)
        }
        TK![float] => parse_basic_literal(p, NodeKind::lit_float),
        TK![true] | TK![false] => parse_basic_literal(p, NodeKind::lit_boolean),

        TK![string open] => {
            let str = p.node(NodeKind::lit_string);

            str.expect(TK![string open]);

            while p.at_one_of(&[
                TK![string literal],
                TK![esc asciicode],
                TK![esc unicode],
                TK![esc char newline],
                TK![esc char return],
                TK![esc char tab],
                TK![esc char backslash],
                TK![esc char doubleQuote],
                TK![esc char singleQuote],
                TK![esc char other],
            ]) {
                str.eat_current();
            }

            str.expect(TK![string close]);

            str
        }

        TK!['{'] => parse_expr_block(p),

        TK!['('] => {
            let group = p.node(NodeKind::expr_grouping);

            group.expect(TK!['(']);
            parse_expr(p, *group);
            group.expect(TK![')']);

            group
        }

        TK![!] | TK![-] | TK![&] => {
            let prefix = p.node(NodeKind::expr_prefix);

            let op = p.node(NodeKind::expr_prefix_op).parent(*prefix);
            op.eat_current();
            op.eat_newlines();
            drop(op);

            parse_expr(p, *prefix);

            prefix
        }

        _ => return None,
    };

    fn parse_basic_literal<'p>(p: &mut Parser<'p>, kind: NodeKind) -> ParseNode<'p> {
        let lit = p.node(kind);
        lit.eat_current();
        lit
    }

    Some(node)
}

fn try_parse_expr_compound<'p>(
    p: &mut Parser<'p>,
    peeked: TokenKind,
    skipped_newlines: bool,
    lhs: ParseNode<'p>,
    right_binding_power: u8,
) -> Option<(ParseNode<'p>, bool)> {
    let node = match peeked {
        TK![+]
        | TK![-]
        | TK![*]
        | TK![/]
        | TK![==]
        | TK![!=]
        | TK![>]
        | TK![>=]
        | TK![<]
        | TK![<=]
            if !skipped_newlines =>
        {
            let infix = p.node(NodeKind::expr_infix);

            // lhs
            let _ = lhs.parent(*infix);

            // op
            let op = p.node(NodeKind::expr_infix_op).parent(*infix);
            op.eat_current();
            op.eat_newlines();
            drop(op);

            // rhs
            parse_expr_binding_power(p, right_binding_power, *infix);

            infix
        }

        TK!['('] if !skipped_newlines => {
            let call = p.node(NodeKind::expr_call);

            let _ = lhs.parent(*call);

            call.expect(TK!['(']);
            call.eat_newlines();

            if !p.at(TK![')']) {
                let args = p.node(NodeKind::expr_call_args).parent(*call);

                while !p.at(TK![')']) {
                    if p.at(TK![ident]) && p.at_next(TK![=]) {
                        let named = p.node(NodeKind::expr_call_arg_named).parent(*args);

                        named.expect(TK![ident]);
                        named.expect(TK![=]);
                        named.eat_newlines();

                        parse_expr(p, *named);
                    } else {
                        parse_expr(p, *args);
                    }

                    if args.eat(TK![,]) || args.eat(TK![newline]) {
                        args.eat_newlines();
                    } else {
                        break;
                    }
                }
            }

            call.expect(TK![')']);

            call
        }

        TK![.] => {
            let field = p.node(NodeKind::expr_field);

            // lhs
            let _ = lhs.parent(*field);

            // '.'
            field.eat_newlines();
            field.eat_current();
            field.eat_newlines();

            // rhs
            parse_expr_binding_power(p, right_binding_power, *field);

            field
        }

        _ if !skipped_newlines => return None,

        _ => return Some((lhs, true)),
    };

    Some((node, false))
}

pub fn parse_expr_block<'p>(p: &mut Parser<'p>) -> ParseNode<'p> {
    let block = p.node(NodeKind::expr_block);

    block.expect(TK!['{']);
    block.eat_newlines();

    while !p.at(TK!['}']) {
        if p.at(TK![::]) {
            parse_decl(p, *block);
        } else if let Some(expr) = try_parse_expr(p) {
            if p.at_skipping_newlines(TK!['}']) {
                let end = p.node(NodeKind::expr_block_end).parent(*block);
                let _ = expr.parent(*end);
                end.eat_newlines();
                break;
            }
            let _ = expr.parent(*block);

            if block.eat(TK![;]) || block.eat(TK![newline]) {
                block.eat_newlines();
            } else {
                break;
            }
        } else {
            let range = p.prev_token_range().clone();
            p.report(|r| report_expr_error(r, range.clone()));
        }
    }

    block.expect(TK!['}']);

    block
}

fn report_expr_error(r: Report, range: Range<usize>) -> Report {
    r.error()
        .message("expected expression")
        .label(Label::new().range(range).message("after here"))
}
