use crate::parse::{self, cst, TokenKind};
use std::fmt;

#[derive(Debug, Clone, Copy)]
pub enum TerminalColor {
    BlackDark,
    BlackLight,
    RedDark,
    RedLight,
    GreenDark,
    GreenLight,
    YellowDark,
    YellowLight,
    BlueDark,
    BlueLight,
    MagentaDark,
    MagentaLight,
    CyanDark,
    CyanLight,
    WhiteDark,
    WhiteLight,
    Reset,
}

impl fmt::Display for TerminalColor {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            TerminalColor::BlackDark => "\u{001b}[30m",
            TerminalColor::BlackLight => "\u{001b}[30;1m",
            TerminalColor::RedDark => "\u{001b}[31m",
            TerminalColor::RedLight => "\u{001b}[31;1m",
            TerminalColor::GreenDark => "\u{001b}[32m",
            TerminalColor::GreenLight => "\u{001b}[32;1m",
            TerminalColor::YellowDark => "\u{001b}[33m",
            TerminalColor::YellowLight => "\u{001b}[33;1m",
            TerminalColor::BlueDark => "\u{001b}[34m",
            TerminalColor::BlueLight => "\u{001b}[34;1m",
            TerminalColor::MagentaDark => "\u{001b}[35m",
            TerminalColor::MagentaLight => "\u{001b}[34;1m",
            TerminalColor::CyanDark => "\u{001b}[36m",
            TerminalColor::CyanLight => "\u{001b}[36;1m",
            TerminalColor::WhiteDark => "\u{001b}[37m",
            TerminalColor::WhiteLight => "\u{001b}[37;1m",
            TerminalColor::Reset => "\u{001b}[0m",
        })
    }
}

pub fn print_token(source: &str, tk: parse::TK, range: std::ops::Range<u32>) {
    let slice = &source[range.start as usize..range.end as usize];
    let slice = unescape_string(slice);
    println!(
        "{}{:?}{}@{}{:?}{}  {}'{}'{}",
        TerminalColor::CyanDark,
        tk,
        TerminalColor::Reset,
        TerminalColor::WhiteLight,
        range,
        TerminalColor::Reset,
        TerminalColor::GreenDark,
        slice,
        TerminalColor::Reset,
    );
}

pub fn unescape_string(str: &str) -> String {
    str.chars()
        .map(|ch| {
            let mut _tmp = [0; 4];
            match ch {
                '\n' => "\\n",
                _ => ch.encode_utf8(&mut _tmp),
            }
            .to_string()
        })
        .collect::<String>()
}

pub fn print_cst(
    source: &str,
    node: cst::Node,
    tokens: &[TokenKind],
    ranges: &[std::ops::Range<u32>],
    indent: usize,
) {
    for child in node.children {
        for _ in 0..indent {
            print!("  ");
        }
        match child {
            cst::Element::Node(node) => {
                println!(
                    "{}{:?}{}",
                    TerminalColor::MagentaDark,
                    node.kind,
                    TerminalColor::Reset
                );
                print_cst(source, node, tokens, ranges, indent + 1);
            }
            cst::Element::Token(tok) => {
                // let tk = tokens[tok];
                let tk = *tokens.get(tok).unwrap_or_else(|| &TokenKind::EOF);
                // let range = ranges[tok].clone();
                let range = ranges.get(tok).unwrap_or_else(|| &(0..0)).clone();
                print_token(source, tk, range);
            }
        }
    }
}
