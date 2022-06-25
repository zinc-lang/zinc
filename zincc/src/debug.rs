use crate::{
    parse::{self, cst, TokenKind},
    util::AutoIndentingWriter,
};
use std::{
    fmt,
    io::{self, Write},
};

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

pub fn format_token(
    source: &str,
    tk: parse::TK,
    range: &std::ops::Range<usize>,
    use_color: bool,
) -> String {
    let slice = &source[range.start as usize..range.end as usize];
    let slice = unescape_string(slice);
    if use_color {
        format!(
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
        )
    } else {
        format!("{:?}@{:?}  '{}'", tk, range, slice)
    }
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

pub fn print_cst<W: Write>(
    writer: &mut W,
    cst: &cst::Cst,
    source: &str,
    tokens: &[TokenKind],
    ranges: &[std::ops::Range<usize>],
    use_color: bool,
) -> io::Result<()> {
    CstPrinter::new(writer, cst, source, tokens, ranges, use_color).print(cst.root())
}

pub struct CstPrinter<'s, W: Write> {
    f: AutoIndentingWriter<'s, W>,
    cst: &'s cst::Cst,
    source: &'s str,
    tokens: &'s [TokenKind],
    ranges: &'s [std::ops::Range<usize>],
    use_color: bool,
}

impl<'s, W: Write> CstPrinter<'s, W> {
    pub fn new(
        writer: &'s mut W,
        cst: &'s cst::Cst,
        source: &'s str,
        tokens: &'s [TokenKind],
        ranges: &'s [std::ops::Range<usize>],
        use_color: bool,
    ) -> Self {
        Self {
            f: AutoIndentingWriter::new(writer, 2),
            cst,
            source,
            tokens,
            ranges,
            use_color,
        }
    }

    pub fn print(&mut self, node: &cst::Node) -> io::Result<()> {
        for elem in node.elements.iter() {
            match elem {
                cst::Element::Token(i) => {
                    let tk = *self.tokens.get(*i as usize).unwrap();
                    let range = self.ranges.get(*i as usize).unwrap();
                    writeln!(
                        self.f,
                        "{}",
                        format_token(self.source, tk, range, self.use_color)
                    )?;
                }
                cst::Element::Node(n) => {
                    if self.use_color {
                        writeln!(
                            self.f,
                            "{}{:?}{}",
                            TerminalColor::MagentaDark,
                            n.kind,
                            TerminalColor::Reset
                        )?;
                    } else {
                        writeln!(self.f, "{:?}", n.kind)?;
                    }
                    self.f.push_indent();
                    self.print(self.cst.get(*n))?;
                    self.f.pop_indent();
                }
            }
        }
        self.f.flush()
    }
}
