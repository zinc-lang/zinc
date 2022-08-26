use crate::{
    parse::{self, cst, TokenKind},
    util::AutoIndentingWriter,
};
use std::io::{self, Write};
use termcolor::{Color, ColorSpec, WriteColor};

pub fn write_token<W: Write>(
    out: &mut W,
    source: &str,
    tk: parse::TK,
    range: &std::ops::Range<usize>,
    use_color: bool,
) -> io::Result<()> {
    let slice = &source[range.start as usize..range.end as usize];
    let slice = unescape_string(slice);
    if use_color {
        let mut ansi = termcolor::Ansi::new(out);

        ansi.set_color(ColorSpec::new().set_fg(Some(Color::Cyan)))?;
        write!(ansi, "{tk:?}")?;
        ansi.reset()?;
        write!(ansi, "@")?;
        ansi.set_color(ColorSpec::new().set_fg(Some(Color::White)))?;
        write!(ansi, "{range:?}")?;
        ansi.reset()?;
        write!(ansi, "  ")?;
        ansi.set_color(ColorSpec::new().set_fg(Some(Color::Green)))?;
        write!(ansi, "'{slice}'")?;
        ansi.reset()?;
    } else {
        write!(out, "{tk:?}@{range:?}  '{slice}'")?;
    }

    Ok(())
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

pub fn write_cst<W: Write>(
    writer: &mut W,
    cst: &cst::Cst,
    source: &str,
    tokens: &[TokenKind],
    ranges: &[std::ops::Range<usize>],
    use_color: bool,
) -> io::Result<()> {
    CstWriter::new(writer, cst, source, tokens, ranges, use_color).print(cst.root())
}

pub struct CstWriter<'s, W: Write> {
    f: AutoIndentingWriter<'s, W>,
    cst: &'s cst::Cst,
    source: &'s str,
    tokens: &'s [TokenKind],
    ranges: &'s [std::ops::Range<usize>],
    use_color: bool,
}

impl<'s, W: Write> CstWriter<'s, W> {
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
                    let tk = *self.tokens.get(i.get()).unwrap();
                    if tk.is_trivia() {
                        continue;
                    }
                    let range = self.ranges.get(i.get()).unwrap();
                    write_token(&mut self.f, self.source, tk, range, self.use_color)?;
                    writeln!(self.f)?;
                }
                cst::Element::Node(n) => {
                    if self.use_color {
                        let mut ansi = termcolor::Ansi::new(&mut self.f);

                        ansi.set_color(ColorSpec::new().set_fg(Some(Color::Magenta)))?;
                        write!(ansi, "{:?}", n.kind)?;
                        ansi.reset()?;
                        writeln!(self.f)?;
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
