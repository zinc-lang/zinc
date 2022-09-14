use crate::{
    source_map::{self, SourceFileId},
    util,
};
use std::{
    io,
    ops::{Deref, Range},
};

type Span = Range<usize>;

#[derive(Debug)]
pub struct Report {
    pub level: Level,
    pub file: SourceFileId,
    pub span: Span,
    /// Main message, no context
    pub message: String,
    /// Short message, displayed in context
    pub short: Option<String>,
}

impl Report {
    #[track_caller]
    pub fn builder() -> Builder {
        Builder::new()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Level {
    Error,
    // Warning,
    Unimpl,
}

impl Level {
    pub fn get(&self) -> (colored::Color, &'static str) {
        match self {
            Level::Error => (colored::Color::BrightRed, "error"),
            Level::Unimpl => (colored::Color::BrightCyan, "unimpl"),
        }
    }
}

#[derive(Debug, Default)]
pub struct Builder {
    level: Option<Level>,
    file: Option<SourceFileId>,
    span: Option<Span>,
    message: Option<String>,
    short: Option<String>,

    #[cfg(debug_assertions)]
    created_at: LocationWrapper,
}

type StaticLocationRef = &'static std::panic::Location<'static>;

#[derive(Debug)]
struct LocationWrapper(StaticLocationRef);

impl Default for LocationWrapper {
    #[track_caller]
    fn default() -> Self {
        Self(std::panic::Location::caller())
    }
}

impl Deref for LocationWrapper {
    type Target = StaticLocationRef;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl Builder {
    #[track_caller]
    pub fn new() -> Self {
        Self::default()
    }

    pub fn error(mut self) -> Self {
        debug_assert!(self.level.is_none());
        self.level = Some(Level::Error);
        self
    }

    pub fn unimpl(mut self) -> Self {
        debug_assert!(self.level.is_none());
        self.level = Some(Level::Unimpl);
        self
    }

    pub fn file(mut self, file: SourceFileId) -> Self {
        self.file = Some(file);
        self
    }

    pub fn maybe_set_file(mut self, set_file: impl FnOnce() -> SourceFileId) -> Self {
        if self.file.is_none() {
            self.file = Some(set_file())
        }
        self
    }

    pub fn span(mut self, span: Span) -> Self {
        self.span = Some(span);
        self
    }

    pub fn maybe_set_span(mut self, set_span: impl FnOnce() -> Span) -> Self {
        if self.span.is_none() {
            self.span = Some(set_span());
        }
        self
    }

    pub fn message(mut self, message: impl Into<String>) -> Self {
        self.message = Some(message.into());
        self
    }

    pub fn short(mut self, short: impl Into<String>) -> Self {
        self.short = Some(short.into());
        self
    }

    #[track_caller]
    pub fn build(self) -> Report {
        macro_rules! get {
            ($what:ident) => {
                self.$what.unwrap_or_else(|| {
                    #[cfg(debug_assertions)]
                    panic!("{} not set: {:?}", stringify!(what), *self.created_at);
                    #[cfg(not(debug_assertions))]
                    panic!();
                })
            };
        }

        Report {
            level: get!(level),
            file: get!(file),
            span: get!(span),
            message: get!(message),
            short: self.short,
        }
    }
}

/// # Returns
/// `(errors, unimpls)`
pub fn split(reports: Vec<Report>) -> (Vec<Report>, Vec<Report>) {
    let mut errors = Vec::new();
    let mut unimpls = Vec::new();

    for report in reports.into_iter() {
        match report.level {
            Level::Error => errors.push(report),
            Level::Unimpl => unimpls.push(report),
        }
    }

    (errors, unimpls)
}

pub fn format_report<W: io::Write>(
    f: &mut W,
    report: &Report,
    source_map: &source_map::SourceMap,
) -> io::Result<()> {
    use colored::Colorize;

    let (highlight_color, level_str) = report.level.get();

    writeln!(
        f,
        "{}: {}",
        level_str.color(highlight_color).bold(),
        report.message.bold()
    )?;

    let source = &source_map.sources[&report.file];
    let location = util::FileLocation::from_range(source, &report.span).unwrap();

    let largest_line = location.start.line.max(location.end.line);
    let padding_length = ((largest_line as f64).log10().floor() + 1.0) as usize;
    let padding = format!("{:width$}", ' ', width = padding_length);

    let file_path = source_map.files[report.file].path_relative();
    writeln!(
        f,
        "{} {} {}:{}:{}",
        padding,
        "@".bright_blue().bold(),
        file_path.display(),
        location.start.line,
        location.end.column
    )?;

    writeln!(f, "{} {}", padding, "|".bright_blue().bold())?;

    if location.start.line == location.end.line || location.start.line + 1 == location.end.line {
        let line_offsets = &source_map.lex_data[&report.file].line_offsets;
        let line_offset_index = line_offsets
            .iter()
            .position(|&offset| offset >= report.span.start)
            .unwrap();
        let offending_line_range =
            line_offsets[line_offset_index - 1]..line_offsets[line_offset_index] - 1;

        write!(
            f,
            "{} ",
            format!("{:width$} |", location.start.line, width = padding_length)
                .bright_blue()
                .bold(),
        )?;

        let span_start = &source[offending_line_range.start..report.span.start];
        let span_mid = &source[report.span.clone()];
        let span_end = &source[report.span.end..offending_line_range.end];

        writeln!(
            f,
            "{}{}{}",
            span_start,
            span_mid.color(highlight_color),
            span_end
        )?;

        if let Some(short) = &report.short {
            let highlight_start_offset = report.span.end - offending_line_range.start - 1;
            writeln!(
                f,
                "{} {} {:width$}{} {}",
                padding,
                "|".bright_blue().bold(),
                ' ',
                "^".color(highlight_color),
                short.color(highlight_color).bold(),
                width = highlight_start_offset,
            )?;
        } else {
            writeln!(f, "{} {}", padding, "|".bright_blue().bold())?;
        }
    } else {
        todo!()
    }

    writeln!(f)?;

    Ok(())
}
