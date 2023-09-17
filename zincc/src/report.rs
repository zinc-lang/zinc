#![allow(dead_code)]

use crate::source_map::{SourceFileId, SourceMap};
use colored::{Color, Colorize};
use itertools::Itertools;
use std::{io, ops::Range};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Level {
    Error,
    // Warning,
    Unimpl,
    // Note,
}

// impl Level {
//     pub fn get(&self) -> (colored::Color, &'static str) {
//         match self {
//             Level::Error => (colored::Color::BrightRed, "error"),
//             Level::Unimpl => (colored::Color::BrightCyan, "unimpl"),
//             Level::Note => (colored::Color::BrightGreen, "note"),
//         }
//     }

//     pub fn is_fatal(&self) -> bool {
//         matches!(self, Self::Error | Self::Unimpl)
//     }
// }
//

#[derive(Debug)]
pub struct Report {
    level: Option<Level>,
    file: Option<SourceFileId>,
    offset: Option<usize>,
    message: Option<String>,
    // @TODO: Note and help should be able to have their own code windows
    note: Option<String>,
    help: Option<String>,
    labels: Vec<Label>,
}

impl Report {
    pub fn new() -> Self {
        Self {
            level: None,
            file: None,
            offset: None,
            message: None,
            note: None,
            help: None,
            labels: Vec::new(),
        }
    }

    pub fn level(mut self, level: Level) -> Self {
        self.level = Some(level);
        self
    }

    #[track_caller]
    pub fn get_level(&self) -> Level {
        self.level.expect("report should have label")
    }

    pub fn error(self) -> Self {
        self.level(Level::Error)
    }

    pub fn unimpl(self) -> Self {
        self.level(Level::Unimpl)
    }

    pub fn file(mut self, id: SourceFileId) -> Self {
        self.file = Some(id);
        self
    }

    pub fn offset(mut self, offset: usize) -> Self {
        self.offset = Some(offset);
        self
    }

    pub fn message(mut self, message: impl ToString) -> Self {
        self.message = Some(message.to_string());
        self
    }

    pub fn note(mut self, note: impl ToString) -> Self {
        self.note = Some(note.to_string());
        self
    }

    pub fn help(mut self, help: impl ToString) -> Self {
        self.help = Some(help.to_string());
        self
    }

    pub fn label(mut self, label: Label) -> Self {
        self.labels.push(label);
        self
    }
}

impl Default for Report {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug)]
pub struct Label {
    range: Option<Range<usize>>,
    color: Option<Color>,
    message: Option<String>,
}

impl Label {
    pub fn new() -> Self {
        Self {
            range: None,
            color: None,
            message: None,
        }
    }

    pub fn range(mut self, range: Range<usize>) -> Self {
        self.range = Some(range);
        self
    }

    pub fn color(mut self, color: Color) -> Self {
        self.color = Some(color);
        self
    }

    pub fn message(mut self, message: impl ToString) -> Self {
        self.message = Some(message.to_string());
        self
    }
}

impl Default for Label {
    fn default() -> Self {
        Self::new()
    }
}

pub fn write_report<W: io::Write>(
    f: &mut W,
    report: &Report,
    source_map: &SourceMap,
) -> io::Result<()> {
    let level = report.level.unwrap_or_else(|| {
        error!("Fatal Bug: Report has no level at time of formatting");
        panic!("Report has no level at time of formatting");
    });

    let message = report.message.clone().unwrap_or_else(|| {
        error!("Fatal Bug: Report has no message at time of formatting");
        panic!("Report has no message at time of formatting");
    });

    let primary_color = match level {
        Level::Error => Color::BrightRed,
        Level::Unimpl => Color::Green,
    };

    let level_str = match level {
        Level::Error => "error",
        Level::Unimpl => "unimpl",
    };

    writeln!(
        f,
        "{}: {}",
        level_str.color(primary_color).bold(),
        message.bold()
    )?;

    let file_id = report.file.unwrap_or_else(|| {
        error!("Fatal Bug: Report has no file at time of formatting");
        panic!("Report has no file at time of formatting");
    });

    let file = &source_map[file_id];
    let path = file.path_relative();

    let offset = report.offset.unwrap_or_else(|| {
        error!("Fatal Bug: Report has no offset at time of formatting");
        panic!("Report has no offset at time of formatting");
    });

    // let file_location = util::FileLocation::from_offset(source, offset)

    // let line_offset_index = line_offsets
    //     .iter()
    //     .position(|&line_offset| line_offset >= offset)
    //     .unwrap();
    // let offending_line_range =
    //     line_offsets[line_offset_index - 1]..line_offsets[line_offset_index] - 1;

    // let line_number = line_offset_index + 1;

    // let line_numbers = report.labels.iter().map(|l| {
    // line_offsets
    //     .iter()
    //     .position(|&line_offset| line_offset >= offset)
    //     .unwrap();
    // });

    let mut offsets = report
        .labels
        .iter()
        .map(|l| l.range.clone().unwrap())
        .flat_map(|r| [r.start, r.end])
        .collect_vec();
    offsets.push(offset);
    let offsets = offsets //
        .iter()
        .sorted()
        .dedup();

    let line_offsets = file.line_offsets();

    let largest_line = offsets
        .map(|offset| {
            line_offsets
                .iter()
                .position(|line_offset| line_offset >= offset)
                .unwrap()
                + 1
        })
        .dedup()
        .max()
        .unwrap();

    let padding_length = ((largest_line as f64).log10().floor() + 1.0) as usize;
    let padding = format!("{:width$}", ' ', width = padding_length);

    writeln!(
        f,
        "{} {} {}",
        padding,
        "@".bright_blue().bold(),
        path.display(),
    )?;

    writeln!(f, "{} {}", padding, "│".bright_blue().bold())?;

    let source = file.source();

    for label in report.labels.iter() {
        let range = label.range.as_ref().unwrap();
        let message = label.message.as_ref().unwrap();
        let color = label.color.unwrap_or(primary_color);

        let start_line_offset_index = line_offsets
            .iter()
            .position(|&offset| offset >= range.start)
            .unwrap();
        let end_line_offset_index = line_offsets
            .iter()
            .position(|&offset| offset >= range.end)
            .unwrap();

        if start_line_offset_index == end_line_offset_index {
            let line_number = start_line_offset_index + 1;
            let offending_line_range = line_offsets[start_line_offset_index - 1]
                ..line_offsets[start_line_offset_index] - 1;

            // let line = &source[offending_line_range];

            // writeln!(
            //     f,
            //     "{} {}",
            //     format!("{} {}", line_number, "|",).bright_blue().bold(),
            //     line
            // )?;
            write!(
                f,
                "{}",
                format!("{} {}", line_number, "│",).bright_blue().bold(),
            )?;

            let line_start = &source[offending_line_range.start..range.start];
            let line_mid = &source[range.clone()];
            let line_end = &source[range.end..offending_line_range.end];

            writeln!(f, "{}{}{}", line_start, line_mid.color(color), line_end)?;

            write!(f, "{} {}", padding, "·".bright_blue().bold())?;
            let width = range.end - offending_line_range.start - 1;
            writeln!(f, "{:width$}{}{}", ' ', "─".color(color), "┐".color(color))?;

            write!(f, "{} {}", padding, "·".bright_blue().bold())?;
            writeln!(
                f,
                "{:width$} {}",
                ' ',
                format!("└─ {}", message).color(color)
            )?;
        } else {
            todo!()
        }
    }

    writeln!(f, "{} {}", padding, "│".bright_blue().bold())?;
    // writeln!(f, "{} {}", padding, "┘".bright_blue().bold())?;

    Ok(())
}

// type StaticLocationRef = &'static std::panic::Location<'static>;

// #[derive(Debug)]
// pub struct Report {
//     /// level of report.
//     /// Must be set.
//     level: Option<Level>,
//     /// File for report.
//     /// Must be set.
//     file: Option<SourceFileId>,
//     /// Span for area of concern.
//     /// Must be set.
//     span: Option<Span>,
//     /// Main message, no context
//     /// Must be set.
//     message: Option<String>,
//     /// Short message, displayed in context
//     /// Does not have ot be set.
//     short: Option<String>,
//     /// Extra information as a report.
//     /// Does not have to be set.
//     sub_report: Option<Box<Report>>,

//     #[cfg(debug_assertions)]
//     created_at: StaticLocationRef,
// }

// impl Report {
//     #[track_caller]
//     pub fn new() -> Self {
//         Self {
//             level: None,
//             file: None,
//             span: None,
//             message: None,
//             short: None,
//             sub_report: None,
//             created_at: std::panic::Location::caller(),
//         }
//     }

//     #[track_caller]
//     pub fn level(&self) -> Level {
//         self.level.unwrap()
//     }

//     pub fn error(self) -> Self {
//         self.set_level(Level::Error)
//     }

//     pub fn unimpl(self) -> Self {
//         self.set_level(Level::Unimpl)
//     }

//     pub fn note(self) -> Self {
//         self.set_level(Level::Note)
//     }

//     #[track_caller]
//     fn set_level(mut self, level: Level) -> Self {
//         debug_assert!(self.level.is_none());
//         self.level = Some(level);
//         self
//     }

//     pub fn file(mut self, file: SourceFileId) -> Self {
//         self.file = Some(file);
//         self
//     }

//     pub fn maybe_set_file(mut self, set_file: impl FnOnce() -> SourceFileId) -> Self {
//         if self.file.is_none() {
//             self.file = Some(set_file())
//         }
//         self
//     }

//     pub fn span(mut self, span: Span) -> Self {
//         self.span = Some(span);
//         self
//     }

//     pub fn maybe_set_span(mut self, set_span: impl FnOnce() -> Span) -> Self {
//         if self.span.is_none() {
//             self.span = Some(set_span());
//         }
//         self
//     }

//     pub fn message(mut self, message: impl ToString) -> Self {
//         self.message = Some(message.to_string());
//         self
//     }

//     pub fn short(mut self, short: impl ToString) -> Self {
//         self.short = Some(short.to_string());
//         self
//     }

//     pub fn sub_report(mut self, report: Report) -> Self {
//         self.sub_report = Some(Box::new(report));
//         self
//     }

//     pub fn map_sub_report(mut self, func: impl FnOnce(Report) -> Report) -> Self {
//         self.sub_report = self.sub_report.map(|s| *s).map(func).map(Box::new);
//         self
//     }
// }

// pub fn format_report<W: io::Write>(
//     f: &mut W,
//     report: &Report,
//     // source_map: &source_map::SourceMap,
//     path: impl AsRef<std::path::Path>,
//     source: &str,
//     line_offsets: &[usize],
//     // cst: &Cst,
// ) -> io::Result<()> {
//     use colored::Colorize;

//     let path = path.as_ref();

//     let (highlight_color, level_str) = report
//         .level
//         .map(|l| l.get())
//         .unwrap_or((colored::Color::BrightRed, " NO LEVEL GIVEN"));

//     let message = report
//         .message
//         .clone()
//         .unwrap_or_else(|| "NO MESSAGE GIVEN".to_string());

//     writeln!(
//         f,
//         "{}: {}",
//         level_str.color(highlight_color).bold(),
//         message.bold()
//     )?;

//     // Write out code window
//     // let file = report.file.map(|f| &source_map[f]);
//     // if let Some(file) = file {

//     // let path = file.path_relative();
//     // let source = file.source();
//     // let cst = file.cst();

//     if let Some(span) = &report.span {
//         let location = util::FileLocation::from_range(source, span).unwrap();

//         let largest_line = location.start.line.max(location.end.line);
//         let padding_length = ((largest_line as f64).log10().floor() + 1.0) as usize;
//         let padding = format!("{:width$}", ' ', width = padding_length);

//         writeln!(
//             f,
//             "{} {} {}:{}:{}",
//             padding,
//             "@".bright_blue().bold(),
//             path.display(),
//             location.start.line,
//             location.end.column
//         )?;

//         writeln!(f, "{} {}", padding, "|".bright_blue().bold())?;

//         if location.start.line == location.end.line || location.start.line + 1 == location.end.line
//         {
//             let line_offset_index = line_offsets
//                 .iter()
//                 .position(|&offset| offset >= span.start)
//                 .unwrap();
//             let offending_line_range =
//                 line_offsets[line_offset_index - 1]..line_offsets[line_offset_index] - 1;

//             write!(
//                 f,
//                 "{} ",
//                 format!("{:width$} |", location.start.line, width = padding_length)
//                     .bright_blue()
//                     .bold(),
//             )?;

//             let span_start = &source[offending_line_range.start..span.start];
//             let span_mid = &source[span.clone()];
//             let span_end = &source[span.end..offending_line_range.end];

//             writeln!(
//                 f,
//                 "{}{}{}",
//                 span_start,
//                 span_mid.color(highlight_color),
//                 span_end
//             )?;

//             if let Some(short) = &report.short {
//                 let highlight_start_offset = span.end - offending_line_range.start - 1;
//                 writeln!(
//                     f,
//                     "{} {} {:width$}{} {}",
//                     padding,
//                     "|".bright_blue().bold(),
//                     ' ',
//                     "^".color(highlight_color),
//                     short.color(highlight_color).bold(),
//                     width = highlight_start_offset,
//                 )?;
//             }

//             writeln!(f, "{} {}", padding, "|".bright_blue().bold())?;
//         } else {
//             // @TODO
//             writeln!(
//                 f,
//                 "{}",
//                 "TODO: location spans over multiple lines".bright_green()
//             )?;
//         }
//     } else {
//         writeln!(f, "{}", path.display())?;
//         writeln!(f, "{}", "NO RANGE GIVEN".bright_red())?;
//     }
//     // } else {
//     //     writeln!(f, "{}", "NO FILE GIVEN".bright_red())?;
//     // }

//     if report.level.is_none()
//         || report.file.is_none()
//         || report.span.is_none()
//         || report.message.is_none()
//     {
//         writeln!(
//             f,
//             "{}",
//             "REPORT HAS MISSING INFORMATION THIS IS A BUG".bright_red()
//         )?;
//         writeln!(
//             f,
//             "{}",
//             format!(
//                 "REPORT WAS CREATED AT {}:{}:{}",
//                 report.created_at.file(),
//                 report.created_at.line(),
//                 report.created_at.column()
//             )
//             .bright_red()
//         )?;
//     }

//     if let Some(sub) = &report.sub_report {
//         // format_report(f, sub, source_map)?;
//         format_report(f, sub, path, source, line_offsets)?
//     }

//     writeln!(f)?;

//     Ok(())
// }
