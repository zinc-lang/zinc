//! # Options
//!
//! Module in charge of getting and parsing command line arguments.
//! This uses the `clap` library which handles most of the work such as most of
//! the parsing and error handling/reporting.
//!
//! The code in this module is mostly self documenting refer to code within
//! [`Options::command()`] to see a complete list of options and their
//! documentation. This is most the same information that would be printed if
//! called with `--help`.

use clap::{builder::PossibleValuesParser, Arg, ArgAction, ArgMatches, Command, ValueHint};
use std::str::FromStr;

/// Produced parsed options
#[derive(Debug)]
pub struct Options {
    pub path: String,
    pub print_times: bool,
    pub dumps: Vec<DumpOption>,
    pub show_trivia: bool,
    pub show_newlines: bool,
    pub color: bool,
    pub log_level: log::LevelFilter,
    pub log_target_filter: Option<regex::Regex>,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, clap::ValueEnum)]
#[allow(non_camel_case_types)]
pub enum DumpOption {
    tokens,
    cst,
    ast,
}

impl Options {
    /// Produce `Self` using the arguments provided by the os
    pub fn get_from_os_args() -> Self {
        Self::from_matches(Self::command().get_matches())
    }

    // pub fn get_from(iter: impl IntoIterator<Item = impl Into<std::ffi::OsString> + Clone>) -> Self {
    //     Self::from_matches(Self::command().get_matches_from(iter))
    // }

    /// Create the clap [`Command`]
    pub fn command() -> Command {
        Command::new("zincc")
            .arg(
                Arg::new("FILE")
                    .required(true)
                    .value_hint(ValueHint::FilePath),
            )
            .arg(
                Arg::new("print-times")
                    .short('T')
                    .help("Print how long certain processes took")
                    .action(ArgAction::SetTrue),
            )
            .arg(
                Arg::new("dump")
                    .long("dump")
                    .short('D')
                    .help("Print out information for debug purposes")
                    .num_args(1)
                    .value_parser(clap::builder::EnumValueParser::<DumpOption>::new())
                    .action(ArgAction::Append),
            )
            .arg(
                Arg::new("show-trivia")
                    .long("show-trivia")
                    .help("Show trivia when dumping")
                    .action(ArgAction::SetTrue),
            )
            .arg(
                Arg::new("show-newlines")
                    .long("show-newlines")
                    .help("Show newlines when dumping")
                    .action(ArgAction::SetTrue),
            )
            .arg(
                Arg::new("color")
                    .long("color")
                    .help("When to use color")
                    .num_args(1)
                    .value_name("when")
                    .ignore_case(true)
                    .value_parser(PossibleValuesParser::new(["auto", "always", "never"])),
            )
            .arg(
                Arg::new("log-level")
                    .long("log-level")
                    .help("Set the log level")
                    .num_args(1)
                    .value_name("level")
                    .ignore_case(true)
                    .default_value("off")
                    .value_parser(PossibleValuesParser::new([
                        "off", "error", "warn", "info", "debug", "trace",
                    ])),
            )
            .arg(
                Arg::new("log-target-filter")
                    .long("log-target-filter")
                    .help("Filter logs based on target with given regex")
                    .value_name("regex")
                    .num_args(1)
                    .value_parser(RegexSyntaxParser::new()),
            )
    }

    /// Produce `Self` from [`ArgMatches`] created from [`Command`] created by
    /// [`Self::command()`]
    pub fn from_matches(matches: ArgMatches) -> Self {
        let path = matches.get_one::<String>("FILE").unwrap().to_owned();

        let print_times = matches.get_flag("print-times");

        let dumps = matches
            .get_many::<DumpOption>("dump")
            .unwrap_or_default()
            .cloned()
            .collect::<Vec<_>>();

        let show_trivia = matches.get_flag("show-trivia");

        let show_newlines = matches.get_flag("show-newlines");

        let color = match matches
            .get_one::<String>("color")
            .unwrap_or(&"auto".to_string())
            .as_ref()
        {
            "auto" => atty::is(atty::Stream::Stdout),
            "always" => true,
            "never" => false,
            _ => unreachable!(),
        };

        let log_level = matches
            .get_one::<String>("log-level")
            .map(|str| {
                log::LevelFilter::from_str(str).expect("arg parser should have caught this earlier")
            })
            .unwrap();

        let log_target_filter = matches
            .get_one::<regex::Regex>("log-target-filter")
            .cloned();

        Self {
            path,
            dumps,
            show_trivia,
            show_newlines,
            print_times,
            color,
            log_level,
            log_target_filter,
        }
    }
}

#[derive(Clone)]
struct RegexSyntaxParser;

impl RegexSyntaxParser {
    pub fn new() -> Self {
        Self {}
    }
}

impl clap::builder::TypedValueParser for RegexSyntaxParser {
    type Value = regex::Regex;

    fn parse_ref(
        &self,
        cmd: &clap::Command,
        arg: Option<&clap::Arg>,
        value: &std::ffi::OsStr,
    ) -> Result<Self::Value, clap::Error> {
        self.parse(cmd, arg, value.to_owned())
    }

    fn parse(
        &self,
        cmd: &clap::Command,
        _arg: Option<&clap::Arg>,
        value: std::ffi::OsString,
    ) -> Result<Self::Value, clap::Error> {
        let value = value
            .into_string()
            .map_err(|_| clap::Error::new(clap::error::ErrorKind::InvalidUtf8).with_cmd(cmd))?;

        // @TODO: Find out how to properly report errors
        let regex = regex::Regex::new(&value).map_err(|regex_err| {
            let mut err = clap::Error::new(clap::error::ErrorKind::ValueValidation).with_cmd(cmd);

            let _ = err.insert(
                clap::error::ContextKind::Usage,
                clap::error::ContextValue::String(regex_err.to_string()),
            );

            err
        })?;

        Ok(regex)
    }
}
