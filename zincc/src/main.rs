#[macro_use]
extern crate tracing;

pub mod util;

mod ast;
mod debug;
mod parse;
mod report;
mod source_map;

use source_map::{SourceFile, SourceFileId, SourceMap};

fn main() {
    let options = Options::get();

    if options.log {
        setup_logger().expect("failed to setup logger");
    }

    let mut source_map = SourceMap::new(SourceFile::new(options.path).unwrap());

    let file_id = source_map.root;

    read_file_source(&mut source_map, file_id).unwrap();

    let mut timer = util::time::Timer::new();
    let stderr = &mut std::io::stderr();

    {
        let reports = info_span!("Lexer")
            .in_scope(|| timer.spanned("lexer", || parse::lex(&mut source_map, file_id)));

        if options.dumps.contains(&DumpOption::tokens) {
            let source = &source_map.sources[&file_id];

            source_map.lex_data[&file_id]
                .debug_zip()
                .for_each(|(&tk, range)| {
                    if !tk.is_trivia() {
                        debug::write_token(stderr, source, tk, range, options.color).unwrap();
                        eprintln!();
                    }
                });

            eprintln!();
        }

        // As of currently the lexer only produces errors
        if !reports.is_empty() {
            reports
                .iter()
                .for_each(|report| report::format_report(stderr, report, &source_map).unwrap());

            eprintln!();
            eprintln!("Aborting due to errors");
            std::process::exit(1);
        }
    }

    {
        let reports = info_span!("Parser")
            .in_scope(|| timer.spanned("parser", || parse::parse(&mut source_map, file_id)));

        if options.dumps.contains(&DumpOption::cst) {
            let source = &source_map.sources[&file_id];
            let lex_data = &source_map.lex_data[&file_id];
            let cst = &source_map.csts[&file_id];

            debug::write_cst(
                stderr,
                cst,
                source,
                &lex_data.tokens,
                &lex_data.ranges,
                options.color,
            )
            .unwrap();
            eprintln!();
        }

        if !reports.is_empty() {
            let (errors, unimpls) = report::split(reports);

            if !unimpls.is_empty() {
                unimpls
                    .iter()
                    .for_each(|report| report::format_report(stderr, report, &source_map).unwrap());

                eprintln!("\nAborting due to encountering unimplemented features");
                std::process::exit(1);
            }

            if !errors.is_empty() {
                errors
                    .iter()
                    .for_each(|report| report::format_report(stderr, report, &source_map).unwrap());

                eprintln!("\nAborting due to errors");
                std::process::exit(1);
            }
        }
    }

    {
        let ast = info_span!("AstGen").in_scope(|| {
            timer.spanned("ast-gen", || {
                let gen = ast::gen::Generator::new(&mut source_map, file_id);
                gen.generate()
            })
        });

        if options.dumps.contains(&DumpOption::ast) {
            dbg!(ast);
        }
    }

    if options.print_times {
        eprintln!("\nTimes:");
        timer.write(stderr).unwrap();
    }
}

fn setup_logger() -> Result<(), fern::InitError> {
    use fern::colors::Color;

    let colors = fern::colors::ColoredLevelConfig::new()
        .trace(Color::White)
        .debug(Color::Magenta)
        .info(Color::Blue)
        .warn(Color::Yellow)
        .error(Color::Red);

    fern::Dispatch::new()
        .format(move |out, message, record| {
            out.finish(format_args!(
                "{}[{}][{}] {}",
                chrono::Local::now().format("[%H:%M:%S]"),
                record.target(),
                colors.color(record.level()),
                message
            ))
        })
        .chain(std::io::stderr())
        .apply()?;
    Ok(())
}

fn read_file_source(map: &mut SourceMap, file_id: SourceFileId) -> std::io::Result<()> {
    debug_assert!(!map.sources.contains_key(&file_id));

    let file = &map.files[file_id];

    let source = util::read_file_to_string(file.path()).map(|s| s + "\n\0")?;
    map.sources.insert(file_id, source);

    Ok(())
}

#[derive(Debug)]
pub struct Options {
    path: String,
    print_times: bool,
    dumps: Vec<DumpOption>,
    color: bool,
    log: bool,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, strum::EnumString, strum::EnumVariantNames)]
#[allow(non_camel_case_types)]
pub enum DumpOption {
    tokens,
    cst,
    ast,
}

impl Options {
    pub fn get() -> Self {
        use clap::{builder::PossibleValuesParser, Arg, ArgAction, Command};
        use std::str::FromStr;
        use strum::VariantNames;

        let matches = Command::new("zincc")
            .arg(
                Arg::new("FILE")
                    .required(true)
                    .value_hint(clap::ValueHint::FilePath),
            )
            .arg(
                Arg::new("print_times")
                    .short('T')
                    .help("Print how long processes took"),
            )
            .arg(
                Arg::new("dump")
                    .long("dump")
                    .short('D')
                    .takes_value(true)
                    .value_parser(PossibleValuesParser::new(DumpOption::VARIANTS))
                    .action(ArgAction::Append),
            )
            .arg(
                Arg::new("color")
                    .long("color")
                    .takes_value(true)
                    .value_name("WHEN")
                    .value_parser(PossibleValuesParser::new(["auto", "always", "never"])),
            )
            .arg(Arg::new("log").long("log").help("Print logs"))
            .get_matches();

        let path = matches.value_of("FILE").unwrap().to_string();

        let print_times = matches.contains_id("print_times");

        let dumps = matches
            .get_many::<String>("dump")
            .unwrap_or_default()
            .map(|s| DumpOption::from_str(s).unwrap())
            .collect();

        let color = match matches.value_of("color").unwrap_or("auto") {
            "auto" => atty::is(atty::Stream::Stdout),
            "always" => true,
            "never" => false,
            _ => unreachable!(),
        };

        let log = matches.contains_id("log");

        Self {
            path,
            dumps,
            print_times,
            color,
            log,
        }
    }
}
