#![feature(step_trait)]
#![feature(alloc_layout_extra)]

#[macro_use]
extern crate tracing;
extern crate ansi_escapes as ansi;

pub mod util;

mod ast;
mod debug;
mod options;
mod parse;
mod report;
mod source_map;

use options::{DumpOption, Options};
use source_map::{SourceFile, SourceMap};

// @TODO: Document

fn main() {
    let options = Options::get_from_os_args();

    if options.log_level != log::LevelFilter::Off {
        setup_logger(
            std::io::stderr(),
            options.log_level,
            options.log_target_filter,
        )
        .expect("logger should be able to setup");
    }

    let source_file_path = std::path::Path::new(&options.path);

    let mut source_file = match SourceFile::new(source_file_path) {
        Ok(ok) => ok,
        Err(err) => {
            eprintln!("{}", err);
            eprintln!(
                "Failed to open file of path '{}'",
                source_file_path.display()
            );
            std::process::exit(1);
        }
    };

    match source_file.read_source() {
        Ok(_) => {}
        Err(err) => {
            eprintln!("{}", err);
            eprintln!(
                "Failed to read file of path '{}'",
                source_file_path.display()
            );
            std::process::exit(1);
        }
    }

    let mut source_map = SourceMap::new(source_file);
    let file_id = source_map.root();

    let mut timer = util::time::Timer::new();

    let stderr = &mut std::io::stderr();

    // Lex
    let tokens = {
        let file = &mut source_map[file_id];

        let (lex_data, reports) = info_span!("Lexer").in_scope(|| {
            let source = file.source();
            timer.spanned("lexer", || parse::lex(source))
        });

        file.set_line_offsets(lex_data.line_offsets);

        let reports = reports
            .into_iter()
            .map(|r| r.file(file_id))
            .collect::<Vec<_>>();

        if options.dumps.contains(&DumpOption::tokens) {
            let source = file.source();

            eprintln!("Dumping Tokens:");
            debug::write_tokens(
                stderr,
                options.color,
                !options.show_trivia,
                !options.show_newlines,
                source,
                &lex_data.tokens,
            )
            .expect("stderr should be able to be written to");
            eprintln!();
        }

        #[cfg(debug_assertions)]
        debug::verify_tokens(&lex_data.tokens);

        if !reports.is_empty() {
            for report in reports {
                report::write_report(stderr, &report, &source_map)
                    .expect("stderr should be able to be written to");
            }

            eprintln!("Aborting due to errors");
            std::process::exit(1);
        }

        print_reports(stderr, reports, &source_map, || {
            eprintln!("Aborting due to errors");
            std::process::exit(1);
        })
        .expect("stderr should be able to be written to");

        lex_data.tokens
    };

    // Parse
    {
        let (cst, reports) =
            info_span!("Parser").in_scope(|| timer.spanned("parser", || parse::parse(tokens)));

        let reports = reports
            .into_iter()
            .map(|r| r.file(file_id))
            .collect::<Vec<_>>();

        let file = &mut source_map[file_id];
        file.set_cst(cst);

        if options.dumps.contains(&DumpOption::cst) {
            let source = file.source();
            let cst = file.cst();

            eprintln!("Dumping CST:");
            debug::write_cst_from_root(
                stderr,
                options.color,
                !options.show_trivia,
                !options.show_newlines,
                source,
                cst,
            )
            .unwrap();
            eprintln!();
        }

        // Do verification only after dumping cst so we can inspect in the case
        // of an error
        #[cfg(debug_assertions)]
        {
            let cst = file.cst();
            debug::verify_cst(cst);
        }

        print_reports(stderr, reports, &source_map, || {
            eprintln!("Aborting due to errors");
            std::process::exit(1);
        })
        .expect("stderr should be able to be written to");
    }

    let strings = util::index::StringInterningVec::new();

    // Ast Generation
    let strings = {
        let mut gen = ast::gen::AstGen::new(strings);

        let ast = info_span!("AstGen")
            .in_scope(|| timer.spanned("ast-gen", || gen.generate_ast_file(file_id, &source_map)));

        let file = &mut source_map[file_id];
        file.set_ast(ast);

        let strings = gen.strings;

        if options.dumps.contains(&DumpOption::ast) {
            dbg!(&strings);
            let ast = file.ast();
            dbg!(ast);
        }

        strings
    };

    let _ = strings;

    if options.print_times {
        eprintln!("Times:");
        timer.write(stderr).unwrap();
    }
}

fn print_reports(
    f: &mut impl std::io::Write,
    mut reports: Vec<report::Report>,
    source_map: &SourceMap,
    on_fatal: impl Fn(),
) -> std::io::Result<()> {
    if reports.is_empty() {
        return Ok(());
    }

    reports.sort_by_key(|a| a.get_level());

    for report in reports.iter() {
        report::write_report(f, report, source_map)?;
    }

    let fatal = true; // Currently all reports are fatal

    if fatal {
        on_fatal()
    }

    Ok(())
}

fn setup_logger(
    output: impl Into<fern::Output>,
    level: log::LevelFilter,
    target_filter: Option<regex::Regex>,
) -> Result<(), fern::InitError> {
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
        .level(level)
        .chain(
            if let Some(target_filter) = target_filter {
                fern::Dispatch::new()
                    .filter(move |metadata| target_filter.is_match(metadata.target()))
            } else {
                fern::Dispatch::new()
            }
            .chain(output),
        )
        .apply()?;

    Ok(())
}
