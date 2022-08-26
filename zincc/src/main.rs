#[macro_use]
extern crate tracing;

pub mod util;

mod debug;
mod parse;
mod source_map;

use source_map::{SourceFile, SourceFileId, SourceMap};

fn main() {
    let options = Options::get();

    setup_logger().expect("failed to setup logger");

    let mut source_map = SourceMap::new(SourceFile::new(options.path).unwrap());

    let file_id = source_map.root;

    read_file_source(&mut source_map, file_id).unwrap();

    let mut timer = util::time::Timer::new();
    let stderr = &mut std::io::stderr();

    {
        let lex_errors = info_span!("Lexer").in_scope(|| {
            timer.spanned("lexical analyser", || parse::lex(&mut source_map, file_id))
        });

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

        // Print errors and exit if there are any
        if !lex_errors.is_empty() {
            let source = &source_map.sources[&file_id];

            for err in lex_errors {
                let loc = parse::FileLocation::from_offset(source, err.offset).unwrap();
                // @TODO: Better error formatting
                eprintln!("error: {:?}  @[{}:{}]", err.kind, loc.line, loc.column);
            }

            eprintln!();
            eprintln!("Aborting due to errors");
            std::process::exit(1);
        }
    }

    {
        let parse_errors = info_span!("Parser")
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

        // Print errors and exit if there are any
        if !parse_errors.is_empty() {
            // // let source = &source_map.sources[&file_id];
            // // let lex_data = &source_map.lex_data[&file_id];

            // for error in parse_errors {
            //     // match error {
            //     //     parse::ParseError::Expected(err) => {
            //     //         let at = lex_data.tokens[err.at as usize];
            //     //         let found = lex_data.tokens[err.found as usize];

            //     //         let at_range = &lex_data.ranges[err.at as usize];
            //     //         let at_loc =
            //     //             parse::FileLocation::from_offset(source, at_range.start as usize)
            //     //                 .unwrap();

            //     //         // @TODO: Better error formatting
            //     //         eprintln!(
            //     //         "error: expected '{:?}' at '{:?}' in '{:?}', but found '{:?}'  @[{}:{}]",
            //     //         err.what, at, err.context, found, at_loc.line, at_loc.column
            //     //     );
            //     //     }
            //     // }
            // }

            // eprintln!("\nAborting due to errors");
            // std::process::exit(1);

            todo!()
        }
    }

    // let ast_map = timer.spanned("ast generation", || {
    //     ast::gen(&parse_res.cst, &source, &lex_res.tokens, &lex_res.ranges)
    // });

    // drop(parse_res);

    // if options.dumps.contains(&DumpOption::ast) {
    //     eprintln!("{:#?}\n", ast_map);
    // }

    // let (nr_res, strings) = timer.spanned("name resolution", || {
    //     nameres::resolve(&source, &lex_res.ranges, &ast_map)
    // });

    // drop(source);
    // drop(lex_res);
    // drop(ast_map);

    // if options.dumps.contains(&DumpOption::nameres) {
    //     eprintln!("{:#?}\n", nr_res);
    // }

    // let ty_map = timer.spanned("type checking", || typer::resolve(&nr_res));

    // if options.dumps.contains(&DumpOption::typer) {
    //     eprintln!("{:#?}\n", ty_map);
    // }

    // // let zir = timer.spanned("zir generation", zir::test::create_test_funcs);
    // let zir = timer.spanned("zir generation", || zir::gen(&nr_res, &ty_map, strings));

    // drop(nr_res);
    // drop(ty_map);

    // if options.dumps.contains(&DumpOption::zir) {
    //     eprint!("\n=-=-=  ZIR Dump  =-=-=");
    //     zir::print::dump(&zir, stderr).unwrap();
    //     eprintln!();
    // }

    // let (_llvm_ctx, llvm_mod, _llvm_funcs, _llvm_blocks) =
    //     timer.spanned("llvm-ir generation", || zir::codegen::codegen(&zir));

    // timer.spanned("llvm-ir verification", || {
    //     use std::os::unix::prelude::{FromRawFd, IntoRawFd};

    //     let mut fd = unsafe { std::fs::File::from_raw_fd(1) };
    //     llvm::verify_module(&llvm_mod, &mut fd);
    //     let _ = fd.into_raw_fd();
    // });

    // if options.dumps.contains(&DumpOption::llvm) {
    //     eprintln!("\n=-=-=  LLVM-IR Module Dump  =-=-=");
    //     llvm_mod.dump();
    //     eprintln!();
    // }

    if options.print_times {
        eprintln!("\nTimes:");
        timer.write(stderr).unwrap();
    }
}

fn setup_logger() -> Result<(), fern::InitError> {
    fern::Dispatch::new()
        .format(|out, message, record| {
            out.finish(format_args!(
                "{}[{}][{}] {}",
                chrono::Local::now().format("[%H:%M:%S]"),
                record.target(),
                record.level(),
                message
            ))
        })
        .chain(std::io::stderr())
        .apply()?;
    Ok(())
}

fn read_file_source(map: &mut SourceMap, file_id: SourceFileId) -> std::io::Result<()> {
    let file = &map.files[file_id];
    let path = file.get_path();

    let source = util::read_file_to_string(path).map(|s| s + "\n\0")?;
    map.sources.insert(file_id, source);

    Ok(())
}

#[derive(Debug)]
pub struct Options {
    path: String,
    print_times: bool,
    dumps: Vec<DumpOption>,
    color: bool,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
#[allow(non_camel_case_types)]
pub enum DumpOption {
    tokens,
    cst,
    // ast,
    // nameres,
    // strings,
    // typer,
    // zir,
    // llvm,
}

impl Options {
    pub fn get() -> Self {
        use clap::{builder::PossibleValuesParser, Arg, ArgAction, Command};

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
                    .value_parser(PossibleValuesParser::new(&[
                        "tokens",
                        "cst",
                        // "ast",
                        // "nameres",
                        // "strings",
                        // "typer",
                        // "zir",
                        // "llvm",
                    ]))
                    .action(ArgAction::Append),
            )
            .arg(
                Arg::new("color")
                    .long("color")
                    .takes_value(true)
                    .value_name("WHEN")
                    .value_parser(PossibleValuesParser::new(&["auto", "always", "never"])),
            )
            .get_matches();

        let path = matches.value_of("FILE").unwrap().to_string();

        let print_times = matches.contains_id("print_times");

        let dumps = matches
            .get_many::<String>("dump")
            .unwrap_or_default()
            .cloned()
            .map(|s| match s.as_str() {
                "tokens" => DumpOption::tokens,
                "cst" => DumpOption::cst,
                // "ast" => DumpOption::ast,
                // "nameres" => DumpOption::nameres,
                // "strings" => DumpOption::strings,
                // "typer" => DumpOption::typer,
                // "zir" => DumpOption::zir,
                // "llvm" => DumpOption::llvm,
                _ => unreachable!(),
            })
            .collect();

        let color = match matches.value_of("color").unwrap_or("auto") {
            "auto" => atty::is(atty::Stream::Stdout),
            "always" => true,
            "never" => false,
            _ => unreachable!(),
        };

        Self {
            path,
            dumps,
            print_times,
            color,
        }
    }
}
