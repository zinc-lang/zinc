mod debug;
pub mod util;

mod ast;
mod parse;

mod nameres;
mod typer;

#[allow(dead_code)]
mod zir;

// @TODO: Write tests, for everything

fn main() {
    let options = Options::get();

    let source = util::read_file_to_string(&options.path)
        .map(|s| s + "\n\0")
        .unwrap_or_else(|err| {
            eprintln!("Failed to read file at path '{}', {}", options.path, err);
            std::process::exit(1);
        });

    let mut timer = util::time::Timer::new();
    let stderr = &mut std::io::stderr();

    let lex_res = timer.spanned("lexical analysis", || parse::lex(&source));

    if options.dumps.contains(&DumpOption::tokens) {
        lex_res.debug_zip().for_each(|(tk, range)| {
            debug::write_token(stderr, &source, tk, &range, options.color).unwrap();
            eprintln!();
        });
        eprintln!();
    }

    // Print errors and exit if there are any
    if !lex_res.errors.is_empty() {
        for err in lex_res.errors {
            let loc = parse::FileLocation::from_offset(&source, err.offset).unwrap();
            // @TODO: Better error formatting
            eprintln!("error: {:?}  @[{}:{}]", err.kind, loc.line, loc.column);
        }

        eprintln!("\nAborting due to errors");
        std::process::exit(1);
    }

    let parse_res = timer.spanned("parsing", || parse::parse(&lex_res.tokens));

    if options.dumps.contains(&DumpOption::cst) {
        debug::write_cst(
            stderr,
            &parse_res.cst,
            &source,
            &lex_res.tokens,
            &lex_res.ranges,
            options.color,
        )
        .unwrap();
        eprintln!();
    }

    // Print errors and exit if there are any
    if !parse_res.errors.is_empty() {
        for error in parse_res.errors {
            match error {
                parse::ParseError::Expected(err) => {
                    let at = lex_res.tokens[err.at as usize];
                    let found = lex_res.tokens[err.found as usize];

                    let at_range = &lex_res.ranges[err.at as usize];
                    let at_loc =
                        parse::FileLocation::from_offset(&source, at_range.start as usize).unwrap();

                    // @TODO: Better error formatting
                    eprintln!(
                        "error: expected '{:?}' at '{:?}' in '{:?}', but found '{:?}'  @[{}:{}]",
                        err.what, at, err.context, found, at_loc.line, at_loc.column
                    );
                }
            }
        }

        eprintln!("\nAborting due to errors");
        std::process::exit(1);
    }

    let ast_map = timer.spanned("ast generation", || {
        ast::gen(&parse_res.cst, &source, &lex_res.tokens, &lex_res.ranges)
    });

    if options.dumps.contains(&DumpOption::ast) {
        eprintln!("{:#?}\n", ast_map);
    }

    let (nr_res, strings) = timer.spanned("name resolution", || {
        nameres::resolve(&source, &lex_res.ranges, &ast_map)
    });

    if options.dumps.contains(&DumpOption::nameres) {
        eprintln!("{:#?}\n", nr_res);
    }

    let ty_map = timer.spanned("type checking", || typer::resolve(&nr_res));

    if options.dumps.contains(&DumpOption::typer) {
        eprintln!("{:#?}\n", ty_map);
    }

    // let zir = timer.spanned("zir generation", zir::test::create_test_funcs);
    let zir = timer.spanned("zir generation", || zir::gen(&nr_res, &ty_map, strings));

    if options.dumps.contains(&DumpOption::zir) {
        eprint!("\n=-=-=  ZIR Dump  =-=-=");
        zir::print::dump(&zir, stderr).unwrap();
        eprintln!();
    }

    let (_llvm_ctx, llvm_mod, _llvm_funcs, _llvm_blocks) =
        timer.spanned("llvm-ir generation", || zir::codegen::codegen(&zir));

    timer.spanned("llvm-ir verification", || {
        use std::os::unix::prelude::{FromRawFd, IntoRawFd};

        let mut fd = unsafe { std::fs::File::from_raw_fd(1) };
        llvm::verify_module(&llvm_mod, &mut fd);
        let _ = fd.into_raw_fd();
    });

    if options.dumps.contains(&DumpOption::llvm) {
        eprintln!("\n=-=-=  LLVM-IR Module Dump  =-=-=");
        llvm_mod.dump();
        eprintln!();
    }

    if options.print_times {
        eprintln!("Times:");
        timer.write(stderr).unwrap();
    }
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
    ast,
    nameres,
    zir,
    typer,
    llvm,
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
                        "tokens", "cst", "ast", "nameres", "zir", "typer", "llvm",
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
                "ast" => DumpOption::ast,
                "nameres" => DumpOption::nameres,
                "zir" => DumpOption::zir,
                "typer" => DumpOption::typer,
                "llvm" => DumpOption::llvm,
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
