mod debug;
pub mod util;

mod ast;
mod parse;

#[allow(dead_code)]
mod nameres;
#[allow(dead_code)]
mod typer;
#[allow(dead_code)]
mod zir;

// @TODO: Write tests, for everything
// @TODO: Literal prefixes

fn main() {
    let options = Options::get();

    assert_eq!(options.files.len(), 1);
    let source_path = &options.files[0];

    // Read file to string
    let source = util::read_file_to_string(source_path)
        .map(|s| s + "\n\0")
        .unwrap_or_else(|e| {
            eprintln!("Failed to read file at path '{}', {}", source_path, e);
            std::process::exit(1);
        });

    let mut timer = util::time::Timer::new();
    let stderr = &mut std::io::stderr();

    // Lex the source
    let lex_res = timer.spanned("lexing", || parse::lex(&source));

    if options.dumps.contains(&DumpOption::tokens) {
        // Print tokens
        lex_res.debug_zip().for_each(|(tk, range)| {
            debug::write_token(stderr, &source, tk, &range, true).unwrap();
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

    // Parse the tokens
    let parse_res = timer.spanned("parsing", || parse::parse(&lex_res.tokens));

    if options.dumps.contains(&DumpOption::cst) {
        // Print cst
        debug::write_cst(
            stderr,
            &parse_res.cst,
            &source,
            &lex_res.tokens,
            &lex_res.ranges,
            true,
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

    // Generate the ast
    let ast_map = timer.spanned("astgen", || {
        ast::gen(&parse_res.cst, &source, &lex_res.tokens, &lex_res.ranges)
    });

    if options.dumps.contains(&DumpOption::ast) {
        // Print the ast
        eprintln!("{:#?}\n", ast_map);
    }

    // Perform the name resolution
    let nr_res = timer.spanned("nameres", || {
        nameres::resolve(&source, &lex_res.ranges, &ast_map)
    });

    if options.dumps.contains(&DumpOption::nameres) {
        // Print name resolution result
        eprintln!("{:#?}\n", nr_res);
    }

    let _ = timer.spanned("typing", || {});

    if options.dumps.contains(&DumpOption::typer) {
        eprintln!("typer\n");
    }

    // @TODO: Actually consume something derived from the input source
    let zir = timer.spanned("zirgen", zir::test::create_test_funcs);

    if options.dumps.contains(&DumpOption::zir) {
        eprint!("\n=-=-=  ZIR Dump  =-=-=");
        zir::print::dump(&zir, stderr).unwrap();
        eprintln!();
    }

    let (_llvm_ctx, llvm_mod, _llvm_funcs, _llvm_blocks) =
        timer.spanned("codegen", || zir::codegen::codegen(&zir));

    timer.spanned("llvm verify", || {
        use std::os::unix::prelude::{FromRawFd, IntoRawFd};

        let mut fd = unsafe { std::fs::File::from_raw_fd(1) };
        llvm::verify_module(&llvm_mod, &mut fd);
        let _ = fd.into_raw_fd();
    });

    if options.dumps.contains(&DumpOption::llvm) {
        eprintln!("\n=-=-=  LLVM Module Dump  =-=-=");
        llvm_mod.dump();
        eprintln!();
    }

    if options.print_times {
        eprintln!("Times:");
        timer.write("  ", stderr).unwrap();
    }
}

#[derive(Debug)]
pub struct Options {
    files: Vec<String>,
    print_times: bool,
    dumps: Vec<DumpOption>,
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
            .arg(Arg::new("FILES").required(true).multiple_values(true))
            .arg(
                Arg::new("print_times")
                    .long("print-times")
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
            .get_matches();

        let files = matches.get_many("FILES").unwrap().cloned().collect();

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

        Self {
            files,
            dumps,
            print_times,
        }
    }
}
