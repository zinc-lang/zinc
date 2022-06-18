pub mod debug;
pub mod parse;

// pub mod zir_test;

// Could be refactored out
// pub mod llvm;
pub mod util;

fn main() {
    // zir_test::zir_test();

    let options = get_options();

    assert_eq!(options.files.len(), 1);
    let source_path = &options.files[0];

    let source = util::read_file_to_string(source_path)
        .map(|s| if !s.ends_with('\n') { s + "\n" } else { s })
        .unwrap_or_else(|e| {
            eprintln!("Failed to read file at path '{}', {}", source_path, e);
            std::process::exit(1);
        });

    let mut stopwatch = util::Stopwatch::start();

    stopwatch.reset();
    let lex_res = parse::lex(&source);
    let duration_lex = stopwatch.read();

    if options.verbose_tokens {
        lex_res.debug_zip().for_each(|(tk, range, _)| {
            eprintln!("{}", debug::format_token(&source, tk, &range));
        });
        println!();
    }

    stopwatch.reset();
    let parse_res = parse::parse(&lex_res.tokens);
    let duration_parse = stopwatch.read();

    if options.verbose_cst {
        debug::print_cst(
            &mut std::io::stderr(),
            &parse_res.cst,
            &source,
            &lex_res.tokens,
            &lex_res.spans,
            &parse_res.cst.root,
        )
        .unwrap();
        eprintln!();
    }

    if !parse_res.errors.is_empty() {
        for error in parse_res.errors {
            match error {
                parse::ParseError::Expected(err) => {
                    let at = lex_res.tokens[err.at as usize];
                    let found = lex_res.tokens[err.found as usize];

                    let at_range = &lex_res.spans[err.at as usize];
                    let at_loc = parse::FileLocation::from_offset(at_range.start as usize, &source);

                    println!(
                        "error: expected '{:?}' at '{:?}' in '{:?}', but found '{:?}'  @[{}:{}]",
                        err.what, at, err.context, found, at_loc.line, at_loc.column
                    );
                }
            }
        }

        println!("\nAborting due to errors");
        std::process::exit(1);
    }

    stopwatch.reset();
    // let ast = ast::gen::root(&parse_res.cst);
    let duration_astgen = stopwatch.read();

    // if options.verbose_ast {
    //     dbg!(&ast);
    //     println!();
    // }

    if options.print_times {
        println!("times:");
        println!("  lexing:\t{:?}", duration_lex);
        println!("  parsing:\t{:?}", duration_parse);
        println!("  astgen:\t{:?}", duration_astgen);
        println!(
            "  total:\t{:?}",
            duration_lex + duration_parse + duration_astgen
        );
    }
}

#[derive(Debug)]
pub struct Options {
    files: Vec<String>,

    verbose_tokens: bool,
    verbose_cst: bool,
    verbose_ast: bool,

    print_times: bool,
}

fn get_options() -> Options {
    use clap::builder::PossibleValuesParser;
    use clap::Arg;

    let matches = clap::Command::new("zincc")
        .arg(Arg::new("FILES").required(true).multiple_values(true))
        .arg(
            Arg::new("verbose")
                .long("verbose")
                .short('v')
                .value_parser(PossibleValuesParser::new(&["tokens", "cst", "ast"]))
                .takes_value(true)
                .action(clap::ArgAction::Append),
        )
        .arg(
            Arg::new("print_times")
                .long("print_times")
                .short('T')
                .help("Print how long processes took"),
        )
        .get_matches();

    let files = matches
        .get_many::<String>("FILES")
        .unwrap()
        .cloned()
        .collect();

    let verbose = matches
        .get_many::<String>("verbose")
        .map(|s| s.cloned().collect::<Vec<String>>())
        .unwrap_or_default();

    let verbose_tokens = verbose.contains(&"tokens".to_string());
    let verbose_cst = verbose.contains(&"cst".to_string());
    let verbose_ast = verbose.contains(&"ast".to_string());

    let print_times = matches.contains_id("print_times");

    Options {
        files,
        verbose_tokens,
        verbose_cst,
        verbose_ast,
        print_times,
    }
}
