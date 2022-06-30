pub mod debug;
pub mod util;

pub mod ast;
pub mod parse;
pub mod zir;

fn main() {
    let options = Options::get();

    assert_eq!(options.files.len(), 1);
    let source_path = &options.files[0];

    let source = util::read_file_to_string(source_path)
        .map(|s| s + "\n\0")
        .unwrap_or_else(|e| {
            eprintln!("Failed to read file at path '{}', {}", source_path, e);
            std::process::exit(1);
        });

    let mut timer = util::time::Timer::new();

    let lex_res = timer.spanned("lexing", || parse::lex(&source));

    if options.dump_tokens {
        lex_res.debug_zip().for_each(|(tk, range, _)| {
            eprintln!("{}", debug::format_token(&source, tk, &range, true));
        });
        eprintln!();
    }

    let parse_res = timer.spanned("parsing", || parse::parse(&lex_res.tokens));

    if options.dump_cst {
        debug::write_cst(
            &mut std::io::stderr(),
            &parse_res.cst,
            &source,
            &lex_res.tokens,
            &lex_res.spans,
            true,
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

    let ast = timer.spanned("astgen", || {
        ast::gen(&parse_res.cst, &source, &lex_res.tokens, &lex_res.spans)
    });

    if options.dump_ast {
        eprintln!("{:#?}\n", ast);
    }

    let _ = timer.spanned("hirgen", || {
        // hir::gen()
    });

    // @TODO: Actually consume something derived from the input source
    let zir = timer.spanned("zirgen", || zir::test::create_test_funcs());

    if options.dump_zir {
        eprint!("\n=-=-=  ZIR Dump  =-=-=");
        zir::print::dump(&zir, &mut std::io::stderr()).unwrap();
    }

    let (_llvm_ctx, llvm_mod, _llvm_funcs, _llvm_blocks) =
        timer.spanned("codegen", || zir::codegen::codegen(&zir));

    // @FIXME: exits at this call, for whatever reason
    // llvm::verify_module(&llvm_mod, &mut unsafe {
    //     <std::fs::File as std::os::unix::prelude::FromRawFd>::from_raw_fd(1)
    // });

    if options.dump_llvm {
        eprintln!("\n=-=-=  LLVM Module Dump  =-=-=");
        llvm_mod.dump();
    }

    if options.print_times {
        timer.print();
    }
}

#[derive(Debug)]
pub struct Options {
    files: Vec<String>,

    dump_tokens: bool,
    dump_cst: bool,
    dump_ast: bool,
    dump_zir: bool,
    dump_llvm: bool,

    print_times: bool,
}

impl Options {
    pub fn get() -> Self {
        use clap::builder::PossibleValuesParser;
        use clap::Arg;

        let matches = clap::Command::new("zincc")
            .arg(Arg::new("FILES").required(true).multiple_values(true))
            .arg(
                Arg::new("dump")
                    .long("dump")
                    .short('D')
                    .takes_value(true)
                    .value_parser(PossibleValuesParser::new(&[
                        "tokens", "cst", "ast", "zir", "llvm",
                    ]))
                    .action(clap::ArgAction::Append),
            )
            .arg(
                Arg::new("print_times")
                    .long("print-times")
                    .short('T')
                    .help("Print how long processes took"),
            )
            .get_matches();

        let files = matches.get_many("FILES").unwrap().cloned().collect();

        let print_times = matches.contains_id("print_times");

        fn get_list<'a>(matches: &'a clap::ArgMatches, id: &str) -> Vec<&'a str> {
            matches
                .get_many::<String>(id)
                .unwrap_or_default()
                .map(|s| s.as_str())
                .collect::<Vec<_>>()
        }

        let dump = get_list(&matches, "dump");

        let dump_tokens = dump.contains(&"tokens");
        let dump_cst = dump.contains(&"cst");
        let dump_ast = dump.contains(&"ast");
        let dump_zir = dump.contains(&"zir");
        let dump_llvm = dump.contains(&"llvm");

        Self {
            files,

            dump_tokens,
            dump_cst,
            dump_ast,
            dump_zir,
            dump_llvm,

            print_times,
        }
    }
}
