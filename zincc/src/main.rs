pub mod debug;
pub mod util;

pub mod ast;
pub mod hir;
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
    let stderr = &mut std::io::stderr();

    let lex_res = timer.spanned("lexing", || parse::lex(&source));

    if options.dumps.contains(&"tokens".to_string()) {
        lex_res.debug_zip().for_each(|(tk, range, _)| {
            eprintln!("{}", debug::format_token(&source, tk, &range, true));
        });
        eprintln!();
    }

    let parse_res = timer.spanned("parsing", || parse::parse(&lex_res.tokens));

    if options.dumps.contains(&"cst".to_string()) {
        debug::write_cst(
            stderr,
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

    let (ast_map, ast_root) = timer.spanned("astgen", || {
        ast::gen(&parse_res.cst, &source, &lex_res.tokens, &lex_res.spans)
    });

    if options.dumps.contains(&"ast".to_string()) {
        eprintln!("{:#?}\n", ast_map);
    }

    let (interned_strings, hir_scope_descs) = timer.spanned("hir stage1", || {
        let sd = hir::gen::SharedData::new(&source, &lex_res.spans, &ast_map);
        let scopes = hir::gen::stage1(&sd, &ast_root);

        (sd.strings.into_inner(), scopes)
    });

    if options.dumps.contains(&"hir".to_string()) {
        eprintln!("strings: {:#?}", interned_strings);
        eprintln!("scope descs: {:#?}\n", hir_scope_descs);
    }

    // @TODO: Actually consume something derived from the input source
    let zir = timer.spanned("zirgen", zir::test::create_test_funcs);

    if options.dumps.contains(&"zir".to_string()) {
        eprint!("\n=-=-=  ZIR Dump  =-=-=");
        zir::print::dump(&zir, stderr).unwrap();
    }

    let (_llvm_ctx, llvm_mod, _llvm_funcs, _llvm_blocks) =
        timer.spanned("codegen", || zir::codegen::codegen(&zir));

    // @FIXME: exits at this call, for whatever reason
    // llvm::verify_module(&llvm_mod, &mut unsafe {
    //     <std::fs::File as std::os::unix::prelude::FromRawFd>::from_raw_fd(1)
    // });

    if options.dumps.contains(&"llvm".to_string()) {
        eprintln!("\n=-=-=  LLVM Module Dump  =-=-=");
        llvm_mod.dump();
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
    dumps: Vec<String>,
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
                        "tokens", "cst", "ast", "zir", "llvm", "hir",
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
            .collect();

        Self {
            files,
            dumps,
            print_times,
        }
    }
}
