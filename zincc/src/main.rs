pub mod debug;
pub mod parse;

pub mod debug;

// Could be refactored out
pub mod util;

fn main() {
    let args = std::env::args().collect::<Vec<_>>();
    let source_path = &args[1];

    let source = util::read_file_to_string(source_path)
        .map(|s| if !s.ends_with('\n') { s + "\n" } else { s })
        .unwrap_or_else(|e| {
            eprintln!("Failed to read file at path '{}', {}", source_path, e);
            std::process::exit(1);
        });

    // println!("{}", source);

    let mut stopwatch = util::Stopwatch::start();

    stopwatch.reset();
    let lex_res = parse::lex(&source);
    let duration_lex = stopwatch.read();

    // @Verbose: Printing tokens
    lex_res.debug_zip().for_each(|(tk, range, _)| {
        debug::print_token(&source, tk, range);
    });

    stopwatch.reset();
    let parse_res = parse::parse(&lex_res.tokens);
    let duration_parse = stopwatch.read();

    // @Verbose: Printing cst
    println!();
    debug::print_cst(&source, parse_res.cst, &lex_res.tokens, &lex_res.spans, 0);

    if !parse_res.errors.is_empty() {
        println!();
        for error in parse_res.errors {
            match error {
                parse::ParseError::Expected(err) => {
                    let at = lex_res.tokens[err.at];
                    let found = lex_res.tokens[err.found];
                    print!(
                        "error: expected '{:?}' at '{:?}' in '{:?}', but found '{:?}'",
                        err.what, at, err.context, found
                    );

                    let at_range = &lex_res.spans[err.at];
                    let at_loc = parse::FileLocation::from_offset(at_range.start as usize, &source);

                    print!(" @[{}:{}]", at_loc.line, at_loc.column);

                    println!();
                }
            }
        }
    }

    // @Verbose printing times
    println!("\ntimes:");
    println!("  lexing: \t{:?}", duration_lex);
    println!("  parsing: \t{:?}", duration_parse);
    println!("  total: \t{:?}", duration_lex + duration_parse);
}
