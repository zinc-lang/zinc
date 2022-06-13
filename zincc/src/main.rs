pub mod debug;
pub mod parse;

fn main() {
    let args = std::env::args().collect::<Vec<_>>();
    let source_path = &args[1];

    let source = read_file_to_string(source_path)
        .map(|s| if !s.ends_with('\n') { s + "\n" } else { s })
        .unwrap_or_else(|e| {
            eprintln!("Failed to read file at path '{}', {}", source_path, e);
            std::process::exit(1);
        });

    // println!("{}", source);

    let mut stopwatch = Stopwatch::start();

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

// pub fn format_duration(duration: std::time::Duration) -> String {
//     let ns = duration.as_nanos();
//     if ns < 1000 {
//         return format!("{} ns", ns);
//     }

//     let us = duration.as_micros();
//     if us < 1000 {
//         return format!("{} μs", us);
//     }

//     let ms = duration.as_millis();
//     if ms < 1000 {
//         return format!("{} ms", ms);
//     }

//     let s = duration.as_secs();
//     return format!("{} s", s);
// }

#[derive(Debug)]
pub struct Stopwatch {
    time: std::time::Instant,
}

impl Stopwatch {
    pub fn start() -> Stopwatch {
        Stopwatch {
            time: std::time::Instant::now(),
        }
    }

    pub fn read(&self) -> std::time::Duration {
        let now = std::time::Instant::now();
        now - self.time
    }

    pub fn reset(&mut self) {
        self.time = std::time::Instant::now();
    }

    pub fn lap(&mut self) -> std::time::Duration {
        let time = self.read();
        self.reset();
        time
    }
}

fn read_file_to_string<P: AsRef<std::path::Path>>(path: P) -> std::io::Result<String> {
    use std::io::Read;
    let mut file = std::fs::File::open(path)?;
    let mut str = String::new();
    file.read_to_string(&mut str)?;
    Ok(str)
}
