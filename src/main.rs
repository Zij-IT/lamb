#![warn(clippy::pedantic)]
#![allow(clippy::range_plus_one)]
#![allow(clippy::module_name_repetitions)]
#![feature(trait_alias)]

mod lamb_parse;

use lamb_parse::parse_source;

fn main() {
    let sample = include_str!("../examples/errors.lb");
    let sample_src = "../examples/errors.lb";

    // let _res = dbg!(ast_from_source(sample, sample_src));

    repl();
}

fn repl() {
    use std::io::Write;

    let stdin = std::io::stdin();
    let mut stdout = std::io::stdout();
    let mut buffer = String::new();

    loop {
        buffer.clear();

        let _res = stdout.write_all("|> ".as_bytes());
        let _res = stdout.flush();

        if stdin.read_line(&mut buffer).is_err() {
            continue;
        }

        let ast = lamb_parse::parse_expression(&buffer);
        match ast {
            Ok(prog) => println!("{:?}", prog),
            Err(es) => {
                for e in es {
                    e.eprint(&buffer).unwrap();
                }
            }
        }
    }
}
