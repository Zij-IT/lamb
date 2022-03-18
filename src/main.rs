#![warn(clippy::pedantic)]
#![feature(trait_alias)]

use std::io::Write;

mod ast;
mod lexer;
mod parser;
mod token;

fn main() {
    let stdin = std::io::stdin();
    let mut stdout = std::io::stdout();
    let mut buffer = String::new();

    loop {
        buffer.clear();

        let _res = stdout.write_all(".> ".as_bytes());
        let _res = stdout.flush();

        if stdin.read_line(&mut buffer).is_ok() && process_input(&buffer).is_err() {
            continue;
        }
    }
}

fn process_input(src: &str) -> Result<(), ()> {
    let tokens = ok_and_print(lexer::lex(src)).map(|x| {
        x.into_iter()
            .map(lexer::Spanned::into_inner)
            .collect::<Vec<_>>()
    })?;

    ok_and_print(parser::parse(&tokens)).map(|_| ())
}

fn ok_and_print<T, E>(res: Result<T, E>) -> Result<T, ()>
where
    T: IntoIterator + std::fmt::Debug,
    E: std::fmt::Debug,
{
    match res {
        Ok(ok) => {
            println!("OK: {:?}", ok);
            Ok(ok)
        }
        Err(e) => {
            println!("ERR: {:?}", e);
            Err(())
        }
    }
}
