#![warn(clippy::pedantic)]

use std::io::Write;

mod lexer;
mod token;

fn main() {
    let stdin = std::io::stdin();
    let mut stdout = std::io::stdout();
    let mut buffer = String::new();

    loop {
        buffer.clear();

        let _res = stdout.write_all(".> ".as_bytes());
        let _res = stdout.flush();

        if stdin.read_line(&mut buffer).is_ok() {
            match lexer::lex(&buffer) {
                Ok(vec) => println!(
                    "OK: {:?}",
                    vec.into_iter()
                        .map(lexer::Spanned::into_inner)
                        .collect::<Vec<_>>()
                ),
                Err(err) => println!("ERR: {:?}", err),
            }
        }
    }
}
