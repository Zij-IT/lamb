#![warn(clippy::pedantic)]
#![allow(
    clippy::cast_sign_loss,
    clippy::cast_possible_truncation,
    clippy::cast_possible_wrap
)]

use chumsky::Parser;

mod ast;
mod cli;
mod ffi;
mod optimization;
mod parse;
mod token;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let options = cli::LambOptions::parse();

    let contents = std::fs::read_to_string(options.path.as_ref().unwrap()).unwrap();

    let tokens = match token::lexer().parse(&*contents).into_output_errors() {
        (Some(t), errs) if errs.is_empty() => t,
        (_, errs) => {
            dbg!(errs);
            Err("Lexer failed: ^")?
        }
    };

    let script = match parse::script().parse(&*tokens).into_output_errors() {
        (Some(t), errs) if errs.is_empty() => t,
        (_, errs) => {
            dbg!(errs);
            Err("Parser failed: ^")?
        }
    };

    Ok(())
}
