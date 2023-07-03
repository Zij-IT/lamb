#![warn(clippy::pedantic)]
#![allow(
    clippy::cast_sign_loss,
    clippy::cast_possible_truncation,
    clippy::cast_possible_wrap
)]

use chumsky::{input::Stream, prelude::Input, span::SimpleSpan, Parser};

use crate::{report::report_errors, tokenize::Token};

mod ast;
mod cli;
mod ffi;
mod optimization;
mod parse;
mod report;
mod tokenize;

fn main() {
    let options = cli::LambOptions::parse();

    let contents = std::fs::read_to_string(options.path.as_ref().unwrap()).unwrap();

    let tokens = match tokenize::lamb().parse(&*contents).into_output_errors() {
        (Some(t), errs) if errs.is_empty() => t,
        (Some(t), errs) => {
            report_errors(options.path.as_ref().unwrap(), errs, "[Lamb] Lexer Errors:");
            t
        }
        (_, errs) => {
            report_errors(options.path.as_ref().unwrap(), errs, "[Lamb] Lexer Errors:");
            return;
        }
    };

    let eoi = SimpleSpan::new(contents.len(), contents.len());
    let _script = match parse::script()
        .parse(
            Stream::from_iter(tokens.into_iter().filter(|(tok, _span)| match tok {
                Token::Error(..) => false,
                _ => true,
            }))
            .spanned(eoi),
        )
        .into_output_errors()
    {
        (Some(t), errs) if errs.is_empty() => t,
        (Some(t), errs) => {
            report_errors(
                options.path.as_ref().unwrap(),
                errs,
                "[Lamb] Parser Errors:",
            );
            t
        }
        (_, errs) => {
            report_errors(
                options.path.as_ref().unwrap(),
                errs,
                "[Lamb] Parser Errors:",
            );
            return;
        }
    };

    println!("Parsed '{}' successfully", options.path.unwrap().display());
}
