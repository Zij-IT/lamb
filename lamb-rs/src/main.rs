#![warn(clippy::pedantic)]
#![allow(
    clippy::cast_sign_loss,
    clippy::cast_possible_truncation,
    clippy::cast_possible_wrap
)]

use ast::Script;
use chumsky::{
    input::Stream,
    prelude::{Input, Rich},
    span::SimpleSpan,
    Parser,
};

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
    let path = options.path.as_ref().unwrap();

    let contents = std::fs::read_to_string(path).unwrap();

    let tokens = match tokenize::lamb().parse(&*contents).into_output_errors() {
        (Some(t), errs) if errs.is_empty() => t,
        (Some(t), errs) => {
            report_errors(path, errs, "[Lamb] Lexer Errors:");
            t
        }
        (_, errs) => {
            report_errors(path, errs, "[Lamb] Lexer Errors:");
            return;
        }
    };

    let eoi = SimpleSpan::new(contents.len(), contents.len());
    let _script = match parse_script(tokens, eoi) {
        (Some(t), errs) if errs.is_empty() => t,
        (Some(t), errs) => {
            report_errors(path, errs, "[Lamb] Parser Errors:");
            t
        }
        (_, errs) => {
            report_errors(path, errs, "[Lamb] Parser Errors:");
            return;
        }
    };

    println!("Parsed '{}' successfully", options.path.unwrap().display());
}

fn parse_script<'a, I>(toks: I, eoi: SimpleSpan) -> (Option<Script>, Vec<Rich<'a, Token>>)
where
    I: IntoIterator<Item = (Token, SimpleSpan)> + 'a,
{
    parse::script()
        .parse(
            Stream::from_iter(
                toks.into_iter()
                    .filter(|(tok, _span)| matches!(tok, Token::Error(..))),
            )
            .spanned(eoi),
        )
        .into_output_errors()
}
