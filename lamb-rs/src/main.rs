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

use crate::{cli::DebugLevel, report::errors, tokenize::Token};

mod ast;
mod cli;
mod ffi;
mod optimization;
mod parse;
mod report;
mod tokenize;

fn main() {
    let cli::LambOptions {
        debug_level,
        gc_debug_level: _,
        optimization_level: _,
        path,
    } = cli::LambOptions::parse();

    let src = match path.as_ref() {
        Some(path) => std::fs::read_to_string(path).unwrap(),
        None => repl_input(),
    };

    let tokens = match tokenize::lamb().parse(&*src).into_output_errors() {
        (Some(t), errs) if errs.is_empty() => t,
        (_, errs) => {
            errors(path.as_deref(), errs, "[Lamb] Lexer Errors:");
            return;
        }
    };

    let eoi = SimpleSpan::new(src.len(), src.len());
    let script = match parse_script(tokens, eoi) {
        (Some(t), errs) if errs.is_empty() => t,
        (_, errs) => {
            errors(path.as_deref(), errs, "[Lamb] Parser Errors:");
            return;
        }
    };

    ffi::run_script(
        &script,
        debug_level == DebugLevel::Full,
        debug_level != DebugLevel::None,
    );
}

fn repl_input() -> String {
    let mut input = String::new();
    loop {
        break;
    }

    input
}

fn parse_script<'a, I>(toks: I, eoi: SimpleSpan) -> (Option<Script>, Vec<Rich<'a, Token>>)
where
    I: IntoIterator<Item = (Token, SimpleSpan)> + 'a,
{
    parse::script()
        .parse(Stream::from_iter(toks.into_iter()).spanned(eoi))
        .into_output_errors()
}
