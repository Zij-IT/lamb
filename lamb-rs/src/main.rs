#![warn(clippy::pedantic)]
#![allow(
    clippy::cast_sign_loss,
    clippy::cast_possible_truncation,
    clippy::cast_possible_wrap
)]

use std::io::{BufRead, Write};

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

const REPL_START: &'static [u8] = concat!(
    ",---@> Baaaah... Welcome to the Lamb REPL! (Lamb v0.1.0)\n",
    " W-W'  Type ':quit' to exit, or ':run' to run the input.\n",
    "^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^\n",
)
.as_bytes();

fn main() -> Result<(), Box<dyn std::error::Error>> {
    human_panic::setup_panic!();

    let cli::LambOptions {
        debug_level,
        gc_debug_level: _,
        optimization_level: _,
        path,
    } = cli::LambOptions::parse();

    let src = match path.as_ref().map(std::fs::read_to_string) {
        Some(src) => src?,
        None => match repl_input()? {
            Some(src) => src,
            None => return Ok(()),
        },
    };

    let tokens = match tokenize::lamb().parse(&*src).into_output_errors() {
        (Some(t), errs) if errs.is_empty() => t,
        (_, errs) => {
            errors(path.as_deref(), errs, "[Lamb] Lexer Errors:");
            return Ok(());
        }
    };

    let eoi = SimpleSpan::new(src.len(), src.len());
    let script = match parse_script(tokens, eoi) {
        (Some(t), errs) if errs.is_empty() => t,
        (_, errs) => {
            errors(path.as_deref(), errs, "[Lamb] Parser Errors:");
            return Ok(());
        }
    };

    ffi::run_script(
        &script,
        debug_level == DebugLevel::Full,
        debug_level != DebugLevel::None,
    );

    Ok(())
}

fn repl_input() -> std::io::Result<Option<String>> {
    let mut input = String::new();
    let mut line = String::new();

    let mut stdin = std::io::stdin().lock();
    let mut stdout = std::io::stdout().lock();
    let mut write = |b| -> std::io::Result<()> {
        stdout.write(b)?;
        stdout.flush()?;
        Ok(())
    };

    write(REPL_START)?;
    write(b">>> ")?;

    while stdin.read_line(&mut line)? != 0 {
        match line.as_str().trim() {
            ":quit" => return Ok(None),
            ":run" => break,
            _ => input.push_str(line.as_str()),
        }

        write(b">>> ")?;
        line.clear();
    }

    write(b"\n")?;
    Ok(Some(input))
}

fn parse_script<'a, I>(toks: I, eoi: SimpleSpan) -> (Option<Script>, Vec<Rich<'a, Token>>)
where
    I: IntoIterator<Item = (Token, SimpleSpan)> + 'a,
{
    parse::script()
        .parse(Stream::from_iter(toks.into_iter()).spanned(eoi))
        .into_output_errors()
}
