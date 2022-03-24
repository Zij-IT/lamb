#![warn(clippy::pedantic)]
#![allow(clippy::range_plus_one)]
#![allow(clippy::module_name_repetitions)]
#![feature(trait_alias)]

use lamb_parse::ast;
use lamb_parse::error::LexError;
use lamb_parse::span::Spanned;
use lamb_parse::token::Token;

mod lamb_parse;

fn main() {
    let sample = include_str!("../examples/errors.lb");
    let sample_src = "../examples/errors.lb";

    // let _res = dbg!(ast_from_source(sample, sample_src));

    repl();
}

fn tokens_from_source(
    src_code: &str,
    _src_name: &str,
) -> Result<Vec<Spanned<Token>>, Vec<LexError>> {
    use chumsky::Parser;
    use lamb_parse::{lexer, parser, span};

    let len = src_code.chars().count();
    let eof = len..len;

    let (tokens, lex_errors) = lexer::lexer().parse_recovery(chumsky::Stream::from_iter(
        eof,
        src_code.chars().enumerate().map(|(i, c)| (c, i..i + 1)),
    ));

    tokens.ok_or(lex_errors)
}

fn ast_from_source(src_code: &str, _src_name: &str) -> Result<ast::Program, ()> {
    use chumsky::Parser;
    use lamb_parse::{lexer, parser, span};

    let len = src_code.chars().count();
    let eof = len..len;

    let (tokens, lex_errors) = lexer::lexer().parse_recovery(chumsky::Stream::from_iter(
        eof.clone(),
        src_code.chars().enumerate().map(|(i, c)| (c, i..i + 1)),
    ));

    let tokens = tokens.ok_or(())?;
    let (exprs, parse_errors) = parser::parse_program().parse_recovery(chumsky::Stream::from_iter(
        eof,
        tokens.into_iter().map(span::Spanned::into_tuple),
    ));

    if lex_errors.is_empty() && parse_errors.is_empty() {
        let exprs = exprs.unwrap();
        println!("{:?}", exprs);
        Ok(exprs)
    } else {
        for e in lex_errors {
            e.eprint(src_code).unwrap();
        }
        for e in parse_errors {
            e.eprint(src_code).unwrap();
        }

        Err(())
    }
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

        let _ = stdin.read_line(&mut buffer).is_ok()
            &&  ast_from_source(&buffer, "").is_ok();
    }
}