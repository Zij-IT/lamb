#![warn(clippy::pedantic)]
#![feature(trait_alias)]

mod ast;
mod error;
mod lexer;
mod parser;
mod span;
mod token;

fn main() {
    let sample = include_str!("../examples/errors.lb");
    let sample_src = "../examples/errors.lb";

    let _res = ast_from_source(sample, sample_src);
}

fn ast_from_source(src_code: &str, _src_name: &str) -> Result<Vec<ast::Expr>, ()> {
    use chumsky::Parser;

    let len = src_code.chars().count();
    let eof = len..len;

    let (tokens, lex_errors) = lexer::lexer().parse_recovery(chumsky::Stream::from_iter(
        eof.clone(),
        src_code.chars().enumerate().map(|(i, c)| (c, i..i + 1)),
    ));

    let tokens = tokens.ok_or(())?;
    let (exprs, parse_errors) = parser::parser().parse_recovery(chumsky::Stream::from_iter(
        eof,
        tokens.into_iter().map(span::Spanned::into_tuple),
    ));

    if lex_errors.is_empty() && parse_errors.is_empty() {
        Ok(exprs.unwrap_or_default())
    } else {
        for e in lex_errors.into_iter() {
            e.eprint(src_code).unwrap();
        }
        for e in parse_errors.into_iter() {
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

        let _res = stdout.write_all(".> ".as_bytes());
        let _res = stdout.flush();

        if stdin.read_line(&mut buffer).is_ok() && process_input(&buffer).is_err() {
            continue;
        }
    }
}

fn process_input(src: &str) -> Result<(), ()> {
    use chumsky::Parser;

    let tokens = ok_and_print(lexer::lexer().parse(src)).map(|x| {
        x.into_iter()
            .map(span::Spanned::into_inner)
            .collect::<Vec<_>>()
    })?;

    ok_and_print(parser::parser().parse(&*tokens)).map(|_| ())
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
