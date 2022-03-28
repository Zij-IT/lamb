use chumsky::Parser;

pub mod ast;
pub mod error;
mod lexer;
mod parser;
mod span;
mod token;

pub fn parse_expression<S: AsRef<str>>(src_code: S) -> Result<ast::Expr, Vec<error::SyntaxError>> {
    let len = src_code.as_ref().chars().count();
    let eof = len..len;

    let (tokens, lex_errors) = lexer::lexer().parse_recovery(chumsky::Stream::from_iter(
        eof.clone(),
        src_code
            .as_ref()
            .chars()
            .enumerate()
            .map(|(i, c)| (c, i..i + 1)),
    ));

    let tokens = tokens.ok_or_else(|| {
        lex_errors
            .clone()
            .into_iter()
            .map(error::SyntaxError::LexError)
            .collect::<Vec<_>>()
    })?;

    let (expr, parse_errors) = parser::parse_raw_expression().parse_recovery(chumsky::Stream::from_iter(
        eof,
        tokens.into_iter().map(span::Spanned::into_tuple),
    ));

    let syn_errors = lex_errors
        .into_iter()
        .map(error::SyntaxError::LexError)
        .chain(parse_errors.into_iter().map(error::SyntaxError::ParseError))
        .collect::<Vec<_>>();

    if syn_errors.is_empty() {
        Ok(expr.unwrap())
    } else {
        Err(syn_errors)
    }
}

pub fn parse_source<S1, S2>(
    _src_name: S1,
    src_code: S2,
) -> Result<ast::Program, Vec<error::SyntaxError>>
where
    S1: AsRef<str>,
    S2: AsRef<str>,
{
    let len = src_code.as_ref().chars().count();
    let eof = len..len;

    let (tokens, lex_errors) = lexer::lexer().parse_recovery(chumsky::Stream::from_iter(
        eof.clone(),
        src_code
            .as_ref()
            .chars()
            .enumerate()
            .map(|(i, c)| (c, i..i + 1)),
    ));

    let tokens = tokens.ok_or_else(|| {
        lex_errors
            .clone()
            .into_iter()
            .map(error::SyntaxError::LexError)
            .collect::<Vec<_>>()
    })?;

    let (exprs, parse_errors) = parser::parse_program().parse_recovery(chumsky::Stream::from_iter(
        eof,
        tokens.into_iter().map(span::Spanned::into_tuple),
    ));

    let syn_errors = lex_errors
        .into_iter()
        .map(error::SyntaxError::LexError)
        .chain(parse_errors.into_iter().map(error::SyntaxError::ParseError))
        .collect::<Vec<_>>();

    if syn_errors.is_empty() {
        Ok(exprs.unwrap())
    } else {
        Err(syn_errors)
    }
}

