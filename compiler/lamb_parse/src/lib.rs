mod error;
mod parse;
mod tokenize;

use chumsky::{
    extra,
    input::{SpannedInput, Stream},
    prelude::{Input, Rich},
    span::SimpleSpan,
    Parser,
};
pub use error::SyntaxError;
use tokenize::Token;

type StreamInp = SpannedInput<Token, SimpleSpan, Stream<std::vec::IntoIter<(Token, SimpleSpan)>>>;

pub type SyntaxResult<'a, T> = Result<T, Vec<SyntaxError<'a>>>;

pub fn script(src: &str) -> SyntaxResult<lamb_ast::Script> {
    parse(src, parse::script())
}

pub fn statement(src: &str) -> SyntaxResult<lamb_ast::Statement> {
    parse(src, parse::statement())
}

pub fn expr(src: &str) -> SyntaxResult<lamb_ast::Expr> {
    parse(src, parse::expr())
}

fn parse<'a, T, P>(src: &'a str, parser: P) -> Result<T, Vec<SyntaxError>>
where
    P: Parser<'a, StreamInp, T, extra::Err<Rich<'a, Token, SimpleSpan>>>,
{
    let tokens = match tokenize::lamb().parse(src).into_output_errors() {
        (Some(t), errs) if errs.is_empty() => t,
        (_, errs) => {
            return Err(errs.into_iter().map(SyntaxError::Lexical).collect());
        }
    };

    let eoi = SimpleSpan::new(src.len(), src.len());
    match parser
        .parse(Stream::from_iter(tokens).spanned(eoi))
        .into_output_errors()
    {
        (Some(t), errs) if errs.is_empty() => Ok(t),
        (_, errs) => Err(errs.into_iter().map(SyntaxError::Syntactic).collect()),
    }
}
