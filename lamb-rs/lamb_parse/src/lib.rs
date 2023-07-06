mod error;
mod parse;
mod tokenize;

use chumsky::{input::Stream, prelude::Input, span::SimpleSpan, Parser};
pub use error::SyntaxError;

pub fn parse_script(src: &str) -> Result<lamb_ast::Script, Vec<SyntaxError>> {
    let tokens = match tokenize::lamb().parse(src).into_output_errors() {
        (Some(t), errs) if errs.is_empty() => t,
        (_, errs) => {
            return Err(errs.into_iter().map(SyntaxError::Lexical).collect());
        }
    };

    let eoi = SimpleSpan::new(src.len(), src.len());
    match parse::script()
        .parse(Stream::from_iter(tokens.into_iter()).spanned(eoi))
        .into_output_errors()
    {
        (Some(t), errs) if errs.is_empty() => Ok(t),
        (_, errs) => Err(errs.into_iter().map(SyntaxError::Syntactic).collect()),
    }
}
