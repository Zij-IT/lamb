mod parse;
mod tokenize;

use std::path::Path;

use tokenize::Token;

use chumsky::{
    input::Stream,
    prelude::{Input, Rich},
    span::SimpleSpan,
    Parser,
};

pub fn parse_script<'p, F1, F2, P>(
    src: &str,
    path: P,
    mut on_errs_c: F1,
    mut on_errs_t: F2,
) -> Result<lamb_ast::Script, ()>
where
    P: Into<Option<&'p Path>>,
    F1: FnMut(P, &[Rich<char>], String),
    F2: FnMut(P, &[Rich<Token>], String),
{
    let tokens = match tokenize::lamb().parse(src).into_output_errors() {
        (Some(t), errs) if errs.is_empty() => t,
        (_, errs) => {
            on_errs_c(path, &errs, "[Lamb] Lexer Errors:".into());
            return Err(());
        }
    };

    let eoi = SimpleSpan::new(src.len(), src.len());
    match parse::script()
        .parse(Stream::from_iter(tokens.into_iter()).spanned(eoi))
        .into_output_errors()
    {
        (Some(t), errs) if errs.is_empty() => Ok(t),
        (_, errs) => {
            on_errs_t(path, &errs, "[Lamb] Parser Errors:".into());
            Err(())
        }
    }
}
