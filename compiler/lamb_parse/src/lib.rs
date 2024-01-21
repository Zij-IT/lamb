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

#[cfg(test)]
mod test {
    use lamb_ast::{Block, Export, Exportable, Ident, Import, Script};

    #[test]
    fn parses_exports() {
        let input = r#"
            export { main, bacon, tomato };
        "#;

        assert_eq!(
            super::script(input),
            Ok(Script {
                exports: Some(Export {
                    items: vec![
                        Exportable::new("main", None),
                        Exportable::new("bacon", None),
                        Exportable::new("tomato", None)
                    ]
                }),
                imports: vec![],
                block: Block::empty(),
            })
        );
    }

    #[test]
    fn parses_exports_none() {
        let input = r#"
            export { };
        "#;

        assert_eq!(
            super::script(input),
            Ok(Script {
                exports: Some(Export { items: vec![] }),
                imports: vec![],
                block: Block::empty(),
            })
        );
    }

    #[test]
    fn parses_qualified_import() {
        let input = r#"
            from "./hello/there" as T;
        "#;

        assert_eq!(
            super::script(input),
            Ok(Script {
                exports: None,
                imports: vec![Import {
                    path: "./hello/there".into(),
                    alias: Some(Ident("T".into())),
                    imports: None,
                }],
                block: Block::empty(),
            })
        );
    }

    #[test]
    fn parses_non_qualified_import_items() {
        let input = r#"
            from "./hello/there" import (add);
        "#;

        assert_eq!(
            super::script(input),
            Ok(Script {
                exports: None,
                imports: vec![Import {
                    path: "./hello/there".into(),
                    alias: None,
                    imports: Some(vec![Ident("add".into())])
                }],
                block: Block::empty(),
            })
        );
    }

    #[test]
    fn parses_qualified_import_items() {
        let input = r#"
            from "./hello/there" as T import (add, sub);
        "#;

        assert_eq!(
            super::script(input),
            Ok(Script {
                exports: None,
                imports: vec![Import {
                    path: "./hello/there".into(),
                    alias: Some(Ident("T".into())),
                    imports: Some(vec![Ident("add".into()), Ident("sub".into()),]),
                }],
                block: Block::empty(),
            })
        );
    }
}
