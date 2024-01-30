mod ast;
mod error;
mod node;
mod parse;
mod tokenize;

pub use ast::*;
use chumsky::{
    extra,
    input::{SpannedInput, Stream},
    prelude::{Input, Rich},
    span::SimpleSpan,
    Parser,
};
pub use error::SyntaxError;
pub use node::Node;
use tokenize::Token;

type StreamInp = SpannedInput<Token, SimpleSpan, Stream<std::vec::IntoIter<(Token, SimpleSpan)>>>;

pub type Span = SimpleSpan;
pub type SourceNode<T> = Node<T, Span>;
pub type SyntaxResult<'a, T> = Result<T, Vec<SyntaxError<'a>>>;

pub fn script(src: &str) -> SyntaxResult<SourceNode<Script>> {
    parse(
        src,
        parse::script().map_with(|script, extra| SourceNode::new(script, extra.span())),
    )
}

pub fn statement(src: &str) -> SyntaxResult<SourceNode<Statement>> {
    parse(
        src,
        parse::statement().map_with(|stat, extra| SourceNode::new(stat, extra.span())),
    )
}

pub fn expr(src: &str) -> SyntaxResult<SourceNode<Expr>> {
    parse(
        src,
        parse::expr().map_with(|expr, extra| SourceNode::new(expr, extra.span())),
    )
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
    use crate::SourceNode;

    use super::ast::{Block, Export, Exportable, Ident, Import, Script};

    #[test]
    fn parses_exports() {
        let input = r#"
            export { main, bacon, tomato };
        "#;

        assert_eq!(
            super::script(input).map(SourceNode::into_inner),
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
            super::script(input).map(SourceNode::into_inner),
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
            super::script(input).map(SourceNode::into_inner),
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
            super::script(input).map(SourceNode::into_inner),
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
            super::script(input).map(SourceNode::into_inner),
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
