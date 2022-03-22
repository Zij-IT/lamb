use super::ast;
use super::error::{ParseError, SyntaxPattern};
use super::token::Token;

use chumsky::prelude::*;

pub trait LambParser<T> = Parser<Token, T, Error = ParseError> + Clone;

pub fn parser() -> impl LambParser<Vec<ast::Expr>> {
    parse_literal().map(ast::Expr::Literal).map(|x| vec![x])
}

fn parse_literal() -> impl LambParser<ast::Literal> {
    // TODO: Add error handling for invalid literals because if the compiler
    //       panics, we have a problem.
    filter_map(|span, tok: Token| {
        Ok(match tok {
            Token::Str(s) => ast::Literal::Str(s),
            Token::Char(c) => ast::Literal::Char(c),
            Token::Bool(b) => ast::Literal::Bool(b),
            Token::Int(i) => match i.parse() {
                Ok(i) => ast::Literal::Int(i),
                Err(_) => return Err(ParseError::invalid_int_literal(span)),
            },
            Token::Real(r) => {
                ast::Literal::Real(r.parse().expect(&format!("Invalid real literal: {}", r)))
            }
            _ => {
                return Err(ParseError::expected_input_found(
                    span,
                    [Some(SyntaxPattern::Literal)],
                    Some(SyntaxPattern::Token(tok)),
                ))
            }
        })
    })
}

fn parse_raw_ident() -> impl LambParser<ast::Ident> {
    filter_map(|span, tok: Token| {
        Ok(match tok {
            Token::Ident(s) => ast::Ident::new(s),
            _ => {
                return Err(ParseError::expected_input_found(
                    span,
                    [Some(SyntaxPattern::Ident)],
                    Some(SyntaxPattern::Token(tok)),
                ))
            }
        })
    })
}
