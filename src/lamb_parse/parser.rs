use super::ast;
use super::error::{ParseError, SyntaxPattern};
use super::token::Token;

use chumsky::prelude::*;

pub trait LambParser<T> = Parser<Token, T, Error = ParseError> + Clone;

pub fn parser() -> impl LambParser<Vec<ast::Expr>> {
    parse_expr().repeated().then_ignore(end())
}

// TODO: Refactor this... somehow...
fn parse_expr() -> impl LambParser<ast::Expr> {
    recursive(|expr| {
        let raw = {
            let literal = parse_literal().map(ast::Expr::Literal);
            let ident = parse_raw_ident().map(ast::Expr::Ident);

            let items = expr
                .clone()
                .separated_by(just(Token::Comma))
                .allow_trailing()
                .or_not()
                .map(Option::unwrap_or_default);

            let list = items
                .clone()
                .delimited_by(just(Token::BrackOpen), just(Token::BrackClose))
                .map(ast::Expr::List);

            let tuple = items
                .clone()
                .delimited_by(just(Token::ParenOpen), just(Token::ParenClose))
                .map(ast::Expr::Tuple);

            let parens = expr
                .clone()
                .delimited_by(just(Token::ParenOpen), just(Token::ParenClose));

            let func_call = parse_raw_ident()
                .then(
                    items
                        .clone()
                        .delimited_by(just(Token::ParenOpen), just(Token::ParenClose)),
                )
                .map(|(i, args)| ast::Expr::Call(i, args));

            let atom = parse_definition(expr.clone())
                .or(func_call)
                .or(literal)
                .or(ident)
                .or(list)
                .or(parens)
                .or(tuple)
                .recover_with(nested_delimiters(
                    Token::ParenOpen,
                    Token::ParenClose,
                    [
                        (Token::BrackOpen, Token::BrackClose),
                        (Token::BraceOpen, Token::BraceClose),
                    ],
                    |_| ast::Expr::Error,
                ))
                .recover_with(nested_delimiters(
                    Token::BrackOpen,
                    Token::BrackClose,
                    [
                        (Token::ParenOpen, Token::ParenClose),
                        (Token::BraceOpen, Token::BraceClose),
                    ],
                    |_| ast::Expr::Error,
                ));

            let unary = just(Token::Not)
                .to(ast::UnaryOp::Not)
                .or(just(Token::Minus).to(ast::UnaryOp::Neg))
                .repeated()
                .then(atom)
                .foldr(|op, expr| ast::Expr::Unary(op, Box::new(expr)));

            let op = just(Token::Div)
                .to(ast::BinaryOp::Div)
                .or(just(Token::Mul).to(ast::BinaryOp::Mul))
                .or(just(Token::Rem).to(ast::BinaryOp::Rem));

            let binary = next_binary(unary /*.boxed()*/, op);

            let op = just(Token::Plus)
                .to(ast::BinaryOp::Add)
                .or(just(Token::Minus).to(ast::BinaryOp::Sub));

            let binary = next_binary(binary, op);

            let op = just(Token::BitAnd).to(ast::BinaryOp::BitAnd);
            let binary = next_binary(binary, op);

            let op = just(Token::BitXor).to(ast::BinaryOp::BitXor);
            let binary = next_binary(binary, op);

            let op = just(Token::BitOr).to(ast::BinaryOp::BitOr);
            let binary = next_binary(binary, op);

            let op = just(Token::Concat).to(ast::BinaryOp::Concat);
            let binary = next_binary(binary, op);

            let op = just(Token::Gt)
                .to(ast::BinaryOp::Gt)
                .or(just(Token::Lt).to(ast::BinaryOp::Lt))
                .or(just(Token::Ge).to(ast::BinaryOp::Ge))
                .or(just(Token::Le).to(ast::BinaryOp::Le))
                .or(just(Token::NotEq).to(ast::BinaryOp::NotEq))
                .or(just(Token::EqEq).to(ast::BinaryOp::EqEq));

            let binary = next_binary(binary, op);

            let op = just(Token::LogAnd)
                .to(ast::BinaryOp::LogAnd)
                .or(just(Token::LogOr).to(ast::BinaryOp::LogOr));

            next_binary(binary, op)
        };

        let block = expr
            .clone()
            .repeated()
            .delimited_by(just(Token::BraceOpen), just(Token::BraceClose))
            .map(ast::Expr::Block);

        let iff = just(Token::If)
            .ignore_then(expr.clone())
            .then(block.clone())
            .then(
                just(Token::Elif)
                    .ignore_then(expr)
                    .then(block.clone())
                    .repeated(),
            )
            .then(just(Token::Else).ignore_then(block.clone()).or_not())
            .map(|((ifs, elifs), else_block)| {
                ast::Expr::If(
                    (Box::new(ifs.0), Box::new(ifs.1)),
                    elifs,
                    else_block.map(Box::new),
                )
            });

        raw.or(block).or(iff)
    })
}

fn parse_literal() -> impl LambParser<ast::Literal> {
    // TODO: Add error handling for invalid literals because if the compiler
    //       panics, we have a problem.
    filter_map(|span, tok: Token| {
        Ok(match tok {
            Token::Str(s) => ast::Literal::Str(s),
            Token::Char(c) => ast::Literal::Char(c),
            Token::Bool(b) => ast::Literal::Bool(b),
            Token::Int(i) => ast::Literal::Int(i.parse().unwrap()),
            Token::Real(r) => ast::Literal::Real(r.parse().unwrap()),
            // TODO: Figure out what to make the 'expected' Literal
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
            // TODO: Figure out what to make the 'expected' Ident
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

fn next_binary(
    prev: impl LambParser<ast::Expr> + 'static,
    op: impl LambParser<ast::BinaryOp> + 'static,
) -> impl LambParser<ast::Expr> + 'static {
    prev.clone()
        .then(op.then(prev).repeated())
        .foldl(|a, (op, b)| ast::Expr::Binary(Box::new(a), op, Box::new(b)))
        .boxed()
}

fn parse_definition(expr: impl LambParser<ast::Expr>) -> impl LambParser<ast::Expr> {
    let value_def = parse_raw_ident()
        .then_ignore(just(Token::Assign))
        .then(expr.clone())
        .map(|(i, e)| ast::Expr::ValueDef(i, Box::new(e)));

    let func_def = parse_raw_ident()
        .then_ignore(just(Token::Assign))
        .then(
            parse_raw_ident()
                .separated_by(just(Token::Comma))
                .allow_trailing(),
        )
        .then_ignore(just(Token::Arrow))
        .then(expr)
        .map(|((i, is), e)| ast::Expr::FunctionDef(i, is, Box::new(e)));

    func_def.or(value_def)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn simple_literal() {
        let lit = parse_expr().parse(&[Token::Int("1234".into())]);
        assert_eq!(lit, Ok(ast::Expr::Literal(ast::Literal::Int(1234))));
    }

    #[test]
    fn simple_unary() {
        let lit = parse_expr().parse(&[Token::Not, Token::Bool(false)]);
        assert_eq!(
            lit,
            Ok(ast::Expr::Unary(
                ast::UnaryOp::Not,
                Box::new(ast::Expr::Literal(ast::Literal::Bool(false)))
            ))
        );
    }

    #[test]
    fn nested_unary() {
        let lit = parse_expr().parse(&[Token::Not, Token::Not, Token::Not, Token::Bool(false)]);
        let unary_not = |inner| ast::Expr::Unary(ast::UnaryOp::Not, Box::new(inner));

        assert_eq!(
            lit,
            Ok(unary_not(unary_not(unary_not(ast::Expr::Literal(
                ast::Literal::Bool(false)
            )))))
        );
    }

    #[test]
    fn simple_binary() {
        let lit = parse_expr().parse(&[
            Token::Int("123".into()),
            Token::Plus,
            Token::Int("321".into()),
        ]);

        assert_eq!(
            lit,
            Ok(ast::Expr::Binary(
                Box::new(ast::Expr::Literal(ast::Literal::Int(123))),
                ast::BinaryOp::Add,
                Box::new(ast::Expr::Literal(ast::Literal::Int(321)))
            ))
        );
    }

    #[test]
    fn nested_binary() {
        let lit = parse_expr().parse(&[
            Token::ParenOpen,
            Token::Int("123".into()),
            Token::Plus,
            Token::Int("321".into()),
            Token::ParenClose,
            Token::Mul,
            Token::Int("1".into()),
            Token::Div,
            Token::Int("2".into()),
        ]);

        let binary = |lhs, op, rhs| ast::Expr::Binary(Box::new(lhs), op, Box::new(rhs));

        let start = binary(
            ast::Expr::Literal(ast::Literal::Int(123)),
            ast::BinaryOp::Add,
            ast::Expr::Literal(ast::Literal::Int(321)),
        );

        let next = binary(
            start,
            ast::BinaryOp::Mul,
            ast::Expr::Literal(ast::Literal::Int(1)),
        );

        let next = binary(
            next,
            ast::BinaryOp::Div,
            ast::Expr::Literal(ast::Literal::Int(2)),
        );

        assert_eq!(lit, Ok(next));
    }

    #[test]
    fn mixed_unary_binary() {
        let lit = parse_expr().parse(&[
            Token::ParenOpen,
            Token::Int("123".into()),
            Token::Plus,
            Token::Int("321".into()),
            Token::ParenClose,
            Token::Mul,
            Token::Int("1".into()),
            Token::Div,
            Token::Not,
            Token::Int("2".into()),
        ]);

        let binary = |lhs, op, rhs| ast::Expr::Binary(Box::new(lhs), op, Box::new(rhs));

        let unary_not = |inner| ast::Expr::Unary(ast::UnaryOp::Not, Box::new(inner));

        let start = binary(
            ast::Expr::Literal(ast::Literal::Int(123)),
            ast::BinaryOp::Add,
            ast::Expr::Literal(ast::Literal::Int(321)),
        );

        let next = binary(
            start,
            ast::BinaryOp::Mul,
            ast::Expr::Literal(ast::Literal::Int(1)),
        );

        let next = binary(
            next,
            ast::BinaryOp::Div,
            unary_not(ast::Expr::Literal(ast::Literal::Int(2))),
        );

        assert_eq!(lit, Ok(next));
    }

    #[test]
    fn simple_list() {
        let lit = parse_expr().parse(&[Token::BrackOpen, Token::BrackClose]);

        assert_eq!(lit, Ok(ast::Expr::List(Vec::new())));
    }

    #[test]
    fn list_with_expr() {
        let lit = parse_expr().parse(&[
            Token::BrackOpen,
            Token::Int("1".into()),
            Token::Plus,
            Token::Int("2".into()),
            Token::Comma,
            Token::BrackClose,
        ]);

        assert_eq!(
            lit,
            Ok(ast::Expr::List(vec![ast::Expr::Binary(
                Box::new(ast::Expr::Literal(ast::Literal::Int(1))),
                ast::BinaryOp::Add,
                Box::new(ast::Expr::Literal(ast::Literal::Int(2))),
            )]))
        );
    }
}
