use super::ast;
use super::error::{ParseError, SyntaxPattern};
use super::token::Token;

use chumsky::prelude::*;

pub trait LambParser<T> = Parser<Token, T, Error = ParseError> + Clone;

pub fn parser() -> impl LambParser<Vec<ast::Expr>> {
    parse_raw_literal().map(ast::Expr::Literal).map(|x| vec![x])
}

pub fn parse_program() -> impl LambParser<ast::Program> {
    parse_exports()
        .or_not()
        .then(parse_imports().repeated())
        .then(parse_statement().repeated())
        .then_ignore(end())
        // .then(parse_expression().or_not())
        .map(|((exports, imports), statements)| ast::Program {
            exports,
            imports,
            statements,
            final_expr: None,
        })
}

fn parse_exports() -> impl LambParser<ast::Export> {
    just(Token::Export)
        .ignore_then(just(Token::ParenOpen))
        .ignore_then(
            parse_raw_ident()
                .separated_by(just(Token::Comma))
                .allow_trailing(),
        )
        .then_ignore(just(Token::ParenClose))
        .map(|exports| ast::Export { exports })
}

fn parse_imports() -> impl LambParser<ast::Import> {
    just(Token::Import)
        .ignore_then(parse_raw_ident())
        .then_ignore(just(Token::ParenOpen))
        .then(
            parse_raw_ident()
                .separated_by(just(Token::Comma))
                .allow_trailing(),
        )
        .then_ignore(just(Token::ParenClose))
        .map(|(from, imports)| ast::Import { from, imports })
}

fn parse_statement() -> impl LambParser<ast::Statement> {
    recursive(|stmt| {
        let expr = recursive(|expr| {
            let with_block = parse_expression_with_block(expr.clone(), stmt.clone());
            let without_block = parse_expression_without_block(expr, stmt.clone());

            without_block.or(with_block)
        });

        let with_block = parse_expression_with_block(expr.clone(), stmt.clone());
        let without_block = parse_expression_without_block(expr.clone(), stmt);

        choice((
            parse_definition(expr),
            parse_expression_statement(with_block, without_block),
            just(Token::Semicolon).to(ast::Statement::Empty),
        ))
    })
}

fn parse_expression_with_block<Expr, Stmt>(expr: Expr, stmt: Stmt) -> impl LambParser<ast::Expr>
where
    Expr: LambParser<ast::Expr>,
    Stmt: LambParser<ast::Statement>,
{
    let block_expr = just(Token::BraceOpen)
        .ignore_then(stmt.clone().repeated().then(expr.clone().or_not()))
        .then_ignore(just(Token::BraceClose))
        .map(|(stmts, end_expr)| ast::Expr::BlockExpression(stmts, end_expr.map(Box::new)));

    let loop_loop = just(Token::Loop)
        .ignore_then(block_expr.clone())
        .map(|expr| ast::Expr::Loop(Box::new(expr)));

    let for_loop = just(Token::For)
        .ignore_then(parse_raw_ident())
        .then_ignore(just(Token::In))
        .then(expr.clone())
        .then(block_expr.clone())
        .map(|((ident, range), block)| ast::Expr::For(ident, Box::new(range), Box::new(block)));

    let while_loop = just(Token::While)
        .ignore_then(expr.clone())
        .then(block_expr.clone())
        .map(|(cond, block)| ast::Expr::While(Box::new(cond), Box::new(block)));

    let if_expr = just(Token::If)
        .ignore_then(expr.clone())
        .then(block_expr.clone())
        .then(
            just(Token::Elif)
                .ignore_then(expr.clone())
                .then(block_expr.clone())
                .repeated(),
        )
        .then(just(Token::Else).ignore_then(block_expr.clone()).or_not())
        .map(|(((cond, block), elifs), els)| {
            ast::Expr::If(Box::new(cond), Box::new(block), elifs, els.map(Box::new))
        });

    block_expr
        .or(loop_loop)
        .or(for_loop)
        .or(while_loop)
        .or(if_expr)
}

fn parse_expression_without_block<Expr, Stmt>(expr: Expr, stmt: Stmt) -> impl LambParser<ast::Expr>
where
    Expr: LambParser<ast::Expr>,
    Stmt: LambParser<ast::Statement>,
{
    let lit = parse_raw_literal().map(ast::Expr::Literal);

    let ident = parse_raw_ident().map(ast::Expr::Ident);

    lit.or(ident)
}

fn parse_definition(expr: impl LambParser<ast::Expr>) -> impl LambParser<ast::Statement> {
    let value_def = parse_raw_ident()
        .then_ignore(just(Token::Assign))
        .then(expr.clone())
        .then_ignore(just(Token::Semicolon))
        .map(|(ident, val)| ast::Statement::ValueDefinition(ident, val));

    let func_def = parse_raw_ident()
        .then_ignore(just(Token::Assign))
        .then_ignore(just(Token::Lambda))
        .then(parse_raw_ident().separated_by(just(Token::Comma)))
        .then(expr)
        .then_ignore(just(Token::Semicolon))
        .map(|((ident, params), expr)| ast::Statement::FunctionDefinition(ident, params, expr));

    func_def.or(value_def)
}

fn parse_expression_statement<Block, NoBlock>(
    block: Block,
    no_block: NoBlock,
) -> impl LambParser<ast::Statement>
where
    Block: LambParser<ast::Expr>,
    NoBlock: LambParser<ast::Expr>,
{
    no_block
        .then_ignore(just(Token::Semicolon))
        .or(block.then_ignore(just(Token::Semicolon).or_not()))
        .map(ast::Statement::ExprStmt)
}

fn parse_raw_literal() -> impl LambParser<ast::Literal> {
    // Result<Result<ast::Literal, invalid_int_literal>, expected_input_found>
    // Having the nested error allows for better error messages, so I am okay with the verbosity.
    filter_map(|span, tok: Token| match tok {
        Token::Str(s) => Ok(Ok(ast::Literal::Str(s))),
        Token::Char(c) => Ok(Ok(ast::Literal::Char(c))),
        Token::Bool(b) => Ok(Ok(ast::Literal::Bool(b))),
        Token::Int(i) => match i.parse() {
            Ok(i) => Ok(Ok(ast::Literal::Int(i))),
            Err(_) => Ok(Err(ParseError::invalid_int_literal(span))),
        },
        Token::Real(r) => Ok(Ok(ast::Literal::Real(
            r.parse().expect(&format!("Invalid real literal: {}", r)),
        ))),
        _ => Err(ParseError::expected_input_found(
            span,
            [Some(SyntaxPattern::Literal)],
            Some(SyntaxPattern::Token(tok)),
        )),
    })
    .validate(|x, span, emit| {
        match x {
            Ok(x) => x,
            Err(pe) => {
                emit(pe);

                // Dummy Value to allow the parser to continue as if everything was fine
                ast::Literal::Int(0)
            }
        }
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parses_exports() {
        // export(add_one, add_two)
        let tokens = [
            Token::Export,
            Token::ParenOpen,
            Token::Ident("add_one".into()),
            Token::Comma,
            Token::Ident("add_two".into()),
            Token::ParenClose,
        ];

        let res = parse_exports().parse(&tokens);
        assert_eq!(
            res,
            Ok(ast::Export {
                exports: vec![
                    ast::Ident::new("add_one".into()),
                    ast::Ident::new("add_two".into())
                ]
            })
        );
    }

    #[test]
    fn parses_imports() {
        // import other_module(add_one, add_two)
        let tokens = [
            Token::Import,
            Token::Ident("other_module".into()),
            Token::ParenOpen,
            Token::Ident("add_one".into()),
            Token::Comma,
            Token::Ident("add_two".into()),
            Token::ParenClose,
        ];

        let res = parse_imports().parse(&tokens);
        assert_eq!(
            res,
            Ok(ast::Import {
                from: ast::Ident::new("other_module".into()),
                imports: vec![
                    ast::Ident::new("add_one".into()),
                    ast::Ident::new("add_two".into())
                ]
            })
        );
    }
}
