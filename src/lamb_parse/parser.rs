use super::ast;
use super::error::{ParseError, SyntaxPattern};
use super::token::Token;
use super::span::Spanned;

use chumsky::prelude::*;

pub trait LambParser<T> = Parser<Token, T, Error = ParseError> + Clone + 'static;

enum Chain {
    ListIndex(ast::Expr),
    Call(Vec<ast::Expr>),
}

pub fn parse_program() -> impl LambParser<Spanned<ast::Program>> {
    parse_exports()
        .map_with_span(Spanned::new)
        .or_not()
        .then(parse_imports().repeated())
        .then(parse_statement().clone().repeated())
        .then_ignore(end())
        .map(
            |((exports, imports), statements)| ast::Program {
                exports,
                imports,
                statements,
            },
        )
        .map_with_span(Spanned::new)
}

pub fn parse_raw_expression() -> impl LambParser<ast::Expr> {
    let stmt = parse_statement();
    parse_expression(stmt)
}

fn parse_exports() -> impl LambParser<ast::Export> {
    just(Token::Export)
        .ignore_then(just(Token::ParenOpen))
        .ignore_then(
            parse_raw_ident()
                .map_with_span(Spanned::new)
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
        let expr = parse_expression(stmt.clone());
        let with_block = parse_block_expression(expr.clone(), stmt);

        choice((
            parse_definition(expr.clone()),
            parse_expression_statement(with_block, expr),
            just(Token::Semicolon).to(ast::Statement::Empty),
        ))
    })
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

fn parse_expression<StmtP>(stmt: StmtP) -> impl LambParser<ast::Expr>
where
    StmtP: LambParser<ast::Statement>,
{
    recursive(|expr| {
        let block_expr = parse_block_expression(expr.clone(), stmt.clone());
        let chainable = parse_atom_expression(expr.clone()).or(block_expr.clone());
        let chain = parse_expression_chain(expr.clone(), chainable);
        let operators = parse_operator_expression(chain);
        let loop_expr = parse_loop_expression(expr);

        operators.or(loop_expr)
    })
}

fn parse_atom_expression(expr: impl LambParser<ast::Expr>) -> impl LambParser<ast::Expr> {
    let literal = parse_raw_literal().map(ast::Expr::Literal);

    let ident = parse_raw_ident().map(ast::Expr::Ident);

    let comma_list = expr
        .clone()
        .separated_by(just(Token::Comma))
        .allow_trailing();

    let list = just(Token::BrackOpen)
        .ignore_then(comma_list)
        .then_ignore(just(Token::BrackClose))
        .map(ast::Expr::List);

    let group = expr
        .clone()
        .delimited_by(just(Token::ParenOpen), just(Token::ParenClose));

    let tuple = just(Token::ParenOpen)
        .ignore_then(expr.clone().then_ignore(just(Token::Comma)))
        .then(expr.clone().repeated())
        .then_ignore(just(Token::ParenClose))
        .map(|(first, mut other)| {
            other.insert(0, first);
            ast::Expr::Tuple(other)
        });

    let lambda = just(Token::Lambda)
        .ignore_then(parse_raw_ident().separated_by(just(Token::Comma)))
        .then_ignore(just(Token::Arrow))
        .then(expr)
        .map(|(params, body)| ast::Expr::Lambda(params, Box::new(body)));

    literal.or(tuple).or(lambda).or(ident).or(group).or(list)
}

fn parse_expression_chain(
    expr: impl LambParser<ast::Expr>,
    chainable: impl LambParser<ast::Expr>,
) -> impl LambParser<ast::Expr> {
    let call = just(Token::ParenOpen)
        .ignore_then(
            expr.clone()
                .separated_by(just(Token::Comma))
                .allow_trailing(),
        )
        .then_ignore(just(Token::ParenClose))
        .map(Chain::Call);

    let list_index = just(Token::BrackOpen)
        .ignore_then(expr)
        .then_ignore(just(Token::BrackClose))
        .map(Chain::ListIndex);

    let chains = call.or(list_index);

    chainable
        .then(chains.repeated())
        .foldl(|expr, link| match link {
            Chain::Call(args) => ast::Expr::Call(Box::new(expr), args),
            Chain::ListIndex(idx) => ast::Expr::ListIndex(Box::new(expr), Box::new(idx)),
        })
}

fn parse_operator_expression(expr: impl LambParser<ast::Expr>) -> impl LambParser<ast::Expr> {
    let op = just(Token::Not)
        .to(ast::UnaryOp::Not)
        .or(just(Token::Minus).to(ast::UnaryOp::Neg));

    let unary = op
        .repeated()
        .then(expr)
        .foldr(|op, expr| ast::Expr::Unary(op, Box::new(expr)));

    // Arithmetic
    let op = just(Token::Div)
        .to(ast::BinaryOp::Div)
        .or(just(Token::Mul).to(ast::BinaryOp::Mul))
        .or(just(Token::Rem).to(ast::BinaryOp::Rem));

    let binary = next_binary(unary, op);

    let op = just(Token::Plus)
        .to(ast::BinaryOp::Add)
        .or(just(Token::Minus).to(ast::BinaryOp::Sub));

    let binary = next_binary(binary, op);

    // Bitwise
    let op = just(Token::BitAnd).to(ast::BinaryOp::BitAnd);

    let binary = next_binary(binary, op);

    let op = just(Token::BitXor).to(ast::BinaryOp::BitXor);

    let binary = next_binary(binary, op);

    let op = just(Token::BitOr).to(ast::BinaryOp::BitOr);

    let binary = next_binary(binary, op);

    // Concat
    let op = just(Token::Concat).to(ast::BinaryOp::Concat);

    let binary = next_binary(binary, op);

    let op = just(Token::DotDot).to(ast::BinaryOp::Range);

    let binary = next_binary(binary, op);

    // Logical
    let op = just(Token::Gt)
        .to(ast::BinaryOp::Gt)
        .or(just(Token::Lt).to(ast::BinaryOp::Lt))
        .or(just(Token::Ge).to(ast::BinaryOp::Ge))
        .or(just(Token::Le).to(ast::BinaryOp::Le));

    let binary = next_binary(binary, op);

    // Comparison
    let op = just(Token::NotEq)
        .to(ast::BinaryOp::NotEq)
        .or(just(Token::EqEq).to(ast::BinaryOp::EqEq));

    let binary = next_binary(binary, op);

    // Logical
    let op = just(Token::LogAnd)
        .to(ast::BinaryOp::LogAnd)
        .or(just(Token::LogOr).to(ast::BinaryOp::LogOr));

    let binary = next_binary(binary, op);

    // Function
    let op = just(Token::ApplyLeft)
        .to(ast::BinaryOp::ApplyLeft)
        .or(just(Token::ApplyRight).to(ast::BinaryOp::ApplyRight))
        .or(just(Token::ComposeLeft).to(ast::BinaryOp::ComposeLeft))
        .or(just(Token::ComposeRight).to(ast::BinaryOp::ComposeRight));

    next_binary(binary, op)
}

fn parse_loop_expression(expr: impl LambParser<ast::Expr>) -> impl LambParser<ast::Expr> {
    just(Token::Continue)
        .to(ast::Expr::Continue)
        .or(just(Token::Return)
            .ignore_then(expr.clone())
            .map(|x| ast::Expr::Return(Box::new(x))))
        .or(just(Token::Break)
            .ignore_then(expr)
            .map(|x| ast::Expr::Break(Box::new(x))))
}

fn parse_block_expression<ExprP, StmtP>(expr: ExprP, stmt: StmtP) -> impl LambParser<ast::Expr>
where
    ExprP: LambParser<ast::Expr>,
    StmtP: LambParser<ast::Statement>,
{
    let block_expr = just(Token::BraceOpen)
        .ignore_then(stmt.repeated().then(expr.clone().or_not()))
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
                .ignore_then(expr)
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

fn next_binary(
    prev: impl LambParser<ast::Expr>,
    op: impl LambParser<ast::BinaryOp>,
) -> impl LambParser<ast::Expr> {
    prev.clone()
        .then(op.then(prev).repeated())
        .foldl(|a, (op, b)| ast::Expr::Binary(op, Box::new(a), Box::new(b)))
        .boxed()
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
        Token::Real(r) => {
            Ok(Ok(ast::Literal::Real(r.parse().unwrap_or_else(|_| {
                panic!("Invalid real literal: {}", r)
            }))))
        }
        _ => Err(ParseError::expected_input_found(
            span,
            [Some(SyntaxPattern::Literal)],
            Some(SyntaxPattern::Token(tok)),
        )),
    })
    .validate(|x, _span, emit| {
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
