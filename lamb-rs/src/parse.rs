use crate::{
    ast::{
        Assign, Atom, Binary, BinaryOp, Block, Case, CaseArm, Either, Elif, Else, Expr, FuncCall,
        FuncDef, Ident, If, Index, Literal, Script, Statement, Unary, UnaryOp,
    },
    token::{Delim, Op, Token},
};
use chumsky::{extra, prelude::Rich, primitive::choice, Parser};
use chumsky::{primitive::just, recursive::recursive, select, IterParser};

type T<'a> = &'a [Token];

type E<'a> = extra::Err<Rich<'a, Token>>;

// expr() and expr[] would lead the parser to reparse the expression
// if it's not parsed as: `expr` then (`()` or `[]`). To do this, a
// middle man is introduced
enum Chain {
    Index(Expr),
    Call(Vec<Expr>),
}

macro_rules! bin {
    ($tok:ident, $str:expr) => {{
        chumsky::pratt::left_infix(just(Token::Op(Op::$tok)), $str, |lhs, rhs| {
            Expr::Binary(Binary::new(lhs, rhs, BinaryOp::$tok))
        })
    }};
}

macro_rules! unary {
    ($op:expr, $uop:expr, $str:expr) => {{
        chumsky::pratt::prefix(just(Token::Op($op)), $str, |rhs| {
            Expr::Unary(Unary::new(rhs, $uop))
        })
    }};
}

pub fn script<'a>() -> impl Parser<'a, T<'a>, Script, E<'a>> {
    statement().repeated().collect().map(|stats| Script {
        block: Block { stats, value: None },
    })
}

pub fn statement<'a>() -> impl Parser<'a, T<'a>, Statement, E<'a>> {
    recursive(|stat| {
        let expr = recursive(|expr| {
            let literal = literal().map(|l| Expr::Atom(Atom::Literal(l)));

            let parend = parend(expr.clone());

            let array = array(expr.clone());

            let ident = ident().map(|i| Expr::Atom(Atom::Ident(i)));

            let block = block(stat.clone(), expr.clone());

            let if_ = if_(expr.clone(), block.clone());

            let fun_def = fun_def(expr.clone()).map(Expr::FuncDef);

            let case = case(stat.clone(), expr.clone()).map(Expr::Case);

            let atom = literal
                .or(parend)
                .or(array)
                .or(ident)
                .or(if_)
                .or(fun_def)
                .or(case)
                .or(block.map(Expr::Block));

            let chain = atom.foldl(
                call(expr.clone())
                    .map(Chain::Call)
                    .or(index(expr).map(Chain::Index))
                    .repeated(),
                |lhs, rhs| match rhs {
                    Chain::Index(i) => Expr::Index(Index {
                        indexee: Box::new(lhs),
                        index: Box::new(i),
                    }),
                    Chain::Call(args) => Expr::FuncCall(FuncCall {
                        callee: Box::new(lhs),
                        args,
                    }),
                },
            );

            let binaries = choice((
                bin!(LApply, 1),
                bin!(RApply, 1),
                bin!(LCompose, 2),
                bin!(RCompose, 2),
                bin!(LogAnd, 3),
                bin!(LogOr, 4),
                bin!(Gt, 5),
                bin!(Ge, 5),
                bin!(Lt, 5),
                bin!(Le, 5),
                bin!(Eq, 5),
                bin!(Ne, 5),
                bin!(BinOr, 6),
                bin!(BinXor, 7),
                bin!(BinAnd, 8),
                bin!(RShift, 9),
                bin!(LShift, 9),
                bin!(Add, 10),
                bin!(Sub, 10),
                bin!(Div, 11),
                bin!(Mul, 11),
                bin!(Mod, 11),
            ));

            let prefixes = choice((
                unary!(Op::Sub, UnaryOp::NumNeg, 12),
                unary!(Op::LogNot, UnaryOp::LogNot, 12),
                unary!(Op::BinComp, UnaryOp::BinNot, 12),
            ));

            chain.pratt(binaries).with_prefix_ops(prefixes)
        });

        let assign = select! { Token::Ident(i) => i }
            .then_ignore(just(Token::Define))
            .then(expr.clone())
            .then_ignore(just(Token::Semi))
            .map(|(i, e)| {
                Statement::Assign(Assign {
                    assignee: i.into(),
                    value: e,
                })
            });

        let expr_stmt = expr
            .clone()
            .then_ignore(just(Token::Semi))
            .map(Statement::Expr);

        let ret = just(Token::Return)
            .ignore_then(expr.or_not())
            .then_ignore(just(Token::Semi))
            .map(Statement::Return);

        assign.or(expr_stmt).or(ret)
    })
}

fn index<'a>(
    expr: impl Parser<'a, T<'a>, Expr, E<'a>> + Clone,
) -> impl Parser<'a, T<'a>, Expr, E<'a>> + Clone {
    expr.delimited_by(
        just(Token::Open(Delim::Brack)),
        just(Token::Close(Delim::Brack)),
    )
}

fn call<'a>(
    expr: impl Parser<'a, T<'a>, Expr, E<'a>> + Clone,
) -> impl Parser<'a, T<'a>, Vec<Expr>, E<'a>> + Clone {
    expr.separated_by(just(Token::Comma))
        .allow_trailing()
        .collect()
        .delimited_by(
            just(Token::Open(Delim::Paren)),
            just(Token::Close(Delim::Paren)),
        )
}

fn case<'a>(
    stat: impl Parser<'a, T<'a>, Statement, E<'a>> + Clone,
    expr: impl Parser<'a, T<'a>, Expr, E<'a>> + Clone,
) -> impl Parser<'a, T<'a>, Case, E<'a>> + Clone {
    just(Token::Case)
        .ignore_then(expr.clone().map(Box::new))
        .then(
            literal()
                .map(Either::Left)
                .or(ident().map(Either::Right))
                .then_ignore(just(Token::Arrow))
                .then(
                    block(stat, expr.clone())
                        .then_ignore(just(Token::Comma).or_not())
                        .map(Either::Left)
                        .or(expr.then_ignore(just(Token::Comma)).map(Either::Right)),
                )
                .map(|(pattern, on_match)| CaseArm { pattern, on_match })
                .repeated()
                .collect()
                .delimited_by(
                    just(Token::Open(Delim::Brace)),
                    just(Token::Close(Delim::Brace)),
                ),
        )
        .map(|(value, arms)| Case { value, arms })
}

fn fun_def<'a>(
    expr: impl Parser<'a, T<'a>, Expr, E<'a>> + Clone,
) -> impl Parser<'a, T<'a>, FuncDef, E<'a>> + Clone {
    just(Token::Rec)
        .or_not()
        .map(|o| o.is_some())
        .then_ignore(just(Token::Fn))
        .then(
            ident()
                .clone()
                .separated_by(just(Token::Comma))
                .allow_trailing()
                .collect()
                .delimited_by(
                    just(Token::Open(Delim::Paren)),
                    just(Token::Close(Delim::Paren)),
                ),
        )
        .then_ignore(just(Token::Arrow))
        .then(expr.map(Box::new))
        .map(|((is_recursive, args), body)| FuncDef {
            args,
            body,
            is_recursive,
        })
}

fn block<'a>(
    stat: impl Parser<'a, T<'a>, Statement, E<'a>> + Clone,
    expr: impl Parser<'a, T<'a>, Expr, E<'a>> + Clone,
) -> impl Parser<'a, T<'a>, Block, E<'a>> + Clone {
    stat.repeated()
        .collect()
        .then(expr.or_not())
        .delimited_by(
            just(Token::Open(Delim::Brace)),
            just(Token::Close(Delim::Brace)),
        )
        .map(|(stats, value)| Block {
            stats,
            value: value.map(Box::new),
        })
}

fn if_<'a>(
    expr: impl Parser<'a, T<'a>, Expr, E<'a>> + Clone,
    block: impl Parser<'a, T<'a>, Block, E<'a>> + Clone,
) -> impl Parser<'a, T<'a>, Expr, E<'a>> + Clone {
    just(Token::If)
        .ignore_then(expr.clone())
        .then(block.clone())
        .then(
            just(Token::Elif)
                .ignore_then(expr)
                .then(block.clone())
                .map(|(cond, block)| Elif { cond, block })
                .repeated()
                .collect(),
        )
        .then(
            just(Token::Else)
                .ignore_then(block)
                .map(|block| Box::new(Else { block }))
                .or_not(),
        )
        .map(|(((cond, block), elifs), els)| {
            Expr::If(If {
                cond: Box::new(cond),
                block: Box::new(block),
                elifs,
                els,
            })
        })
}

fn ident<'a>() -> impl Parser<'a, T<'a>, Ident, E<'a>> + Clone {
    select! { Token::Ident(i) => i.into() }
}

fn literal<'a>() -> impl Parser<'a, T<'a>, Literal, E<'a>> + Clone {
    select! {
        Token::Nil => Literal::Nil,
        Token::Num(l) => Literal::Num(l),
        Token::Bool(l) => Literal::Bool(l),
        Token::Char(l) => Literal::Char(l),
        Token::Str(l) => Literal::Str(l),
    }
}

fn parend<'a>(
    expr: impl Parser<'a, T<'a>, Expr, E<'a>> + Clone,
) -> impl Parser<'a, T<'a>, Expr, E<'a>> + Clone {
    expr.delimited_by(
        just(Token::Open(Delim::Paren)),
        just(Token::Close(Delim::Paren)),
    )
}

fn array<'a>(
    expr: impl Parser<'a, T<'a>, Expr, E<'a>> + Clone,
) -> impl Parser<'a, T<'a>, Expr, E<'a>> + Clone {
    expr.separated_by(just(Token::Comma))
        .allow_trailing()
        .collect()
        .map(|v| Expr::Atom(Atom::Array(v)))
        .delimited_by(
            just(Token::Open(Delim::Brack)),
            just(Token::Close(Delim::Brack)),
        )
}
