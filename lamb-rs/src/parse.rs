use crate::{
    ast::{
        Assign, Atom, Binary, BinaryOp, Block, Case, CaseArm, Either, Elif, Else, Expr, FuncCall,
        FuncDef, Ident, If, Index, Literal, Script, Statement, Unary, UnaryOp,
    },
    tokenize::{Delim, Op, Token},
};
use chumsky::{
    extra,
    input::ValueInput,
    prelude::{Input, Rich},
    primitive::{any, choice, none_of},
    recovery::{via_parser, ViaParser},
    util::MaybeSync,
    Parser,
};

use chumsky::{primitive::just, recursive::recursive, select, IterParser};

type E<'a, S> = extra::Err<Rich<'a, Token, S>>;

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

pub fn script<'a, I, S>() -> impl Parser<'a, I, Script, E<'a, S>>
where
    S: 'a,
    I: Input<'a, Token = Token, Span = S> + ValueInput<'a>,
{
    statement().repeated().collect().map(|stats| Script {
        block: Block { stats, value: None },
    })
}

pub fn statement<'a, I, S>() -> impl Parser<'a, I, Statement, E<'a, S>>
where
    S: 'a,
    I: Input<'a, Token = Token, Span = S> + ValueInput<'a>,
{
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

        let assign = ident()
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

fn index<'a, I, S>(
    expr: impl Parser<'a, I, Expr, E<'a, S>> + Clone,
) -> impl Parser<'a, I, Expr, E<'a, S>> + Clone
where
    S: 'a,
    I: Input<'a, Token = Token, Span = S> + ValueInput<'a>,
{
    safe_delimited(expr, Delim::Brack, |_| Expr::Error)
}

fn call<'a, I, S>(
    expr: impl Parser<'a, I, Expr, E<'a, S>> + Clone,
) -> impl Parser<'a, I, Vec<Expr>, E<'a, S>> + Clone
where
    S: 'a,
    I: Input<'a, Token = Token, Span = S> + ValueInput<'a>,
{
    let args = expr
        .separated_by(just(Token::Comma))
        .allow_trailing()
        .collect();

    safe_delimited(args, Delim::Paren, |_| Vec::new())
}

fn case<'a, I, S>(
    stat: impl Parser<'a, I, Statement, E<'a, S>> + Clone,
    expr: impl Parser<'a, I, Expr, E<'a, S>> + Clone,
) -> impl Parser<'a, I, Case, E<'a, S>> + Clone
where
    S: 'a,
    I: Input<'a, Token = Token, Span = S> + ValueInput<'a>,
{
    just(Token::Case)
        .ignore_then(expr.clone().map(Box::new))
        .then({
            let arm = literal()
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
                .collect();

            safe_delimited(arm, Delim::Brace, |_| Vec::new())
        })
        .map(|(value, arms)| Case { value, arms })
}

fn fun_def<'a, I, S>(
    expr: impl Parser<'a, I, Expr, E<'a, S>> + Clone,
) -> impl Parser<'a, I, FuncDef, E<'a, S>> + Clone
where
    S: 'a,
    I: Input<'a, Token = Token, Span = S> + ValueInput<'a>,
{
    just(Token::Rec)
        .or_not()
        .map(|o| o.is_some())
        .then_ignore(just(Token::Fn))
        .then({
            let ident = ident()
                .clone()
                .separated_by(just(Token::Comma))
                .allow_trailing()
                .collect();

            safe_delimited(ident, Delim::Paren, |_| Vec::new())
        })
        .then_ignore(just(Token::Arrow))
        .then(expr.map(Box::new))
        .map(|((is_recursive, args), body)| FuncDef {
            args,
            body,
            is_recursive,
        })
}

fn block<'a, I, S>(
    stat: impl Parser<'a, I, Statement, E<'a, S>> + Clone,
    expr: impl Parser<'a, I, Expr, E<'a, S>> + Clone,
) -> impl Parser<'a, I, Block, E<'a, S>> + Clone
where
    S: 'a,
    I: Input<'a, Token = Token, Span = S> + ValueInput<'a>,
{
    let block = stat
        .repeated()
        .collect()
        .then(expr.or_not())
        .map(|(stats, value)| Block {
            stats,
            value: value.map(Box::new),
        });

    safe_delimited(block, Delim::Brace, |_| Block {
        stats: Vec::new(),
        value: None,
    })
}

fn if_<'a, I, S>(
    expr: impl Parser<'a, I, Expr, E<'a, S>> + Clone,
    block: impl Parser<'a, I, Block, E<'a, S>> + Clone,
) -> impl Parser<'a, I, Expr, E<'a, S>> + Clone
where
    S: 'a,
    I: Input<'a, Token = Token, Span = S> + ValueInput<'a>,
{
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

fn ident<'a, I, S>() -> impl Parser<'a, I, Ident, E<'a, S>> + Clone
where
    S: 'a,
    I: Input<'a, Token = Token, Span = S> + ValueInput<'a>,
{
    select! {
        Token::Ident(i) => i.into()
    }
}

fn literal<'a, I, S>() -> impl Parser<'a, I, Literal, E<'a, S>> + Clone
where
    S: 'a,
    I: Input<'a, Token = Token, Span = S> + ValueInput<'a>,
{
    select! {
        Token::Nil => Literal::Nil,
        Token::Num(l) => Literal::Num(l),
        Token::Bool(l) => Literal::Bool(l),
        Token::Char(l) => Literal::Char(l),
        Token::Str(l) => Literal::Str(l),
    }
}

fn parend<'a, I, S>(
    expr: impl Parser<'a, I, Expr, E<'a, S>> + Clone,
) -> impl Parser<'a, I, Expr, E<'a, S>> + Clone
where
    S: 'a,
    I: Input<'a, Token = Token, Span = S> + ValueInput<'a>,
{
    safe_delimited(expr, Delim::Paren, |_| Expr::Error)
}

fn array<'a, I, S>(
    expr: impl Parser<'a, I, Expr, E<'a, S>> + Clone,
) -> impl Parser<'a, I, Expr, E<'a, S>> + Clone
where
    S: 'a,
    I: Input<'a, Token = Token, Span = S> + ValueInput<'a>,
{
    let values = expr
        .separated_by(just(Token::Comma))
        .allow_trailing()
        .collect()
        .map(|v| Expr::Atom(Atom::Array(v)));

    safe_delimited(values, Delim::Brack, |_| Expr::Error)
}

fn safe_delimited<'a, I, O, S, F, P>(
    p: P,
    delim: Delim,
    f: F,
) -> impl Parser<'a, I, O, E<'a, S>> + Clone
where
    S: 'a,
    I: ValueInput<'a, Token = Token, Span = S> + 'a,
    F: Fn(I::Span) -> O + Clone,
    P: Parser<'a, I, O, E<'a, S>> + Clone,
{
    p.delimited_by(just(Token::Open(delim)), just(Token::Close(delim)))
        .recover_with(delim_recovery(delim, f))
}

fn delim_recovery<'a, I, E, F, T>(delim: Delim, f: F) -> ViaParser<impl Parser<'a, I, T, E> + Clone>
where
    I: ValueInput<'a, Token = Token> + 'a,
    I::Token: PartialEq + Clone,
    E: extra::ParserExtra<'a, I>,
    F: Fn(I::Span) -> T + Clone,
{
    via_parser(nested_delimiters(
        Token::Open(delim),
        Token::Close(delim),
        [
            (Token::Open(Delim::Paren), Token::Close(Delim::Paren)),
            (Token::Open(Delim::Brack), Token::Close(Delim::Brack)),
            (Token::Open(Delim::Brace), Token::Close(Delim::Brace)),
        ],
        f,
    ))
}

fn nested_delimiters<'a, I, O, E, F, const N: usize>(
    start: I::Token,
    end: I::Token,
    others: [(I::Token, I::Token); N],
    fallback: F,
) -> impl Parser<'a, I, O, E> + Clone
where
    I: ValueInput<'a> + 'a,
    I::Token: PartialEq + Clone + MaybeSync,
    E: extra::ParserExtra<'a, I> + MaybeSync,
    F: Fn(I::Span) -> O + Clone,
{
    // TODO: Does this actually work? TESTS!
    recursive({
        let (start, end) = (start.clone(), end.clone());
        |block| {
            let mut many_block = Parser::boxed(
                block
                    .clone()
                    .delimited_by(just(start.clone()), just(end.clone())),
            );
            for (s, e) in &others {
                many_block = Parser::boxed(
                    many_block.or(block.clone().delimited_by(just(s.clone()), just(e.clone()))),
                );
            }

            let skip = [start, end]
                .into_iter()
                .chain(IntoIterator::into_iter(others).flat_map(|(s, e)| [s, e]))
                .collect::<Vec<_>>();

            many_block
                .or(any().and_is(none_of(skip)).ignored())
                .repeated()
        }
    })
    .delimited_by(just(start), just(end))
    .map_with_span(move |_, span| fallback(span))
}