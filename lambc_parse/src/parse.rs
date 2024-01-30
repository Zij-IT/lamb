use crate::{
    ast,
    tokenize::{Delim, Op, Token},
    Node, SourceNode, Span,
};

use chumsky::{
    extra,
    input::ValueInput,
    prelude::{Input, Rich},
    primitive::{any, choice, just, none_of},
    recovery::{via_parser, ViaParser},
    recursive::recursive,
    select,
    util::MaybeSync,
    IterParser, Parser,
};

use super::{
    ArrayPattern, Assign, Atom, Block, Case, CaseArm, Either, Elif, Else, Export, Exportable, Expr,
    FuncCall, FuncDef, Ident, If, Import, Index, Literal, Pattern, PatternTop, Script, Statement,
    UnaryOp,
};

macro_rules! bin {
    ($tok:ident, $str:expr) => {{
        ::chumsky::pratt::infix(
            ::chumsky::pratt::left($str),
            ::chumsky::primitive::just(crate::tokenize::Token::Op(crate::tokenize::Op::$tok)),
            |lhs, _, rhs, sp| {
                crate::Expr::Binary(crate::Node::new(
                    crate::Binary::new(lhs, rhs, crate::BinaryOp::$tok),
                    sp,
                ))
            },
        )
    }};
}

macro_rules! unary {
    ($op:expr, $uop:expr, $str:expr) => {{
        ::chumsky::pratt::prefix(
            $str,
            ::chumsky::primitive::just(crate::tokenize::Token::Op($op)),
            |_, rhs, sp| crate::Expr::Unary(crate::Node::new(crate::Unary::new(rhs, $uop), sp)),
        )
    }};
}

// expr() and expr[] would lead the parser to reparse the expression
// if it's not parsed as: `expr` then (`()` or `[]`). To do this, a
// middle man is introduced in the form of `Chain`
enum Chain {
    Index(Expr),
    Call(Vec<Expr>),
}

type E<'a> = extra::Err<Rich<'a, Token, Span>>;

pub fn script<'a, I>() -> impl Parser<'a, I, Script, E<'a>>
where
    I: Input<'a, Token = Token, Span = Span> + ValueInput<'a>,
{
    export()
        .or_not()
        .then(import().repeated().collect())
        .then(statement().repeated().collect())
        .map(|((exports, imports), stats)| Script {
            exports,
            imports,
            block: Block { stats, value: None },
        })
}

pub fn import<'a, I>() -> impl Parser<'a, I, Import, E<'a>>
where
    I: Input<'a, Token = Token, Span = Span> + ValueInput<'a>,
{
    let alias = ident().filter(|i| i.0 == "as").ignore_then(ident());

    let import = ident()
        .filter(|i| i.0 == "import")
        .ignore_then(safe_delimited(
            ident()
                .separated_by(just(Token::Comma))
                .at_least(1)
                .allow_trailing()
                .collect(),
            Delim::Paren,
            |_| Vec::new(),
        ));

    ident()
        .filter(|i| i.0 == "from")
        .ignore_then(select! { Token::Str(s) => s.clone(), })
        .then(alias.or_not())
        .then(import.or_not())
        .then_ignore(just(Token::Semi))
        .map(|((path, alias), imports)| Import {
            path,
            alias,
            imports,
        })
}

pub fn export<'a, I>() -> impl Parser<'a, I, Export, E<'a>>
where
    I: Input<'a, Token = Token, Span = Span> + ValueInput<'a>,
{
    let exportable = ident()
        .then(
            ident()
                .filter(|i| i.0 == "as")
                .ignore_then(ident())
                .or_not(),
        )
        .map(|(name, alias)| Exportable { name, alias });

    ident()
        .filter(|i| i.0 == "export")
        .ignore_then(safe_delimited(
            exportable
                .separated_by(just(Token::Comma))
                .allow_trailing()
                .collect(),
            Delim::Brace,
            |_| Vec::new(),
        ))
        .then_ignore(just(Token::Semi))
        .map(|exports| Export { items: exports })
}

pub fn statement<'a, I>() -> impl Parser<'a, I, Statement, E<'a>>
where
    I: Input<'a, Token = Token, Span = Span> + ValueInput<'a>,
{
    stat_inner(expr())
}

pub fn expr<'a, I>() -> impl Parser<'a, I, Expr, E<'a>> + Clone
where
    I: Input<'a, Token = Token, Span = Span> + ValueInput<'a>,
{
    recursive(|expr| {
        // This cannot call `statement`, as that would result in infinite
        // recursion. A statment must be defined in terms of an expression
        // and in this case, we need the statements to be define by *THIS*
        // expr that is currently being constructed.
        //
        // The definitions are pulled apart for easier maintainability.
        let stat = stat_inner(expr.clone());

        let literal =
            literal().map_with(|l, ex| Expr::Atom(Node::new(Atom::Literal(l), ex.span())));

        let parend = parend(expr.clone());

        let array = array(expr.clone());

        let path = ident()
            .separated_by(just(Token::Colon).then(just(Token::Colon)))
            .at_least(2)
            .collect()
            .map_with(|segments, ex| {
                Expr::Path(SourceNode::new(ast::Path { segments }, ex.span()))
            });

        let ident = ident().map_with(|i, ex| Expr::Atom(Node::new(Atom::Ident(i), ex.span())));

        let block = block(stat.clone(), expr.clone());

        let if_ = if_(expr.clone(), block.clone());

        let fun_def =
            fun_def(expr.clone()).map_with(|fu, ex| Expr::FuncDef(Node::new(fu, ex.span())));

        let case = case(stat.clone(), expr.clone())
            .map_with(|ca, ex| Expr::Case(Node::new(ca, ex.span())));

        let atom = literal
            .or(parend)
            .or(array)
            .or(path)
            .or(ident)
            .or(if_)
            .or(fun_def)
            .or(case)
            .or(block.map_with(|bl, ex| Expr::Block(Node::new(bl, ex.span()))))
            .labelled("an expression");

        let chain = atom.foldl_with(
            call(expr.clone())
                .map(Chain::Call)
                .or(index(expr).map(Chain::Index))
                .repeated(),
            |lhs, rhs, ex| match rhs {
                Chain::Index(i) => Expr::Index(Node::new(
                    Index {
                        indexee: Box::new(lhs),
                        index: Box::new(i),
                    },
                    ex.span(),
                )),
                Chain::Call(args) => Expr::FuncCall(Node::new(
                    FuncCall {
                        callee: Box::new(lhs),
                        args,
                    },
                    ex.span(),
                )),
            },
        );

        chain.pratt((
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
            unary!(Op::Sub, UnaryOp::NumNeg, 12),
            unary!(Op::LogNot, UnaryOp::LogNot, 12),
            unary!(Op::BinComp, UnaryOp::BinNot, 12),
        ))
    })
}

fn stat_inner<'a, I>(
    expr: impl Parser<'a, I, Expr, E<'a>> + Clone,
) -> impl Parser<'a, I, Statement, E<'a>> + Clone
where
    I: Input<'a, Token = Token, Span = Span> + ValueInput<'a>,
{
    let assign = ident()
        .then_ignore(just(Token::Define))
        .then(expr.clone())
        .then_ignore(just(Token::Semi))
        .map_with(|(i, e), ex| {
            Statement::Assign(Node::new(
                Assign {
                    assignee: i,
                    value: e,
                },
                ex.span(),
            ))
        });

    let expr_stmt = expr
        .clone()
        .then_ignore(just(Token::Semi))
        .map_with(|expr, ex| Statement::Expr(Node::new(expr, ex.span())));

    let ret = just(Token::Return)
        .ignore_then(expr.or_not())
        .then_ignore(just(Token::Semi))
        .map_with(|expr, ex| Statement::Return(expr.map(|i| Node::new(i, ex.span()))));

    assign.or(expr_stmt).or(ret)
}

fn index<'a, I>(
    expr: impl Parser<'a, I, Expr, E<'a>> + Clone,
) -> impl Parser<'a, I, Expr, E<'a>> + Clone
where
    I: Input<'a, Token = Token, Span = Span> + ValueInput<'a>,
{
    safe_delimited(expr, Delim::Brack, |_| Expr::Error)
}

fn call<'a, I>(
    expr: impl Parser<'a, I, Expr, E<'a>> + Clone,
) -> impl Parser<'a, I, Vec<Expr>, E<'a>> + Clone
where
    I: Input<'a, Token = Token, Span = Span> + ValueInput<'a>,
{
    let args = expr
        .separated_by(just(Token::Comma))
        .allow_trailing()
        .collect();

    safe_delimited(args, Delim::Paren, |_| Vec::new())
}

fn case<'a, I>(
    stat: impl Parser<'a, I, Statement, E<'a>> + Clone,
    expr: impl Parser<'a, I, Expr, E<'a>> + Clone,
) -> impl Parser<'a, I, Case, E<'a>> + Clone
where
    I: Input<'a, Token = Token, Span = Span> + ValueInput<'a>,
{
    just(Token::Case)
        .ignore_then(expr.clone().map(Box::new))
        .then({
            let arm = pattern()
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

fn fun_def<'a, I>(
    expr: impl Parser<'a, I, Expr, E<'a>> + Clone,
) -> impl Parser<'a, I, FuncDef, E<'a>> + Clone
where
    I: Input<'a, Token = Token, Span = Span> + ValueInput<'a>,
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

fn block<'a, I>(
    stat: impl Parser<'a, I, Statement, E<'a>> + Clone,
    expr: impl Parser<'a, I, Expr, E<'a>> + Clone,
) -> impl Parser<'a, I, Block, E<'a>> + Clone
where
    I: Input<'a, Token = Token, Span = Span> + ValueInput<'a>,
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

fn if_<'a, I>(
    expr: impl Parser<'a, I, Expr, E<'a>> + Clone,
    block: impl Parser<'a, I, Block, E<'a>> + Clone,
) -> impl Parser<'a, I, Expr, E<'a>> + Clone
where
    I: Input<'a, Token = Token, Span = Span> + ValueInput<'a>,
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
        .map_with(|(((cond, block), elifs), els), sp| {
            Expr::If(Node::new(
                If {
                    cond: Box::new(cond),
                    block: Box::new(block),
                    elifs,
                    els,
                },
                sp.span(),
            ))
        })
}

fn ident<'a, I>() -> impl Parser<'a, I, Ident, E<'a>> + Clone
where
    I: Input<'a, Token = Token, Span = Span> + ValueInput<'a>,
{
    select! {
        Token::Ident(i) => i.into()
    }
}

fn literal<'a, I>() -> impl Parser<'a, I, Literal, E<'a>> + Clone
where
    I: Input<'a, Token = Token, Span = Span> + ValueInput<'a>,
{
    (select! {
        Token::Nil => Literal::Nil,
        Token::Bool(l) => Literal::Bool(l),
        Token::Char(l) => Literal::Char(l),
        Token::Str(l) => Literal::Str(l),
    })
    .or(just(Token::Op(Op::Sub))
        .or_not()
        .then(select! {
            Token::Num(l) => Literal::Num(l),
            Token::Double(l) => Literal::Double(l),
        })
        .map(|(sub, num)| match num {
            Literal::Num(n) if sub.is_some() => Literal::Num(-n),
            Literal::Double(d) if sub.is_some() => Literal::Double(-d),
            Literal::Num(n) => Literal::Num(n),
            Literal::Double(d) => Literal::Double(d),
            _ => unreachable!(),
        }))
}

fn parend<'a, I>(
    expr: impl Parser<'a, I, Expr, E<'a>> + Clone,
) -> impl Parser<'a, I, Expr, E<'a>> + Clone
where
    I: Input<'a, Token = Token, Span = Span> + ValueInput<'a>,
{
    safe_delimited(expr, Delim::Paren, |_| Expr::Error)
}

fn array<'a, I>(
    expr: impl Parser<'a, I, Expr, E<'a>> + Clone,
) -> impl Parser<'a, I, Expr, E<'a>> + Clone
where
    I: Input<'a, Token = Token, Span = Span> + ValueInput<'a>,
{
    let values = expr
        .separated_by(just(Token::Comma))
        .allow_trailing()
        .collect()
        .map_with(|v, ex| Expr::Atom(Node::new(Atom::Array(v), ex.span())));

    safe_delimited(values, Delim::Brack, |_| Expr::Error)
}

fn safe_delimited<'a, I, O, F, P>(p: P, delim: Delim, f: F) -> impl Parser<'a, I, O, E<'a>> + Clone
where
    I: ValueInput<'a, Token = Token, Span = Span> + 'a,
    F: Fn(I::Span) -> O + Clone,
    P: Parser<'a, I, O, E<'a>> + Clone,
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

// This was copied from the original `chumsky` implementation that is currently
// not available in 1.0.0.alpha-4.
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
    .map_with(move |_, ex| fallback(ex.span()))
}

fn pattern<'a, I>() -> impl Parser<'a, I, Pattern, E<'a>> + Clone
where
    I: Input<'a, Token = Token, Span = Span> + ValueInput<'a>,
{
    recursive(|pat| {
        pattern_top(pat)
            .separated_by(just(Token::Op(Op::BinOr)))
            .allow_leading()
            .at_least(1)
            .collect()
            .map(|pattern| Pattern { pattern })
    })
}

fn pattern_top<'a, I>(
    pat: impl Parser<'a, I, Pattern, E<'a>> + Clone + 'a,
) -> impl Parser<'a, I, PatternTop, E<'a>> + Clone + 'a
where
    I: Input<'a, Token = Token, Span = Span> + ValueInput<'a>,
{
    recursive(|top| {
        let rest_pat = just(Token::DotDot).to(PatternTop::Rest);
        let lit_pat = literal().map(PatternTop::Literal);
        let arr_pat = array_pattern(pat.clone()).map(PatternTop::Array);
        let id_pat = ident()
            .then(
                just(Token::PatBind)
                    .ignore_then(top.map(|p| Pattern { pattern: vec![p] }))
                    .or_not(),
            )
            .map(|(id, pat)| PatternTop::Ident(id, pat.map(Box::new)));

        choice((rest_pat, lit_pat, id_pat, arr_pat))
    })
}

fn array_pattern<'a, I>(
    pattern: impl Parser<'a, I, Pattern, E<'a>> + Clone,
) -> impl Parser<'a, I, ArrayPattern, E<'a>> + Clone
where
    I: Input<'a, Token = Token, Span = Span> + ValueInput<'a>,
{
    // Possible Array Patterns:
    //   [1, 2, .., 8]: lhs, dots, rhs
    //   [.., 8, 9]:    ___, dots, rhs
    //   [1, 2, ..]:    lhs, dots, ___
    //   [1, 2]:        lhs, ____, ___
    //   [..]:          ___, dots, ___
    //
    //   [1, 2, .., 8]: lhs, dots, rhs
    //   [1, 2, ..]:    lhs, dots, ___
    //   [1, 2]:        lhs, ____, ___
    //  pattern (comma dots (comma pattern)?)?
    //
    //   [.., 8, 9]:    ___, dots, rhs
    //   [..]:          ___, dots, ___
    //  dots (comma pattern)?

    let arr_pattern = pattern
        .clone()
        .separated_by(just(Token::Comma))
        .collect::<Vec<_>>()
        .validate(|mut pats, _, _| {
            let indices = pats
                .iter()
                .enumerate()
                .filter_map(|(idx, pat)| match pat.pattern.as_slice() {
                    [p] if p.is_rest() => Some(idx),
                    _ => None,
                })
                .collect::<Vec<_>>();

            match indices.as_slice() {
                [i] => {
                    let head = pats[..*i].to_vec();
                    let tail = pats.get(i + 1..).map_or(Vec::new(), Vec::from);
                    let dots = pats.swap_remove(*i).pattern.swap_remove(0);
                    (head, Some(Box::new(dots)), tail)
                }
                [] => (pats, None, vec![]),
                _ => panic!("BAD PATTERN"),
            }
        });

    safe_delimited(
        arr_pattern.map(|(head, dots, tail)| ArrayPattern::Elements { head, tail, dots }),
        Delim::Brack,
        |_| ArrayPattern::Err,
    )
}
