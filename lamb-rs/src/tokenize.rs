use chumsky::{
    error, extra,
    prelude::*,
    text::{ascii, int},
};

macro_rules! parse_num {
    ($prefix:literal, $filter:expr, $radix:literal, $on_fail:literal $(,)?) => {
        just($prefix)
            .ignore_then(any().filter(|c: &char| c.is_whitespace()).slice())
            .validate(|bin: &str, span, emitter| {
                if !bin.chars().all(|c| c.is_digit($radix) || c == '_') {
                    emitter.emit(Rich::custom(span, $on_fail));
                    return Token::Error(Error::InvalidNumLit);
                }

                let num = bin.chars().filter(|&c| c != '_').collect::<String>();
                match i64::from_str_radix(num.as_str(), $radix) {
                    Ok(num) => Token::Num(num),
                    Err(_) => unreachable!(),
                }
            })
    };
}
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum Op {
    Add,
    Sub,
    Div,
    Mul,
    Mod,
    BinAnd,
    BinOr,
    BinXor,
    BinComp,
    LShift,
    RShift,
    Eq,
    Ne,
    Gt,
    Lt,
    Ge,
    Le,
    LogAnd,
    LogOr,
    LogNot,
    LCompose,
    RCompose,
    LApply,
    RApply,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum Delim {
    Brace,
    Brack,
    Paren,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Token {
    Op(Op),

    // Delimeters
    Open(Delim),
    Close(Delim),

    // Literals
    Nil,
    Num(i64),
    Bool(bool),
    Char(char),
    Str(String),
    Ident(String),

    // Keywords
    Fn,
    Case,
    If,
    Elif,
    Else,
    Return,
    Struct,
    Enum,
    Rec,

    // Syntax
    Arrow,
    Define,
    Comma,
    Colon,
    Semi,

    // Error Handling
    Error(Error),
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum Error {
    InvalidChar(char),
    InvalidNumLit,
}

type I<'a> = &'a str;
type E<'a> = extra::Err<Rich<'a, char>>;

pub fn lamb<'a>() -> impl Parser<'a, I<'a>, Vec<(Token, SimpleSpan)>, E<'a>> {
    // TODO:
    // + Reduce the weird budget being used for odd choice
    //   of escape char
    let word = ascii::ident().map(|s| match s {
        "fn" => Token::Fn,
        "if" => Token::If,
        "nil" => Token::Nil,
        "rec" => Token::Rec,
        "case" => Token::Case,
        "enum" => Token::Enum,
        "elif" => Token::Elif,
        "else" => Token::Else,
        "true" => Token::Bool(true),
        "false" => Token::Bool(false),
        "struct" => Token::Struct,
        "return" => Token::Return,
        _ => Token::Ident(s.into()),
    });

    let lang_element = choice((
        chars(),
        strings(),
        syntax(),
        ops(),
        delimeters(),
        word,
        numbers(),
    ))
    .or(any().validate(|t: char, span, emitter| {
        emitter.emit(<Rich<_> as error::Error<I<'a>>>::expected_found(
            None,
            Some(t.into()),
            span,
        ));
        Token::Error(Error::InvalidChar(t))
    }));

    let comment = just("--").ignore_then(none_of('\n').repeated()).padded();

    lang_element
        .map_with_span(|tok, sp| (tok, sp))
        .padded_by(comment.repeated())
        .padded()
        .repeated()
        .collect()
        .then_ignore(end())
}

fn chars<'a>() -> impl Parser<'a, I<'a>, Token, E<'a>> {
    just('\'')
        .ignore_then(any().filter(|c| *c != ':' && *c != '\'').or(escape_chars()))
        .then_ignore(just('\''))
        .map(Token::Char)
}

fn strings<'a>() -> impl Parser<'a, I<'a>, Token, E<'a>> {
    just('"')
        .ignore_then(
            any()
                .filter(|c| *c != ':' && *c != '"')
                .or(escape_chars())
                .repeated()
                .collect(),
        )
        .then_ignore(just('"'))
        .map(Token::Str)
}

fn numbers<'a>() -> impl Parser<'a, I<'a>, Token, E<'a>> {
    let binary = parse_num!(
        "0b",
        |c| matches!(c, '1' | '0' | '_'),
        2,
        "Binary number may only contain [01_]",
    );

    let hex = parse_num!(
        "0h",
        |c| char::is_ascii_hexdigit(&c) || c.eq(&'_'),
        16,
        "Hex number may only contain [a-fA-F0-9_]",
    );

    let octal = parse_num!(
        "0o",
        |c| matches!(c, '0'..='7' | '_'),
        8,
        "Octal number may only contain [0-7_]"
    );

    let decimal = just("0d")
        .or_not()
        .ignore_then(int(10).separated_by(just('_')).at_least(1).slice())
        .map(|s: I<'a>| s.chars().filter(|&c| c != '_').collect::<String>())
        .from_str()
        .unwrapped()
        .map(Token::Num);

    choice((binary, hex, octal, decimal))
}

fn delimeters<'a>() -> impl Parser<'a, I<'a>, Token, E<'a>> {
    choice((
        just('{').to(Token::Open(Delim::Brace)),
        just('}').to(Token::Close(Delim::Brace)),
        just('[').to(Token::Open(Delim::Brack)),
        just(']').to(Token::Close(Delim::Brack)),
        just('(').to(Token::Open(Delim::Paren)),
        just(')').to(Token::Close(Delim::Paren)),
    ))
}

fn syntax<'a>() -> impl Parser<'a, I<'a>, Token, E<'a>> {
    choice((
        just("->").to(Token::Arrow),
        just(":=").to(Token::Define),
        just(',').to(Token::Comma),
        just(';').to(Token::Semi),
        just(':').to(Token::Colon),
    ))
}

fn ops<'a>() -> impl Parser<'a, I<'a>, Token, E<'a>> {
    choice((
        just(".>").to(Token::Op(Op::RCompose)),
        just("<.").to(Token::Op(Op::LCompose)),
        just("$>").to(Token::Op(Op::RApply)),
        just("<$").to(Token::Op(Op::LApply)),
        just("<<").to(Token::Op(Op::LShift)),
        just(">>").to(Token::Op(Op::RShift)),
        just("!=").to(Token::Op(Op::Ne)),
        just(">=").to(Token::Op(Op::Ge)),
        just("<=").to(Token::Op(Op::Le)),
        just("&&").to(Token::Op(Op::LogAnd)),
        just("||").to(Token::Op(Op::LogOr)),
        just('+').to(Token::Op(Op::Add)),
        just('-').to(Token::Op(Op::Sub)),
        just('*').to(Token::Op(Op::Mul)),
        just('/').to(Token::Op(Op::Div)),
        just('%').to(Token::Op(Op::Mod)),
        just('&').to(Token::Op(Op::BinAnd)),
        just('|').to(Token::Op(Op::BinOr)),
        just('^').to(Token::Op(Op::BinXor)),
        just('~').to(Token::Op(Op::BinComp)),
        just('=').to(Token::Op(Op::Eq)),
        just('>').to(Token::Op(Op::Gt)),
        just('<').to(Token::Op(Op::Lt)),
        just('!').to(Token::Op(Op::LogNot)),
    ))
}

fn escape_chars<'a>() -> impl Parser<'a, I<'a>, char, E<'a>> {
    just(':').ignore_then(choice((
        just(':'),
        just('/'),
        just('"'),
        just('\''),
        just('b').to('\x08'),
        just('f').to('\x0C'),
        just('n').to('\n'),
        just('r').to('\r'),
        just('t').to('\t'),
    )))
}
