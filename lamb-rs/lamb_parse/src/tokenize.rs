use chumsky::{
    error, extra,
    prelude::*,
    text::{ascii, digits},
};

macro_rules! parse_num {
    ($prefix:literal, $filter:expr, $radix:literal, $on_fail:literal $(,)?) => {
        just($prefix)
            .ignore_then(digits($radix).repeated().at_least(1).to_slice())
            .validate(|num: &str, ex, emitter| {
                if !num.chars().all(|c| c.is_digit($radix) || c == '_') {
                    emitter.emit(Rich::custom(ex.span(), $on_fail));
                    return Token::Error(Error::InvalidNumLit);
                }

                let num = num.chars().filter(|&c| c != '_').collect::<String>();
                match i64::from_str_radix(num.as_str(), $radix) {
                    Ok(num) => Token::Num(num),
                    Err(_) => unreachable!(),
                }
            })
    };
}
#[derive(Copy, Clone, PartialEq, Debug, Eq, Hash)]
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

impl std::fmt::Display for Op {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let item = match self {
            Op::Add => "+",
            Op::Sub => "-",
            Op::Div => "/",
            Op::Mul => "*",
            Op::Mod => "%",
            Op::BinAnd => "&",
            Op::BinOr => "|",
            Op::BinXor => "^",
            Op::BinComp => "~",
            Op::LShift => "<<",
            Op::RShift => ">>",
            Op::Eq => "=",
            Op::Ne => "!=",
            Op::Gt => ">",
            Op::Lt => "<",
            Op::Ge => ">=",
            Op::Le => "<=",
            Op::LogAnd => "&&",
            Op::LogOr => "||",
            Op::LogNot => "!",
            Op::LCompose => "<.",
            Op::RCompose => ".>",
            Op::LApply => "<$",
            Op::RApply => "$>",
        };

        write!(f, "{item}")
    }
}

#[derive(Copy, Clone, PartialEq, Debug, Eq, Hash)]
pub enum Delim {
    Brace,
    Brack,
    Paren,
}

#[derive(Clone, PartialEq, Debug, Eq, Hash)]
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
    PatBind,
    DotDot,

    // Error Handling
    Error(Error),
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let item: &dyn std::fmt::Display = match self {
            Token::Bool(b) => b,
            Token::Char(c) => c,
            Token::Ident(i) => i,
            Token::Num(n) => n,
            Token::Op(op) => op,
            Token::Str(s) => s,
            Token::Open(Delim::Brace) => &"{",
            Token::Open(Delim::Brack) => &"[",
            Token::Open(Delim::Paren) => &"(",
            Token::Close(Delim::Brace) => &"}",
            Token::Close(Delim::Brack) => &"]",
            Token::Close(Delim::Paren) => &")",
            Token::Nil => &"nil",
            Token::Fn => &"fn",
            Token::Case => &"case",
            Token::If => &"if",
            Token::Elif => &"elif",
            Token::Else => &"else",
            Token::Return => &"return",
            Token::Struct => &"struct",
            Token::Enum => &"enum",
            Token::Rec => &"rec",
            Token::Arrow => &"arrow",
            Token::Define => &":=",
            Token::Comma => &",",
            Token::Colon => &":",
            Token::Semi => &";",
            Token::PatBind => &"@",
            Token::DotDot => &"..",
            Token::Error(e) => {
                write!(f, "{e:?}")?;
                return Ok(());
            }
        };

        write!(f, "{item}")
    }
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
        numbers(),
        word,
    ))
    .or(any().validate(|t: char, ex, emitter| {
        emitter.emit(<Rich<_> as error::Error<I<'a>>>::expected_found(
            None,
            Some(t.into()),
            ex.span(),
        ));
        Token::Error(Error::InvalidChar(t))
    }));

    let comment = just("--").ignore_then(none_of('\n').repeated()).padded();

    lang_element
        .map_with(|tok, ex| (tok, ex.span()))
        .padded_by(comment.repeated())
        .padded()
        .repeated()
        .collect()
        .then_ignore(comment.repeated())
        .then_ignore(end())
}

fn chars<'a>() -> impl Parser<'a, I<'a>, Token, E<'a>> {
    just('\'')
        .ignore_then(
            any()
                .filter(|c| *c != ':' && *c != '\'')
                .or(escape_chars())
                .repeated()
                .to_slice(),
        )
        .then_ignore(just('\''))
        .validate(|ch, ex, emitter| {
            if ch.len() == 1 {
                Token::Char(ch.chars().next().expect("ch should have length 1"))
            } else {
                emitter.emit(Rich::custom(
                    ex.span(),
                    format!(
                        "Char literal should contain {} 1 character",
                        if ch.len() > 1 { "at most" } else { "at least" }
                    ),
                ));
                Token::Char('f')
            }
        })
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
        .ignore_then(digits(10).separated_by(just('_')).at_least(1).to_slice())
        .map(|s: I<'a>| s.chars().filter(|&c| c != '_').collect::<String>())
        .from_str()
        .unwrapped()
        .map(Token::Num);

    choice((binary, hex, octal, decimal))
        .then(ascii::ident().or_not().map_with(|t, ex| (t, ex.span())))
        .validate(|(num, (t, s)), _, emitter| {
            if !t.map_or(true, str::is_empty) {
                emitter.emit(Rich::custom(s, "Invalid suffix for number literal"));
            }

            num
        })
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
        just("..").to(Token::DotDot),
        just("->").to(Token::Arrow),
        just(":=").to(Token::Define),
        just(',').to(Token::Comma),
        just(';').to(Token::Semi),
        just(':').to(Token::Colon),
        just('@').to(Token::PatBind),
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

#[cfg(test)]
mod test {
    use super::*;
    #[test]
    fn numbers_() {
        let test = |s| numbers().parse(s).into_result().unwrap();
        let test_err = |s| numbers().parse(s).into_result().unwrap_err();

        [
            "00",
            "12_341",
            "0h0123_4567_89ab_cDEF",
            "0d123_496_789",
            "0o0123_4567",
            "0b01010_1010",
        ]
        .map(test);

        [
            "0acdefgh",
            "0h_ghij_klmn",
            "0d_abcd_efgh",
            "0o_89ab_cdef",
            "0b_0123_4567",
        ]
        .map(test_err);
    }
}
