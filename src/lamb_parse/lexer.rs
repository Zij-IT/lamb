use super::error::LexError;
use super::span::Spanned;
use super::token::Token;

use chumsky::prelude::*;

pub fn lexer() -> impl Parser<char, Vec<Spanned<Token>>, Error = LexError> + Clone {
    let reals = lex_real();
    let ints = lex_int();
    let chars = lex_char();
    let strings = lex_string();
    let keywords = lex_keywords();
    let control = lex_control();
    let operators = lex_operators();
    let comments = lex_comments();
    let misc = lex_misc();

    let token = choice((
        comments.to(None),
        operators.map(Some),
        chars.map(Some),
        control.map(Some),
        reals.map(Some),
        ints.map(Some),
        strings.map(Some),
        keywords.map(Some),
        misc.map(Some),
    ))
    .or(any()
        .validate(|x, span, emit| {
            emit(LexError::expected_input_found(span, None, Some(x)));
            x
        })
        .map(|ch| Some(Token::Error(ch))))
    .map_with_span(Spanned::new)
    .padded()
    .recover_with(skip_then_retry_until([]));

    token
        .repeated()
        .map(|v| {
            v.into_iter()
                .filter_map(|x| {
                    let (opt, span): (Option<Token>, std::ops::Range<usize>) = x.into_tuple();
                    opt.map(|x| Spanned::new(x, span))
                })
                .collect()
        })
        .padded()
        .then_ignore(end())
}

fn lex_real() -> impl Parser<char, Token, Error = LexError> + Clone {
    text::int(10)
        .chain(just('.'))
        .chain::<char, _, _>(text::digits(10))
        .collect::<String>()
        .map(Token::Real)
}

fn lex_int() -> impl Parser<char, Token, Error = LexError> + Clone {
    text::int(10).map(Token::Int)
}

fn escape() -> impl Parser<char, char, Error = LexError> + Clone {
    just('\\').ignore_then(
        just('\\')
            .or(just('/'))
            .or(just('"'))
            .or(just('\''))
            .or(just('b').to('\x08'))
            .or(just('f').to('\x0C'))
            .or(just('n').to('\n'))
            .or(just('r').to('\r'))
            .or(just('t').to('\t')),
    )
}

fn lex_char() -> impl Parser<char, Token, Error = LexError> + Clone {
    just('\'')
        .ignore_then(filter(|c| *c != '\\' && *c != '\'').or(escape()))
        .then_ignore(just('\''))
        .map(Token::Char)
        .labelled("Char")
}

fn lex_string() -> impl Parser<char, Token, Error = LexError> + Clone {
    just('"')
        .ignore_then(filter(|c| *c != '\\' && *c != '"').or(escape()).repeated())
        .then_ignore(just('"'))
        .collect::<String>()
        .map(Token::Str)
        .labelled("String")
}

fn lex_keywords() -> impl Parser<char, Token, Error = LexError> + Clone {
    text::ident().map(|s: String| match s.as_str() {
        "true" => Token::Bool(true),
        "false" => Token::Bool(false),
        "for" => Token::For,
        "in" => Token::In,
        "while" => Token::While,
        "loop" => Token::Loop,
        "return" => Token::Return,
        "break" => Token::Break,
        "continue" => Token::Continue,
        "if" => Token::If,
        "elif" => Token::Elif,
        "else" => Token::Else,
        "import" => Token::Import,
        "export" => Token::Export,
        "_" => Token::Wildcard,
        _ => Token::Ident(s),
    })
}

fn lex_control() -> impl Parser<char, Token, Error = LexError> + Clone {
    choice((
        just('(').to(Token::ParenOpen),
        just(')').to(Token::ParenClose),
        just('{').to(Token::BraceOpen),
        just('}').to(Token::BraceClose),
        just('[').to(Token::BrackOpen),
        just(']').to(Token::BrackClose),
    ))
}

fn lex_operators() -> impl Parser<char, Token, Error = LexError> + Clone {
    choice((
        just("->").to(Token::Arrow),
        just(":=").to(Token::Assign),
        just("<=").to(Token::Le),
        just(">=").to(Token::Ge),
        just("~=").to(Token::NotEq),
        just("==").to(Token::EqEq),
        just("++").to(Token::Concat),
        just("&&").to(Token::LogAnd),
        just("||").to(Token::LogOr),
        just('+').to(Token::Plus),
        just('-').to(Token::Minus),
        just('*').to(Token::Mul),
        just('/').to(Token::Div),
        just('%').to(Token::Rem),
        just('&').to(Token::BitAnd),
        just('|').to(Token::BitOr),
        just('^').to(Token::BitXor),
        just('~').to(Token::Not),
        just('>').to(Token::Gt),
        just('<').to(Token::Lt),
    ))
}

fn lex_misc() -> impl Parser<char, Token, Error = LexError> + Clone {
    choice((
        just(',').to(Token::Comma),
        just(':').to(Token::Colon),
        just(';').to(Token::Semicolon),
        just("..").to(Token::DotDot),
        just('\\').to(Token::Lambda),
    ))
}

fn lex_comments() -> impl Parser<char, (), Error = LexError> + Clone {
    just("--")
        .then(take_until(text::newline().or(end())))
        .ignored()
}
