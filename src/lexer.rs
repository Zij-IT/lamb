use super::span::Spanned;
use super::token::Token;

use chumsky::prelude::*;

pub fn lex(src: &str) -> Result<Vec<Spanned<Token>>, Vec<Simple<char>>> {
    let reals = lex_real();
    let ints = lex_int();
    let chars = lex_char();
    let strings = lex_string();
    let keywords = lex_keywords();
    let control = lex_control();
    let operators = lex_operators();
    let comments = lex_comments();
    let misc = lex_misc();

    let token = comments.or_not().ignore_then(
        choice((
            operators, chars, control, reals, ints, strings, keywords, misc,
        ))
        .or(any().map(Token::Error))
        .map_with_span(Spanned::new)
        .padded()
        .recover_with(skip_then_retry_until([])),
    );

    token.repeated().padded().then_ignore(end()).parse(src)
}

fn lex_real() -> impl Parser<char, Token, Error = Simple<char>> + Clone {
    text::int(10)
        .chain(just('.'))
        .chain::<char, _, _>(text::digits(10))
        .collect::<String>()
        .map(Token::Real)
}

fn lex_int() -> impl Parser<char, Token, Error = Simple<char>> + Clone {
    text::int(10).map(Token::Int)
}

fn escape() -> impl Parser<char, char, Error = Simple<char>> + Clone {
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

fn lex_char() -> impl Parser<char, Token, Error = Simple<char>> + Clone {
    just('\'')
        .ignore_then(filter(|c| *c != '\\' && *c != '\'').or(escape()))
        .then_ignore(just('\''))
        .map(Token::Char)
        .labelled("Char")
}

fn lex_string() -> impl Parser<char, Token, Error = Simple<char>> + Clone {
    just('"')
        .ignore_then(filter(|c| *c != '\\' && *c != '"').or(escape()).repeated())
        .then_ignore(just('"'))
        .collect::<String>()
        .map(Token::Str)
        .labelled("String")
}

fn lex_keywords() -> impl Parser<char, Token, Error = Simple<char>> + Clone {
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

fn lex_control() -> impl Parser<char, Token, Error = Simple<char>> + Clone {
    choice((
        just('(').to(Token::ParenOpen),
        just(')').to(Token::ParenClose),
        just('{').to(Token::BraceOpen),
        just('}').to(Token::BraceClose),
        just('[').to(Token::BrackOpen),
        just(']').to(Token::BrackClose),
    ))
}

fn lex_operators() -> impl Parser<char, Token, Error = Simple<char>> + Clone {
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

fn lex_misc() -> impl Parser<char, Token, Error = Simple<char>> + Clone {
    choice((just(',').to(Token::Comma), just(':').to(Token::Colon)))
}

fn lex_comments() -> impl Parser<char, (), Error = Simple<char>> + Clone {
    just("--").then(take_until(text::newline())).ignored()
}
