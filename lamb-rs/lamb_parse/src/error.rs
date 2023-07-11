use std::ops::Range;

use chumsky::error::{Rich, RichPattern, RichReason};

use crate::tokenize::Token;

#[derive(Clone, PartialEq, Eq)]
pub enum SyntaxError<'a> {
    Lexical(Rich<'a, char>),
    Syntactic(Rich<'a, Token>),
}

impl std::fmt::Debug for SyntaxError<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SyntaxError::Lexical(c) => {
                if f.alternate() {
                    write!(f, "{c:#?}")
                } else {
                    write!(f, "{c:?}")
                }
            }
            SyntaxError::Syntactic(t) => {
                if f.alternate() {
                    write!(f, "{t:#?}")
                } else {
                    write!(f, "{t:?}")
                }
            }
        }
    }
}

impl<'a> SyntaxError<'a> {
    pub fn raw_span(&self) -> Range<usize> {
        match self {
            SyntaxError::Lexical(l) => l.span().into_range(),
            SyntaxError::Syntactic(s) => s.span().into_range(),
        }
    }

    pub fn reasons(&self) -> Vec<String> {
        match self {
            SyntaxError::Lexical(l) => format_reason(l.reason()),
            SyntaxError::Syntactic(s) => format_reason(s.reason()),
        }
    }
}

pub fn format_reason<T>(reason: &RichReason<T>) -> Vec<String>
where
    T: std::fmt::Display,
{
    match reason {
        RichReason::ExpectedFound { found, expected } => {
            let found = match found {
                Some(f) => format!("'{}'", &**f),
                None => format!("EOF"),
            };

            match expected.len() {
                0 => vec![format!("Unexpected item: {found}")],
                1 => vec![format!(
                    "expected {}, found {found}",
                    format_expected(expected)
                )],
                _ => vec![format!(
                    "expected {}, found {found}",
                    format_expected(expected)
                )],
            }
        }
        RichReason::Custom(s) => vec![s.clone()],
        RichReason::Many(reasons) => reasons.iter().flat_map(format_reason).collect(),
    }
}

fn format_expected<'a, T>(patterns: &[RichPattern<'a, T>]) -> String
where
    T: std::fmt::Display,
{
    let len = patterns.len();
    match len {
        0 => "nothing".into(),
        1 => patterns[0].to_string(),
        2 => format!(
            "either {} or {}",
            patterns[0].to_string(),
            patterns[1].to_string()
        ),
        len => {
            let first = &patterns[0];
            let mid = patterns[1..len - 1]
                .iter()
                .fold(String::new(), |mut acc, next| {
                    acc.push_str(", ");
                    acc.push_str(&next.to_string());
                    acc
                });
            let last = &patterns[len - 1];

            format!(
                "one of {}{}, or {}",
                first.to_string(),
                mid,
                last.to_string()
            )
        }
    }
}
