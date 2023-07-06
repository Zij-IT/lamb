use std::ops::Range;

use chumsky::error::{Rich, RichReason};

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
    T: std::fmt::Debug,
{
    match reason {
        RichReason::ExpectedFound { found, .. } => {
            vec![format!("Found {found:?}.")]
        }
        RichReason::Custom(s) => vec![s.clone()],
        RichReason::Many(reasons) => reasons.iter().flat_map(format_reason).collect(),
    }
}
