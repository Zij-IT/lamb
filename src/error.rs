#![allow(clippy::module_name_repetitions)]

use std::collections::HashSet;
use std::ops::Range;

use super::token::Token;

pub type LexError = SyntaxError<char>;
pub type ParseError = SyntaxError<Token>;

#[derive(Debug, Clone)]
pub enum ErrorKind<T> {
    UnexpectedEnd,
    Unexpected { found: Option<T> },
}

#[derive(Debug, Clone)]
pub struct SyntaxError<T: std::hash::Hash + std::cmp::Eq> {
    kind: ErrorKind<T>,
    span: Range<usize>,
    expected: HashSet<Option<T>>,
    label: Option<&'static str>,
}

impl<T: std::hash::Hash + std::cmp::Eq> chumsky::Error<T> for SyntaxError<T> {
    type Span = Range<usize>;
    type Label = &'static str;

    fn expected_input_found<Iter: IntoIterator<Item = Option<T>>>(
        span: Self::Span,
        expected: Iter,
        found: Option<T>,
    ) -> Self {
        Self {
            span,
            expected: expected.into_iter().collect(),
            kind: found.map_or(ErrorKind::UnexpectedEnd, |found| ErrorKind::Unexpected {
                found: Some(found),
            }),
            label: None,
        }
    }

    fn with_label(self, label: Self::Label) -> Self {
        Self {
            label: Some(label),
            ..self
        }
    }

    fn merge(self, other: Self) -> Self {
        Self {
            expected: self
                .expected
                .into_iter()
                .chain(other.expected.into_iter())
                .collect(),
            ..self
        }
    }
}