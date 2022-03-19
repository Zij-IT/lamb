#![allow(clippy::module_name_repetitions)]

use std::collections::HashSet;
use std::ops::Range;

use super::token::Token;

pub type LexError = SyntaxError<char>;
pub type ParseError = SyntaxError<Token>;

pub trait ErrorElement = std::hash::Hash + std::cmp::Eq + std::fmt::Display;

#[derive(Debug, Clone)]
pub enum ErrorKind<T> {
    UnexpectedEnd,
    Unexpected { found: Option<T> },
}

#[derive(Debug, Clone)]
pub struct SyntaxError<T: ErrorElement> {
    kind: ErrorKind<T>,
    span: Range<usize>,
    expected: HashSet<Option<T>>,
    label: Option<&'static str>,
}

impl<T: ErrorElement> SyntaxError<T> {
    pub fn eprint(&self, sample: &str) -> std::io::Result<()> {
        use ariadne::{Color, Fmt, Label, Report, ReportKind, Source, Span};

        let expectations = {
            let expected_end = self.expected.contains(&None) || self.expected.is_empty();

            let mut expectations = self
                .expected
                .iter()
                .filter_map(|x| {
                    x.as_ref()
                        .map(|x| x.to_string().fg(Color::Blue).to_string())
                })
                .collect::<Vec<String>>();

            if expected_end {
                expectations.push("the end of the expression".to_string());
            }

            expectations.join(", ")
        };

        let msg = match &self.kind {
            ErrorKind::UnexpectedEnd => {
                format!("Unexpected end of source")
            }
            ErrorKind::Unexpected { found } => {
                let base = format!("Expected {}", expectations);
                if let Some(found) = found {
                    format!("{}, but found '{}'", base, found.to_string().fg(Color::Red))
                } else {
                    base
                }
            }
        };

        let label = match &self.kind {
            ErrorKind::UnexpectedEnd => String::from("End of input"),
            ErrorKind::Unexpected { .. } => {
                format!("Expected {}", expectations)
            }
        };

        Report::build(ReportKind::Error, (), self.start())
            .with_code("EL01")
            .with_message(msg)
            .with_label(
                Label::new(self.start()..self.end())
                    .with_color(Color::Red)
                    .with_message(label),
            )
            .finish()
            .eprint(Source::from(sample))
    }
}

impl<T: ErrorElement> ariadne::Span for SyntaxError<T> {
    type SourceId = ();

    fn source(&self) -> &Self::SourceId {
        todo!("Syntax Errors are not connected to a source, but to a magical range.")
    }

    fn start(&self) -> usize {
        self.span.start
    }

    fn end(&self) -> usize {
        self.span.end
    }
}

impl<T: ErrorElement> chumsky::Error<T> for SyntaxError<T> {
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
