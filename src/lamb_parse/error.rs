use std::collections::HashSet;
use std::hash::Hash;
use std::ops::Range;

use ariadne::{Color, Fmt, Label, Report, ReportKind, Source, Span};

use super::token::Token;

#[derive(Debug, Clone)]
pub enum SyntaxError {
    ParseError(ParseError),
    LexError(LexError),
}

impl SyntaxError {
    pub fn eprint(&self, src: &str) -> std::io::Result<()> {
        match self {
            Self::ParseError(p) => p.eprint(src),
            Self::LexError(l) => l.eprint(src),
        }
    }
}

impl From<LexError> for SyntaxError {
    fn from(l: LexError) -> Self {
        Self::LexError(l)
    }
}

impl From<ParseError> for SyntaxError {
    fn from(p: ParseError) -> Self {
        Self::ParseError(p)
    }
}

// --- LEX ERROR --- {

#[derive(Debug, Clone, PartialEq)]
pub struct LexError(Details<char>);

impl LexError {
    pub fn eprint(&self, sample: &str) -> std::io::Result<()> {
        let msg = match self.0.kind {
            Kind::Unexpected { found: Some(t) } => {
                format!("Character not recognized by Lamb: '{}'", t.fg(Color::Red))
            }
            _ => unreachable!("Constructed invalid LexError"),
        };

        Report::build(ReportKind::Error, (), self.start())
            .with_code("EL01")
            .with_message(msg)
            .with_label(
                Label::new(self.start()..self.end())
                    .with_color(Color::Red)
                    .with_message("This character has no meaning."),
            )
            .finish()
            .eprint(Source::from(sample))
    }
}

impl chumsky::Error<char> for LexError {
    type Span = Range<usize>;
    type Label = &'static str;

    fn expected_input_found<Iter: IntoIterator<Item = Option<char>>>(
        span: Self::Span,
        expected: Iter,
        found: Option<char>,
    ) -> Self {
        Self(Details {
            span,
            expected: expected.into_iter().collect(),
            kind: found.map_or(Kind::UnexpectedEnd, |found| Kind::Unexpected {
                found: Some(found),
            }),
            label: None,
        })
    }

    fn with_label(self, label: Self::Label) -> Self {
        Self(Details {
            label: Some(label),
            ..self.0
        })
    }

    fn merge(self, other: Self) -> Self {
        Self(Details {
            expected: self
                .0
                .expected
                .into_iter()
                .chain(other.0.expected.into_iter())
                .collect(),
            ..self.0
        })
    }
}

impl ariadne::Span for LexError {
    type SourceId = ();

    fn source(&self) -> &Self::SourceId {
        todo!("Syntax Errors are not connected to a source, but to a magical range.")
    }

    fn start(&self) -> usize {
        self.0.span.start
    }

    fn end(&self) -> usize {
        self.0.span.end
    }
}

// --- LEX ERROR --- }
// --- PARSE ERROR --- {

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum SyntaxPattern {
    Token(Token),
    Ident,
    Literal,
}

impl SyntaxPattern {
    pub fn is_operator(&self) -> bool {
        match self {
            SyntaxPattern::Ident | SyntaxPattern::Literal => false,
            SyntaxPattern::Token(t) => t.is_operator(),
        }
    }
}

impl std::fmt::Display for SyntaxPattern {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Literal => write!(f, "literal"),
            Self::Ident => write!(f, "identifier"),
            Self::Token(tok) => write!(f, "{}", tok),
        }
    }
}

impl From<Token> for SyntaxPattern {
    fn from(t: Token) -> Self {
        Self::Token(t)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ParseError(Details<SyntaxPattern>);

impl ParseError {
    pub fn invalid_int_literal(span: Range<usize>) -> Self {
        Self(Details {
            kind: Kind::InvalidIntLiteral,
            span,
            expected: HashSet::default(),
            label: None,
        })
    }

    pub fn eprint(&self, sample: &str) -> std::io::Result<()> {
        let expectations = self.expectations_as_string();

        let msg = match &self.0.kind {
            Kind::Unexpected { found: Some(t) } => {
                format!(
                    "Expected {}, but found '{}'",
                    expectations,
                    t.to_string().fg(Color::Red)
                )
            }
            Kind::Unexpected { .. } => {
                format!("Expected {}", expectations)
            }
            Kind::UnexpectedEnd => "Unexpected end of input".to_string(),
            Kind::InvalidIntLiteral => "Invalid int literal".to_string(),
        };

        let label = match &self.0.kind {
            Kind::InvalidIntLiteral => {
                format!(
                    "An int literal must be between: {} and {}",
                    "-9223372036854775808".fg(Color::Blue),
                    "9223372036854775807".fg(Color::Blue),
                )
            }
            _ => "".into(),
        };

        Report::build(ReportKind::Error, (), self.start())
            .with_code("ES01")
            .with_message(msg)
            .with_label(
                Label::new(self.start()..self.end())
                    .with_color(Color::Red)
                    .with_message(label),
            )
            .finish()
            .eprint(Source::from(sample))
    }

    fn expects_operator(&self) -> bool {
        self.0
            .expected
            .iter()
            .any(|x| x.as_ref().map_or(false, SyntaxPattern::is_operator))
    }

    fn expectations_as_string(&self) -> String {
        let mut msg = String::new();

        let early_end = self.0.expected.contains(&None) || self.0.expected.is_empty();
        let wants_op = self.expects_operator();

        let mut expectations = self
            .0
            .expected
            .iter()
            .filter_map(Option::as_ref)
            .filter(|x| !x.is_operator())
            .map(|x| format!("'{}'", x.to_string().fg(Color::Blue)))
            .collect::<Vec<String>>();

        expectations.sort_by_key(String::len);

        match expectations.len() {
            0 => {}
            1 => msg.push_str("a "),
            _ => msg.push_str("one of "),
        }

        msg.push_str(&expectations.join(", "));

        // 2 § 2;

        if wants_op {
            msg.push_str(", or an operator");
        }

        if early_end {
            msg.push_str(", or the end of the expression");
        }

        msg
    }
}

impl<T> chumsky::Error<T> for ParseError
where
    T: Into<SyntaxPattern>,
{
    type Span = Range<usize>;
    type Label = &'static str;

    fn expected_input_found<Iter: IntoIterator<Item = Option<T>>>(
        span: Self::Span,
        expected: Iter,
        found: Option<T>,
    ) -> Self {
        Self(Details {
            span,
            expected: expected.into_iter().map(|x| x.map(Into::into)).collect(),
            kind: found.map_or(Kind::UnexpectedEnd, |found| Kind::Unexpected {
                found: Some(found.into()),
            }),
            label: None,
        })
    }

    fn with_label(self, label: Self::Label) -> Self {
        Self(Details {
            label: Some(label),
            ..self.0
        })
    }

    fn merge(self, other: Self) -> Self {
        Self(Details {
            expected: self
                .0
                .expected
                .into_iter()
                .chain(other.0.expected.into_iter())
                .collect(),
            ..self.0
        })
    }
}

impl ariadne::Span for ParseError {
    type SourceId = ();

    fn source(&self) -> &Self::SourceId {
        todo!("Syntax Errors are not connected to a source, but to a magical range.")
    }

    fn start(&self) -> usize {
        self.0.span.start
    }

    fn end(&self) -> usize {
        self.0.span.end
    }
}

// --- PARSE ERROR --- }

#[derive(Debug, Clone, PartialEq)]
enum Kind<T> {
    UnexpectedEnd,
    Unexpected { found: Option<T> },
    InvalidIntLiteral,
}

#[derive(Debug, Clone, PartialEq)]
struct Details<T: Hash + PartialEq + Eq> {
    kind: Kind<T>,
    span: Range<usize>,
    expected: HashSet<Option<T>>,
    label: Option<&'static str>,
}
