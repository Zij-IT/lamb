//! Parsing Lamb Source Code
//!
//! This crate defines the tools necessary to parse Lamb source code, including
//! the [`Parser`] and the components of the [Abstract Syntax Tree]. Additionally
//! the location of an item in the source code can be tracked using the [`Span`]
//! type.
//!
//! [Abstract Syntax Tree]: https://en.wikipedia.org/wiki/Abstract_syntax_tree
mod ast;
mod parse;
mod tokenize;

pub use ast::*;
use miette::SourceSpan;
pub use parse::{Error, Parser};
use tokenize::Lexer;
pub use tokenize::{TokKind, Token};

/// A location within a source file
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct Span {
    start: usize,
    end: usize,
}

impl Span {
    /// Creates a new `Span` for the specified `FileId`. `start` and `end` mark byte offsets in
    /// the source file.
    pub const fn new(start: usize, end: usize) -> Self {
        Self { start, end }
    }

    /// Helper function to join two spans from the same file. The resulting span will have the
    /// the start from `start` and end from `end`.
    pub const fn connect(start: Self, end: Self) -> Self {
        Self { start: start.start, end: end.end }
    }
}

impl From<Span> for SourceSpan {
    fn from(value: Span) -> Self {
        SourceSpan::new(value.start.into(), value.end - value.start)
    }
}
