mod ast;
mod parse;
mod tokenize;

pub use ast::*;
use miette::SourceSpan;
pub use parse::Parser;
use tokenize::Lexer;
pub use tokenize::{TokKind, Token};

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct Span {
    file: FileId,
    start: usize,
    end: usize,
}

impl Span {
    /// Creates a new `Span` for the specified `FileId`
    pub fn new(start: usize, end: usize, file: FileId) -> Self {
        Self { start, end, file }
    }
}

impl From<Span> for SourceSpan {
    fn from(value: Span) -> Self {
        SourceSpan::new(value.start.into(), value.end - value.start)
    }
}

#[derive(Clone, Copy, Eq, PartialEq, Debug)]
pub struct FileId(usize);
