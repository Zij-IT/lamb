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
    /// Creates a new `Span` for the specified `FileId`. `start` and `end` mark byte offsets in
    /// the source file.
    pub fn new(start: usize, end: usize, file: FileId) -> Self {
        Self { start, end, file }
    }

    /// Helper function to join two spans from the same file. The resulting span will have the
    /// the start from `start` and end from `end`.
    pub fn connect(start: Self, end: Self) -> Self {
        assert_eq!(start.file, end.file);

        Self {
            start: start.start,
            end: end.end,
            file: start.file,
        }
    }
}

impl From<Span> for SourceSpan {
    fn from(value: Span) -> Self {
        SourceSpan::new(value.start.into(), value.end - value.start)
    }
}

#[derive(Clone, Copy, Eq, PartialEq, Debug)]
pub struct FileId(usize);
