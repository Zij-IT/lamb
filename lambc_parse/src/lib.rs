mod tokenize;

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

#[derive(Clone, Copy, Eq, PartialEq, Debug)]
pub struct FileId(usize);
