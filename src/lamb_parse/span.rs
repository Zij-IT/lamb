use std::fmt::Debug;
use std::ops::Range;
use ariadne::Span;

pub struct Spanned<T>(T, Range<usize>);

impl<T> Spanned<T> {
    pub fn new(item: T, span: Range<usize>) -> Self {
        Self(item, span)
    }

    pub fn into_tuple(self) -> (T, Range<usize>) {
        (self.0, self.1)
    }

    pub fn into_inner(self) -> T {
        self.0
    }

    pub fn start(&self) -> usize {
        self.1.start
    }

    pub fn end(&self) -> usize {
        self.1.end
    }
}

impl<T: PartialEq> PartialEq for Spanned<T> {
    fn eq(&self, other: &Self) -> bool {
        self.0 == self.0
    }
}

impl <T: Debug> Debug for Spanned<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "[{}..{}]: {:?}", self.1.start, self.1.end, self.0)
    }
}

impl<T: Clone> Clone for Spanned<T> {
    fn clone(&self) -> Self {
        Self(self.0.clone(), self.1.clone())
    }
}

impl<T> std::ops::Deref for Spanned<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}