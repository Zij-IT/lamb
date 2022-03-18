use std::fmt::Debug;
use std::ops::Range;

#[derive(Clone, PartialEq, Debug)]
pub struct Spanned<T: Clone + Debug + PartialEq>(T, Range<usize>);

impl<T: Clone + Debug + PartialEq> Spanned<T> {
    pub fn new(item: T, span: Range<usize>) -> Self {
        Self(item, span)
    }

    pub fn into_inner(self) -> T {
        self.0
    }
}
