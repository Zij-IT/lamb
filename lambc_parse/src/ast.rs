use crate::Span;

#[derive(Debug, Eq, PartialEq)]
pub enum I64Base {
    Bin,
    Oct,
    Dec,
    Hex,
}

#[derive(Debug, Eq, PartialEq)]
pub struct I64Lit {
    pub base: I64Base,
    pub value: String,
    pub span: Span,
}
