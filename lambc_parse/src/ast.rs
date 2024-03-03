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

#[derive(Debug, Eq, PartialEq)]
pub struct F64Lit {
    pub value: String,
    pub span: Span,
}

#[derive(Debug, Eq, PartialEq)]
pub struct BoolLit {
    pub value: bool,
    pub span: Span,
}

#[derive(Debug, Eq, PartialEq)]
pub struct NilLit {
    pub span: Span,
}

#[derive(Debug, Eq, PartialEq)]
pub struct StrLit {
    pub text: Option<StrText>,
    pub span: Span,
}

#[derive(Debug, Eq, PartialEq)]
pub struct StrText {
    pub inner: String,
    pub span: Span,
}

#[derive(Debug, Eq, PartialEq)]
pub struct CharLit {
    pub text: Option<CharText>,
    pub span: Span,
}

#[derive(Debug, Eq, PartialEq)]
pub struct CharText {
    pub inner: String,
    pub span: Span,
}

#[derive(Debug, Eq, PartialEq)]
pub struct Ident {
    pub raw: String,
    pub span: Span,
}

#[derive(Debug, Eq, PartialEq)]
pub struct List {
    pub values: Vec<Expr>,
    pub span: Span,
}

#[derive(Debug, Eq, PartialEq)]
pub enum Expr {
    Ident(Ident),
    Char(CharLit),
    String(StrLit),
    Bool(BoolLit),
    Nil(NilLit),
    I64(I64Lit),
    F64(F64Lit),
    List(List),
}
