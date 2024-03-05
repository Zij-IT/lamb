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
pub struct Group {
    pub value: Expr,
    pub span: Span,
}

#[derive(Debug, Eq, PartialEq)]
pub struct FnDef {
    pub args: Vec<Ident>,
    pub body: Expr,
    pub recursive: bool,
    pub span: Span,
}

#[derive(Debug, Eq, PartialEq)]
pub struct Block {
    pub statements: Vec<Statement>,
    pub value: Option<Expr>,
    pub span: Span,
}

#[derive(Debug, Eq, PartialEq)]
pub struct IfCond {
    pub cond: Expr,
    pub body: Block,
    pub span: Span,
}

#[derive(Debug, Eq, PartialEq)]
pub struct Else {
    pub body: Block,
    pub span: Span,
}

#[derive(Debug, Eq, PartialEq)]
pub struct If {
    pub cond: IfCond,
    pub elif: Vec<IfCond>,
    pub els_: Option<Else>,
    pub span: Span,
}

#[derive(Debug, Eq, PartialEq)]
pub struct Case {
    pub scrutinee: Expr,
    pub arms: Vec<CaseArm>,
    pub span: Span,
}

#[derive(Debug, Eq, PartialEq)]
pub struct CaseArm {
    pub pattern: Pattern,
    pub body: Expr,
    pub span: Span,
}

#[derive(Debug, Eq, PartialEq)]
pub struct Pattern {
    pub inner: Vec<InnerPattern>,
    pub span: Span,
}

#[derive(Debug, Eq, PartialEq)]
pub enum InnerPattern {
    Literal(Box<LiteralPattern>),
    Array(Box<ArrayPattern>),
    Ident(Box<IdentPattern>),
    Rest(Box<RestPattern>),
}

impl InnerPattern {
    pub fn span(&self) -> Span {
        match self {
            InnerPattern::Literal(p) => p.span(),
            InnerPattern::Array(p) => p.span,
            InnerPattern::Ident(p) => p.span,
            InnerPattern::Rest(p) => p.span,
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct RestPattern {
    pub span: Span,
}

#[derive(Debug, Eq, PartialEq)]
pub enum LiteralPattern {
    String(StrLit),
    Bool(BoolLit),
    Char(CharLit),
    I64(I64Lit),
    Nil(NilLit),
}

impl LiteralPattern {
    pub fn span(&self) -> Span {
        match self {
            Self::String(p) => p.span,
            Self::Bool(p) => p.span,
            Self::Char(p) => p.span,
            Self::I64(p) => p.span,
            Self::Nil(p) => p.span,
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct IdentPattern {
    pub ident: Ident,
    pub bound: Option<Box<InnerPattern>>,
    pub span: Span,
}

#[derive(Debug, Eq, PartialEq)]
pub struct ArrayPattern {
    pub patterns: Vec<Pattern>,
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
    Group(Box<Group>),
    FnDef(Box<FnDef>),
    Block(Box<Block>),
    If(Box<If>),
    Case(Box<Case>),
}

impl Expr {
    pub fn span(&self) -> Span {
        match self {
            Expr::Ident(e) => e.span,
            Expr::Char(e) => e.span,
            Expr::String(e) => e.span,
            Expr::Bool(e) => e.span,
            Expr::Nil(e) => e.span,
            Expr::I64(e) => e.span,
            Expr::F64(e) => e.span,
            Expr::List(e) => e.span,
            Expr::Group(e) => e.span,
            Expr::FnDef(e) => e.span,
            Expr::Block(e) => e.span,
            Expr::If(e) => e.span,
            Expr::Case(e) => e.span,
        }
    }

    pub fn ends_with_block(&self) -> bool {
        match self {
            Expr::Ident(_)
            | Expr::Char(_)
            | Expr::String(_)
            | Expr::Bool(_)
            | Expr::Nil(_)
            | Expr::I64(_)
            | Expr::F64(_)
            | Expr::List(_)
            | Expr::Group(_) => false,
            Expr::Block(_) | Expr::If(_) | Expr::Case(_) => true,
            Expr::FnDef(f) => f.body.ends_with_block(),
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct Define {
    pub ident: Ident,
    pub value: Expr,
    pub span: Span,
}

#[derive(Debug, Eq, PartialEq)]
pub struct ExprStatement {
    pub expr: Expr,
    pub span: Span,
}

#[derive(Debug, Eq, PartialEq)]
pub enum Statement {
    Define(Define),
    Expr(ExprStatement),
}
