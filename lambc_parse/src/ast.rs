use crate::{Span, TokKind};

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
pub struct Index {
    pub lhs: Expr,
    pub rhs: Expr,
    pub span: Span,
}

#[derive(Debug, Eq, PartialEq)]
pub struct Call {
    pub callee: Expr,
    pub args: Vec<Expr>,
    pub span: Span,
}

#[derive(Debug, Eq, PartialEq)]
pub struct Binary {
    pub lhs: Expr,
    pub op: BinaryOp,
    pub rhs: Expr,
    pub span: Span,
    // Must be a subspan of `span`
    pub op_span: Span,
}

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub enum BinaryOp {
    Appl,
    Appr,
    Cpsl,
    Cpsr,
    Land,
    Lor,
    Eq,
    Ne,
    Ge,
    Gt,
    Le,
    Lt,
    Bor,
    Bxor,
    Band,
    Shr,
    Shl,
    Add,
    Sub,
    Div,
    Mod,
    Mul,
}

impl BinaryOp {
    pub fn infix_bp(self) -> (u8, u8) {
        match self {
            BinaryOp::Appl => (1, 2),
            BinaryOp::Appr => (1, 2),
            BinaryOp::Cpsl => (2, 3),
            BinaryOp::Cpsr => (2, 3),
            BinaryOp::Land => (3, 4),
            BinaryOp::Lor => (4, 5),
            BinaryOp::Eq => (5, 6),
            BinaryOp::Ne => (5, 6),
            BinaryOp::Ge => (5, 6),
            BinaryOp::Gt => (5, 6),
            BinaryOp::Le => (5, 6),
            BinaryOp::Lt => (5, 6),
            BinaryOp::Bor => (6, 7),
            BinaryOp::Bxor => (7, 8),
            BinaryOp::Band => (8, 9),
            BinaryOp::Shr => (9, 10),
            BinaryOp::Shl => (9, 10),
            BinaryOp::Add => (10, 11),
            BinaryOp::Sub => (10, 11),
            BinaryOp::Div => (11, 12),
            BinaryOp::Mod => (11, 12),
            BinaryOp::Mul => (11, 12),
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct Unary {
    pub rhs: Expr,
    pub op: UnaryOp,
    pub span: Span,
    // Must be a subspan of `span`
    pub op_span: Span,
}

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub enum UnaryOp {
    Nneg,
    Lnot,
    Bneg,
}

impl UnaryOp {
    pub(crate) fn rbp(&self) -> u8 {
        match self {
            UnaryOp::Nneg | UnaryOp::Lnot | UnaryOp::Bneg => 11,
        }
    }
}

impl From<TokKind> for UnaryOp {
    fn from(value: TokKind) -> Self {
        match value {
            TokKind::Sub => UnaryOp::Nneg,
            TokKind::Bneg => UnaryOp::Bneg,
            TokKind::Lnot => UnaryOp::Lnot,
            _ => unimplemented!("Bad compiler."),
        }
    }
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
    Index(Box<Index>),
    Call(Box<Call>),
    Unary(Box<Unary>),
    Binary(Box<Binary>),
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
            Expr::Index(e) => e.span,
            Expr::Call(e) => e.span,
            Expr::Unary(e) => e.span,
            Expr::Binary(e) => e.span,
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
            | Expr::Index(_)
            | Expr::Call(_)
            | Expr::Unary(_)
            | Expr::Group(_) => false,
            Expr::Block(_) | Expr::If(_) | Expr::Case(_) => true,
            Expr::Binary(b) => b.rhs.ends_with_block(),
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
