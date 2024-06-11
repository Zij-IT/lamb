use crate::{Span, TokKind};

pub trait Spanned {
    fn span(&self) -> Span;
}

impl Spanned for Ident {
    fn span(&self) -> Span {
        self.span
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum I64Base {
    Bin,
    Oct,
    Dec,
    Hex,
}

impl I64Base {
    pub fn to_base(&self) -> u32 {
        match self {
            I64Base::Bin => 2,
            I64Base::Oct => 8,
            I64Base::Dec => 10,
            I64Base::Hex => 16,
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct I64Lit {
    pub base: I64Base,
    pub value: String,
    pub span: Span,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct F64Lit {
    pub value: String,
    pub span: Span,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct BoolLit {
    pub value: bool,
    pub span: Span,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct NilLit {
    pub span: Span,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct StrLit {
    pub text: Option<StrText>,
    pub span: Span,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct StrText {
    pub inner: String,
    pub span: Span,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct CharLit {
    pub text: Option<CharText>,
    pub span: Span,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct CharText {
    pub inner: String,
    pub span: Span,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Ident {
    pub raw: String,
    pub span: Span,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct List<IdKind> {
    pub values: Vec<Expr<IdKind>>,
    pub span: Span,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Group<IdKind> {
    pub value: Expr<IdKind>,
    pub span: Span,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct FnDef<IdKind> {
    pub args: Vec<IdKind>,
    pub body: Expr<IdKind>,
    pub recursive: bool,
    pub span: Span,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Block<IdKind> {
    pub statements: Vec<Statement<IdKind>>,
    pub value: Option<Expr<IdKind>>,
    pub span: Span,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct IfCond<IdKind> {
    pub cond: Expr<IdKind>,
    pub body: Block<IdKind>,
    pub span: Span,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Else<IdKind> {
    pub body: Block<IdKind>,
    pub span: Span,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct If<IdKind> {
    pub cond: IfCond<IdKind>,
    pub elif: Vec<IfCond<IdKind>>,
    pub els_: Option<Else<IdKind>>,
    pub span: Span,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Case<IdKind> {
    pub scrutinee: Expr<IdKind>,
    pub arms: Vec<CaseArm<IdKind>>,
    pub span: Span,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct CaseArm<IdKind> {
    pub pattern: Pattern<IdKind>,
    pub body: Expr<IdKind>,
    pub span: Span,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Pattern<IdKind> {
    pub inner: Vec<InnerPattern<IdKind>>,
    pub span: Span,
}
impl<IdKind> Pattern<IdKind> {
    // TODO: Get rid of this when introducing hir
    pub fn binding_names(&self) -> Vec<&IdKind> {
        self.inner.iter().flat_map(InnerPattern::binding_names).collect()
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum InnerPattern<IdKind> {
    Literal(Box<LiteralPattern>),
    Array(Box<ArrayPattern<IdKind>>),
    Ident(Box<IdentPattern<IdKind>>),
    Rest(Box<RestPattern>),
}

impl<IdKind> InnerPattern<IdKind> {
    // TODO: Get rid of this when introducing hir
    pub fn binding_names(&self) -> Vec<&IdKind> {
        match self {
            InnerPattern::Literal(_) => vec![],
            InnerPattern::Rest(_) => vec![],
            InnerPattern::Array(arr) => {
                arr.patterns.iter().flat_map(Pattern::binding_names).collect()
            }
            InnerPattern::Ident(id) => {
                vec![&id.ident]
            }
        }
    }

    pub fn span(&self) -> Span {
        match self {
            InnerPattern::Literal(p) => p.span(),
            InnerPattern::Array(p) => p.span,
            InnerPattern::Ident(p) => p.span,
            InnerPattern::Rest(p) => p.span,
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct RestPattern {
    pub span: Span,
}

#[derive(Clone, Debug, Eq, PartialEq)]
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

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct IdentPattern<IdKind> {
    pub ident: IdKind,
    pub bound: Option<Box<InnerPattern<IdKind>>>,
    pub span: Span,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ArrayPattern<IdKind> {
    pub patterns: Vec<Pattern<IdKind>>,
    pub span: Span,
}
impl<IdKind> ArrayPattern<IdKind> {
    pub fn as_parts(
        &self,
    ) -> (&[Pattern<IdKind>], &[Pattern<IdKind>], Option<&InnerPattern<IdKind>>)
    {
        let mut splits =
            self.patterns.split(|pat| match pat.inner.as_slice() {
                [InnerPattern::Rest(..)] => true,
                [InnerPattern::Ident(ip)] => {
                    matches!(ip.bound.as_deref(), Some(InnerPattern::Rest(..)))
                }
                _ => false,
            });

        let head = splits.next().unwrap_or_default();
        let (tail, rest) = if let Some(tail) = splits.next() {
            (tail, self.patterns[head.len()].inner.get(0))
        } else {
            ([].as_slice(), None)
        };

        (head, tail, rest)
    }

    pub fn head_span(&self) -> Option<Span> {
        let head = self.as_parts().0;
        head.first()
            .zip(head.last())
            .map(|(first, last)| Span::connect(first.span, last.span))
    }

    pub fn tail_span(&self) -> Option<Span> {
        let tail = self.as_parts().1;
        tail.first()
            .zip(tail.last())
            .map(|(first, last)| Span::connect(first.span, last.span))
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Index<IdKind> {
    pub lhs: Expr<IdKind>,
    pub rhs: Expr<IdKind>,
    pub span: Span,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Call<IdKind> {
    pub callee: Expr<IdKind>,
    pub args: Vec<Expr<IdKind>>,
    pub span: Span,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Binary<IdKind> {
    pub lhs: Expr<IdKind>,
    pub op: BinaryOp,
    pub rhs: Expr<IdKind>,
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

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Unary<IdKind> {
    pub rhs: Expr<IdKind>,
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

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Return<IdKind> {
    pub value: Option<Expr<IdKind>>,
    pub span: Span,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Path {
    pub head: Ident,
    pub tail: Vec<Ident>,
    pub span: Span,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Expr<IdKind> {
    Ident(IdKind),
    Char(CharLit),
    String(StrLit),
    Bool(BoolLit),
    Nil(NilLit),
    I64(I64Lit),
    F64(F64Lit),
    List(List<IdKind>),
    Group(Box<Group<IdKind>>),
    FnDef(Box<FnDef<IdKind>>),
    Block(Box<Block<IdKind>>),
    If(Box<If<IdKind>>),
    Case(Box<Case<IdKind>>),
    Index(Box<Index<IdKind>>),
    Call(Box<Call<IdKind>>),
    Unary(Box<Unary<IdKind>>),
    Binary(Box<Binary<IdKind>>),
    Return(Box<Return<IdKind>>),
    Path(Box<Path>),
}

impl<IdKind: Spanned> Expr<IdKind> {
    pub fn span(&self) -> Span {
        match self {
            Expr::Ident(e) => e.span(),
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
            Expr::Return(e) => e.span,
            Expr::Path(e) => e.span,
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
            | Expr::Path(_)
            | Expr::Group(_) => false,
            Expr::Return(r) => {
                r.value.as_ref().map_or(false, |e| e.ends_with_block())
            }
            Expr::Unary(u) => u.rhs.ends_with_block(),
            Expr::Binary(b) => b.rhs.ends_with_block(),
            Expr::FnDef(f) => f.body.ends_with_block(),
            Expr::Block(_) | Expr::If(_) | Expr::Case(_) => true,
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Define<IdKind> {
    pub ident: IdKind,
    pub typ: Option<Type<IdKind>>,
    pub value: Expr<IdKind>,
    pub span: Span,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Type<IdKind> {
    /// Represents all function types
    Fn(Box<FnType<IdKind>>),
    /// Represents all other types. This is here
    /// because functions have named types,
    Named(Box<NamedType<IdKind>>),
}

impl<IdKind> Type<IdKind> {
    pub fn span(&self) -> Span {
        match self {
            Type::Fn(fun) => fun.span,
            Type::Named(nt) => nt.span,
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct FnType<IdKind> {
    pub args: Vec<Type<IdKind>>,
    pub gens: Option<Generics<IdKind>>,
    pub ret_type: Option<Type<IdKind>>,
    pub span: Span,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct NamedType<IdKind> {
    pub name: IdKind,
    pub gens: Option<Generics<IdKind>>,
    pub span: Span,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Generics<IdKind> {
    pub params: Vec<Generic<IdKind>>,
    pub span: Span,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Generic<IdKind> {
    pub id: IdKind,
    pub span: Span,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ExprStatement<IdKind> {
    pub expr: Expr<IdKind>,
    pub span: Span,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Statement<IdKind> {
    Define(Define<IdKind>),
    Expr(ExprStatement<IdKind>),
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Item<IdKind> {
    Def(Define<IdKind>),
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ImportItem<IdKind> {
    pub item: IdKind,
    pub alias: Option<IdKind>,
    pub span: Span,
}
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Import<IdKind, PathKind> {
    pub file: PathKind,
    pub name: Option<IdKind>,
    pub items: Vec<ImportItem<IdKind>>,
    pub star: bool,
    pub span: Span,
    pub path_span: Span,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ExportItem<IdKind> {
    pub item: IdKind,
    pub alias: Option<IdKind>,
    pub span: Span,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Export<IdKind> {
    pub items: Vec<ExportItem<IdKind>>,
    pub span: Span,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Module<IdKind, PathKind> {
    pub exports: Vec<Export<IdKind>>,
    pub imports: Vec<Import<IdKind, PathKind>>,
    pub items: Vec<Item<IdKind>>,
    pub path: PathKind,
    pub span: Span,
}
