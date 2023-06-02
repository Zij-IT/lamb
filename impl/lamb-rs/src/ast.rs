use crate::ffi::AstNode_T;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Either<L, R> {
    Left(L),
    Right(R),
}
#[derive(Debug, Clone, PartialEq)]
pub struct Script {
    pub block: Block,
}

impl Script {
    pub fn to_ptr(self) -> *mut AstNode_T {
        crate::convert::Convert::convert(self)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Statement {
    Assign(Assign),
    Expr(Expr),
    Return(Option<Expr>),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Assign {
    pub assignee: Ident,
    pub value: Expr,
}

#[derive(Clone, PartialEq, Eq)]
pub enum Expr {
    Binary(Binary),
    Unary(Unary),
    FuncCall(FuncCall),
    Index(Index),
    If(If),
    Case(Case),
    FuncDef(FuncDef),
    Block(Block),
    Atom(Atom),
}

impl std::fmt::Debug for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::Binary(i) => write!(f, "{i:#?}"),
            Expr::Unary(i) => write!(f, "{i:#?}"),
            Expr::FuncCall(i) => write!(f, "{i:#?}"),
            Expr::Index(i) => write!(f, "{i:#?}"),
            Expr::If(i) => write!(f, "{i:#?}"),
            Expr::Case(i) => write!(f, "{i:#?}"),
            Expr::FuncDef(i) => write!(f, "{i:#?}"),
            Expr::Block(i) => write!(f, "{i:#?}"),
            Expr::Atom(i) => write!(f, "{i:#?}"),
        }
    }
}

impl<T> From<T> for Expr
where
    T: Into<Atom>,
{
    fn from(value: T) -> Self {
        Self::Atom(value.into())
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Binary {
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
    pub op: BinaryOp,
}

impl Binary {
    pub fn new(lhs: Expr, rhs: Expr, op: BinaryOp) -> Self {
        Self {
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
            op,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BinaryOp {
    Add,
    Sub,
    Div,
    Mul,
    Mod,
    LApply,
    RApply,
    LCompose,
    RCompose,
    Gt,
    Ge,
    Lt,
    Le,
    Eq,
    Ne,
    LogOr,
    LogAnd,
    BinOr,
    BinAnd,
    BinXor,
    RShift,
    LShift,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Unary {
    pub rhs: Box<Expr>,
    pub op: UnaryOp,
}

impl Unary {
    pub fn new(rhs: Expr, op: UnaryOp) -> Self {
        Self {
            rhs: Box::new(rhs),
            op,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum UnaryOp {
    NumNeg,
    LogNot,
    BinNot,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FuncCall {
    pub callee: Box<Expr>,
    pub args: Vec<Expr>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Index {
    pub indexee: Box<Expr>,
    pub index: Box<Expr>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct If {
    pub cond: Box<Expr>,
    pub block: Box<Block>,
    pub elifs: Vec<Elif>,
    pub els: Option<Box<Else>>,
}

impl If {
    pub fn new(cond: Expr, block: Block, elifs: Vec<Elif>, els: Option<Else>) -> Self {
        Self {
            cond: Box::new(cond),
            block: Box::new(block),
            elifs,
            els: els.map(Box::new),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Case {
    pub value: Box<Expr>,
    pub arms: Vec<CaseArm>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CaseArm {
    pub pattern: Either<Literal, Ident>,
    pub on_match: Either<Block, Expr>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Literal {
    Num(i64),
    Str(String),
    Char(char),
    Bool(bool),
    Nil,
}

impl From<i64> for Literal {
    fn from(value: i64) -> Self {
        Self::Num(value)
    }
}

impl From<String> for Literal {
    fn from(value: String) -> Self {
        Self::Str(value)
    }
}

impl From<char> for Literal {
    fn from(value: char) -> Self {
        Self::Char(value)
    }
}

impl From<bool> for Literal {
    fn from(value: bool) -> Self {
        Self::Bool(value)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FuncDef {
    pub args: Vec<Ident>,
    pub body: Box<Expr>,
    pub is_recursive: bool,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Block {
    pub stats: Vec<Statement>,
    pub value: Option<Box<Expr>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Elif {
    pub cond: Expr,
    pub block: Block,
}

impl Elif {
    pub fn new(cond: Expr, block: Block) -> Self {
        Self { cond, block }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Else {
    pub block: Block,
}

impl Else {
    pub fn new(block: Block) -> Self {
        Self { block }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Ident(pub String);

impl<T> From<T> for Ident
where
    T: Into<String>,
{
    fn from(value: T) -> Self {
        Self(value.into())
    }
}

#[derive(Clone, PartialEq, Eq)]
pub enum Atom {
    Literal(Literal),
    Ident(Ident),
    Array(Vec<Expr>),
}

impl std::fmt::Debug for Atom {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Atom::Literal(l) => write!(f, "{l:#?}"),
            Atom::Ident(i) => write!(f, "{i:#?}"),
            Atom::Array(v) => write!(f, "{v:#?}"),
        }
    }
}

impl From<Literal> for Atom {
    fn from(value: Literal) -> Self {
        Self::Literal(value)
    }
}

impl From<Ident> for Atom {
    fn from(value: Ident) -> Self {
        Self::Ident(value)
    }
}

impl From<Vec<Expr>> for Atom {
    fn from(value: Vec<Expr>) -> Self {
        Self::Array(value)
    }
}
