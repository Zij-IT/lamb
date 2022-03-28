use super::span::Spanned;

// Program ::= [Exports] { Imports } { Statement } [ Expression ]
#[derive(Clone, PartialEq, Debug)]
pub struct Program {
    pub exports: Option<Spanned<Export>>,
    pub imports: Vec<Spanned<Import>>,
    pub statements: Vec<Spanned<Statement>>,
}

// Exports ::= 'export' '(' [ Ident {',' Ident }] ')'
#[derive(Clone, PartialEq, Debug)]
pub struct Export {
    pub exports: Vec<Spanned<Ident>>,
}

// Imports ::= 'import' Ident ['(' [ Ident {',' Ident }] ')'
#[derive(Clone, PartialEq, Debug)]
pub struct Import {
    pub from: Spanned<Ident>,
    pub imports: Vec<Spanned<Ident>>,
}

// Statement ::= ';'
//             | Definition
//             | ExpressionStatement
#[derive(Clone, PartialEq, Debug)]
pub enum Statement {
    // ;
    Empty,

    // Expression ';'
    ExprStmt(Spanned<Expr>),

    // Ident := Expression ';'
    ValueDefinition(Spanned<Ident>, Spanned<Expr>),

    // Ident := [ Ident {',' Ident } ] '->' Expression ';'
    FunctionDefinition(Spanned<Ident>, Vec<Spanned<Ident>>, Spanned<Expr>),
}

#[derive(Clone, PartialEq, Debug)]
pub enum Expr {
    // --- ExpressionsWithoutBlock ---{
    // 1 1.0 'c' "string" true
    Literal(Spanned<Literal>),

    // (a-z|A-Z|'_') { (a-z|A-Z|'_') }
    Ident(Spanned<Ident>),

    // Expression BinaryOp Expression
    Binary(Spanned<BinaryOp>, Box<Spanned<Self>>, Box<Spanned<Self>>),

    // UnaryOp Expression
    Unary(Spanned<UnaryOp>, Box<Spanned<Self>>),

    // '[' [ Expression {',' Expression } ] ']'
    List(Vec<Spanned<Self>>),

    // Expression '[' Expression ']'
    ListIndex(Box<Spanned<Self>>, Box<Spanned<Self>>),

    // '(' Expression ',' { Expression ',' } ')'
    Tuple(Vec<Spanned<Self>>),

    // Expression '.' IntegerLiteral
    // TupleIndex(Box<Self>, Box<Self>),

    // Expression '(' [ Expression {',' Expression } ] ')'
    Call(Box<Spanned<Self>>, Vec<Spanned<Self>>),

    // '\'[ Ident {',' Ident } ] '->' Expression
    Lambda(Vec<Spanned<Ident>>, Box<Spanned<Self>>),

    // 'continue'
    Continue,

    // 'break' [ Expression ]
    Break(Box<Spanned<Self>>),

    // 'return' [ Expression ]
    Return(Box<Spanned<Self>>),
    // --- ExpressionsWithoutBlock ---}

    // --- ExpressionWithBlock ---{
    BlockExpression(Vec<Spanned<Statement>>, Option<Box<Spanned<Self>>>),
    Loop(Box<Spanned<Self>>),
    For(Spanned<Ident>, Box<Spanned<Self>>, Box<Spanned<Self>>),
    While(Box<Spanned<Self>>, Box<Spanned<Self>>),
    If(Box<Spanned<Self>>, Box<Spanned<Self>>, Vec<(Spanned<Self>, Spanned<Self>)>, Option<Box<Spanned<Self>>>), // --- ExpressionWithBlock ---}
}

#[derive(Clone, PartialEq, Debug)]
pub enum Literal {
    Int(i64),
    Real(f64),
    Str(String),
    Char(char),
    Bool(bool),
}

impl std::fmt::Display for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Int(i) => write!(f, "{}", i),
            Self::Real(r) => write!(f, "{}", r),
            Self::Str(s) => write!(f, "{}", s),
            Self::Char(c) => write!(f, "{}", c),
            Self::Bool(b) => write!(f, "{}", b),
        }
    }
}

#[derive(Clone, PartialEq, Debug)]
pub struct Ident(String);

impl Ident {
    pub fn new(s: String) -> Self {
        Self(s)
    }
}

impl std::fmt::Display for Ident {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum BinaryOp {
    // Arithmetic
    Add,
    Sub,
    Div,
    Mul,
    Rem,

    // Bitwise
    BitAnd,
    BitOr,
    BitXor,

    // Logical
    LogAnd,
    LogOr,

    // Comparison
    Lt,
    Gt,
    Le,
    Ge,
    EqEq,
    NotEq,

    // Other
    Concat,
    Range,

    // Function
    ApplyLeft,
    ApplyRight,
    ComposeLeft,
    ComposeRight,
}

impl std::fmt::Display for BinaryOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            // Operators
            Self::Add => write!(f, "+"),
            Self::Sub => write!(f, "-"),
            Self::Mul => write!(f, "*"),
            Self::Div => write!(f, "/"),
            Self::Rem => write!(f, "%"),

            Self::BitAnd => write!(f, "&"),
            Self::BitOr => write!(f, "|"),
            Self::BitXor => write!(f, "^"),

            Self::LogAnd => write!(f, "&&"),
            Self::LogOr => write!(f, "||"),

            Self::Lt => write!(f, "<"),
            Self::Gt => write!(f, ">"),
            Self::Le => write!(f, "<="),
            Self::Ge => write!(f, ">="),
            Self::EqEq => write!(f, "=="),
            Self::NotEq => write!(f, "~="),

            Self::Concat => write!(f, "++"),
            Self::Range => write!(f, ".."),

            Self::ApplyLeft => write!(f, "<$"),
            Self::ApplyRight => write!(f, "$>"),
            Self::ComposeLeft => write!(f, "<."),
            Self::ComposeRight => write!(f, ".>"),
        }
    }
}

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum UnaryOp {
    Not,
    Neg,
}

impl std::fmt::Display for UnaryOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Not => write!(f, "~"),
            Self::Neg => write!(f, "-"),
        }
    }
}
