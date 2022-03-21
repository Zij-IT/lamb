#[derive(Clone, PartialEq, Debug)]
pub enum Expr {
    // Values
    Literal(Literal),
    Ident(Ident),
    List(Vec<Self>),
    Tuple(Vec<Self>),

    // Definitions
    ValueDef(Ident, Box<Self>),
    FunctionDef(Ident, Vec<Ident>, Box<Self>),

    // Operations
    Unary(UnaryOp, Box<Self>),
    Binary(Box<Self>, BinaryOp, Box<Self>),

    // Control Flow
    // if (cond, <block_expr>) elif (cond, <block_expr>) else <block_expr>
    // if (cond, <block_expr>) else <block_expr>
    // if (cond, <block_expr>)
    If((Box<Self>, Box<Self>), Vec<(Self, Self)>, Option<Box<Self>>),
    Block(Vec<Self>),

    // Misc
    Call(Ident, Vec<Self>),
    Error,
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
