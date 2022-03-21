use std::fmt::write;

#[derive(Clone, Debug, PartialEq, Hash, Eq)]
pub enum Token {
    // Operators
    Plus,
    Minus,
    Mul,
    Div,
    Rem,

    BitAnd,
    BitOr,
    BitXor,

    LogAnd,
    LogOr,

    Lt,
    Gt,
    Le,
    Ge,
    EqEq,
    NotEq,

    Not,
    Concat,
    Assign,

    // Values
    Int(String),
    Real(String),
    Str(String),
    Char(char),
    Bool(bool),

    // Control Flow
    If,
    Elif,
    Else,
    For,
    In,
    Break,
    Return,
    Continue,
    While,
    Loop,

    // Brackets
    ParenOpen,
    ParenClose,
    BraceOpen,
    BraceClose,
    BrackOpen,
    BrackClose,

    // Misc
    Colon,
    Lambda,
    Arrow,
    Comma,
    DotDot,
    Semicolon,
    Wildcard,
    Import,
    Export,
    Ident(String),
    Error(char),
}

impl Token {
    pub fn is_operator(&self) -> bool {
        matches!(
            self,
            Self::Plus
                | Self::Minus
                | Self::Mul
                | Self::Div
                | Self::Rem
                | Self::BitAnd
                | Self::BitOr
                | Self::BitXor
                | Self::LogAnd
                | Self::LogOr
                | Self::Lt
                | Self::Gt
                | Self::Le
                | Self::Ge
                | Self::EqEq
                | Self::NotEq
                | Self::Not
                | Self::Concat
        )
    }
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        #[allow(clippy::match_same_arms)]
        match self {
            // Operators
            Self::Plus => write!(f, "+"),
            Self::Minus => write!(f, "-"),
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

            Self::Not => write!(f, "~"),
            Self::Concat => write!(f, "++"),
            Self::Assign => write!(f, ":="),

            // Values
            Self::Str(s) => write!(f, "{}", s),
            Self::Int(i) => write!(f, "{}", i),
            Self::Real(r) => write!(f, "{}", r),
            Self::Char(c) => write!(f, "{}", c),
            Self::Bool(b) => write!(f, "{}", b),

            // Control Flow
            Self::If => write!(f, "if"),
            Self::Elif => write!(f, "elif"),
            Self::Else => write!(f, "else"),
            Self::For => write!(f, "for"),
            Self::In => write!(f, "in"),
            Self::Break => write!(f, "break"),
            Self::Return => write!(f, "return"),
            Self::Continue => write!(f, "continue"),
            Self::While => write!(f, "while"),
            Self::Loop => write!(f, "loop"),

            // Brackets
            Self::ParenOpen => write!(f, "("),
            Self::ParenClose => write!(f, ")"),
            Self::BraceOpen => write!(f, "{{"),
            Self::BraceClose => write!(f, "}}"),
            Self::BrackOpen => write!(f, "["),
            Self::BrackClose => write!(f, "]"),

            // Misc
            Self::Arrow => write!(f, "->"),
            Self::Lambda => write!(f, "\\"),
            Self::Colon => write!(f, ":"),
            Self::Semicolon => write!(f, ";"),
            Self::Comma => write!(f, ","),
            Self::Wildcard => write!(f, "_"),
            Self::DotDot => write!(f, ".."),
            Self::Import => write!(f, "import"),
            Self::Export => write!(f, "export"),
            Self::Ident(i) => write!(f, "{}", i),
            Self::Error(c) => write!(f, "{}", c),
        }
    }
}
