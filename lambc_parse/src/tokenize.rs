use crate::FileId;

pub enum TokKind {
    OpenBrace,
    CloseBrace,
    OpenBrack,
    CloseBrack,
    OpenParen,
    CloseParen,

    Nil,
    I64,
    Char,
    Double,
    True,
    False,
    String,

    Add,
    Sub,
    Div,
    Mul,
    Mod,
    Band,
    Bor,
    Xor,
    Bneg,
    Shl,
    Shr,
    Eq,
    Ne,
    Gt,
    Lt,
    Ge,
    Le,
    LogAnd,
    LogOr,
    LogNot,
    LCompose,
    RCompose,
    LApply,
    RApply,
}

pub struct Token<'a> {
    kind: TokKind,
    slice: &'a str,
}

pub struct Lexer<'a> {
    input: &'a [u8],
    file: FileId,
    at: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a [u8], file: FileId) -> Self {
        Self { input, file, at: 0 }
    }

    pub fn next_token(&mut self) -> Option<Token> {
        match self.current() {
            byte if byte.is_ascii_alphabetic() || byte == b'_' => self.identlike(),
            byte if byte.is_ascii_digit() => self.number(),
            b'{' => self.simple(TokKind::Todo),
            b'}' => self.simple(TokKind::Todo),
            b'[' => self.simple(TokKind::Todo),
            b']' => self.simple(TokKind::Todo),
            b'(' => self.simple(TokKind::Todo),
            b')' => self.simple(TokKind::Todo),
            b'@' => self.simple(TokKind::Todo),
            b';' => self.simple(TokKind::Todo),
            b',' => self.simple(TokKind::Todo),
            b'+' => self.simple(TokKind::Todo),
            b'*' => self.simple(TokKind::Todo),
            b'/' => self.simple(TokKind::Todo),
            b'%' => self.simple(TokKind::Todo),
            b'=' => self.simple(TokKind::Todo),
            b'^' => self.simple(TokKind::Todo),
            b'~' => self.simple(TokKind::Todo),
            b'<' => self.less(),
            b'>' => self.greater(),
            b'.' => self.dot(),
            b':' => self.colon(),
            b'-' => self.dash(),
            b'!' => self.bang(),
            b'|' => self.pipe(),
            b'&' => self.and(),
            b'$' => self.dollar(),
            b'"' => self.string(),
            b'\'' => self.char(),
            b'\0' => self.simple(Token::End),
            _ => todo!(),
        }
    }

    fn current(&self) -> u8 {
        self.input.get(self.at).copied().unwrap_or(0)
    }

    fn next(&self) -> u8 {
        self.input.get(self.at + 1).copied().unwrap_or(0)
    }

    fn simple(&mut self, token: Token) -> Option<(Token, Span)> {
        self.at += 1;
        let start = self.at - 1;
        let end = self.at;

        Some((token, Span::new(start, end, self.file)))
    }

    fn identlike(&mut self) -> Option<(Token, Span)> {
        let start = self.at;
        self.at += 1;
        self.continue_ident();
        let slice = self.slice(start, self.at);
        let tok = match slice {
            "fn" => Token::Fn,
            "if" => Token::If,
            "nil" => Token::Nil,
            "rec" => Token::Rec,
            "case" => Token::Case,
            "enum" => Token::Enum,
            "elif" => Token::Elif,
            "else" => Token::Else,
            "true" => Token::Bool(true),
            "false" => Token::Bool(false),
            "struct" => Token::Struct,
            "return" => Token::Return,
            _ => Token::Ident(slice.into()),
        };

        Some((tok, Span::new(start, self.at, self.file)))
    }

    fn continue_ident(&mut self) {
        loop {
            let byte = self.current();
            if byte.is_ascii_alphanumeric() || byte == b'_' {
                self.at += 1;
            } else {
                break;
            }
        }
    }

    fn number(&mut self) -> Option<(Token, Span)> {
        let start = self.at;

        let curr = self.current();
        let tok = if curr == b'0' && matches!(self.next(), b'X' | b'x') {
            while self.current().is_ascii_hexdigit() {
                self.at += 1;
            }
        } else if curr == b'0' && matches!(self.next(), b'b' | b'B') {
            while self.current().is_ascii_hexdigit() {
                self.at += 1;
            }
        } else if curr == b'0' && matches!(self.next(), b'o' | b'O') {
            while self.current().is_ascii_hexdigit() {
                self.at += 1;
            }
        } else {
        };

        Some((tok, Span::new(start, self.at, self.file)))
    }

    fn less(&mut self) -> Option<(Token, Span)> {
        None
    }

    fn greater(&mut self) -> Option<(Token, Span)> {
        None
    }

    fn dot(&mut self) -> Option<(Token, Span)> {
        None
    }

    fn colon(&mut self) -> Option<(Token, Span)> {
        None
    }

    fn dash(&mut self) -> Option<(Token, Span)> {
        None
    }

    fn bang(&mut self) -> Option<(Token, Span)> {
        None
    }

    fn pipe(&mut self) -> Option<(Token, Span)> {
        None
    }

    fn and(&mut self) -> Option<(Token, Span)> {
        None
    }

    fn dollar(&mut self) -> Option<(Token, Span)> {
        None
    }

    fn string(&mut self) -> Option<(Token, Span)> {
        None
    }

    fn char(&mut self) -> Option<(Token, Span)> {
        None
    }

    fn slice(&self, start: usize, end: usize) -> &str {
        std::str::from_utf8(&self.input[start..end]).unwrap()
    }
}
