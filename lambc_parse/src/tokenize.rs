use std::borrow::Cow;

use crate::{FileId, Span};

/// A type representing what the `slice` field of the [`Token`] type
/// contains.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum TokKind {
    // Delimiters
    /// '{'
    OpenBrace,
    /// '}'
    CloseBrace,
    /// '['
    OpenBrack,
    /// ']'
    CloseBrack,
    /// '('
    OpenParen,
    /// ')'
    CloseParen,

    // Literals
    /// The literal `nil`
    Nil,
    /// Signed 8-Byte integer literal
    I64,
    /// The `char` literal which has the same semantics as the Rust [`char`](https://doc.rust-lang.org/std/primitive.char.html)
    /// type
    Char,
    /// The `f64` literal`
    F64,
    /// The literal `true`
    True,
    /// The literal `false`
    False,
    /// The beginning '"' of a string literal
    StringStart,
    /// The text within a string literal
    StringText,
    /// The end '"' of a string literal
    StringEnd,

    // Operators
    /// '+'
    Add,
    /// '-'
    Sub,
    /// '/'
    Div,
    /// '*'
    Mul,
    /// '%'
    Mod,
    /// '&'
    Band,
    /// '|'
    Bor,
    /// '^'
    Xor,
    /// '~'
    Bneg,
    /// '<<'
    Shl,
    /// '>>'
    Shr,
    /// '='
    Eq,
    /// '!='
    Ne,
    /// '>'
    Gt,
    /// '<'
    Lt,
    /// '>='
    Ge,
    /// '<='
    Le,
    /// '&&'
    Land,
    /// '||'
    Lor,
    /// '!'
    Lnot,
    /// '<.'
    Cpsl,
    /// '.>'
    Cpsr,
    /// '<$'
    Appl,
    /// '$>'
    Appr,
    /// ':='
    Assign,

    // Keyword
    /// 'fn'
    Fn,
    /// 'if'
    If,
    /// 'elif'
    Elif,
    /// 'else'
    Else,
    /// 'rec'
    Rec,
    /// 'case'
    Case,
    /// 'union'
    Union,
    /// 'struct'
    Struct,
    /// 'return'
    Return,
    /// '/[A-Za-z_][A-Za-z_0-9]*/'
    Ident,

    // Syntax
    /// ','
    Comma,
    /// ';'
    Semi,
    /// '@'
    Bind,
    /// '->'
    Arrow,

    // Meta
    /// Used to indicate a comment
    Comment,
    /// Used to indicate the end of the input
    End,
    /// Used to indicate a character not usable by the lexer
    Invalid,
}

enum State {
    String,
    Default,
}

/// Represents a lexeme from the lamb language. See [`TokKind`] for the possible
/// token kinds.
pub struct Token<'a> {
    kind: TokKind,
    span: Span,
    slice: Cow<'a, str>,
}

pub struct Lexer<'a> {
    input: &'a [u8],
    state: State,
    file: FileId,
    at: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a [u8], file: FileId) -> Self {
        Self {
            state: State::Default,
            input,
            file,
            at: 0,
        }
    }

    pub fn next_token(&mut self) -> Token<'a> {
        match self.state {
            State::Default => self.default_token(),
            State::String => self.string_token(),
        }
    }

    fn default_token(&mut self) -> Token<'a> {
        match self.current() {
            byte if byte.is_ascii_alphabetic() || byte == b'_' => self.identlike(),
            byte if byte.is_ascii_digit() => self.number(),
            b'{' => self.simple(TokKind::OpenBrace),
            b'}' => self.simple(TokKind::CloseBrace),
            b'[' => self.simple(TokKind::OpenBrack),
            b']' => self.simple(TokKind::CloseBrack),
            b'(' => self.simple(TokKind::OpenParen),
            b')' => self.simple(TokKind::CloseParen),
            b'@' => self.simple(TokKind::Bind),
            b';' => self.simple(TokKind::Semi),
            b',' => self.simple(TokKind::Comma),
            b'+' => self.simple(TokKind::Add),
            b'*' => self.simple(TokKind::Mul),
            b'/' => self.simple(TokKind::Div),
            b'%' => self.simple(TokKind::Mod),
            b'=' => self.simple(TokKind::Eq),
            b'^' => self.simple(TokKind::Xor),
            b'~' => self.simple(TokKind::Bneg),
            b'<' => self.less(),
            b'>' => self.greater(),
            b'.' => self.dot(),
            b':' => self.colon(),
            b'-' => self.dash(),
            b'!' => self.bang(),
            b'|' => self.pipe(),
            b'&' => self.and(),
            b'$' => self.dollar(),
            b'"' => self.string_start(),
            b'\'' => self.char(),
            b'\0' if self.at_end() => self.token(self.at, TokKind::End),
            _ => self.simple(TokKind::Invalid),
        }
    }

    fn string_token(&mut self) -> Token<'a> {
        let start = self.at;
        match self.current() {
            b'"' => self.string_end(),
            _ if !self.at_end() => self.string_text(),
            _ => self.token(start, TokKind::End),
        }
    }

    fn current(&self) -> u8 {
        self.input.get(self.at).copied().unwrap_or(0)
    }

    fn next(&self) -> u8 {
        self.input.get(self.at + 1).copied().unwrap_or(0)
    }

    fn simple(&mut self, kind: TokKind) -> Token<'a> {
        let start = self.at;
        self.at += 1;

        self.token(start, kind)
    }

    fn identlike(&mut self) -> Token<'a> {
        let start = self.at;
        self.at += 1;
        self.continue_ident();
        let slice = self.slice(start, self.at);
        let kind = match slice {
            "fn" => TokKind::Fn,
            "if" => TokKind::If,
            "nil" => TokKind::Nil,
            "rec" => TokKind::Rec,
            "case" => TokKind::Case,
            "elif" => TokKind::Elif,
            "else" => TokKind::Else,
            "true" => TokKind::True,
            "false" => TokKind::False,
            "union" => TokKind::Union,
            "struct" => TokKind::Struct,
            "return" => TokKind::Return,
            _ => TokKind::Ident,
        };

        self.token(start, kind)
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

    fn string_start(&mut self) -> Token<'a> {
        self.state = State::String;
        self.simple(TokKind::StringStart)
    }

    fn string_text(&mut self) -> Token<'a> {
        todo!()
    }

    fn string_end(&mut self) -> Token<'a> {
        self.state = State::Default;
        self.simple(TokKind::StringEnd)
    }

    fn number(&mut self) -> Token<'a> {
        todo!()
    }

    fn less(&mut self) -> Token<'a> {
        let start = self.at;
        match self.next() {
            b'$' => self.token_from(start, start + 2, TokKind::Appl),
            b'.' => self.token_from(start, start + 2, TokKind::Cpsl),
            b'<' => self.token_from(start, start + 2, TokKind::Shl),
            b'=' => self.token_from(start, start + 2, TokKind::Le),
            _ => self.token_from(start, start + 1, TokKind::Lt),
        }
    }

    fn greater(&mut self) -> Token<'a> {
        let start = self.at;
        match self.next() {
            b'>' => self.token_from(start, start + 2, TokKind::Shr),
            b'=' => self.token_from(start, start + 2, TokKind::Ge),
            _ => self.token_from(start, start + 1, TokKind::Gt),
        }
    }

    fn dot(&mut self) -> Token<'a> {
        let start = self.at;
        match self.next() {
            b'>' => self.token_from(start, start + 2, TokKind::Cpsr),
            _ => self.token_from(start, start + 1, TokKind::Invalid),
        }
    }

    fn colon(&mut self) -> Token<'a> {
        let start = self.at;
        match self.next() {
            b'=' => self.token_from(start, start + 2, TokKind::Assign),
            _ => self.token_from(start, start + 1, TokKind::Invalid),
        }
    }

    fn dash(&mut self) -> Token<'a> {
        let start = self.at;
        match self.next() {
            b'>' => self.token_from(start, start + 2, TokKind::Arrow),
            b'-' => self.eat_comment(),
            _ => self.token_from(start, start + 1, TokKind::Sub),
        }
    }

    fn bang(&mut self) -> Token<'a> {
        let start = self.at;
        match self.next() {
            b'=' => self.token_from(start, start + 2, TokKind::Ne),
            _ => self.token_from(start, start + 1, TokKind::Lnot),
        }
    }

    fn pipe(&mut self) -> Token<'a> {
        let start = self.at;
        match self.next() {
            b'|' => self.token_from(start, start + 2, TokKind::Lor),
            _ => self.token_from(start, start + 1, TokKind::Bor),
        }
    }

    fn and(&mut self) -> Token<'a> {
        let start = self.at;
        match self.next() {
            b'&' => self.token_from(start, start + 2, TokKind::Land),
            _ => self.token_from(start, start + 1, TokKind::Band),
        }
    }

    fn dollar(&mut self) -> Token<'a> {
        let start = self.at;
        match self.next() {
            b'>' => self.token_from(start, start + 2, TokKind::Appr),
            _ => self.token_from(start, start + 1, TokKind::Invalid),
        }
    }

    fn char(&mut self) -> Token<'a> {
        todo!()
    }

    fn at_end(&self) -> bool {
        self.at >= self.input.len()
    }

    fn token(&self, start: usize, kind: TokKind) -> Token<'a> {
        Token {
            kind,
            span: Span::new(start, self.at, self.file),
            slice: self.slice(start, self.at).into(),
        }
    }

    fn token_from(&mut self, start: usize, end: usize, kind: TokKind) -> Token<'a> {
        self.at = end;
        Token {
            kind,
            span: Span::new(start, end, self.file),
            slice: self.slice(start, end).into(),
        }
    }

    fn slice(&self, start: usize, end: usize) -> &'a str {
        std::str::from_utf8(&self.input[start..end]).unwrap()
    }

    fn eat_comment(&mut self) -> Token<'a> {
        let start = self.at;
        while !self.at_end() && self.current() != b'\n' {
            self.at += 1;
        }

        // If a comment ends a file, and the `+1` was added, then it
        // would go beyond the length of the input, and fail.
        if !self.at_end() {
            self.at += 1;
        }

        self.token_from(start, self.at, TokKind::Comment)
    }
}

#[cfg(test)]
mod tests {
    use super::{Lexer, TokKind};
    use crate::FileId;

    fn lex_one(input: &str, kind: TokKind) {
        let mut lexer = Lexer::new(input.as_bytes(), FileId(0));
        let tok = lexer.next_token();
        assert_eq!(tok.kind, kind);
        assert_eq!(tok.slice, input);
        assert!(lexer.at_end());
    }

    fn lex_mult(input: &str, kinds: &[TokKind]) {
        let mut lexer = Lexer::new(input.as_bytes(), FileId(0));
        let mut kinds = kinds.into_iter().copied();

        while let (Some(kind), tok) = (kinds.next(), lexer.next_token()) {
            assert_eq!(kind, tok.kind);
            assert_eq!(&input[tok.span.start..tok.span.end], tok.slice);
        }

        assert!(lexer.at_end());
    }

    #[test]
    fn lexes_simple() {
        // Syntax
        lex_one(";", TokKind::Semi);
        lex_one("@", TokKind::Bind);
        lex_one(",", TokKind::Comma);

        // Delimiters
        lex_one("{", TokKind::OpenBrace);
        lex_one("[", TokKind::OpenBrack);
        lex_one("(", TokKind::OpenParen);
        lex_one(")", TokKind::CloseParen);
        lex_one("]", TokKind::CloseBrack);
        lex_one("}", TokKind::CloseBrace);

        // Operators
        lex_one("+", TokKind::Add);
        lex_one("*", TokKind::Mul);
        lex_one("/", TokKind::Div);
        lex_one("%", TokKind::Mod);
        lex_one("=", TokKind::Eq);
        lex_one("^", TokKind::Xor);
        lex_one("~", TokKind::Bneg);
    }

    #[test]
    fn lexes_none() {
        lex_one("", TokKind::End);
    }

    #[test]
    fn lexes_invalid() {
        lex_one("#", TokKind::Invalid);
    }

    #[test]
    fn lexes_multiple_simple() {
        let input = ";@,{[()]}+*/%=^~";
        let kinds = [
            TokKind::Semi,
            TokKind::Bind,
            TokKind::Comma,
            TokKind::OpenBrace,
            TokKind::OpenBrack,
            TokKind::OpenParen,
            TokKind::CloseParen,
            TokKind::CloseBrack,
            TokKind::CloseBrace,
            TokKind::Add,
            TokKind::Mul,
            TokKind::Div,
            TokKind::Mod,
            TokKind::Eq,
            TokKind::Xor,
            TokKind::Bneg,
            TokKind::End,
        ];

        lex_mult(input, &kinds);
    }

    #[test]
    fn lexes_lt_start() {
        lex_one("<", TokKind::Lt);
        lex_one("<.", TokKind::Cpsl);
        lex_one("<$", TokKind::Appl);
        lex_one("<<", TokKind::Shl);
        lex_one("<=", TokKind::Le);

        let input = "<.<$<<<=<";
        let kinds = [
            TokKind::Cpsl,
            TokKind::Appl,
            TokKind::Shl,
            TokKind::Le,
            TokKind::Lt,
        ];

        lex_mult(input, &kinds);
    }

    #[test]
    fn lexes_gt_start() {
        lex_one(">", TokKind::Gt);
        lex_one(">=", TokKind::Ge);
        lex_one(">>", TokKind::Shr);

        let input = ">>>=>";
        let kinds = [TokKind::Shr, TokKind::Ge, TokKind::Gt];

        lex_mult(input, &kinds);
    }

    #[test]
    fn lexes_dot_start() {
        lex_one(".>", TokKind::Cpsr);
        lex_one(".", TokKind::Invalid);

        let input = ".>.";
        let kinds = [TokKind::Cpsr, TokKind::Invalid];
        lex_mult(input, &kinds);
    }

    #[test]
    fn lexes_colon_start() {
        lex_one(":=", TokKind::Assign);
        lex_one(":", TokKind::Invalid);

        let input = ":=:";
        let kinds = [TokKind::Assign, TokKind::Invalid];
        lex_mult(input, &kinds);
    }

    #[test]
    fn lexes_dash_start() {
        lex_one("-", TokKind::Sub);
        lex_one("->", TokKind::Arrow);
        lex_one("--", TokKind::Comment);
        lex_one("-- this is a comment", TokKind::Comment);

        let input = "->--comment\n-";
        let kinds = [TokKind::Arrow, TokKind::Comment, TokKind::Sub];
        lex_mult(input, &kinds);
    }

    #[test]
    fn lexes_bang_start() {
        lex_one("!", TokKind::Lnot);
        lex_one("!=", TokKind::Ne);

        let input = "!=!";
        let kinds = [TokKind::Ne, TokKind::Lnot];
        lex_mult(input, &kinds);
    }

    #[test]
    fn lexes_pipe_start() {
        lex_one("|", TokKind::Bor);
        lex_one("||", TokKind::Lor);

        let input = "|||";
        let kinds = [TokKind::Lor, TokKind::Bor];
        lex_mult(input, &kinds);
    }

    #[test]
    fn lexes_amp_start() {
        lex_one("&", TokKind::Band);
        lex_one("&&", TokKind::Land);

        let input = "&&&";
        let kinds = [TokKind::Land, TokKind::Band];
        lex_mult(input, &kinds);
    }

    #[test]
    fn lexes_dollar_start() {
        lex_one("$>", TokKind::Appr);
        lex_one("$", TokKind::Invalid);

        let input = "$>$";
        let kinds = [TokKind::Appr, TokKind::Invalid];
        lex_mult(input, &kinds);
    }

    #[test]
    fn lexes_identlike() {
        let kws = [
            "fn", "if", "nil", "rec", "case", "elif", "else", "true", "false", "union", "struct",
        ];

        let kinds = [
            TokKind::Fn,
            TokKind::If,
            TokKind::Nil,
            TokKind::Rec,
            TokKind::Case,
            TokKind::Elif,
            TokKind::Else,
            TokKind::True,
            TokKind::False,
            TokKind::Union,
            TokKind::Struct,
        ];

        for (&input, kind) in kws.iter().zip(kinds) {
            lex_one(input, kind);
        }

        let idents = ["fns", "map", "recu", "cases"];
        for ident in idents {
            lex_one(ident, TokKind::Ident);
        }
    }
}
