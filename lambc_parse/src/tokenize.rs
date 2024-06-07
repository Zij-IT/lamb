use std::borrow::Cow;

use crate::Span;

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
    /// Signed 8-Byte integer literal with the `0b` prefix
    BinI64,
    /// Signed 8-Byte integer literal with the `0o` prefix
    OctI64,
    /// Signed 8-Byte integer literal with the `0x` prefix
    HexI64,
    /// Signed 8-Byte integer literal with the no prefix
    DecI64,
    /// The beginning '\'' of a string literal
    CharStart,
    /// The text of a `char` literal which has the same semantics as the Rust [`char`](https://doc.rust-lang.org/std/primitive.char.html)
    /// type
    CharText,
    /// The end '\'' of a string literal
    CharEnd,
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
    /// 'def'
    Def,
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
    /// '..'
    DotDot,
    /// '::'
    PathSep,

    // Meta
    /// Used to indicate a comment
    Comment,
    /// Used to indicate the end of the input
    End,
    /// Used to indicate a character not usable by the lexer
    Invalid,
}

/// Tracker for the state of the lexer.
enum State {
    /// Active when the lexer is in the middle of lexing a string literal
    String,
    /// Active when the lexer is in the middle of lexing a char literal
    Char,
    /// Active when the lexer is doing anything but lexing a string literal
    Default,
}

impl TokKind {
    pub fn desc(self) -> &'static str {
        match self {
            TokKind::BinI64
            | TokKind::OctI64
            | TokKind::HexI64
            | TokKind::DecI64 => "i64",
            TokKind::OpenBrace => "a '{'",
            TokKind::CloseBrace => "a '}'",
            TokKind::OpenBrack => "a '['",
            TokKind::CloseBrack => "a ']'",
            TokKind::OpenParen => "a '('",
            TokKind::CloseParen => "a ')'",
            TokKind::Nil => "a 'nil' literal",
            TokKind::CharStart => "a '''",
            TokKind::CharText => "the text of a char",
            TokKind::CharEnd => "a '''",
            TokKind::F64 => "a f64",
            TokKind::True => "true",
            TokKind::False => "false",
            TokKind::StringStart => "a '\"'",
            TokKind::StringText => "the text of a string",
            TokKind::StringEnd => "a '\"'",
            TokKind::Add => "a '+'",
            TokKind::Sub => "a '-'",
            TokKind::Div => "a '/'",
            TokKind::Mul => "a '*'",
            TokKind::Mod => "a '%'",
            TokKind::Band => "a '&'",
            TokKind::Bor => "a '|'",
            TokKind::Xor => "a '^'",
            TokKind::Bneg => "a '~'",
            TokKind::Shl => "a '<<'",
            TokKind::Shr => "a '>>'",
            TokKind::Eq => "a '='",
            TokKind::Ne => "a '!='",
            TokKind::Gt => "a '>'",
            TokKind::Lt => "a '<'",
            TokKind::Ge => "a '>='",
            TokKind::Le => "a '<='",
            TokKind::Land => "a '&&'",
            TokKind::Lor => "a '||'",
            TokKind::Lnot => "a '!'",
            TokKind::Cpsl => "a '<.'",
            TokKind::Cpsr => "a '.>'",
            TokKind::Appl => "a '<$'",
            TokKind::Appr => "a '$>'",
            TokKind::Assign => "a ':='",
            TokKind::Fn => "the 'fn' keyword",
            TokKind::If => "the 'if' keyword",
            TokKind::Def => "the 'def' keyword",
            TokKind::Elif => "the 'elif' keyword",
            TokKind::Else => "the 'else' keyword",
            TokKind::Rec => "the 'rec' keyword",
            TokKind::Case => "the 'case' keyword",
            TokKind::Union => "the 'union' keyword",
            TokKind::Struct => "the 'struct' keyword",
            TokKind::Return => "the 'return' keyword",
            TokKind::Ident => "an identifier",
            TokKind::Comma => "a ','",
            TokKind::Semi => "a ';'",
            TokKind::Bind => "a '@'",
            TokKind::Arrow => "a '->'",
            TokKind::DotDot => "the '..' pattern",
            TokKind::PathSep => "a '::'",
            TokKind::Comment => "a comment",
            TokKind::End => "the end of the input",
            TokKind::Invalid => "an unrecognized token",
        }
    }
}

/// Represents a lexeme from the lamb language. See [`TokKind`] for the possible
/// token kinds.
#[derive(Debug, PartialEq, Eq)]
pub struct Token<'a> {
    pub kind: TokKind,
    pub span: Span,
    pub slice: Cow<'a, str>,
}

/// A lexer for the Lamb language which features which outputs [`Token`](Token<'_>) using the
/// [`Lexer::next_token`] method.
pub struct Lexer<'a> {
    input: &'a [u8],
    state: State,
    at: usize,
}

impl<'a> Lexer<'a> {
    /// Constructs a new `Lexer` with the input.
    pub fn new(input: &'a [u8]) -> Self {
        Self { state: State::Default, input, at: 0 }
    }

    /// Returns the next [`Token`] in the input that isn't whitespace or a comment. If the lexer
    /// has reached the end of the input, it will return [`TokKind::End`]
    pub fn next_nontrival_token(&mut self) -> Token<'a> {
        let mut next = self.next_token();
        while next.kind == TokKind::Comment {
            next = self.next_token();
        }

        next
    }

    /// Returns the next [`Token`] in the input. If the lexer
    /// has reached the end of the input, it will return [`TokKind::End`]
    fn next_token(&mut self) -> Token<'a> {
        match self.state {
            State::Default => self.default_token(),
            State::Char => self.char_token(),
            State::String => self.string_token(),
        }
    }

    fn default_token(&mut self) -> Token<'a> {
        self.eat_whitespace();
        match self.current() {
            byte if byte.is_ascii_alphabetic() || byte == b'_' => {
                self.identlike()
            }
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
            b'\'' => self.char_start(),
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

    fn char_token(&mut self) -> Token<'a> {
        let start = self.at;
        match self.current() {
            b'\'' => self.char_end(),
            _ if !self.at_end() => self.char_text(),
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
            "def" => TokKind::Def,
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
        let start = self.at;
        let mut buffer = Vec::new();
        let mut use_buffer = false;
        let slice = loop {
            if use_buffer {
                match self.current() {
                    _ if self.at_end() => {
                        break String::from_utf8(buffer).unwrap().into()
                    }
                    b'"' => break String::from_utf8(buffer).unwrap().into(),
                    b':' => match self.next() {
                        b'n' => buffer.push(b'\n'),
                        b'r' => {
                            self.at += 1;
                            buffer.push(b'\r')
                        }
                        b't' => {
                            self.at += 1;
                            buffer.push(b'\t')
                        }
                        b':' => {
                            self.at += 1;
                            buffer.push(b':')
                        }
                        b'"' => {
                            self.at += 1;
                            buffer.push(b'"')
                        }
                        _ => (),
                    },
                    b => buffer.push(b),
                }
            } else {
                match self.current() {
                    b'"' => break self.slice(start, self.at).into(),
                    b':' => {
                        use_buffer = true;
                        buffer.extend(self.slice(start, self.at).as_bytes());
                        match self.next() {
                            b'n' => {
                                self.at += 1;
                                buffer.push(b'\n')
                            }
                            b'r' => {
                                self.at += 1;
                                buffer.push(b'\r')
                            }
                            b't' => {
                                self.at += 1;
                                buffer.push(b'\t')
                            }
                            b':' => {
                                self.at += 1;
                                buffer.push(b':')
                            }
                            b'"' => {
                                self.at += 1;
                                buffer.push(b'"')
                            }
                            _ => (),
                        }
                    }
                    _ if self.at_end() => {
                        break self.slice(start, self.at).into()
                    }
                    _ => (),
                }
            }

            self.at += 1;
        };

        Token {
            kind: TokKind::StringText,
            span: Span::new(start, self.at),
            slice,
        }
    }

    fn string_end(&mut self) -> Token<'a> {
        self.state = State::Default;
        self.simple(TokKind::StringEnd)
    }

    fn char_start(&mut self) -> Token<'a> {
        self.state = State::Char;
        self.simple(TokKind::CharStart)
    }

    fn char_text(&mut self) -> Token<'a> {
        let start = self.at;
        let mut buffer = String::new();
        let mut use_buffer = false;
        let slice = loop {
            if use_buffer {
                match self.current() {
                    _ if self.at_end() => break buffer.into(),
                    b'\'' => break buffer.into(),
                    b':' => match self.next() {
                        b'n' => buffer.push('n'),
                        b'r' => {
                            self.at += 1;
                            buffer.push('\r')
                        }
                        b't' => {
                            self.at += 1;
                            buffer.push('\t')
                        }
                        b':' => {
                            self.at += 1;
                            buffer.push(':')
                        }
                        b'\'' => {
                            self.at += 1;
                            buffer.push('\'')
                        }
                        _ => (),
                    },
                    b => buffer.push(b as char),
                }
            } else {
                match self.current() {
                    b'\'' => break self.slice(start, self.at).into(),
                    b':' => {
                        use_buffer = true;
                        buffer.push_str(self.slice(start, self.at));
                        match self.next() {
                            b'n' => {
                                self.at += 1;
                                buffer.push('\n')
                            }
                            b'r' => {
                                self.at += 1;
                                buffer.push('\r')
                            }
                            b't' => {
                                self.at += 1;
                                buffer.push('\t')
                            }
                            b':' => {
                                self.at += 1;
                                buffer.push(':')
                            }
                            b'\'' => {
                                self.at += 1;
                                buffer.push('\'')
                            }
                            _ => (),
                        }
                    }
                    _ if self.at_end() => {
                        break self.slice(start, self.at).into()
                    }
                    _ => (),
                }
            }

            self.at += 1;
        };

        Token {
            kind: TokKind::CharText,
            span: Span::new(start, self.at),
            slice,
        }
    }

    fn char_end(&mut self) -> Token<'a> {
        self.state = State::Default;
        self.simple(TokKind::CharEnd)
    }

    fn number(&mut self) -> Token<'a> {
        match (self.current(), self.next()) {
            (b'0', b'x' | b'X') => {
                self.prefix_num(TokKind::HexI64, |b| b.is_ascii_hexdigit())
            }
            (b'0', b'b' | b'B') => {
                self.prefix_num(TokKind::BinI64, |b| matches!(b, b'0' | b'1'))
            }
            (b'0', b'o' | b'O') => {
                self.prefix_num(TokKind::OctI64, |b| matches!(b, b'0'..=b'7'))
            }
            _ => self.dec_number(),
        }
    }

    fn prefix_num<F: Fn(u8) -> bool>(
        &mut self,
        kind: TokKind,
        is_digit: F,
    ) -> Token<'a> {
        self.at += 2;
        let start = self.at;
        while is_digit(self.current()) || self.current() == b'_' {
            self.at += 1;
        }

        // We don't use `self.token` because we want the span to include the prefix, but the
        // slice shouldn't include the prefix
        Token {
            kind,
            span: Span::new(start - 2, self.at),
            slice: self.slice(start, self.at).into(),
        }
    }

    fn dec_number(&mut self) -> Token<'a> {
        let start = self.at;
        let mut kind = TokKind::DecI64;
        loop {
            match self.current() {
                b'0'..=b'9' | b'_' => {}
                b'e' | b'E' => {
                    kind = TokKind::F64;
                    self.at += 1;
                    match self.current() {
                        b'0'..=b'9' | b'_' => {}
                        b'+' | b'-' if self.next().is_ascii_digit() => {
                            self.at += 1;
                        }
                        _ => break,
                    }
                }
                b'.' => kind = TokKind::F64,
                _ => break,
            }

            self.at += 1;
        }

        self.token(start, kind)
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
            b'.' => self.token_from(start, start + 2, TokKind::DotDot),
            b'0'..=b'9' => self.dec_number(),
            _ => self.token_from(start, start + 1, TokKind::Invalid),
        }
    }

    fn colon(&mut self) -> Token<'a> {
        let start = self.at;
        match self.next() {
            b'=' => self.token_from(start, start + 2, TokKind::Assign),
            b':' => self.token_from(start, start + 2, TokKind::PathSep),
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

    fn at_end(&self) -> bool {
        self.at >= self.input.len()
    }

    fn token(&self, start: usize, kind: TokKind) -> Token<'a> {
        Token {
            kind,
            span: Span::new(start, self.at),
            slice: self.slice(start, self.at).into(),
        }
    }

    fn token_from(
        &mut self,
        start: usize,
        end: usize,
        kind: TokKind,
    ) -> Token<'a> {
        self.at = end;
        Token {
            kind,
            span: Span::new(start, end),
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

    fn eat_whitespace(&mut self) {
        while self.current().is_ascii_whitespace() {
            self.at += 1;
        }
    }
}

#[cfg(test)]
mod tests {
    use pretty_assertions::assert_eq;

    use super::{Lexer, TokKind, Token};
    use crate::Span;

    fn tok(kind: TokKind, start: usize, end: usize, slice: &str) -> Token {
        Token { kind, span: Span::new(start, end), slice: slice.into() }
    }

    fn lex(input: &str) -> Lexer {
        Lexer::new(input.as_bytes())
    }

    fn lex_one(input: &str, kind: TokKind) {
        let mut lexer = Lexer::new(input.as_bytes());
        let tok = lexer.next_token();
        assert_eq!(tok.kind, kind);
        assert_eq!(tok.slice, input);
        assert!(lexer.at_end());
    }

    fn lex_mult(input: &str, kinds: &[TokKind]) {
        let mut lexer = Lexer::new(input.as_bytes());
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
        lex_one("..", TokKind::DotDot);
        lex_one(".", TokKind::Invalid);

        let input = ".>...";
        let kinds = [TokKind::Cpsr, TokKind::DotDot, TokKind::Invalid];
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
            "fn", "if", "nil", "rec", "case", "elif", "else", "true", "false",
            "union", "struct",
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

    #[test]
    fn lexes_string() {
        lex_mult(r#""""#, &[TokKind::StringStart, TokKind::StringEnd]);
        let strstart = tok(TokKind::StringStart, 0, 1, "\"");
        let strend = tok(TokKind::StringEnd, 1, 2, "\"");
        let end = tok(TokKind::End, 2, 2, "");

        let mut lexer = lex(r#""""#);
        assert_eq!(strstart, lexer.next_token());
        assert_eq!(strend, lexer.next_token());
        assert_eq!(end, lexer.next_token());

        let strstart = tok(TokKind::StringStart, 0, 1, "\"");
        let strtext = tok(TokKind::StringText, 1, 6, "hello");
        let strend = tok(TokKind::StringEnd, 6, 7, "\"");
        let end = tok(TokKind::End, 7, 7, "");

        let mut lexer = lex(r#""hello""#);
        assert_eq!(strstart, lexer.next_token());
        assert_eq!(strtext, lexer.next_token());
        assert_eq!(strend, lexer.next_token());
        assert_eq!(end, lexer.next_token());

        let strstart = tok(TokKind::StringStart, 0, 1, "\"");
        let strtext = tok(TokKind::StringText, 1, 15, "hello\n\tworld");
        let strend = tok(TokKind::StringEnd, 15, 16, "\"");
        let end = tok(TokKind::End, 16, 16, "");

        let mut lexer = lex(r#""hello:n:tworld""#);
        assert_eq!(strstart, lexer.next_token());
        assert_eq!(strtext, lexer.next_token());
        assert_eq!(strend, lexer.next_token());
        assert_eq!(end, lexer.next_token());
    }

    #[test]
    fn lexes_char() {
        // This will be verified and expressed as an error in the parsing phase
        let charstart = tok(TokKind::CharStart, 0, 1, "'");
        let chartext = tok(TokKind::CharText, 1, 6, "hello");
        let charend = tok(TokKind::CharEnd, 6, 7, "'");
        let end = tok(TokKind::End, 7, 7, "");

        let mut lexer = lex("'hello'");
        assert_eq!(charstart, lexer.next_token());
        assert_eq!(chartext, lexer.next_token());
        assert_eq!(charend, lexer.next_token());
        assert_eq!(end, lexer.next_token());

        let charstart = tok(TokKind::CharStart, 0, 1, "'");
        let chartext = tok(TokKind::CharText, 1, 3, "'");
        let charend = tok(TokKind::CharEnd, 3, 4, "'");
        let end = tok(TokKind::End, 4, 4, "");

        let mut lexer = lex("':''");
        assert_eq!(charstart, lexer.next_token());
        assert_eq!(chartext, lexer.next_token());
        assert_eq!(charend, lexer.next_token());
        assert_eq!(end, lexer.next_token());

        let charstart = tok(TokKind::CharStart, 0, 1, "'");
        let chartext = tok(TokKind::CharText, 1, 3, "ö");
        let charend = tok(TokKind::CharEnd, 3, 4, "'");
        let end = tok(TokKind::End, 4, 4, "");

        let mut lexer = lex("'ö'");
        assert_eq!(charstart, lexer.next_token());
        assert_eq!(chartext, lexer.next_token());
        assert_eq!(charend, lexer.next_token());
        assert_eq!(end, lexer.next_token());
    }

    #[test]
    fn lexes_numbers() {
        let lex_prefixed = |input: &str, kind| {
            let mut lexer = Lexer::new(input.as_bytes());
            let tok = lexer.next_token();
            assert_eq!(tok.kind, kind);
            assert_eq!(tok.slice, &input[2..]);
            assert!(lexer.at_end());
        };

        // Prefix lexing
        lex_prefixed("0xDEAD_beef", TokKind::HexI64);
        lex_prefixed("0X", TokKind::HexI64);
        lex_prefixed("0b1010_0101", TokKind::BinI64);
        lex_prefixed("0B", TokKind::BinI64);
        lex_prefixed("0o31337_", TokKind::OctI64);
        lex_prefixed("0O", TokKind::OctI64);

        // Integer lexing
        lex_one("0012334", TokKind::DecI64);
        lex_one("12334", TokKind::DecI64);
        lex_one("123_34", TokKind::DecI64);

        // Float lexing
        lex_one(".2", TokKind::F64);
        lex_one("7.2", TokKind::F64);
        lex_one("7.2e", TokKind::F64);
        lex_one("7.2e10", TokKind::F64);
        lex_one("7.2e+10", TokKind::F64);
        lex_one("7.2e-10", TokKind::F64);
        lex_one("7.e-10", TokKind::F64);
        lex_one("7.e1", TokKind::F64);
        lex_one("7e-10", TokKind::F64);
    }

    #[test]
    fn lexes_with_whitespace() {
        let lex_one_trim = |input: &str, kind| {
            let mut lexer = Lexer::new(input.as_bytes());
            let tok = lexer.next_token();
            assert_eq!(tok.kind, kind);
            assert_eq!(tok.slice, input.trim());
            let tok = lexer.next_token();

            // This is to make sure the whitespace doesn't make
            // another token!
            assert_eq!(tok.kind, TokKind::End);
            assert_eq!(tok.slice, "");
            assert!(lexer.at_end());
        };

        lex_one_trim("   ;", TokKind::Semi);
        lex_one_trim("@   ", TokKind::Bind);
        lex_one_trim("  >=  ", TokKind::Ge);
        lex_one_trim("  7.  ", TokKind::F64);
        lex_one_trim("\t 0012334 \r\n", TokKind::DecI64);
    }
}
