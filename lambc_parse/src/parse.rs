use crate::{FileId, I64Base, I64Lit, Lexer, Span, TokKind, Token};

#[derive(Debug, PartialEq, Eq)]
pub struct Error {
    message: String,
    span: Span,
}

pub type Result<T> = core::result::Result<T, Error>;

pub struct Parser<'a> {
    lexer: Lexer<'a>,
    peeked: Option<Token<'a>>,
}

impl<'a> Parser<'a> {
    pub fn new(input: &'a [u8], file: FileId) -> Self {
        Self {
            lexer: Lexer::new(input, file),
            peeked: None,
        }
    }

    fn parse_i64(&mut self) -> Result<I64Lit> {
        let tok = self.next();
        let base = match tok.kind {
            TokKind::DecI64 => I64Base::Dec,
            TokKind::BinI64 => I64Base::Bin,
            TokKind::OctI64 => I64Base::Oct,
            TokKind::HexI64 => I64Base::Hex,
            _ => {
                return Err(Error {
                    message: format!("expected i64 literal, found '{}'", tok.slice),
                    span: tok.span,
                })
            }
        };

        Ok(I64Lit {
            base,
            value: tok.slice.into_owned(),
            span: tok.span,
        })
    }

    fn peek(&mut self) -> &Token<'a> {
        match self.peeked {
            Some(ref t) => t,
            None => {
                self.peeked = Some(self.next());
                self.peeked.as_ref().unwrap()
            }
        }
    }

    fn next(&mut self) -> Token<'a> {
        loop {
            let token = self
                .peeked
                .take()
                .unwrap_or_else(|| self.lexer.next_token());

            if token.kind != TokKind::Comment {
                break token;
            }
        }
    }

    fn next_if_is(&mut self, kind: TokKind) -> bool {
        if self.peek().kind == kind {
            self.next();
            true
        } else {
            false
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{parse::Error, FileId, I64Base, I64Lit, Parser, Span};

    fn int(value: &str, base: I64Base, start: usize, end: usize) -> I64Lit {
        I64Lit {
            base,
            value: value.into(),
            span: Span {
                file: FileId(0),
                start,
                end,
            },
        }
    }

    #[test]
    fn parses_int() {
        let parse_int = |inp: &str, out: I64Lit| {
            let mut parser = Parser::new(inp.as_bytes(), FileId(0));
            assert_eq!(parser.parse_i64(), Ok(out));
        };

        parse_int("123", int("123", I64Base::Dec, 0, 3));
        parse_int("0x123", int("123", I64Base::Hex, 0, 5));
        parse_int("0o123", int("123", I64Base::Oct, 0, 5));
        parse_int("0b101", int("101", I64Base::Bin, 0, 5));

        // While these aren't valid int literals, we know they were intended to be
        // ints literals, and so we will handle them as such. This will be caught
        // later (in Compilation for now, but later in lowering).
        parse_int("0x", int("", I64Base::Hex, 0, 2));
        parse_int("0o", int("", I64Base::Oct, 0, 2));
        parse_int("0b", int("", I64Base::Bin, 0, 2));

        let input = "notAnInt";
        let message = format!("expected i64 literal, found '{input}'");
        let mut parser = Parser::new(b"notAnInt", FileId(0));

        assert_eq!(
            parser.parse_i64(),
            Err(Error {
                message,
                span: Span {
                    file: FileId(0),
                    start: 0,
                    end: input.len(),
                }
            })
        )
    }
}