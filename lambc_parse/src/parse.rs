use crate::{BoolLit, F64Lit, FileId, I64Base, I64Lit, Lexer, NilLit, Span, TokKind, Token};

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
        debug_assert!(matches!(
            tok.kind,
            TokKind::DecI64 | TokKind::BinI64 | TokKind::OctI64 | TokKind::HexI64
        ));

        let base = match tok.kind {
            TokKind::DecI64 => I64Base::Dec,
            TokKind::BinI64 => I64Base::Bin,
            TokKind::OctI64 => I64Base::Oct,
            TokKind::HexI64 => I64Base::Hex,
            _ => unreachable!(),
        };

        Ok(I64Lit {
            base,
            value: tok.slice.into_owned(),
            span: tok.span,
        })
    }

    fn parse_f64(&mut self) -> Result<F64Lit> {
        let tok = self.next();
        debug_assert_eq!(tok.kind, TokKind::F64);

        Ok(F64Lit {
            value: tok.slice.into_owned(),
            span: tok.span,
        })
    }

    fn parse_bool(&mut self) -> Result<BoolLit> {
        let tok = self.next();
        debug_assert!(matches!(tok.kind, TokKind::True | TokKind::False));

        Ok(BoolLit {
            value: tok.slice.starts_with('t'),
            span: tok.span,
        })
    }

    fn parse_nil(&mut self) -> Result<NilLit> {
        let tok = self.next();
        debug_assert!(matches!(tok.kind, TokKind::Nil));

        Ok(NilLit { span: tok.span })
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
    use crate::{parse::Error, BoolLit, F64Lit, FileId, I64Base, I64Lit, NilLit, Parser, Span};

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

    #[test]
    fn parses_f64() {
        let parse_f64 = |inp: &str| {
            let mut parser = Parser::new(inp.as_bytes(), FileId(0));
            assert_eq!(
                parser.parse_f64(),
                Ok(F64Lit {
                    value: inp.into(),
                    span: Span {
                        start: 0,
                        end: inp.len(),
                        file: FileId(0),
                    }
                })
            );
        };

        parse_f64(".2");
        parse_f64("7.2");
        parse_f64("7.2e");
        parse_f64("7.2e10");
        parse_f64("7.2e+10");
        parse_f64("7.2e-10");
        parse_f64("7.e-10");
        parse_f64("7.e1");
        parse_f64("7e-10");
    }

    #[test]
    fn parses_bool() {
        let parse_bool = |inp: &str| {
            let mut parser = Parser::new(inp.as_bytes(), FileId(0));
            assert_eq!(
                parser.parse_bool(),
                Ok(BoolLit {
                    value: inp.parse().unwrap(),
                    span: Span {
                        start: 0,
                        end: inp.len(),
                        file: FileId(0),
                    }
                })
            );
        };

        parse_bool("true");
        parse_bool("false");
    }

    #[test]
    fn parses_nil() {
        let mut parser = Parser::new("nil".as_bytes(), FileId(0));
        assert_eq!(
            parser.parse_nil(),
            Ok(NilLit {
                span: Span {
                    start: 0,
                    end: 3,
                    file: FileId(0),
                }
            })
        );
    }
}
