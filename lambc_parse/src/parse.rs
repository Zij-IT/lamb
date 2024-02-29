use crate::{
    BoolLit, CharLit, CharText, F64Lit, FileId, I64Base, I64Lit, Ident, Lexer, NilLit, Span,
    StrLit, StrText, TokKind, Token,
};
use miette::Diagnostic;
use thiserror::Error as ThError;

#[derive(Diagnostic, Debug, ThError, PartialEq, Eq)]
#[error("error[Parse Error]: {message}")]
#[diagnostic()]
pub struct Error {
    message: String,
    #[label]
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

    fn parse_i64(&mut self, tok: Token<'a>) -> I64Lit {
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

        I64Lit {
            base,
            value: tok.slice.into_owned(),
            span: tok.span,
        }
    }

    fn parse_f64(&mut self, tok: Token<'a>) -> F64Lit {
        debug_assert_eq!(tok.kind, TokKind::F64);

        F64Lit {
            value: tok.slice.into_owned(),
            span: tok.span,
        }
    }

    fn parse_bool(&mut self) -> BoolLit {
        let tok = self.next();
        debug_assert!(matches!(tok.kind, TokKind::True | TokKind::False));

        BoolLit {
            value: tok.slice.starts_with('t'),
            span: tok.span,
        }
    }

    fn parse_nil(&mut self) -> NilLit {
        let tok = self.next();
        debug_assert!(matches!(tok.kind, TokKind::Nil));

        NilLit { span: tok.span }
    }

    fn parse_string(&mut self) -> Result<StrLit> {
        let start = self.next();
        debug_assert!(matches!(start.kind, TokKind::StringStart));

        let text = if self.peek().kind == TokKind::StringText {
            let text = self.next();
            Some(StrText {
                inner: text.slice.into_owned(),
                span: text.span,
            })
        } else {
            None
        };

        let end = match self.next() {
            tok @ Token {
                kind: TokKind::StringEnd,
                ..
            } => tok,
            err => {
                // TODO: When switching to miette diagnostics, this one should have
                // a note that indicates where the string was started.
                return Err(Error {
                    message: "unclosed string literal".into(),
                    span: Span {
                        start: err.span.start,
                        end: err.span.start,
                        file: err.span.file,
                    },
                });
            }
        };

        Ok(StrLit {
            text,
            span: Span {
                start: start.span.start,
                end: end.span.end,
                file: end.span.file,
            },
        })
    }

    fn parse_char(&mut self) -> Result<CharLit> {
        let start = self.next();
        debug_assert!(matches!(start.kind, TokKind::CharStart));

        let text = if self.peek().kind == TokKind::CharText {
            let text = self.next();
            Some(CharText {
                inner: text.slice.into_owned(),
                span: text.span,
            })
        } else {
            None
        };

        let end = match self.next() {
            tok @ Token {
                kind: TokKind::CharEnd,
                ..
            } => tok,
            err => {
                // TODO: When switching to miette diagnostics, this one should have
                // a note that indicates where the string was started.
                return Err(Error {
                    message: "unclosed char literal".into(),
                    span: Span {
                        start: err.span.start,
                        end: err.span.start,
                        file: err.span.file,
                    },
                });
            }
        };

        Ok(CharLit {
            text,
            span: Span {
                start: start.span.start,
                end: end.span.end,
                file: end.span.file,
            },
        })
    }

    fn parse_ident(&mut self) -> Ident {
        let tok = self.next();
        debug_assert_eq!(tok.kind, TokKind::Ident);

        Ident {
            raw: tok.slice.into(),
            span: tok.span,
        }
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
    use crate::{
        BoolLit, CharLit, CharText, F64Lit, FileId, I64Base, I64Lit, Ident, NilLit, Parser, Span,
        StrLit, StrText,
    };
    use pretty_assertions::assert_eq;

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
            let tok = parser.next();
            assert_eq!(parser.parse_i64(tok), out);
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
    }

    #[test]
    fn parses_f64() {
        let parse_f64 = |inp: &str| {
            let mut parser = Parser::new(inp.as_bytes(), FileId(0));
            let tok = parser.next();
            assert_eq!(
                parser.parse_f64(tok),
                F64Lit {
                    value: inp.into(),
                    span: Span {
                        start: 0,
                        end: inp.len(),
                        file: FileId(0),
                    }
                }
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
                BoolLit {
                    value: inp.parse().unwrap(),
                    span: Span {
                        start: 0,
                        end: inp.len(),
                        file: FileId(0),
                    }
                }
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
            NilLit {
                span: Span {
                    start: 0,
                    end: 3,
                    file: FileId(0),
                }
            }
        );
    }

    #[test]
    fn parses_string() {
        let input = r#""""#;
        let mut parser = Parser::new(input.as_bytes(), FileId(0));
        assert_eq!(
            parser.parse_string(),
            Ok(StrLit {
                text: None,
                span: Span {
                    start: 0,
                    end: input.len(),
                    file: FileId(0),
                }
            })
        );

        let input = r#""hello""#;
        let mut parser = Parser::new(input.as_bytes(), FileId(0));
        assert_eq!(
            parser.parse_string(),
            Ok(StrLit {
                text: Some(StrText {
                    inner: String::from(input.trim_matches('"')),
                    span: Span {
                        start: 1,
                        end: 6,
                        file: FileId(0),
                    }
                }),
                span: Span {
                    start: 0,
                    end: 7,
                    file: FileId(0),
                }
            })
        );
    }

    #[test]
    fn parses_char() {
        let input = "''";
        let mut parser = Parser::new(input.as_bytes(), FileId(0));
        assert_eq!(
            parser.parse_char(),
            Ok(CharLit {
                text: None,
                span: Span {
                    start: 0,
                    end: input.len(),
                    file: FileId(0),
                }
            })
        );

        let input = "'hello'";
        let mut parser = Parser::new(input.as_bytes(), FileId(0));
        assert_eq!(
            parser.parse_char(),
            Ok(CharLit {
                text: Some(CharText {
                    inner: String::from(input.trim_matches('\'')),
                    span: Span {
                        start: 1,
                        end: 6,
                        file: FileId(0),
                    }
                }),
                span: Span {
                    start: 0,
                    end: 7,
                    file: FileId(0),
                }
            })
        );

        let input = "'ä'";
        let mut parser = Parser::new(input.as_bytes(), FileId(0));
        assert_eq!(
            parser.parse_char(),
            Ok(CharLit {
                text: Some(CharText {
                    inner: String::from(input.trim_matches('\'')),
                    span: Span {
                        start: 1,
                        end: 3,
                        file: FileId(0),
                    }
                }),
                span: Span {
                    start: 0,
                    end: 4,
                    file: FileId(0),
                }
            })
        );
    }

    #[test]
    fn parses_ident() {
        let ident = |ident: &str| {
            let mut parser = Parser::new(ident.as_bytes(), FileId(0));
            assert_eq!(
                parser.parse_ident(),
                Ident {
                    raw: ident.into(),
                    span: Span {
                        file: FileId(0),
                        start: 0,
                        end: ident.len(),
                    }
                }
            )
        };

        ident("improvise");
        ident("adapt");
        ident("overcome");
        ident("reduce");
        ident("map");
    }
}
