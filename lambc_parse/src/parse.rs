use crate::{
    BoolLit, CharLit, CharText, Expr, F64Lit, FileId, FnDef, Group, I64Base, I64Lit, Ident, Lexer,
    List, NilLit, Span, StrLit, StrText, TokKind, Token,
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

    /// TODO: Define the grammar for an expression
    fn parse_expr(&mut self, tok: Token<'a>) -> Result<Expr> {
        self.parse_atom(tok)
    }

    /// Parses the items that can make up an expression without any operators
    /// ```text
    /// Grammar:
    ///
    ///     atom := block
    ///           | list
    ///           | group
    ///           | fn_def
    ///           | case
    ///           | if
    ///           | literal
    /// ```
    fn parse_atom(&mut self, tok: Token<'a>) -> Result<Expr> {
        Ok(match tok.kind {
            TokKind::OpenBrack => self.parse_list(tok)?,
            TokKind::OpenParen => self.parse_group(tok)?,
            TokKind::Fn => self.parse_fn_def(tok, false)?,
            TokKind::Rec => self.parse_fn_def(tok, true)?,
            _ => self.parse_literal(tok)?,
        })
    }

    /// Parses a list expression, which is a comma separated list of expressions followed by and
    /// optional comma
    /// ```text
    /// Grammar:
    ///
    ///     list_expr := '[' expr (',' expr )* ','? ']'
    ///                | '[' ']'
    /// ```
    fn parse_list(&mut self, tok: Token<'a>) -> Result<Expr> {
        debug_assert_eq!(tok.kind, TokKind::OpenBrack);
        let mut next = self.next();
        if next.kind == TokKind::CloseBrack {
            return Ok(Expr::List(List {
                values: vec![],
                span: Span::new(tok.span.start, next.span.end, tok.span.file),
            }));
        }

        let mut values = Vec::new();
        loop {
            values.push(self.parse_expr(next)?);
            next = self.next();

            if next.kind == TokKind::Comma {
                next = self.next();
                if next.kind == TokKind::CloseBrack {
                    break;
                }
            } else if next.kind == TokKind::CloseBrack {
                break;
            } else {
                return Err(Error {
                    message: format!("Expected closing bracket, found '{}'", next.slice),
                    span: Span::new(tok.span.start, next.span.end, tok.span.file),
                });
            }
        }

        Ok(Expr::List(List {
            values,
            span: Span::new(tok.span.start, next.span.end, tok.span.file),
        }))
    }

    /// Parses an expression surrounded with parenthesis
    /// ```text
    /// Grammar:
    ///
    ///     grouped_expr := '(' expr ')'
    /// ```
    fn parse_group(&mut self, tok: Token<'a>) -> Result<Expr> {
        debug_assert_eq!(tok.kind, TokKind::OpenParen);
        let next = self.next();
        let value = self.parse_expr(next)?;

        let next = self.next();
        if next.kind != TokKind::CloseParen {
            Err(Error {
                message: format!("Expected '(', but found '{}'", next.slice),
                span: next.span,
            })
        } else {
            Ok(Expr::Group(Box::new(Group {
                value,
                span: Span::new(tok.span.start, next.span.end, tok.span.file),
            })))
        }
    }

    /// Parses a function definition
    /// ```text
    /// Grammar:
    ///
    ///     function_def := 'rec'? 'fn' '(' [arg_list] ')' '->' expr
    ///
    ///     arg_list := ident (',' ident )* ','?
    /// ```
    fn parse_fn_def(&mut self, tok: Token<'a>, is_recursive: bool) -> Result<Expr> {
        debug_assert!(matches!(tok.kind, TokKind::Rec | TokKind::Fn));
        if tok.kind == TokKind::Rec {
            self.expect(TokKind::Fn)?;
        }

        self.expect(TokKind::OpenParen)?;
        self.expect(TokKind::CloseParen)?;
        self.expect(TokKind::Arrow)?;

        let next = self.next();
        let value = self.parse_expr(next)?;
        let span = value.span();

        Ok(Expr::FnDef(Box::new(FnDef {
            args: vec![],
            body: value,
            recursive: is_recursive,
            span: Span::new(tok.span.start, span.end, tok.span.file),
        })))
    }

    /// Parses any value literal, such as that of a string or number, as well as an identifier.
    /// ```text
    /// Grammar:
    ///
    /// literal := 'nil'
    ///          | number
    ///          | boolean
    ///          | string
    ///          | char
    ///          | ident
    /// ```
    fn parse_literal(&mut self, tok: Token<'a>) -> Result<Expr> {
        Ok(match tok.kind {
            TokKind::Ident => Expr::Ident(self.parse_ident(tok)),
            TokKind::Nil => Expr::Nil(self.parse_nil(tok)),
            TokKind::BinI64 | TokKind::OctI64 | TokKind::HexI64 | TokKind::DecI64 => {
                Expr::I64(self.parse_i64(tok))
            }
            TokKind::CharStart => Expr::Char(self.parse_char(tok)?),
            TokKind::F64 => Expr::F64(self.parse_f64(tok)),
            TokKind::True | TokKind::False => Expr::Bool(self.parse_bool(tok)),
            TokKind::StringStart => Expr::String(self.parse_string(tok)?),
            TokKind::CharText
            | TokKind::CharEnd
            | TokKind::StringText
            | TokKind::StringEnd
            | TokKind::Comment => unreachable!(),
            TokKind::End => {
                return Err(Error {
                    message: "Expected an expression, instead found end of input".into(),
                    span: tok.span,
                })
            }
            _ => {
                return Err(Error {
                    message: format!("Expected an expression, instead found '{}'", tok.slice),
                    span: tok.span,
                })
            }
        })
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

    fn parse_bool(&mut self, tok: Token<'a>) -> BoolLit {
        debug_assert!(matches!(tok.kind, TokKind::True | TokKind::False));

        BoolLit {
            value: tok.slice.starts_with('t'),
            span: tok.span,
        }
    }

    fn parse_nil(&mut self, tok: Token<'a>) -> NilLit {
        debug_assert!(matches!(tok.kind, TokKind::Nil));

        NilLit { span: tok.span }
    }

    fn parse_string(&mut self, tok: Token<'a>) -> Result<StrLit> {
        debug_assert!(matches!(tok.kind, TokKind::StringStart));

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
                start: tok.span.start,
                end: end.span.end,
                file: end.span.file,
            },
        })
    }

    fn parse_char(&mut self, tok: Token<'a>) -> Result<CharLit> {
        debug_assert!(matches!(tok.kind, TokKind::CharStart));

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
                start: tok.span.start,
                end: end.span.end,
                file: end.span.file,
            },
        })
    }

    fn parse_ident(&mut self, tok: Token<'a>) -> Ident {
        debug_assert_eq!(tok.kind, TokKind::Ident);

        Ident {
            raw: tok.slice.into(),
            span: tok.span,
        }
    }

    /// Returns a reference to the next token non-comment token in the input
    fn peek(&mut self) -> &Token<'a> {
        match self.peeked {
            Some(ref t) => t,
            None => {
                self.peeked = Some(self.next());
                self.peeked.as_ref().unwrap()
            }
        }
    }

    /// Returns the next token in the input, consuming the last peeked token if
    /// present.
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

    /// Peeks at the next token, and returns `true` if it is of `kind`. If the peeked token is
    /// of `kind`, then the token is skipped.
    fn next_if_is(&mut self, kind: TokKind) -> bool {
        if self.peek().kind == kind {
            self.next();
            true
        } else {
            false
        }
    }

    fn expect(&mut self, kind: TokKind) -> Result<Token<'a>> {
        let next = self.next();
        if next.kind == kind {
            return Ok(next);
        }

        // TODO: Get a unified way to do expected/found errors
        Err(Error {
            message: format!("Expected {:?}, but found '{}'", kind, next.slice),
            span: next.span,
        })
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        BoolLit, CharLit, CharText, Expr, F64Lit, FileId, FnDef, Group, I64Base, I64Lit, Ident,
        List, NilLit, Parser, Span, StrLit, StrText,
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
            let tok = parser.next();
            assert_eq!(
                parser.parse_bool(tok),
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
        let tok = parser.next();
        assert_eq!(
            parser.parse_nil(tok),
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
        let tok = parser.next();
        assert_eq!(
            parser.parse_string(tok),
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
        let tok = parser.next();
        assert_eq!(
            parser.parse_string(tok),
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
        let tok = parser.next();
        assert_eq!(
            parser.parse_char(tok),
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
        let tok = parser.next();
        assert_eq!(
            parser.parse_char(tok),
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
        let tok = parser.next();
        assert_eq!(
            parser.parse_char(tok),
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
            let tok = parser.next();
            assert_eq!(
                parser.parse_ident(tok),
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

    #[test]
    fn parses_literal() {
        let file = FileId(0);
        let literal = |literal: &str, out| {
            let mut parser = Parser::new(literal.as_bytes(), file);
            let tok = parser.next();
            assert_eq!(parser.parse_literal(tok), out)
        };

        literal(
            "true",
            Ok(Expr::Bool(BoolLit {
                value: true,
                span: Span::new(0, 4, file),
            })),
        );

        literal(
            "false",
            Ok(Expr::Bool(BoolLit {
                value: false,
                span: Span::new(0, 5, file),
            })),
        );

        literal(
            "2.0e2",
            Ok(Expr::F64(F64Lit {
                value: "2.0e2".into(),
                span: Span::new(0, 5, file),
            })),
        );

        literal(
            "nil",
            Ok(Expr::Nil(NilLit {
                span: Span::new(0, 3, file),
            })),
        );

        literal(
            "42",
            Ok(Expr::I64(I64Lit {
                base: I64Base::Dec,
                value: "42".into(),
                span: Span::new(0, 2, file),
            })),
        );

        literal(
            "\"\"",
            Ok(Expr::String(StrLit {
                text: None,
                span: Span::new(0, 2, file),
            })),
        );

        literal(
            "\"hello:nworld\"",
            Ok(Expr::String(StrLit {
                text: Some(StrText {
                    inner: "hello\nworld".into(),
                    span: Span::new(1, 13, file),
                }),
                span: Span::new(0, 14, file),
            })),
        );

        literal(
            "''",
            Ok(Expr::Char(CharLit {
                text: None,
                span: Span::new(0, 2, file),
            })),
        );

        literal(
            "'hello:nworld'",
            Ok(Expr::Char(CharLit {
                text: Some(CharText {
                    inner: "hello\nworld".into(),
                    span: Span::new(1, 13, file),
                }),
                span: Span::new(0, 14, file),
            })),
        );
    }

    #[test]
    fn parses_atom() {
        let file = FileId(0);
        let atom = |atom: &str, out| {
            let mut parser = Parser::new(atom.as_bytes(), file);
            let tok = parser.next();
            assert_eq!(parser.parse_atom(tok), out)
        };

        atom(
            "true",
            Ok(Expr::Bool(BoolLit {
                value: true,
                span: Span::new(0, 4, file),
            })),
        );

        atom(
            "false",
            Ok(Expr::Bool(BoolLit {
                value: false,
                span: Span::new(0, 5, file),
            })),
        );

        atom(
            "2.0e2",
            Ok(Expr::F64(F64Lit {
                value: "2.0e2".into(),
                span: Span::new(0, 5, file),
            })),
        );

        atom(
            "nil",
            Ok(Expr::Nil(NilLit {
                span: Span::new(0, 3, file),
            })),
        );

        atom(
            "42",
            Ok(Expr::I64(I64Lit {
                base: I64Base::Dec,
                value: "42".into(),
                span: Span::new(0, 2, file),
            })),
        );

        atom(
            "\"\"",
            Ok(Expr::String(StrLit {
                text: None,
                span: Span::new(0, 2, file),
            })),
        );

        atom(
            "\"hello:nworld\"",
            Ok(Expr::String(StrLit {
                text: Some(StrText {
                    inner: "hello\nworld".into(),
                    span: Span::new(1, 13, file),
                }),
                span: Span::new(0, 14, file),
            })),
        );

        atom(
            "''",
            Ok(Expr::Char(CharLit {
                text: None,
                span: Span::new(0, 2, file),
            })),
        );

        atom(
            "'hello:nworld'",
            Ok(Expr::Char(CharLit {
                text: Some(CharText {
                    inner: "hello\nworld".into(),
                    span: Span::new(1, 13, file),
                }),
                span: Span::new(0, 14, file),
            })),
        );

        atom(
            "[]",
            Ok(Expr::List(List {
                values: vec![],
                span: Span::new(0, 2, file),
            })),
        );

        atom(
            "[nil, 2, \"hello\"]",
            Ok(Expr::List(List {
                values: vec![
                    Expr::Nil(NilLit {
                        span: Span::new(1, 4, file),
                    }),
                    Expr::I64(I64Lit {
                        base: I64Base::Dec,
                        value: "2".into(),
                        span: Span::new(6, 7, file),
                    }),
                    Expr::String(StrLit {
                        text: Some(StrText {
                            inner: "hello".into(),
                            span: Span::new(10, 15, file),
                        }),
                        span: Span::new(9, 16, file),
                    }),
                ],
                span: Span::new(0, 17, file),
            })),
        );

        atom(
            "(1)",
            Ok(Expr::Group(Box::new(Group {
                value: Expr::I64(I64Lit {
                    base: I64Base::Dec,
                    value: "1".into(),
                    span: Span::new(1, 2, file),
                }),
                span: Span::new(0, 3, file),
            }))),
        );
    }

    #[test]
    fn parses_nested_list() {
        let file = FileId(0);
        let atom = |atom: &str, out| {
            let mut parser = Parser::new(atom.as_bytes(), file);
            let tok = parser.next();
            assert_eq!(parser.parse_atom(tok), out)
        };

        atom(
            "[nil, [], [nil, 2,],]",
            Ok(Expr::List(List {
                values: vec![
                    Expr::Nil(NilLit {
                        span: Span::new(1, 4, file),
                    }),
                    Expr::List(List {
                        values: vec![],
                        span: Span::new(6, 8, file),
                    }),
                    Expr::List(List {
                        values: vec![
                            Expr::Nil(NilLit {
                                span: Span::new(11, 14, file),
                            }),
                            Expr::I64(I64Lit {
                                base: I64Base::Dec,
                                value: "2".into(),
                                span: Span::new(16, 17, file),
                            }),
                        ],
                        span: Span::new(10, 19, file),
                    }),
                ],
                span: Span::new(0, 21, file),
            })),
        );

        atom(
            "fn() -> nil",
            Ok(Expr::FnDef(Box::new(FnDef {
                args: vec![],
                body: Expr::Nil(NilLit {
                    span: Span::new(8, 11, file),
                }),
                recursive: false,
                span: Span::new(0, 11, file),
            }))),
        );

        atom(
            "rec fn() -> nil",
            Ok(Expr::FnDef(Box::new(FnDef {
                args: vec![],
                body: Expr::Nil(NilLit {
                    span: Span::new(12, 15, file),
                }),
                recursive: true,
                span: Span::new(0, 15, file),
            }))),
        );
    }

    #[test]
    fn parses_nested_group() {
        let file = FileId(0);
        let atom = |atom: &str, out| {
            let mut parser = Parser::new(atom.as_bytes(), file);
            let tok = parser.next();
            assert_eq!(parser.parse_atom(tok), out)
        };

        atom(
            "((((1))))",
            Ok(Expr::Group(Box::new(Group {
                value: Expr::Group(Box::new(Group {
                    value: Expr::Group(Box::new(Group {
                        value: Expr::Group(Box::new(Group {
                            value: Expr::I64(I64Lit {
                                base: I64Base::Dec,
                                value: "1".into(),
                                span: Span::new(4, 5, file),
                            }),
                            span: Span::new(3, 6, file),
                        })),
                        span: Span::new(2, 7, file),
                    })),
                    span: Span::new(1, 8, file),
                })),
                span: Span::new(0, 9, file),
            }))),
        );
    }

    #[test]
    fn parses_nested_fn_def() {
        let file = FileId(0);
        let atom = |atom: &str, out| {
            let mut parser = Parser::new(atom.as_bytes(), file);
            let tok = parser.next();
            assert_eq!(parser.parse_atom(tok), out)
        };

        atom(
            "fn() -> fn() -> fn() -> nil",
            Ok(Expr::FnDef(Box::new(FnDef {
                args: vec![],
                body: Expr::FnDef(Box::new(FnDef {
                    args: vec![],
                    body: Expr::FnDef(Box::new(FnDef {
                        args: vec![],
                        body: Expr::Nil(NilLit {
                            span: Span::new(24, 27, file),
                        }),
                        recursive: false,
                        span: Span::new(16, 27, file),
                    })),
                    recursive: false,
                    span: Span::new(8, 27, file),
                })),
                recursive: false,
                span: Span::new(0, 27, file),
            }))),
        );
    }
}
