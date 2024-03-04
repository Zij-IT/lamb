use crate::{
    BoolLit, CharLit, CharText, Define, Expr, ExprStatement, F64Lit, FileId, FnDef, Group, I64Base,
    I64Lit, Ident, Lexer, List, NilLit, Span, Statement, StrLit, StrText, TokKind, Token,
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
    peek1: Option<Token<'a>>,
    peek2: Option<Token<'a>>,
}

impl<'a> Parser<'a> {
    pub fn new(input: &'a [u8], file: FileId) -> Self {
        Self {
            lexer: Lexer::new(input, file),
            peek1: None,
            peek2: None,
        }
    }

    /// Parses a statement as defined by the following grammar
    /// ```text
    /// Grammar:
    ///
    ///     stmt := ident ':=' expr ';'
    ///           | expr  ';'
    /// ```
    pub fn parse_stmt(&mut self) -> Result<Statement> {
        if self.peek2().kind == TokKind::Assign {
            let peek1 = self.peek1();
            if peek1.kind != TokKind::Ident {
                return Err(Error {
                    message: format!("Expected identifier, found '{}'", peek1.slice),
                    span: peek1.span,
                });
            }

            let span = peek1.span;
            let ident = self.parse_ident();
            self.expect(TokKind::Assign)?;
            let value = self.parse_expr()?;
            let semi = self.expect(TokKind::Semi)?;

            Ok(Statement::Define(Define {
                ident,
                value,
                span: Span::connect(span, semi.span),
            }))
        } else {
            let expr = self.parse_expr()?;
            let span = expr.span();
            let semi = self.expect(TokKind::Semi)?;

            Ok(Statement::Expr(ExprStatement {
                expr,
                span: Span::connect(span, semi.span),
            }))
        }
    }

    /// TODO: Define the grammar for an expression
    pub fn parse_expr(&mut self) -> Result<Expr> {
        self.parse_atom()
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
    fn parse_atom(&mut self) -> Result<Expr> {
        let tok = self.peek1();
        Ok(match tok.kind {
            TokKind::OpenBrack => self.parse_list()?,
            TokKind::OpenParen => self.parse_group()?,
            TokKind::Fn => self.parse_fn_def(false)?,
            TokKind::Rec => self.parse_fn_def(true)?,
            _ => self.parse_literal()?,
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
    fn parse_list(&mut self) -> Result<Expr> {
        let tok = self.next();
        debug_assert_eq!(tok.kind, TokKind::OpenBrack);
        let (values, end_tok) =
            self.parse_node_list(TokKind::CloseBrack, |this| this.parse_expr())?;

        Ok(Expr::List(List {
            values,
            span: Span::connect(tok.span, end_tok.span),
        }))
    }

    /// Parses an expression surrounded with parenthesis
    /// ```text
    /// Grammar:
    ///
    ///     grouped_expr := '(' expr ')'
    /// ```
    fn parse_group(&mut self) -> Result<Expr> {
        let tok = self.next();
        debug_assert_eq!(tok.kind, TokKind::OpenParen);
        let value = self.parse_expr()?;

        let next = self.next();
        if next.kind != TokKind::CloseParen {
            Err(Error {
                message: format!("Expected '(', but found '{}'", next.slice),
                span: next.span,
            })
        } else {
            Ok(Expr::Group(Box::new(Group {
                value,
                span: Span::connect(tok.span, next.span),
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
    fn parse_fn_def(&mut self, is_recursive: bool) -> Result<Expr> {
        let tok = self.next();
        debug_assert!(matches!(tok.kind, TokKind::Rec | TokKind::Fn));
        if tok.kind == TokKind::Rec {
            self.expect(TokKind::Fn)?;
        }

        self.expect(TokKind::OpenParen)?;
        let (args, _) = self.parse_node_list(TokKind::CloseParen, |this| {
            let next = this.peek1();
            if next.kind == TokKind::Ident {
                Ok(this.parse_ident())
            } else {
                Err(Error {
                    message: format!("Expected identifier or ')', found '{}'", next.slice),
                    span: next.span,
                })
            }
        })?;

        self.expect(TokKind::Arrow)?;

        let value = self.parse_expr()?;
        let span = value.span();

        Ok(Expr::FnDef(Box::new(FnDef {
            args,
            body: value,
            recursive: is_recursive,
            span: Span::connect(tok.span, span),
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
    fn parse_literal(&mut self) -> Result<Expr> {
        let tok = self.peek1();
        Ok(match tok.kind {
            TokKind::Ident => Expr::Ident(self.parse_ident()),
            TokKind::Nil => Expr::Nil(self.parse_nil()),
            TokKind::BinI64 | TokKind::OctI64 | TokKind::HexI64 | TokKind::DecI64 => {
                Expr::I64(self.parse_i64())
            }
            TokKind::CharStart => Expr::Char(self.parse_char()?),
            TokKind::F64 => Expr::F64(self.parse_f64()),
            TokKind::True | TokKind::False => Expr::Bool(self.parse_bool()),
            TokKind::StringStart => Expr::String(self.parse_string()?),
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

    fn parse_i64(&mut self) -> I64Lit {
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

        I64Lit {
            base,
            value: tok.slice.into_owned(),
            span: tok.span,
        }
    }

    fn parse_f64(&mut self) -> F64Lit {
        let tok = self.next();
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
        let tok = self.next();
        debug_assert!(matches!(tok.kind, TokKind::StringStart));

        let text = if self.peek1().kind == TokKind::StringText {
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

    fn parse_char(&mut self) -> Result<CharLit> {
        let tok = self.next();
        debug_assert!(matches!(tok.kind, TokKind::CharStart));

        let text = if self.peek1().kind == TokKind::CharText {
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

    fn parse_ident(&mut self) -> Ident {
        let tok = self.next();
        debug_assert_eq!(tok.kind, TokKind::Ident);

        Ident {
            raw: tok.slice.into(),
            span: tok.span,
        }
    }

    /// Parses a series of comma separated `T`, with a trailing comma allowed.
    fn parse_node_list<T, F>(
        &mut self,
        end_kind: TokKind,
        parse_elem: F,
    ) -> Result<(Vec<T>, Token<'a>)>
    where
        F: Fn(&mut Self) -> Result<T>,
    {
        let mut values = Vec::new();
        let tok = self.peek1();
        if tok.kind == end_kind {
            return Ok((values, self.next()));
        }

        let span = tok.span;
        let end_token = loop {
            values.push(parse_elem(self)?);

            let next = self.next();
            if next.kind == TokKind::Comma {
                let next = self.peek1();
                if next.kind == end_kind {
                    break self.next();
                }
            } else if next.kind == end_kind {
                break next;
            } else {
                return Err(Error {
                    message: format!("Expected {:?}, found '{}'", end_kind, next.slice),
                    span: Span::connect(span, next.span),
                });
            }
        };

        Ok((values, end_token))
    }

    /// Returns a reference to the next token non-comment token in the input
    fn peek1(&mut self) -> &Token<'a> {
        match self.peek1 {
            Some(ref t) => t,
            None => {
                self.peek1 = self.peek2.take().or_else(|| Some(self.next()));
                self.peek1.as_ref().unwrap()
            }
        }
    }

    /// Returns a reference to the next token non-comment token in the input
    fn peek2(&mut self) -> &Token<'a> {
        self.peek1();
        match self.peek2 {
            Some(ref t) => t,
            None => {
                self.peek2 = Some(self.lexer.next_nontrival_token());
                self.peek2.as_ref().unwrap()
            }
        }
    }

    /// Returns the next token in the input, consuming the last peeked token if
    /// present.
    fn next(&mut self) -> Token<'a> {
        let p1 = self.peek1.take();
        let p2 = self.peek2.take();
        self.peek1 = p2;

        p1.unwrap_or_else(|| self.lexer.next_nontrival_token())
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
        BoolLit, CharLit, CharText, Define, Expr, ExprStatement, F64Lit, FileId, FnDef, Group,
        I64Base, I64Lit, Ident, List, NilLit, Parser, Span, Statement, StrLit, StrText,
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
            assert_eq!(parser.parse_i64(), out);
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
            assert_eq!(
                parser.parse_f64(),
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

    #[test]
    fn parses_literal() {
        let file = FileId(0);
        let literal = |literal: &str, out| {
            let mut parser = Parser::new(literal.as_bytes(), file);
            assert_eq!(parser.parse_literal(), out)
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
            assert_eq!(parser.parse_atom(), out)
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
            assert_eq!(parser.parse_atom(), out)
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
            "rec fn(a,) -> nil",
            Ok(Expr::FnDef(Box::new(FnDef {
                args: vec![Ident {
                    raw: "a".into(),
                    span: Span::new(7, 8, file),
                }],
                body: Expr::Nil(NilLit {
                    span: Span::new(14, 17, file),
                }),
                recursive: true,
                span: Span::new(0, 17, file),
            }))),
        );
    }

    #[test]
    fn parses_nested_group() {
        let file = FileId(0);
        let atom = |atom: &str, out| {
            let mut parser = Parser::new(atom.as_bytes(), file);
            assert_eq!(parser.parse_atom(), out)
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
            assert_eq!(parser.parse_atom(), out)
        };

        atom(
            "fn(a, b) -> fn(c) -> fn(d, e,) -> nil",
            Ok(Expr::FnDef(Box::new(FnDef {
                args: vec![
                    Ident {
                        raw: "a".into(),
                        span: Span::new(3, 4, file),
                    },
                    Ident {
                        raw: "b".into(),
                        span: Span::new(6, 7, file),
                    },
                ],
                body: Expr::FnDef(Box::new(FnDef {
                    args: vec![Ident {
                        raw: "c".into(),
                        span: Span::new(15, 16, file),
                    }],
                    body: Expr::FnDef(Box::new(FnDef {
                        args: vec![
                            Ident {
                                raw: "d".into(),
                                span: Span::new(24, 25, file),
                            },
                            Ident {
                                raw: "e".into(),
                                span: Span::new(27, 28, file),
                            },
                        ],
                        body: Expr::Nil(NilLit {
                            span: Span::new(34, 37, file),
                        }),
                        recursive: false,
                        span: Span::new(21, 37, file),
                    })),
                    recursive: false,
                    span: Span::new(12, 37, file),
                })),
                recursive: false,
                span: Span::new(0, 37, file),
            }))),
        );
    }

    #[test]
    fn parses_statement() {
        let file = FileId(0);
        let stmt = |stmt: &str, out| {
            let mut parser = Parser::new(stmt.as_bytes(), file);
            assert_eq!(parser.parse_stmt(), out)
        };

        stmt(
            "x := 2;",
            Ok(Statement::Define(Define {
                ident: Ident {
                    raw: "x".into(),
                    span: Span::new(0, 1, file),
                },
                value: Expr::I64(I64Lit {
                    base: I64Base::Dec,
                    value: "2".into(),
                    span: Span::new(5, 6, file),
                }),
                span: Span::new(0, 7, file),
            })),
        );

        stmt(
            "2;",
            Ok(Statement::Expr(ExprStatement {
                expr: Expr::I64(I64Lit {
                    base: I64Base::Dec,
                    value: "2".into(),
                    span: Span::new(0, 1, file),
                }),
                span: Span::new(0, 2, file),
            })),
        );

        stmt(
            "fn(a, b) -> fn(c) -> fn(d, e,) -> nil;",
            Ok(Statement::Expr(ExprStatement {
                expr: Expr::FnDef(Box::new(FnDef {
                    args: vec![
                        Ident {
                            raw: "a".into(),
                            span: Span::new(3, 4, file),
                        },
                        Ident {
                            raw: "b".into(),
                            span: Span::new(6, 7, file),
                        },
                    ],
                    body: Expr::FnDef(Box::new(FnDef {
                        args: vec![Ident {
                            raw: "c".into(),
                            span: Span::new(15, 16, file),
                        }],
                        body: Expr::FnDef(Box::new(FnDef {
                            args: vec![
                                Ident {
                                    raw: "d".into(),
                                    span: Span::new(24, 25, file),
                                },
                                Ident {
                                    raw: "e".into(),
                                    span: Span::new(27, 28, file),
                                },
                            ],
                            body: Expr::Nil(NilLit {
                                span: Span::new(34, 37, file),
                            }),
                            recursive: false,
                            span: Span::new(21, 37, file),
                        })),
                        recursive: false,
                        span: Span::new(12, 37, file),
                    })),
                    recursive: false,
                    span: Span::new(0, 37, file),
                })),
                span: Span::new(0, 38, file),
            })),
        );
    }
}
