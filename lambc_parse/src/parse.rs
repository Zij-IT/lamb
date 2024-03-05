use crate::{
    ArrayPattern, Block, BoolLit, Case, CaseArm, CharLit, CharText, Define, Else, Expr,
    ExprStatement, F64Lit, FileId, FnDef, Group, I64Base, I64Lit, Ident, IdentPattern, If, IfCond,
    InnerPattern, Lexer, List, LiteralPattern, NilLit, Pattern, RestPattern, Span, Statement,
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
            let ident = self.parse_ident()?;
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
            TokKind::OpenBrace => self.parse_block()?,
            TokKind::OpenBrack => self.parse_list()?,
            TokKind::OpenParen => self.parse_group()?,
            TokKind::Fn => self.parse_fn_def(false)?,
            TokKind::Rec => self.parse_fn_def(true)?,
            TokKind::If => self.parse_if()?,
            TokKind::Case => self.parse_case()?,
            _ => self.parse_literal()?,
        })
    }

    /// Parses a list statements and an optional expression for the blocks value, which are delimited by braces.
    /// ```text
    /// Grammar:
    ///
    ///     block_expr := '{' stat* expr? '}'
    /// ```
    fn parse_block(&mut self) -> Result<Expr> {
        Ok(Expr::Block(Box::new(self.parse_raw_block()?)))
    }

    fn parse_raw_block(&mut self) -> Result<Block> {
        let open = self.expect(TokKind::OpenBrace)?;
        let mut stmts = Vec::new();
        let (value, close) = loop {
            if self.peek2().kind == TokKind::Assign {
                let stmt = self.parse_stmt()?;
                stmts.push(stmt);
            } else if self.peek1().kind == TokKind::CloseBrace {
                break (None, self.next());
            } else {
                let expr = self.parse_expr()?;
                let peek1 = self.peek1();
                if peek1.kind == TokKind::Semi {
                    let semi = self.next();
                    let span = expr.span();
                    let stmt = Statement::Expr(ExprStatement {
                        expr,
                        span: Span::connect(span, semi.span),
                    });

                    stmts.push(stmt);
                } else if peek1.kind == TokKind::CloseBrace {
                    break (Some(expr), self.next());
                } else {
                    let err_tok = self.next();
                    return Err(Error {
                        message: format!("Expected a ';' or '}}', but found '{}'", err_tok.slice),
                        span: err_tok.span,
                    });
                }
            }
        };

        Ok(Block {
            statements: stmts,
            value,
            span: Span::connect(open.span, close.span),
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
        let tok = self.expect(TokKind::OpenBrack)?;
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
        let tok = self.expect(TokKind::OpenParen)?;
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
        let tok = if is_recursive {
            let tok = self.expect(TokKind::Rec)?;
            self.expect(TokKind::Fn)?;
            tok
        } else {
            self.expect(TokKind::Fn)?
        };

        self.expect(TokKind::OpenParen)?;
        let (args, _) = self.parse_node_list(TokKind::CloseParen, |this| {
            let next = this.peek1();
            if next.kind == TokKind::Ident {
                Ok(this.parse_ident()?)
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

    fn parse_if(&mut self) -> Result<Expr> {
        let start = self.expect(TokKind::If)?;
        let cond = self.parse_expr()?;
        let body = self.parse_raw_block()?;
        let span = Span::connect(start.span, body.span);
        let if_ = IfCond { cond, body, span };

        let mut end_span = span;

        // Vec<Elif>
        let mut elifs = Vec::new();
        while self.peek1().kind == TokKind::Elif {
            let elif = self.next();
            let cond = self.parse_expr()?;
            let body = self.parse_raw_block()?;
            let span = Span::connect(elif.span, body.span);
            end_span = span;
            elifs.push(IfCond { cond, body, span });
        }

        let els_ = if self.peek1().kind == TokKind::Else {
            let els_ = self.next();
            let body = self.parse_raw_block()?;
            let span = Span::connect(els_.span, body.span);
            end_span = span;
            Some(Else { body, span })
        } else {
            None
        };

        let span = Span::connect(if_.span, end_span);
        Ok(Expr::If(Box::new(If {
            cond: if_,
            elif: elifs,
            els_,
            span,
        })))
    }

    fn parse_case(&mut self) -> Result<Expr> {
        let case = self.expect(TokKind::Case)?;
        let scrutinee = self.parse_expr()?;
        self.expect(TokKind::OpenBrace)?;

        let mut arms = Vec::new();
        while self.peek1().kind != TokKind::CloseBrace {
            arms.push(self.parse_case_arm()?);
        }

        let close = self.expect(TokKind::CloseBrace)?;
        Ok(Expr::Case(Box::new(Case {
            scrutinee,
            arms,
            span: Span::connect(case.span, close.span),
        })))
    }

    /// Parses any value literal, such as that of a string or number, as well as an identifier.
    /// ```text
    /// Grammar:
    ///
    /// case_arm := pattern '->' block_expr
    ///           | pattern '->' expr ','
    /// ```
    fn parse_case_arm(&mut self) -> Result<CaseArm> {
        let pattern = self.parse_pattern()?;
        self.expect(TokKind::Arrow)?;
        let value = self.parse_expr()?;
        let end_span = if !value.ends_with_block() {
            self.expect(TokKind::Comma)?.span
        } else {
            // Eat optional comma
            if self.peek1().kind == TokKind::Comma {
                self.next();
            }

            value.span()
        };

        let span = pattern.span;
        Ok(CaseArm {
            pattern,
            body: value,
            span: Span::connect(span, end_span),
        })
    }

    fn parse_pattern(&mut self) -> Result<Pattern> {
        let first = self.parse_inner_pattern()?;
        let first_span = first.span();
        let mut last_span = first_span;
        let mut patterns = vec![first];

        loop {
            if self.peek1().kind != TokKind::Bor {
                break;
            }

            self.expect(TokKind::Bor)?;
            let pat = self.parse_inner_pattern()?;
            last_span = pat.span();
            patterns.push(pat);
        }

        Ok(Pattern {
            inner: patterns,
            span: Span::connect(first_span, last_span),
        })
    }

    fn parse_inner_pattern(&mut self) -> Result<InnerPattern> {
        let peek1 = self.peek1();
        match peek1.kind {
            TokKind::OpenBrack => self.parse_array_pattern(),
            TokKind::DotDot => self.parse_rest_pattern(),
            TokKind::Ident => self.parse_ident_pattern(),
            _ => self.parse_literal_pattern(),
        }
    }

    fn parse_array_pattern(&mut self) -> Result<InnerPattern> {
        let start = self.expect(TokKind::OpenBrack)?.span;
        let (patterns, end) =
            self.parse_node_list(TokKind::CloseBrack, |this| this.parse_pattern())?;

        Ok(InnerPattern::Array(Box::new(ArrayPattern {
            patterns,
            span: Span::connect(start, end.span),
        })))
    }

    fn parse_rest_pattern(&mut self) -> Result<InnerPattern> {
        let span = self.expect(TokKind::DotDot)?.span;
        Ok(InnerPattern::Rest(Box::new(RestPattern { span })))
    }

    fn parse_ident_pattern(&mut self) -> Result<InnerPattern> {
        let ident = self.parse_ident()?;
        let (bound, span) = if self.peek1().kind == TokKind::Bind {
            self.next();
            let bound = self.parse_inner_pattern()?;
            let end_span = bound.span();
            (Some(Box::new(bound)), end_span)
        } else {
            (None, ident.span)
        };

        let span = Span::connect(ident.span, span);
        Ok(InnerPattern::Ident(Box::new(IdentPattern {
            ident,
            bound,
            span,
        })))
    }

    fn parse_literal_pattern(&mut self) -> Result<InnerPattern> {
        let tok = self.peek1();
        let lit = match tok.kind {
            TokKind::Nil => LiteralPattern::Nil(self.parse_nil()?),
            TokKind::StringStart => LiteralPattern::String(self.parse_string()?),
            TokKind::CharStart => LiteralPattern::Char(self.parse_char()?),
            TokKind::True | TokKind::False => LiteralPattern::Bool(self.parse_bool()?),
            TokKind::BinI64 | TokKind::OctI64 | TokKind::HexI64 | TokKind::DecI64 => {
                LiteralPattern::I64(self.parse_i64()?)
            }
            // Parse a negative number literal
            TokKind::Sub => {
                let neg = self.next();
                let mut num = self.parse_i64()?;
                let spaces = " ".repeat((num.span.start - neg.span.end).saturating_sub(1));
                num.value = format!("-{}{}", spaces, num.value);
                num.span = Span::connect(neg.span, num.span);
                LiteralPattern::I64(num)
            }
            _ => {
                return Err(Error {
                    message: format!("Expected pattern, but found '{}'", tok.slice),
                    span: tok.span,
                })
            }
        };

        Ok(InnerPattern::Literal(Box::new(lit)))
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
            TokKind::Ident => Expr::Ident(self.parse_ident()?),
            TokKind::Nil => Expr::Nil(self.parse_nil()?),
            TokKind::BinI64 | TokKind::OctI64 | TokKind::HexI64 | TokKind::DecI64 => {
                Expr::I64(self.parse_i64()?)
            }
            TokKind::CharStart => Expr::Char(self.parse_char()?),
            TokKind::F64 => Expr::F64(self.parse_f64()?),
            TokKind::True | TokKind::False => Expr::Bool(self.parse_bool()?),
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

    fn parse_i64(&mut self) -> Result<I64Lit> {
        let tok = self.next();
        let base = match tok.kind {
            TokKind::DecI64 => I64Base::Dec,
            TokKind::BinI64 => I64Base::Bin,
            TokKind::OctI64 => I64Base::Oct,
            TokKind::HexI64 => I64Base::Hex,
            _ => {
                return Err(Error {
                    message: format!("Expected number, found '{}'", tok.slice),
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

    fn parse_f64(&mut self) -> Result<F64Lit> {
        let tok = self.expect(TokKind::F64)?;
        Ok(F64Lit {
            value: tok.slice.into_owned(),
            span: tok.span,
        })
    }

    fn parse_bool(&mut self) -> Result<BoolLit> {
        let tok = self.next();
        let tok = match tok.kind {
            TokKind::True | TokKind::False => tok,
            _ => {
                return Err(Error {
                    message: format!("Expected boolean, found '{}'", tok.slice),
                    span: tok.span,
                })
            }
        };

        Ok(BoolLit {
            value: tok.slice.starts_with('t'),
            span: tok.span,
        })
    }

    fn parse_nil(&mut self) -> Result<NilLit> {
        let tok = self.expect(TokKind::Nil)?;
        Ok(NilLit { span: tok.span })
    }

    fn parse_string(&mut self) -> Result<StrLit> {
        let tok = self.expect(TokKind::StringStart)?;
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
        let tok = self.expect(TokKind::CharStart)?;
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

    fn parse_ident(&mut self) -> Result<Ident> {
        let tok = self.expect(TokKind::Ident)?;

        Ok(Ident {
            raw: tok.slice.into(),
            span: tok.span,
        })
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
        ArrayPattern, Block, BoolLit, CharLit, CharText, Define, Else, Expr, ExprStatement, F64Lit,
        FileId, FnDef, Group, I64Base, I64Lit, Ident, IdentPattern, If, IfCond, InnerPattern, List,
        LiteralPattern, NilLit, Parser, Pattern, RestPattern, Span, Statement, StrLit, StrText,
    };
    use pretty_assertions::assert_eq;

    fn int(value: &str, base: I64Base, start: usize, end: usize) -> I64Lit {
        I64Lit {
            base,
            value: value.into(),
            span: span(start, end),
        }
    }

    fn nil(start: usize, end: usize) -> NilLit {
        NilLit {
            span: span(start, end),
        }
    }

    fn bool(value: bool, start: usize, end: usize) -> BoolLit {
        BoolLit {
            value,
            span: span(start, end),
        }
    }

    fn span(start: usize, end: usize) -> Span {
        Span::new(start, end, FileId(0))
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
                Ok(Ident {
                    raw: ident.into(),
                    span: Span {
                        file: FileId(0),
                        start: 0,
                        end: ident.len(),
                    }
                })
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
        macro_rules! atom {
            ($atom:expr) => {
                let mut parser = Parser::new($atom.as_bytes(), FileId(0));
                insta::assert_debug_snapshot!(parser.parse_atom());
            };
        }

        atom!("true");
        atom!("false");
        atom!("2.0e2");
        atom!("nil");
        atom!("42");
        atom!("\"\"");
        atom!("\"hello:nworld\"");
        atom!("''");
        atom!("'hello:nworld'");
        atom!("[]");
        atom!("[nil, 2, \"hello\"]");
        atom!("(1)");
        atom!(
            r#"
            case nil {
                [] | [one] -> fn() -> {}
                [[[[]]]] -> {}
                [2, .., "hi"] -> if true {} elif false {}
                hik @ 2 -> 2,
                -2 -> 2,
            }
        "#
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

        atom(
            "{ x := 2; x }",
            Ok(Expr::Block(Box::new(Block {
                statements: vec![Statement::Define(Define {
                    ident: Ident {
                        raw: "x".into(),
                        span: Span::new(2, 3, file),
                    },
                    value: Expr::I64(I64Lit {
                        base: I64Base::Dec,
                        value: "2".into(),
                        span: Span::new(7, 8, file),
                    }),
                    span: Span::new(2, 9, file),
                })],
                value: Some(Expr::Ident(Ident {
                    raw: "x".into(),
                    span: Span::new(10, 11, file),
                })),
                span: Span::new(0, 13, file),
            }))),
        );

        atom(
            "if true { }",
            Ok(Expr::If(Box::new(If {
                cond: IfCond {
                    cond: Expr::Bool(bool(true, 3, 7)),
                    body: Block {
                        statements: vec![],
                        value: None,
                        span: span(8, 11),
                    },
                    span: span(0, 11),
                },
                elif: vec![],
                els_: None,
                span: span(0, 11),
            }))),
        );

        atom(
            "if true { } elif false { }",
            Ok(Expr::If(Box::new(If {
                cond: IfCond {
                    cond: Expr::Bool(bool(true, 3, 7)),
                    body: Block {
                        statements: vec![],
                        value: None,
                        span: span(8, 11),
                    },
                    span: span(0, 11),
                },
                elif: vec![IfCond {
                    cond: Expr::Bool(bool(false, 17, 22)),
                    body: Block {
                        statements: vec![],
                        value: None,
                        span: span(23, 26),
                    },
                    span: span(12, 26),
                }],
                els_: None,
                span: span(0, 26),
            }))),
        );

        atom(
            "if true { } elif false { } else { }",
            Ok(Expr::If(Box::new(If {
                cond: IfCond {
                    cond: Expr::Bool(bool(true, 3, 7)),
                    body: Block {
                        statements: vec![],
                        value: None,
                        span: span(8, 11),
                    },
                    span: span(0, 11),
                },
                elif: vec![IfCond {
                    cond: Expr::Bool(bool(false, 17, 22)),
                    body: Block {
                        statements: vec![],
                        value: None,
                        span: span(23, 26),
                    },
                    span: span(12, 26),
                }],
                els_: Some(Else {
                    body: Block {
                        statements: vec![],
                        value: None,
                        span: span(32, 35),
                    },
                    span: span(27, 35),
                }),
                span: span(0, 35),
            }))),
        );

        atom(
            "if true { } elif false { } elif false { } else { }",
            Ok(Expr::If(Box::new(If {
                cond: IfCond {
                    cond: Expr::Bool(bool(true, 3, 7)),
                    body: Block {
                        statements: vec![],
                        value: None,
                        span: span(8, 11),
                    },
                    span: span(0, 11),
                },
                elif: vec![
                    IfCond {
                        cond: Expr::Bool(bool(false, 17, 22)),
                        body: Block {
                            statements: vec![],
                            value: None,
                            span: span(23, 26),
                        },
                        span: span(12, 26),
                    },
                    IfCond {
                        cond: Expr::Bool(bool(false, 32, 37)),
                        body: Block {
                            statements: vec![],
                            value: None,
                            span: span(38, 41),
                        },
                        span: span(27, 41),
                    },
                ],
                els_: Some(Else {
                    body: Block {
                        statements: vec![],
                        value: None,
                        span: span(47, 50),
                    },
                    span: span(42, 50),
                }),
                span: span(0, 50),
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

    #[test]
    fn parse_nested_blocks() {
        let file = FileId(0);
        let atom = |atom: &str, out| {
            let mut parser = Parser::new(atom.as_bytes(), file);
            assert_eq!(parser.parse_atom(), out)
        };

        atom(
            "{ { { } } }",
            Ok(Expr::Block(Box::new(Block {
                statements: vec![],
                value: Some(Expr::Block(Box::new(Block {
                    statements: vec![],
                    value: Some(Expr::Block(Box::new(Block {
                        statements: vec![],
                        value: None,
                        span: Span::new(4, 7, file),
                    }))),
                    span: Span::new(2, 9, file),
                }))),
                span: Span::new(0, 11, file),
            }))),
        );

        atom(
            "{ x := 2; { x := 2; { x := { 2 }; } } }",
            Ok(Expr::Block(Box::new(Block {
                statements: vec![Statement::Define(Define {
                    ident: Ident {
                        raw: "x".into(),
                        span: Span::new(2, 3, file),
                    },
                    value: Expr::I64(I64Lit {
                        base: I64Base::Dec,
                        value: "2".into(),
                        span: Span::new(7, 8, file),
                    }),
                    span: Span::new(2, 9, file),
                })],
                value: Some(Expr::Block(Box::new(Block {
                    statements: vec![Statement::Define(Define {
                        ident: Ident {
                            raw: "x".into(),
                            span: Span::new(12, 13, file),
                        },
                        value: Expr::I64(I64Lit {
                            base: I64Base::Dec,
                            value: "2".into(),
                            span: Span::new(17, 18, file),
                        }),
                        span: Span::new(12, 19, file),
                    })],
                    value: Some(Expr::Block(Box::new(Block {
                        statements: vec![Statement::Define(Define {
                            ident: Ident {
                                raw: "x".into(),
                                span: Span::new(22, 23, file),
                            },
                            value: Expr::Block(Box::new(Block {
                                statements: vec![],
                                value: Some(Expr::I64(I64Lit {
                                    base: I64Base::Dec,
                                    value: "2".into(),
                                    span: Span::new(29, 30, file),
                                })),
                                span: Span::new(27, 32, file),
                            })),
                            span: Span::new(22, 33, file),
                        })],
                        value: None,
                        span: Span::new(20, 35, file),
                    }))),
                    span: Span::new(10, 37, file),
                }))),
                span: Span::new(0, 39, file),
            }))),
        );
    }

    #[test]
    fn parses_nested_ifs() {
        let file = FileId(0);
        let atom = |atom: &str, out| {
            let mut parser = Parser::new(atom.as_bytes(), file);
            assert_eq!(parser.parse_atom(), out)
        };

        atom(
            "if true { if true { } } elif false { } elif false { } else { }",
            Ok(Expr::If(Box::new(If {
                cond: IfCond {
                    cond: Expr::Bool(bool(true, 3, 7)),
                    body: Block {
                        statements: vec![],
                        value: Some(Expr::If(Box::new(If {
                            cond: IfCond {
                                cond: Expr::Bool(bool(true, 13, 17)),
                                body: Block {
                                    statements: vec![],
                                    value: None,
                                    span: span(18, 21),
                                },
                                span: span(10, 21),
                            },
                            elif: vec![],
                            els_: None,
                            span: span(10, 21),
                        }))),
                        span: span(8, 23),
                    },
                    span: span(0, 23),
                },
                elif: vec![
                    IfCond {
                        cond: Expr::Bool(bool(false, 29, 34)),
                        body: Block {
                            statements: vec![],
                            value: None,
                            span: span(35, 38),
                        },
                        span: span(24, 38),
                    },
                    IfCond {
                        cond: Expr::Bool(bool(false, 44, 49)),
                        body: Block {
                            statements: vec![],
                            value: None,
                            span: span(50, 53),
                        },
                        span: span(39, 53),
                    },
                ],
                els_: Some(Else {
                    body: Block {
                        statements: vec![],
                        value: None,
                        span: span(59, 62),
                    },
                    span: span(54, 62),
                }),
                span: span(0, 62),
            }))),
        );
    }

    #[test]
    fn parses_literal_pattern() {
        let file = FileId(0);
        let pat = |pat: &str, out| {
            let mut parser = Parser::new(pat.as_bytes(), file);
            let lit = parser.parse_literal_pattern();
            assert_eq!(lit, Ok(InnerPattern::Literal(Box::new(out))));
        };

        pat("2", LiteralPattern::I64(int("2", I64Base::Dec, 0, 1)));

        pat(
            "\"hello\"",
            LiteralPattern::String(StrLit {
                text: Some(StrText {
                    inner: "hello".into(),
                    span: span(1, 6),
                }),
                span: span(0, 7),
            }),
        );

        pat(
            "'h'",
            LiteralPattern::Char(CharLit {
                text: Some(CharText {
                    inner: "h".into(),
                    span: span(1, 2),
                }),
                span: span(0, 3),
            }),
        );

        pat(
            "true",
            LiteralPattern::Bool(BoolLit {
                value: true,
                span: span(0, 4),
            }),
        );

        pat("nil", LiteralPattern::Nil(NilLit { span: span(0, 3) }));
    }

    #[test]
    fn parses_array_pattern() {
        let file = FileId(0);
        let pat = |pat: &str, out| {
            let mut parser = Parser::new(pat.as_bytes(), file);
            let lit = parser.parse_array_pattern();
            assert_eq!(lit, Ok(InnerPattern::Array(Box::new(out))));
        };

        pat(
            "[]",
            ArrayPattern {
                patterns: vec![],
                span: span(0, 2),
            },
        );

        pat(
            "[1, 2]",
            ArrayPattern {
                patterns: vec![
                    Pattern {
                        inner: vec![InnerPattern::Literal(Box::new(LiteralPattern::I64(
                            I64Lit {
                                base: I64Base::Dec,
                                value: "1".into(),
                                span: span(1, 2),
                            },
                        )))],
                        span: span(1, 2),
                    },
                    Pattern {
                        inner: vec![InnerPattern::Literal(Box::new(LiteralPattern::I64(
                            I64Lit {
                                base: I64Base::Dec,
                                value: "2".into(),
                                span: span(4, 5),
                            },
                        )))],
                        span: span(4, 5),
                    },
                ],
                span: span(0, 6),
            },
        );

        pat(
            "[[[]]]",
            ArrayPattern {
                patterns: vec![Pattern {
                    inner: vec![InnerPattern::Array(Box::new(ArrayPattern {
                        patterns: vec![Pattern {
                            inner: vec![InnerPattern::Array(Box::new(ArrayPattern {
                                patterns: vec![],
                                span: span(2, 4),
                            }))],
                            span: span(2, 4),
                        }],
                        span: span(1, 5),
                    }))],
                    span: span(1, 5),
                }],
                span: span(0, 6),
            },
        );
    }

    #[test]
    fn parses_ident_pattern() {
        let file = FileId(0);
        let pat = |pat: &str, out| {
            let mut parser = Parser::new(pat.as_bytes(), file);
            let lit = parser.parse_ident_pattern();
            assert_eq!(lit, Ok(InnerPattern::Ident(Box::new(out))));
        };

        pat(
            "i",
            IdentPattern {
                ident: Ident {
                    raw: "i".into(),
                    span: span(0, 1),
                },
                bound: None,
                span: span(0, 1),
            },
        );

        pat(
            "i @ 1",
            IdentPattern {
                ident: Ident {
                    raw: "i".into(),
                    span: span(0, 1),
                },
                bound: Some(Box::new(InnerPattern::Literal(Box::new(
                    LiteralPattern::I64(I64Lit {
                        base: I64Base::Dec,
                        value: "1".into(),
                        span: span(4, 5),
                    }),
                )))),
                span: span(0, 5),
            },
        );

        pat(
            "i @ ..",
            IdentPattern {
                ident: Ident {
                    raw: "i".into(),
                    span: span(0, 1),
                },
                bound: Some(Box::new(InnerPattern::Rest(Box::new(RestPattern {
                    span: span(4, 6),
                })))),
                span: span(0, 6),
            },
        );
    }
}
