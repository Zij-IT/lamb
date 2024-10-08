//! The home for the Lamb-Parser
//!
//! The [`Parser<'_>`][`Parser`] is a recursive-descent parser which uses [Pratt Parsing] to
//! parse operator-expressions. The parser currently requres a look-ahead of two
//! tokens, and uses a pull-based lexer to lex the tokens. The parser has no
//! support for error-recovery, and abandons parsing after encountering an error.
//!
//! [Pratt Parsing]: https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html

use std::path::PathBuf;

use miette::Diagnostic;
use thiserror::Error as ThError;

use crate::{
    ArrayPattern, Binary, BinaryOp, Block, BoolLit, Call, Case, CaseArm,
    CharLit, CharText, Define, Else, Export, ExportItem, Expr, ExprStatement,
    F64Lit, FnDef, FnType, Generic, Generics, Group, I64Base, I64Lit, Ident,
    IdentPattern, If, IfCond, Import, ImportItem, Index, InnerPattern, Item,
    Lexer, List, LiteralPattern, Module, NamedType, NilLit, Pattern,
    RestPattern, Return, SimpleGeneric, SimpleGenerics, Span, Statement,
    StrLit, StrText, TokKind, Token, Type, Unary, UnaryOp,
};

/// A syntax error which occured either while lexing or parsing
#[derive(Diagnostic, Debug, ThError, PartialEq, Eq)]
#[error("error[Parse Error]: {message}")]
#[diagnostic()]
pub struct Error {
    message: String,
    #[label]
    span: Span,
}

pub type Result<T> = core::result::Result<T, Error>;

/// The Lamb parser, which is used to parse entire [`Module`].
pub struct Parser<'a> {
    file: PathBuf,
    lexer: Lexer<'a>,
    peek1: Option<Token<'a>>,
    peek2: Option<Token<'a>>,
}

impl<'a> Parser<'a> {
    /// Creates a new parser which will parse `input` and use `file` for references to files
    /// within the module (such as for [`Module::path`])
    pub fn new<P: Into<PathBuf>>(input: &'a [u8], file: P) -> Self {
        Self {
            lexer: Lexer::new(input),
            peek1: None,
            peek2: None,
            file: file.into(),
        }
    }

    /// Parses a singular lamb module. A module may consist of the following items
    /// in the following order:
    /// + An export declaration
    /// + Any amount of Import declarations
    /// + Any amount of Items
    pub fn parse_module(&mut self) -> Result<Module<Ident, PathBuf>> {
        // Only a singular export is expected, however this is a simple way to catch
        // when the user writes multiple
        let mut exports = Vec::new();
        loop {
            let peek = self.peek1();
            let peek_is_export =
                peek.kind == TokKind::Ident && peek.slice == "export";
            let peek2_is_brace = self.peek2().kind == TokKind::OpenBrace;

            if peek_is_export && peek2_is_brace {
                exports.push(self.parse_export()?);
            } else {
                break;
            }
        }

        let mut imports = Vec::new();
        loop {
            let peek = self.peek1();
            let peek_is_import =
                peek.kind == TokKind::Ident && peek.slice == "from";
            let peek2_is_brace = self.peek2().kind == TokKind::StringStart;

            if peek_is_import && peek2_is_brace {
                imports.push(self.parse_import()?);
            } else {
                break;
            }
        }

        let mut items = Vec::new();
        let span = loop {
            if let Some(end) = self.eat(TokKind::End) {
                break Span::new(0, end.span.end);
            }

            items.push(self.parse_item()?);
        };

        Ok(Module { exports, imports, items, path: self.file.clone(), span })
    }

    /// Parses an item, such as a top-level definition.
    ///
    /// # Example
    ///
    /// ```
    /// use lambc_parse::{ Parser, Define, Expr, Span, Ident, I64Lit, I64Base, Item };
    ///
    /// let mut parser = Parser::new("def x := 2;".as_bytes(), "<PATH>");
    /// let item = parser.parse_item()?;
    ///
    /// assert_eq!(item, Item::Def(Define {
    ///       ident: Ident { raw: "x".into(), span: Span::new(4, 5) },
    ///       typ: None,
    ///       value: Expr::I64(I64Lit { base: I64Base::Dec, span: Span::new(9, 10), value: "2".into() }),
    ///       span: Span::new(0, 11),
    /// }));
    /// # Ok::<(), Box<dyn std::error::Error>>(())
    /// ```
    pub fn parse_item(&mut self) -> Result<Item<Ident>> {
        match self.peek1().kind {
            TokKind::Def => Ok(Item::Def(self.parse_definition()?)),
            _ => Err(Self::error_expected_str_found("item", self.peek1())),
        }
    }

    /// Parses a complete top-level definition
    ///
    /// # Example
    ///
    /// ```
    /// use lambc_parse::{ Parser, Define, Expr, Span, Ident, I64Lit, I64Base };
    ///
    /// let mut parser = Parser::new("def x := 2;".as_bytes(), "<PATH>");
    /// let def = parser.parse_definition()?;
    ///
    /// assert_eq!(def, Define {
    ///       ident: Ident { raw: "x".into(), span: Span::new(4, 5) },
    ///       typ: None,
    ///       value: Expr::I64(I64Lit { base: I64Base::Dec, span: Span::new(9, 10), value: "2".into() }),
    ///       span: Span::new(0, 11),
    /// });
    /// # Ok::<(), Box<dyn std::error::Error>>(())
    /// ```
    pub fn parse_definition(&mut self) -> Result<Define<Ident>> {
        let start = self.expect(TokKind::Def)?;
        let mut def = self.parse_define()?;
        def.span = Span::connect(start.span, def.span);

        Ok(def)
    }

    /// Parses an expression with no trailing input.
    pub fn parse_expr_end(&mut self) -> Result<Expr<Ident>> {
        let expr = self.parse_expr()?;
        self.expect(TokKind::End)?;
        Ok(expr)
    }

    /// Parses import declarations
    ///
    /// # Example
    ///
    /// ```
    /// use lambc_parse::{ Parser };
    ///
    /// let imports = [
    ///     r#"from "./path1" as path1 import *;"#,
    ///     r#"from "./path2" import { item1, item2 }; "#,
    ///     r#"from "./path3" import { item1 as a, item2 as b }; "#,
    /// ].join("\n");
    ///
    /// let mut parser = Parser::new(imports.as_bytes(), "<PATH>");
    ///
    /// let path1 = parser.parse_import()?;
    /// let path2 = parser.parse_import()?;
    /// let path3 = parser.parse_import()?;
    /// assert!(parser.parse_import().is_err());
    ///
    /// # Ok::<(), Box<dyn std::error::Error>>(())
    /// ```
    pub fn parse_import(&mut self) -> Result<Import<Ident, PathBuf>> {
        // 'from' is not a keyword, and because of this we check the slice
        let start = self.expect_ident("from")?;
        let path = self.parse_string()?;
        let path_span = path.span;
        let path =
            path.text.map_or(PathBuf::from(""), |t| PathBuf::from(t.inner));

        let name =
            self.eat_ident("as").map(|_| self.parse_ident()).transpose()?;

        let mut items = Vec::new();
        let mut is_glob = false;

        if self.eat_ident("import").is_some() {
            if self.eat(TokKind::Mul).is_some() {
                // from path [as alias] import *
                is_glob = true;
            } else {
                // from path [as alias] import '{' items '}'
                let (_, imports, _) = self.parse_node_list(
                    TokKind::OpenBrace,
                    TokKind::CloseBrace,
                    Self::parse_import_item,
                )?;

                items = imports;
            }
        };

        let semi = self.expect(TokKind::Semi)?;

        Ok(Import {
            file: path,
            name,
            items,
            star: is_glob,
            span: Span::connect(start.span, semi.span),
            path_span,
        })
    }

    fn parse_import_item(&mut self) -> Result<ImportItem<Ident>> {
        let item = self.parse_ident()?;
        let (alias, span) = if self.eat_ident("as").is_some() {
            let alias = self.parse_ident()?;
            let span = alias.span;
            (Some(alias), span)
        } else {
            (None, item.span)
        };

        Ok(ImportItem { item, alias, span })
    }

    /// Parses export declarations
    ///
    /// # Example
    ///
    /// ```
    /// use lambc_parse::{ Parser };
    ///
    /// let exports = [
    ///     r#"export { a, b, c };"#,
    ///     r#"export { a as x, b as y, c as z };"#,
    /// ].join("\n");
    ///
    /// let mut parser = Parser::new(exports.as_bytes(), "<PATH>");
    ///
    /// let simple_exports = parser.parse_export()?;
    /// let renamed_exports  = parser.parse_export()?;
    ///
    /// # Ok::<(), Box<dyn std::error::Error>>(())
    /// ```
    pub fn parse_export(&mut self) -> Result<Export<Ident>> {
        let export = self.expect_ident("export")?;
        let (_, items, _) = self.parse_node_list(
            TokKind::OpenBrace,
            TokKind::CloseBrace,
            Self::parse_export_item,
        )?;

        let end = self.expect(TokKind::Semi)?;
        Ok(Export { items, span: Span::connect(export.span, end.span) })
    }

    fn parse_export_item(&mut self) -> Result<ExportItem<Ident>> {
        let item = self.parse_ident()?;
        let (alias, span) = if self.eat_ident("as").is_some() {
            let alias = self.parse_ident()?;
            let span = alias.span;
            (Some(alias), span)
        } else {
            (None, item.span)
        };

        Ok(ExportItem { span: Span::connect(item.span, span), item, alias })
    }

    /// Parses a statement as defined by the following grammar
    /// ```text
    /// Grammar:
    ///
    ///     stmt := ident ':=' expr ';'
    ///           | expr  ';'
    /// ```
    #[cfg(test)]
    fn parse_stmt(&mut self) -> Result<Statement<Ident>> {
        if self.peek1().kind == TokKind::Let {
            self.parse_assign_stmt()
        } else {
            self.parse_expr_stmt()
        }
    }

    #[cfg(test)]
    fn parse_expr_stmt(&mut self) -> Result<Statement<Ident>> {
        let expr = self.parse_expr()?;
        let span = expr.span();
        let semi = self.expect(TokKind::Semi)?;

        Ok(Statement::Expr(ExprStatement {
            expr,
            span: Span::connect(span, semi.span),
        }))
    }

    fn parse_assign_stmt(&mut self) -> Result<Statement<Ident>> {
        let start = self.expect(TokKind::Let)?;
        let mut define = self.parse_define()?;
        define.span = Span::connect(start.span, define.span);

        Ok(Statement::Define(define))
    }

    fn parse_define(&mut self) -> Result<Define<Ident>> {
        let ident = self.parse_ident()?;
        self.expect(TokKind::Colon)?;
        let typ = (self.peek1().kind != TokKind::Eq)
            .then(|| self.parse_type())
            .transpose()?;

        self.expect(TokKind::Eq)?;
        let value = self.parse_expr()?;
        let semi = self.expect(TokKind::Semi)?;

        Ok(Define {
            span: Span::connect(ident.span, semi.span),
            ident,
            value,
            typ,
        })
    }

    fn parse_type(&mut self) -> Result<Type<Ident>> {
        // Types:
        // + basic: `int`
        // + generic: `list[int]`
        // + function: `fn(int) -> int`
        // + generic function: `fn[t](t) -> t`
        //
        // Note: generics specified for a function should be a simple name, and
        // should *NOT* itself have any type arguments.
        if self.peek1().kind == TokKind::Fn {
            return self.parse_fn_type();
        }

        let name = if let Some(nil) = self.eat(TokKind::Nil) {
            Ident { raw: nil.slice.into_owned(), span: nil.span }
        } else {
            self.parse_ident()?
        };

        let gens = (self.peek1().kind == TokKind::OpenBrack)
            .then(|| {
                self.parse_node_list(
                    TokKind::OpenBrack,
                    TokKind::CloseBrack,
                    Self::parse_type,
                )
                .map(|(beg, ids, end)| Generics {
                    params: ids
                        .into_iter()
                        .map(|ty| Generic { span: ty.span(), id: ty })
                        .collect(),
                    span: Span::connect(beg.span, end.span),
                })
            })
            .transpose()?;

        let span = Span::connect(
            name.span,
            gens.as_ref().map(|gens| gens.span).unwrap_or(name.span),
        );

        Ok(Type::Named(Box::new(NamedType { name, gens, span })))
    }

    fn parse_fn_type(&mut self) -> Result<Type<Ident>> {
        let fn_kw = self.expect(TokKind::Fn)?;
        let tparams = (self.peek1().kind == TokKind::OpenBrack)
            .then(|| {
                self.parse_node_list(
                    TokKind::OpenBrack,
                    TokKind::CloseBrack,
                    Self::parse_ident,
                )
                .map(|(beg, tys, end)| SimpleGenerics {
                    params: tys
                        .into_iter()
                        .map(|id| SimpleGeneric { span: id.span, id })
                        .collect(),
                    span: Span::connect(beg.span, end.span),
                })
            })
            .transpose()?;

        let (_, types, end) = self.parse_node_list(
            TokKind::OpenParen,
            TokKind::CloseParen,
            Self::parse_type,
        )?;

        let ret_type =
            self.eat(TokKind::Arrow).map(|_| self.parse_type()).transpose()?;

        let final_span =
            ret_type.as_ref().map(|rt| rt.span()).unwrap_or(end.span);

        Ok(Type::Fn(Box::new(FnType {
            args: types,
            gens: tparams,
            ret_type,
            span: Span::connect(fn_kw.span, final_span),
        })))
    }

    /// ```text
    /// Grammar:
    ///
    ///    expr := expr op expr
    ///          | expr '[' expr ']'
    ///          | expr '(' expr ')'
    /// ```
    fn parse_expr(&mut self) -> Result<Expr<Ident>> {
        self.parse_expr_pratt(0)
    }

    fn parse_expr_pratt(&mut self, min_bp: u8) -> Result<Expr<Ident>> {
        let peek = self.peek1();
        let mut lhs = match peek.kind {
            TokKind::Sub | TokKind::Bneg | TokKind::Lnot => {
                let tok = self.next();
                let op = UnaryOp::from(tok.kind);
                let rhs = self.parse_expr_pratt(op.rbp())?;

                Expr::Unary(Box::new(Unary {
                    span: Span::connect(tok.span, rhs.span()),
                    op_span: tok.span,
                    rhs,
                    op,
                }))
            }
            _ => self.parse_chained_expr()?,
        };

        loop {
            let op = match self.peek1().kind {
                TokKind::Add => BinaryOp::Add,
                TokKind::Sub => BinaryOp::Sub,
                TokKind::Mul => BinaryOp::Mul,
                TokKind::Div => BinaryOp::Div,
                TokKind::Mod => BinaryOp::Mod,
                TokKind::Appl => BinaryOp::Appl,
                TokKind::Appr => BinaryOp::Appr,
                TokKind::Cpsl => BinaryOp::Cpsl,
                TokKind::Cpsr => BinaryOp::Cpsr,
                TokKind::Land => BinaryOp::Land,
                TokKind::Lor => BinaryOp::Lor,
                TokKind::Eq => BinaryOp::Eq,
                TokKind::Ne => BinaryOp::Ne,
                TokKind::Ge => BinaryOp::Ge,
                TokKind::Gt => BinaryOp::Gt,
                TokKind::Le => BinaryOp::Le,
                TokKind::Lt => BinaryOp::Lt,
                TokKind::Bor => BinaryOp::Bor,
                TokKind::Xor => BinaryOp::Bxor,
                TokKind::Band => BinaryOp::Band,
                TokKind::Shl => BinaryOp::Shl,
                TokKind::Shr => BinaryOp::Shr,
                _ => break,
            };

            let (lbp, rbp) = op.infix_bp();
            if lbp < min_bp {
                break;
            }

            let op_tok = self.next();
            let rhs = self.parse_expr_pratt(rbp)?;
            lhs = Expr::Binary(Box::new(Binary {
                span: Span::connect(lhs.span(), rhs.span()),
                op_span: op_tok.span,
                lhs,
                rhs,
                op,
            }));
        }

        Ok(lhs)
    }

    fn parse_chained_expr(&mut self) -> Result<Expr<Ident>> {
        let mut res = self.parse_atom()?;
        while !res.ends_with_block() {
            let peek = self.peek1();
            match peek.kind {
                TokKind::OpenBrack => res = self.parse_index_expr(res)?,
                TokKind::OpenParen => res = self.parse_call_expr(res)?,
                _ => break,
            }
        }

        Ok(res)
    }

    fn parse_call_expr(&mut self, callee: Expr<Ident>) -> Result<Expr<Ident>> {
        let (_, args, end_tok) = self.parse_node_list(
            TokKind::OpenParen,
            TokKind::CloseParen,
            Self::parse_expr,
        )?;

        let span = Span::connect(callee.span(), end_tok.span);
        Ok(Expr::Call(Box::new(Call { callee, args, span })))
    }

    fn parse_index_expr(&mut self, res: Expr<Ident>) -> Result<Expr<Ident>> {
        self.expect(TokKind::OpenBrack)?;
        let index = self.parse_expr()?;
        let close = self.expect(TokKind::CloseBrack)?;
        let span = Span::connect(res.span(), close.span);

        Ok(Expr::Index(Box::new(Index { lhs: res, rhs: index, span })))
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
    fn parse_atom(&mut self) -> Result<Expr<Ident>> {
        let kind = self.peek1().kind;
        Ok(match kind {
            TokKind::OpenBrace => self.parse_block()?,
            TokKind::OpenBrack => self.parse_list()?,
            TokKind::OpenParen => self.parse_group()?,
            TokKind::Fn => self.parse_fn_def()?,
            TokKind::Rec => self.parse_fn_def()?,
            TokKind::If => self.parse_if()?,
            TokKind::Case => self.parse_case()?,
            TokKind::Return => self.parse_return()?,
            _ => self.parse_literal()?,
        })
    }

    /// Parses a list statements and an optional expression for the blocks value, which are delimited by braces.
    /// ```text
    /// Grammar:
    ///
    ///     block_expr := '{' stat* expr? '}'
    /// ```
    fn parse_block(&mut self) -> Result<Expr<Ident>> {
        Ok(Expr::Block(Box::new(self.parse_raw_block()?)))
    }

    fn parse_raw_block(&mut self) -> Result<Block<Ident>> {
        let open = self.expect(TokKind::OpenBrace)?;
        let mut stmts = Vec::new();
        let (value, close) = loop {
            if self.peek1().kind == TokKind::Let {
                stmts.push(self.parse_assign_stmt()?);
            } else if let Some(close) = self.eat(TokKind::CloseBrace) {
                break (None, close);
            } else {
                let expr = self.parse_expr()?;
                if let Some(semi) = self.eat(TokKind::Semi) {
                    let span = expr.span();
                    let stmt = Statement::Expr(ExprStatement {
                        expr,
                        span: Span::connect(span, semi.span),
                    });

                    stmts.push(stmt);
                } else if let Some(close) = self.eat(TokKind::CloseBrace) {
                    break (Some(expr), close);
                } else {
                    return Err(Self::error_expected_str_found(
                        "a ';' or '}'",
                        self.peek1(),
                    ));
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
    fn parse_list(&mut self) -> Result<Expr<Ident>> {
        let (tok, values, end_tok) = self.parse_node_list(
            TokKind::OpenBrack,
            TokKind::CloseBrack,
            Self::parse_expr,
        )?;

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
    fn parse_group(&mut self) -> Result<Expr<Ident>> {
        let tok = self.expect(TokKind::OpenParen)?;
        let value = self.parse_expr()?;
        let next = self.expect(TokKind::CloseParen)?;

        Ok(Expr::Group(Box::new(Group {
            value,
            span: Span::connect(tok.span, next.span),
        })))
    }

    /// Parses a function definition
    /// ```text
    /// Grammar:
    ///
    ///     function_def := 'rec'? 'fn' '(' [arg_list] ')' '->' expr
    ///
    ///     arg_list := ident (',' ident )* ','?
    /// ```
    fn parse_fn_def(&mut self) -> Result<Expr<Ident>> {
        let tok = self.eat(TokKind::Rec).unwrap_or(self.expect(TokKind::Fn)?);
        let (_, args, _) = self.parse_node_list(
            TokKind::OpenParen,
            TokKind::CloseParen,
            Self::parse_ident,
        )?;

        self.expect(TokKind::Arrow)?;

        let value = self.parse_expr()?;
        let span = value.span();

        Ok(Expr::FnDef(Box::new(FnDef {
            args,
            body: value,
            recursive: tok.kind == TokKind::Rec,
            span: Span::connect(tok.span, span),
        })))
    }

    fn parse_if(&mut self) -> Result<Expr<Ident>> {
        let start = self.expect(TokKind::If)?;
        let cond = self.parse_expr()?;
        let body = self.parse_raw_block()?;
        let span = Span::connect(start.span, body.span);
        let if_ = IfCond { cond, body, span };

        let mut end_span = span;

        // Vec<Elif>
        let mut elifs = Vec::new();
        while let Some(elif) = self.eat(TokKind::Elif) {
            let cond = self.parse_expr()?;
            let body = self.parse_raw_block()?;
            let span = Span::connect(elif.span, body.span);
            end_span = span;
            elifs.push(IfCond { cond, body, span });
        }

        let els_ = if let Some(els_) = self.eat(TokKind::Else) {
            let body = self.parse_raw_block()?;
            let span = Span::connect(els_.span, body.span);
            end_span = span;
            Some(Else { body, span })
        } else {
            None
        };

        let span = Span::connect(if_.span, end_span);
        Ok(Expr::If(Box::new(If { cond: if_, elif: elifs, els_, span })))
    }

    fn parse_case(&mut self) -> Result<Expr<Ident>> {
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

    fn parse_return(&mut self) -> Result<Expr<Ident>> {
        let ret = self.expect(TokKind::Return)?;
        let peek = self.peek1();
        let (span, value) = match peek.kind {
            // These tokens are tokens that indicate the end of an expression.
            TokKind::CloseBrace
            | TokKind::CloseBrack
            | TokKind::CloseParen
            | TokKind::Comma
            | TokKind::Semi
            | TokKind::End
            | TokKind::Invalid => (ret.span, None),
            _ => {
                let expr = self.parse_expr()?;
                (expr.span(), Some(expr))
            }
        };

        Ok(Expr::Return(Box::new(Return { value, span })))
    }

    /// Parses any value literal, such as that of a string or number, as well as an identifier.
    /// ```text
    /// Grammar:
    ///
    /// case_arm := pattern '->' block_expr
    ///           | pattern '->' expr ','
    /// ```
    fn parse_case_arm(&mut self) -> Result<CaseArm<Ident>> {
        let pattern = self.parse_pattern()?;
        self.expect(TokKind::Arrow)?;
        let value = self.parse_expr()?;
        let end_span = if value.ends_with_block() {
            self.eat(TokKind::Comma);
            value.span()
        } else {
            self.expect(TokKind::Comma)?.span
        };

        let span = pattern.span;
        Ok(CaseArm {
            pattern,
            body: value,
            span: Span::connect(span, end_span),
        })
    }

    fn parse_pattern(&mut self) -> Result<Pattern<Ident>> {
        let first = self.parse_inner_pattern()?;
        let first_span = first.span();
        let mut last_span = first_span;
        let mut patterns = vec![first];

        while self.eat(TokKind::Bor).is_some() {
            let pat = self.parse_inner_pattern()?;
            last_span = pat.span();
            patterns.push(pat);
        }

        Ok(Pattern {
            inner: patterns,
            span: Span::connect(first_span, last_span),
        })
    }

    fn parse_inner_pattern(&mut self) -> Result<InnerPattern<Ident>> {
        let peek1 = self.peek1();
        match peek1.kind {
            TokKind::OpenBrack => self.parse_array_pattern(),
            TokKind::DotDot => self.parse_rest_pattern(),
            TokKind::Ident => self.parse_ident_pattern(),
            _ => self.parse_literal_pattern(),
        }
    }

    fn parse_array_pattern(&mut self) -> Result<InnerPattern<Ident>> {
        let (start, patterns, end) = self.parse_node_list(
            TokKind::OpenBrack,
            TokKind::CloseBrack,
            Self::parse_pattern,
        )?;

        Ok(InnerPattern::Array(Box::new(ArrayPattern {
            patterns,
            span: Span::connect(start.span, end.span),
        })))
    }

    fn parse_rest_pattern(&mut self) -> Result<InnerPattern<Ident>> {
        let span = self.expect(TokKind::DotDot)?.span;
        Ok(InnerPattern::Rest(Box::new(RestPattern { span })))
    }

    fn parse_ident_pattern(&mut self) -> Result<InnerPattern<Ident>> {
        let ident = self.parse_ident()?;
        let (bound, span) = if self.eat(TokKind::Bind).is_some() {
            let bound = self.parse_inner_pattern()?;
            let end_span = bound.span();
            (Some(Box::new(bound)), end_span)
        } else {
            (None, ident.span)
        };

        let span = Span::connect(ident.span, span);
        Ok(InnerPattern::Ident(Box::new(IdentPattern { ident, bound, span })))
    }

    fn parse_literal_pattern(&mut self) -> Result<InnerPattern<Ident>> {
        let tok = self.peek1();
        let lit = match tok.kind {
            TokKind::Nil => LiteralPattern::Nil(self.parse_nil()?),
            TokKind::StringStart => {
                LiteralPattern::String(self.parse_string()?)
            }
            TokKind::CharStart => LiteralPattern::Char(self.parse_char()?),
            TokKind::True | TokKind::False => {
                LiteralPattern::Bool(self.parse_bool()?)
            }
            TokKind::BinI64
            | TokKind::OctI64
            | TokKind::HexI64
            | TokKind::DecI64 => LiteralPattern::I64(self.parse_i64()?),
            // Parse a negative number literal
            TokKind::Sub => {
                let neg = self.next();
                let mut num = self.parse_i64()?;
                let spaces = " "
                    .repeat((num.span.start - neg.span.end).saturating_sub(1));
                num.value = format!("-{}{}", spaces, num.value);
                num.span = Span::connect(neg.span, num.span);
                LiteralPattern::I64(num)
            }
            _ => return Err(Self::error_expected_str_found("a pattern", tok)),
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
    fn parse_literal(&mut self) -> Result<Expr<Ident>> {
        let tok = self.peek1();
        Ok(match tok.kind {
            TokKind::Ident => Expr::Ident(self.parse_ident()?),
            TokKind::Nil => Expr::Nil(self.parse_nil()?),
            TokKind::CharStart => Expr::Char(self.parse_char()?),
            TokKind::F64 => Expr::F64(self.parse_f64()?),
            TokKind::True | TokKind::False => Expr::Bool(self.parse_bool()?),
            TokKind::StringStart => Expr::String(self.parse_string()?),
            TokKind::BinI64
            | TokKind::OctI64
            | TokKind::HexI64
            | TokKind::DecI64 => Expr::I64(self.parse_i64()?),
            _ => {
                return Err(Self::error_expected_str_found(
                    "an expression",
                    tok,
                ))
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
            _ => return Err(Self::error_expected_str_found("an i64", &tok)),
        };

        Ok(I64Lit { base, value: tok.slice.into_owned(), span: tok.span })
    }

    fn parse_f64(&mut self) -> Result<F64Lit> {
        let tok = self.expect(TokKind::F64)?;
        Ok(F64Lit { value: tok.slice.into_owned(), span: tok.span })
    }

    fn parse_bool(&mut self) -> Result<BoolLit> {
        let tok = self.next();
        let tok = match tok.kind {
            TokKind::True | TokKind::False => tok,
            _ => return Err(Self::error_expected_str_found("boolean", &tok)),
        };

        Ok(BoolLit { value: tok.slice.starts_with('t'), span: tok.span })
    }

    fn parse_nil(&mut self) -> Result<NilLit> {
        let tok = self.expect(TokKind::Nil)?;
        Ok(NilLit { span: tok.span })
    }

    fn parse_string(&mut self) -> Result<StrLit> {
        let tok = self.expect(TokKind::StringStart)?;
        let text = self.eat(TokKind::StringText).map(|text| StrText {
            inner: text.slice.into_owned(),
            span: text.span,
        });

        let end = self.expect(TokKind::StringEnd).map_err(|err| Error {
            message: "unclosed string literal".into(),
            span: Span { start: tok.span.start, end: err.span.start },
        })?;

        Ok(StrLit {
            text,
            span: Span { start: tok.span.start, end: end.span.end },
        })
    }

    fn parse_char(&mut self) -> Result<CharLit> {
        let tok = self.expect(TokKind::CharStart)?;
        let text = self.eat(TokKind::CharText).map(|text| CharText {
            inner: text.slice.into_owned(),
            span: text.span,
        });

        let end = self.expect(TokKind::CharEnd).map_err(|err| Error {
            message: "unclosed char literal".into(),
            span: Span { start: tok.span.start, end: err.span.start },
        })?;

        Ok(CharLit {
            text,
            span: Span { start: tok.span.start, end: end.span.end },
        })
    }

    fn parse_ident(&mut self) -> Result<Ident> {
        let tok = self.expect(TokKind::Ident)?;

        Ok(Ident { raw: tok.slice.into(), span: tok.span })
    }

    /// Parses a series of comma separated `T`, with a trailing comma allowed.
    fn parse_node_list<T, F>(
        &mut self,
        start_kind: TokKind,
        end_kind: TokKind,
        parse_elem: F,
    ) -> Result<(Token<'a>, Vec<T>, Token<'a>)>
    where
        F: Fn(&mut Self) -> Result<T>,
    {
        let start = self.expect(start_kind)?;
        let mut values = Vec::new();

        if self.peek1().kind == end_kind {
            return Ok((start, values, self.next()));
        }

        let end_token = loop {
            values.push(parse_elem(self)?);

            let next = self.next();
            if next.kind == TokKind::Comma {
                if let Some(close) = self.eat(end_kind) {
                    break close;
                }
            } else if next.kind == end_kind {
                break next;
            } else {
                return Err(Self::error_expected_str_found(
                    &format!("comma or {}", end_kind.desc()),
                    &next,
                ));
            }
        };

        Ok((start, values, end_token))
    }

    /// Returns the next token in the input, consuming the last peeked token if
    /// present.
    fn next(&mut self) -> Token<'a> {
        let p1 = self.peek1.take();
        let p2 = self.peek2.take();
        self.peek1 = p2;

        p1.unwrap_or_else(|| self.lexer.next_nontrival_token())
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

    fn eat(&mut self, kind: TokKind) -> Option<Token<'a>> {
        if self.peek1().kind == kind {
            return Some(self.next());
        }

        None
    }

    fn eat_ident<'b>(&mut self, ident: &'b str) -> Option<Token<'a>> {
        let peek = self.peek1();
        if peek.kind == TokKind::Ident && peek.slice == ident {
            return Some(self.next());
        }

        None
    }

    fn expect(&mut self, kind: TokKind) -> Result<Token<'a>> {
        let next = self.next();
        if next.kind == kind {
            return Ok(next);
        }

        Err(Self::error_expected_tok_found(kind, &next))
    }

    fn expect_ident<'b>(&mut self, ident: &'b str) -> Result<Token<'a>> {
        let next = self.next();
        if next.kind == TokKind::Ident && next.slice == ident {
            return Ok(next);
        }

        Err(Self::error_expected_str_found(ident, &next))
    }

    fn error_expected_tok_found(
        expected: TokKind,
        found: &Token<'a>,
    ) -> Error {
        Self::error_expected_str_found(expected.desc(), found)
    }

    fn error_expected_str_found(expected: &str, found: &Token<'a>) -> Error {
        let slice = if found.kind == TokKind::Invalid {
            found.slice.as_ref()
        } else {
            found.kind.desc()
        };

        Error {
            message: format!("Expected {}, but found {}", expected, slice),
            span: found.span,
        }
    }
}

#[cfg(test)]
mod tests {
    use pretty_assertions::assert_eq;

    use crate::{
        BoolLit, CharLit, CharText, Expr, F64Lit, I64Base, I64Lit, Ident,
        NilLit, Parser, Span, StrLit, StrText,
    };

    fn int(value: &str, base: I64Base, start: usize, end: usize) -> I64Lit {
        I64Lit { base, value: value.into(), span: span(start, end) }
    }

    fn span(start: usize, end: usize) -> Span {
        Span::new(start, end)
    }

    #[test]
    fn parses_int() {
        let parse_int = |inp: &str, out: I64Lit| {
            let mut parser = Parser::new(inp.as_bytes(), "");
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
            let mut parser = Parser::new(inp.as_bytes(), "");
            assert_eq!(
                parser.parse_f64(),
                Ok(F64Lit {
                    value: inp.into(),
                    span: Span { start: 0, end: inp.len() }
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
            let mut parser = Parser::new(inp.as_bytes(), "");
            assert_eq!(
                parser.parse_bool(),
                Ok(BoolLit {
                    value: inp.parse().unwrap(),
                    span: Span { start: 0, end: inp.len() }
                })
            );
        };

        parse_bool("true");
        parse_bool("false");
    }

    #[test]
    fn parses_nil() {
        let mut parser = Parser::new("nil".as_bytes(), "");
        assert_eq!(
            parser.parse_nil(),
            Ok(NilLit { span: Span { start: 0, end: 3 } })
        );
    }

    #[test]
    fn parses_string() {
        let input = r#""""#;
        let mut parser = Parser::new(input.as_bytes(), "");
        assert_eq!(
            parser.parse_string(),
            Ok(StrLit {
                text: None,
                span: Span { start: 0, end: input.len() }
            })
        );

        let input = r#""hello""#;
        let mut parser = Parser::new(input.as_bytes(), "");
        assert_eq!(
            parser.parse_string(),
            Ok(StrLit {
                text: Some(StrText {
                    inner: String::from(input.trim_matches('"')),
                    span: Span { start: 1, end: 6 }
                }),
                span: Span { start: 0, end: 7 }
            })
        );
    }

    #[test]
    fn parses_char() {
        let input = "''";
        let mut parser = Parser::new(input.as_bytes(), "");
        assert_eq!(
            parser.parse_char(),
            Ok(CharLit {
                text: None,
                span: Span { start: 0, end: input.len() }
            })
        );

        let input = "'hello'";
        let mut parser = Parser::new(input.as_bytes(), "");
        assert_eq!(
            parser.parse_char(),
            Ok(CharLit {
                text: Some(CharText {
                    inner: String::from(input.trim_matches('\'')),
                    span: Span { start: 1, end: 6 }
                }),
                span: Span { start: 0, end: 7 }
            })
        );

        let input = "'ä'";
        let mut parser = Parser::new(input.as_bytes(), "");
        assert_eq!(
            parser.parse_char(),
            Ok(CharLit {
                text: Some(CharText {
                    inner: String::from(input.trim_matches('\'')),
                    span: Span { start: 1, end: 3 }
                }),
                span: Span { start: 0, end: 4 }
            })
        );
    }

    #[test]
    fn parses_ident() {
        let ident = |ident: &str| {
            let mut parser = Parser::new(ident.as_bytes(), "");
            assert_eq!(
                parser.parse_ident(),
                Ok(Ident {
                    raw: ident.into(),
                    span: Span { start: 0, end: ident.len() }
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
        let literal = |literal: &str, out| {
            let mut parser = Parser::new(literal.as_bytes(), "");
            assert_eq!(parser.parse_literal(), out)
        };

        literal(
            "true",
            Ok(Expr::Bool(BoolLit { value: true, span: Span::new(0, 4) })),
        );

        literal(
            "false",
            Ok(Expr::Bool(BoolLit { value: false, span: Span::new(0, 5) })),
        );

        literal(
            "2.0e2",
            Ok(Expr::F64(F64Lit {
                value: "2.0e2".into(),
                span: Span::new(0, 5),
            })),
        );

        literal("nil", Ok(Expr::Nil(NilLit { span: Span::new(0, 3) })));

        literal(
            "42",
            Ok(Expr::I64(I64Lit {
                base: I64Base::Dec,
                value: "42".into(),
                span: Span::new(0, 2),
            })),
        );

        literal(
            "\"\"",
            Ok(Expr::String(StrLit { text: None, span: Span::new(0, 2) })),
        );

        literal(
            "\"hello:nworld\"",
            Ok(Expr::String(StrLit {
                text: Some(StrText {
                    inner: "hello\nworld".into(),
                    span: Span::new(1, 13),
                }),
                span: Span::new(0, 14),
            })),
        );

        literal(
            "''",
            Ok(Expr::Char(CharLit { text: None, span: Span::new(0, 2) })),
        );

        literal(
            "'hello:nworld'",
            Ok(Expr::Char(CharLit {
                text: Some(CharText {
                    inner: "hello\nworld".into(),
                    span: Span::new(1, 13),
                }),
                span: Span::new(0, 14),
            })),
        );
    }

    #[test]
    fn parses_atom() {
        macro_rules! atom {
            ($atom:expr) => {
                let mut parser = Parser::new($atom.as_bytes(), "");
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
        macro_rules! atom {
            ($atom:expr) => {
                let mut parser = Parser::new($atom.as_bytes(), "");
                insta::assert_debug_snapshot!(parser.parse_atom());
            };
        }

        atom!("[nil, [], [nil, 2,],]");
        atom!("fn() -> nil");
        atom!("rec fn(a,) -> nil");
        atom!("{ let x := 2; x }");
        atom!("if true { }");
        atom!("if true { } elif false { }");
        atom!("if true { } elif false { } else { }");
        atom!("if true { } elif false { } elif false { } else { }");
    }

    #[test]
    fn parses_nested_group() {
        macro_rules! atom {
            ($atom:expr) => {
                let mut parser = Parser::new($atom.as_bytes(), "");
                insta::assert_debug_snapshot!(parser.parse_atom());
            };
        }

        atom!("((((1))))");
    }

    #[test]
    fn parses_nested_fn_def() {
        macro_rules! atom {
            ($atom:expr) => {
                let mut parser = Parser::new($atom.as_bytes(), "");
                insta::assert_debug_snapshot!(parser.parse_atom());
            };
        }

        atom!("fn(a, b) -> fn(c) -> fn(d, e,) -> nil");
    }

    #[test]
    fn parses_statement() {
        macro_rules! stmt {
            ($stmt:expr) => {
                let mut parser = Parser::new($stmt.as_bytes(), "");
                insta::assert_debug_snapshot!(parser.parse_stmt());
            };
        }

        stmt!("let x := 2;");
        stmt!("2;");
        stmt!("fn(a, b) -> fn(c) -> fn(d, e,) -> nil;");
    }

    #[test]
    fn parse_nested_blocks() {
        macro_rules! atom {
            ($atom:expr) => {
                let mut parser = Parser::new($atom.as_bytes(), "");
                insta::assert_debug_snapshot!(parser.parse_atom());
            };
        }

        atom!("{ { { } } }");
        atom!("{ let x := 2; { let x := 2; { let x := { 2 }; } } }");
    }

    #[test]
    fn parses_nested_ifs() {
        macro_rules! atom {
            ($atom:expr) => {
                let mut parser = Parser::new($atom.as_bytes(), "");
                insta::assert_debug_snapshot!(parser.parse_atom());
            };
        }

        atom!(
            "if true { if true { } } elif false { } elif false { } else { }"
        );
    }

    #[test]
    fn parses_literal_pattern() {
        macro_rules! pat {
            ($pat:expr) => {
                let mut parser = Parser::new($pat.as_bytes(), "");
                insta::assert_debug_snapshot!(parser.parse_literal_pattern());
            };
        }

        pat!("2");
        pat!("\"hello\"");
        pat!("'h'");
        pat!("true");
        pat!("nil");
    }

    #[test]
    fn parses_array_pattern() {
        macro_rules! pat {
            ($pat:expr) => {
                let mut parser = Parser::new($pat.as_bytes(), "");
                insta::assert_debug_snapshot!(parser.parse_array_pattern());
            };
        }

        pat!("[]");
        pat!("[1, 2]");
        pat!("[[[]]]");
    }

    #[test]
    fn parses_ident_pattern() {
        macro_rules! pat {
            ($pat:expr) => {
                let mut parser = Parser::new($pat.as_bytes(), "");
                insta::assert_debug_snapshot!(parser.parse_ident_pattern());
            };
        }

        pat!("i");
        pat!("i @ 1");
        pat!("i @ ..");
    }

    #[test]
    fn parses_index() {
        macro_rules! expr {
            ($expr:expr) => {
                let mut parser = Parser::new($expr.as_bytes(), "");
                insta::assert_debug_snapshot!(parser.parse_expr());
            };
        }

        expr!("2[one]");
        expr!("2[one][two]");
        expr!("2[one][two][three]");
    }

    #[test]
    fn parses_call() {
        macro_rules! expr {
            ($expr:expr) => {
                let mut parser = Parser::new($expr.as_bytes(), "");
                insta::assert_debug_snapshot!(parser.parse_expr());
            };
        }

        expr!("2(one)");
        expr!("2(one, two)(three)");
        expr!("2(one)(two, three)(four)");
    }

    #[test]
    fn parses_interlaced_calls_and_index() {
        macro_rules! expr {
            ($expr:expr) => {
                let mut parser = Parser::new($expr.as_bytes(), "");
                insta::assert_debug_snapshot!(parser.parse_expr());
            };
        }

        expr!("2(one, two)[three](four)");
        expr!("2[one](two, three)[four]");
    }

    #[test]
    fn parses_pratt_expressions() {
        macro_rules! expr {
            ($expr:expr) => {
                let mut parser = Parser::new($expr.as_bytes(), "");
                insta::assert_debug_snapshot!(parser.parse_expr())
            };
        }

        expr!("- - - - -2");
        expr!("!!true");
        expr!("-hello[there][people]");
        expr!("2 + hello[0] * 4");
        expr!("me $> improvise .> adapt .> overcome");
    }

    #[test]
    fn parses_module() {
        macro_rules! module {
            ($module:expr) => {
                let mut parser = Parser::new($module.as_bytes(), "");
                insta::assert_debug_snapshot!(parser.parse_module())
            };
        }

        module! {
            r#"
                def id := fn(x) -> x;
                def _ := (id .> id .> id);
            "#
        }

        module! {
            r#"
              def main := fn() -> {
                let part_one := rec fn(xs) -> case xs {
                  ['(', rest @ ..] -> 1 + part_one(rest),
                  [')', rest @ ..] -> -1 + part_one(rest),
                  [] -> 0,
                };

                let part_two := fn(xs) -> {
                  let inner := rec fn(xs, n, sum) -> case sum {
                    -1 -> n,
                    _  -> case xs {
                      ['(', rest @ ..] -> inner(rest, n + 1, sum + 1),
                      [')', rest @ ..] -> inner(rest, n + 1, sum - 1),
                    },
                  };

                  inner(xs, 0, 0)
                };

                print("Part One:: ");
                input $> part_one .> println;

                print("Part Two:: ");
                input $> part_two .> println;
              };
            "#
        }

        module! {
            r#"
            from "my-path" import *;
            def _ := call(fn(x) -> x, 1) $> println;
            "#
        }
    }

    #[test]
    fn parses_import() {
        macro_rules! import {
            ($import:expr) => {
                let mut parser = Parser::new($import.as_bytes(), "");
                insta::assert_debug_snapshot!(parser.parse_import())
            };
        }

        import! {
            r#"from "my-path" as my_path;"#
        }

        import! {
            r#"from "my-path" import *;"#
        }

        import! {
            r#"from "my-path" as alias import *;"#
        }

        import! {
            r#"from "my-path" import { one, two };"#
        }

        import! {
            r#"from "my-path" import { one as i1, two as i2, three };"#
        }

        import! {
            r#"from "my-path" as alias import { one as i1, two as i2, three };"#
        }
    }

    #[test]
    fn parses_export() {
        macro_rules! import {
            ($import:expr) => {
                let mut parser = Parser::new($import.as_bytes(), "");
                insta::assert_debug_snapshot!(parser.parse_export())
            };
        }

        import! {
            r#"export {};"#
        }

        import! {
            r#"export { one };"#
        }

        import! {
            r#"export { one, two };"#
        }

        import! {
            r#"export { one as e1, two as e2 };"#
        }

        import! {
            r#"export { one as e1, two as e2, three };"#
        }
    }

    #[test]
    fn parses_return() {
        macro_rules! return_expr {
            ($ret:expr) => {
                let mut parser = Parser::new($ret.as_bytes(), "");
                insta::assert_debug_snapshot!(parser.parse_return())
            };
        }

        macro_rules! return_in_expr {
            ($ret:expr) => {
                let mut parser = Parser::new($ret.as_bytes(), "");
                insta::assert_debug_snapshot!(parser.parse_expr())
            };
        }

        return_expr! {
            r#"return"#
        }

        return_expr! {
            r#"return 2"#
        }

        return_expr! {
            r#"return return 2"#
        }

        return_in_expr! {
            r#"case nil { nil -> return, }"#
        }

        return_in_expr! {
            r#"[return,]"#
        }

        return_in_expr! {
            r#"(return)"#
        }

        return_in_expr! {
            r#"{ return }"#
        }
    }

    #[test]
    fn parses_types() {
        macro_rules! typ {
            ($ret:expr) => {
                let mut parser = Parser::new($ret.as_bytes(), "");
                insta::assert_debug_snapshot!(parser.parse_type())
            };
        }

        typ!(r#"int"#);
        typ!(r#"list[a]"#);
        typ!(r#"map[k, v]"#);
        typ!(r#"fn[k, v]() -> map[k, v]"#);

        let mut parser = Parser::new("nil".as_bytes(), "");
        assert_eq!(
            parser.parse_type(),
            Ok(crate::Type::Named(Box::new(crate::NamedType {
                name: Ident { raw: "nil".into(), span: Span::new(0, 3) },
                gens: None,
                span: Span::new(0, 3)
            })))
        );

        let mut parser = Parser::new("fn() -> nil".as_bytes(), "");
        assert_eq!(
            parser.parse_type(),
            Ok(crate::Type::Fn(Box::new(crate::FnType {
                args: vec![],
                gens: None,
                span: Span::new(0, 11),
                ret_type: Some(crate::Type::Named(Box::new(
                    crate::NamedType {
                        name: Ident {
                            raw: "nil".into(),
                            span: Span::new(8, 11)
                        },
                        gens: None,
                        span: Span::new(8, 11)
                    }
                )))
            })))
        );
    }
}
