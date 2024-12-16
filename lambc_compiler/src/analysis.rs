mod limit_vec;
mod tree;

use std::num::IntErrorKind;

use lambc_parse::Span;
pub use limit_vec::LimitVec;
use ordered_float::OrderedFloat;

use super::type_check::tree as typed_tree;

#[derive(miette::Diagnostic, thiserror::Error, Debug)]
pub enum Error {
    #[error(
        "Only function definitions or constants are allowed at the top level."
    )]
    ExprNotAllowedTopLevel,
    #[error("Empty char literal")]
    EmptyCharLit,
    #[error("Invalid char literal")]
    InvalidCharLit,
    #[error("Invalid int literal")]
    EmptyIntLit,
    #[error("This integer will overflow")]
    OverflowingIntLit,
    #[error("This integer will underflow")]
    UnderflowingIntLit,
    #[error("Empty float literal")]
    EmptyFloatLiteral,
    #[error("This passes too many arguments. The limit is 255.")]
    TooManyArgs,
    #[error("This function requires too many arguments. The limit is 255.")]
    TooManyParams,
}

use crate::State;

pub struct Analyzer<'a> {
    pub state: &'a mut State,
}

impl<'a> Analyzer<'a> {
    pub fn new(state: &'a mut State) -> Self {
        Self { state }
    }

    pub fn modules(
        &mut self,
        modules: Vec<typed_tree::Module>,
    ) -> Vec<tree::Module> {
        modules.into_iter().map(|m| self.module(m)).collect()
    }

    fn module(&mut self, module: typed_tree::Module) -> tree::Module {
        // While this could be done with a simple map, when there are more items that will become very messy very fast.
        // So I am going to have this be a for loop now.
        let mut defs = vec![];
        for item in module.items {
            match item {
                typed_tree::Item::Def(def) => {
                    defs.push(self.top_level_def(def))
                }
            }
        }

        tree::Module {
            exports: self.exports(module.exports),
            imports: self.imports(module.imports),
            defs,
            path: module.path,
            span: module.span,
        }
    }

    fn exports(
        &mut self,
        exports: Vec<typed_tree::Export>,
    ) -> Vec<tree::Export> {
        exports
            .into_iter()
            .map(|e| tree::Export {
                items: self.export_items(e.items),
                span: e.span,
            })
            .collect()
    }

    fn export_items(
        &mut self,
        items: Vec<typed_tree::ExportItem>,
    ) -> Vec<tree::ExportItem> {
        items
            .into_iter()
            .map(|ei| tree::ExportItem {
                item: ei.item,
                alias: ei.alias,
                span: ei.span,
            })
            .collect()
    }

    fn imports(
        &mut self,
        imports: Vec<typed_tree::Import>,
    ) -> Vec<tree::Import> {
        imports
            .into_iter()
            .map(|e| tree::Import {
                file: e.file,
                star: e.star,
                name: e.name,
                items: self.import_items(e.items),
                span: e.span,
                path_span: e.path_span,
            })
            .collect()
    }

    fn import_items(
        &mut self,
        items: Vec<typed_tree::ImportItem>,
    ) -> Vec<tree::ImportItem> {
        items
            .into_iter()
            .map(|ei| tree::ImportItem {
                item: ei.item,
                alias: ei.alias,
                span: ei.span,
            })
            .collect()
    }

    fn top_level_def(
        &mut self,
        def: typed_tree::TopDefine,
    ) -> tree::TopDefine {
        if !Self::is_expr_allowed_at_top_level(&def.value) {
            // issue an error for this expression because it isn't a valid top-level expression
            self.state.add_error(Error::ExprNotAllowedTopLevel, None);
        }

        tree::TopDefine {
            ident: def.ident,
            typ: def.typ,
            value: self.expr(def.value),
            span: def.span,
        }
    }

    fn expr(&mut self, value: typed_tree::Expr) -> tree::Expr {
        match value {
            typed_tree::Expr::Ident(x) => self.ident(x),
            typed_tree::Expr::Usv(x) => self.usv(x),
            typed_tree::Expr::String(x) => self.string(x),
            typed_tree::Expr::Bool(x) => self.bool(x),
            typed_tree::Expr::Nil(x) => self.nil(x),
            typed_tree::Expr::Int(x) => self.int(x),
            typed_tree::Expr::Double(x) => self.double(x),
            typed_tree::Expr::List(x) => self.list(*x),
            typed_tree::Expr::Group(x) => self.group(*x),
            typed_tree::Expr::FnDef(x) => self.fndef(*x),
            typed_tree::Expr::Block(x) => self.block(*x),
            typed_tree::Expr::If(x) => self.iff(*x),
            typed_tree::Expr::Case(x) => self.case(*x),
            typed_tree::Expr::Index(x) => self.index(*x),
            typed_tree::Expr::Call(x) => self.call(*x),
            typed_tree::Expr::Unary(x) => self.unary(*x),
            typed_tree::Expr::Binary(x) => self.binary(*x),
            typed_tree::Expr::Return(x) => self.ret(*x),
        }
    }

    fn ident(&mut self, x: crate::type_check::TypedVar) -> tree::Expr {
        tree::Expr::Ident(x)
    }

    fn usv(&mut self, x: typed_tree::UsvLit) -> tree::Expr {
        let typed_tree::UsvLit { text, span } = x;
        let usv = match text.map(|t| t.inner.parse()) {
            Some(Ok(c)) => c,
            Some(Err(_)) => {
                self.state.add_error(Error::InvalidCharLit, None);
                'f'
            }
            None => {
                self.state.add_error(Error::EmptyCharLit, None);
                'f'
            }
        };

        tree::Expr::Usv(tree::UsvLit { text: usv, span })
    }

    fn string(&mut self, x: typed_tree::StrLit) -> tree::Expr {
        let typed_tree::StrLit { text, span } = x;

        let mut pos = span.start();
        let chars = text
            .map(|text| {
                text.inner
                    .chars()
                    .map(|usv| {
                        let start = pos;
                        pos += usv.len_utf8();
                        tree::Expr::Usv(tree::UsvLit {
                            text: usv,
                            span: Span::new(start, pos),
                        })
                    })
                    .collect()
            })
            .unwrap_or_default();

        tree::Expr::List(Box::new(tree::List { values: chars, span }))
    }

    fn bool(&mut self, x: typed_tree::BoolLit) -> tree::Expr {
        let typed_tree::BoolLit { value, span } = x;
        tree::Expr::Bool(tree::BoolLit { value, span })
    }

    fn nil(&mut self, x: typed_tree::NilLit) -> tree::Expr {
        let typed_tree::NilLit { span } = x;
        tree::Expr::Nil(tree::NilLit { span })
    }

    fn int(&mut self, x: typed_tree::IntLit) -> tree::Expr {
        let typed_tree::IntLit { base, mut value, span } = x;
        let base = match base {
            typed_tree::IntBase::Bin => 2,
            typed_tree::IntBase::Oct => 8,
            typed_tree::IntBase::Dec => 10,
            typed_tree::IntBase::Hex => 16,
        };

        let value = {
            value.retain(|c| c != '_');
            value
        };

        let value = match i64::from_str_radix(&value, base) {
            Ok(int) => int,
            Err(pie) => {
                let error = match pie.kind() {
                    IntErrorKind::Empty => Error::EmptyIntLit,
                    IntErrorKind::PosOverflow => Error::OverflowingIntLit,
                    IntErrorKind::NegOverflow => Error::UnderflowingIntLit,
                    err => unreachable!(
                        "Lamb ICE: non-matched into error kind: {err:?}"
                    ),
                };

                self.state.add_error(error, None);
                0
            }
        };

        tree::Expr::Int(tree::IntLit { value, span })
    }

    fn double(&mut self, x: typed_tree::DoubleLit) -> tree::Expr {
        let typed_tree::DoubleLit { mut value, span } = x;
        let value = {
            value.retain(|c| c != '_');
            value
        };

        let value = if value.is_empty() {
            self.state.add_error(Error::EmptyFloatLiteral, None);
            OrderedFloat(0.0)
        } else {
            match value.parse::<OrderedFloat<f64>>() {
                Ok(f) => f,
                Err(_) => {
                    unreachable!("Lamb ICE: invalid float literal '{value:?}'")
                }
            }
        };

        tree::Expr::Double(tree::DoubleLit { value, span })
    }

    fn list(&mut self, x: typed_tree::List) -> tree::Expr {
        let typed_tree::List { values, span } = x;
        tree::Expr::List(Box::new(tree::List {
            values: values.into_iter().map(|v| self.expr(v)).collect(),
            span,
        }))
    }

    fn group(&mut self, x: typed_tree::Group) -> tree::Expr {
        let typed_tree::Group { value, span: _ } = x;
        self.expr(value)
    }

    fn fndef(&mut self, x: typed_tree::FnDef) -> tree::Expr {
        let typed_tree::FnDef { args, body, recursive, span } = x;
        let (args, rest) = LimitVec::new_from_vec_truncating(args);
        if !rest.is_empty() {
            self.state.add_error(Error::TooManyParams, None);
        }

        tree::Expr::FnDef(Box::new(tree::FnDef {
            args,
            body: self.expr(body),
            recursive,
            span,
        }))
    }

    fn block(&mut self, x: typed_tree::Block) -> tree::Expr {
        tree::Expr::Block(Box::new(self.block_raw(x)))
    }

    fn block_raw(&mut self, x: typed_tree::Block) -> tree::Block {
        let typed_tree::Block { statements, value, span } = x;
        tree::Block {
            statements: statements.into_iter().map(|s| self.stmt(s)).collect(),
            value: value.map(|e| self.expr(e)),
            span,
        }
    }

    fn iff(&mut self, x: typed_tree::If) -> tree::Expr {
        let typed_tree::If { cond, elif, els_, span } = x;
        tree::Expr::If(Box::new(tree::If {
            cond: self.ifcond(cond),
            elif: elif.into_iter().map(|i| self.ifcond(i)).collect(),
            els_: els_.map(|e| self.els(e)),
            span,
        }))
    }

    fn ifcond(&mut self, cond: typed_tree::IfCond) -> tree::IfCond {
        let typed_tree::IfCond { cond, body, span } = cond;
        tree::IfCond {
            cond: self.expr(cond),
            body: self.block_raw(body),
            span,
        }
    }

    fn els(&mut self, e: typed_tree::Else) -> tree::Else {
        let typed_tree::Else { body, span } = e;
        tree::Else { body: self.block_raw(body), span }
    }

    fn case(&mut self, x: typed_tree::Case) -> tree::Expr {
        let typed_tree::Case { scrutinee, arms, span } = x;
        tree::Expr::Case(Box::new(tree::Case {
            scrutinee: self.expr(scrutinee),
            arms: arms.into_iter().map(|a| self.case_arm(a)).collect(),
            span,
        }))
    }

    fn case_arm(&self, a: typed_tree::CaseArm) -> tree::CaseArm {
        todo!()
    }

    fn index(&mut self, x: typed_tree::Index) -> tree::Expr {
        let typed_tree::Index { lhs, rhs, span } = x;
        tree::Expr::Index(Box::new(tree::Index {
            lhs: self.expr(lhs),
            rhs: self.expr(rhs),
            span,
        }))
    }

    fn call(&mut self, x: typed_tree::Call) -> tree::Expr {
        let typed_tree::Call { callee, args, span } = x;
        let args = args.into_iter().map(|a| self.expr(a)).collect();
        let (args, rest) = LimitVec::new_from_vec_truncating(args);
        if !rest.is_empty() {
            self.state.add_error(Error::TooManyArgs, None);
        }

        tree::Expr::Call(Box::new(tree::Call {
            callee: self.expr(callee),
            args,
            span,
        }))
    }

    fn unary(&mut self, x: typed_tree::Unary) -> tree::Expr {
        let typed_tree::Unary { rhs, op, span, op_span } = x;

        let op = match op {
            typed_tree::UnaryOp::Nneg => tree::UnaryOp::Nneg,
            typed_tree::UnaryOp::Lnot => tree::UnaryOp::Lnot,
            typed_tree::UnaryOp::Bneg => tree::UnaryOp::Bneg,
        };

        tree::Expr::Unary(Box::new(tree::Unary {
            rhs: self.expr(rhs),
            op,
            span,
            op_span,
        }))
    }

    fn binary(&mut self, x: typed_tree::Binary) -> tree::Expr {
        use tree::BinaryOp as Bop;
        use typed_tree::BinaryOp as TyBop;

        let typed_tree::Binary { lhs, op, rhs, span, op_span } = x;
        let lhs = self.expr(lhs);
        let rhs = self.expr(rhs);
        let op = match op {
            TyBop::Appl => Bop::Appl,
            TyBop::Appr => Bop::Appr,
            TyBop::Cpsl => Bop::Cpsl,
            TyBop::Cpsr => Bop::Cpsr,
            TyBop::Land => Bop::Land,
            TyBop::Lor => Bop::Lor,
            TyBop::Eq => Bop::Eq,
            TyBop::Ne => Bop::Ne,
            TyBop::Ge => Bop::Ge,
            TyBop::Gt => Bop::Gt,
            TyBop::Le => Bop::Le,
            TyBop::Lt => Bop::Lt,
            TyBop::Bor => Bop::Bor,
            TyBop::Bxor => Bop::Bxor,
            TyBop::Band => Bop::Band,
            TyBop::Shr => Bop::Shr,
            TyBop::Shl => Bop::Shl,
            TyBop::Add => Bop::Add,
            TyBop::Sub => Bop::Sub,
            TyBop::Div => Bop::Div,
            TyBop::Mod => Bop::Mod,
            TyBop::Mul => Bop::Mul,
        };

        tree::Expr::Binary(Box::new(tree::Binary {
            lhs,
            op,
            rhs,
            span,
            op_span,
        }))
    }

    fn ret(&mut self, x: typed_tree::Return) -> tree::Expr {
        let typed_tree::Return { value, span } = x;
        tree::Expr::Return(Box::new(tree::Return {
            value: value.map(|e| self.expr(e)),
            span,
        }))
    }

    // An expression is allowed at the top-level if it is either a literal, or a function definition.
    fn is_expr_allowed_at_top_level(expr: &typed_tree::Expr) -> bool {
        match expr {
            typed_tree::Expr::Ident(_)
            | typed_tree::Expr::Usv(_)
            | typed_tree::Expr::Nil(_)
            | typed_tree::Expr::Int(_)
            | typed_tree::Expr::Bool(_)
            | typed_tree::Expr::String(_)
            | typed_tree::Expr::Double(_)
            | typed_tree::Expr::FnDef(_) => true,
            typed_tree::Expr::If(_)
            | typed_tree::Expr::Case(_)
            | typed_tree::Expr::Call(_)
            | typed_tree::Expr::Unary(_)
            | typed_tree::Expr::Block(_)
            | typed_tree::Expr::Index(_)
            | typed_tree::Expr::Group(_)
            | typed_tree::Expr::Binary(_)
            | typed_tree::Expr::Return(_) => false,
            typed_tree::Expr::List(elems) => elems
                .values
                .iter()
                .all(|e| Self::is_expr_allowed_at_top_level(e)),
        }
    }

    fn stmt(&mut self, stmt: typed_tree::Stmt) -> tree::Stmt {
        match stmt {
            typed_tree::Stmt::Def(typed_tree::LocalDefine {
                ident,
                typ,
                value,
                span,
            }) => tree::Stmt::Def(tree::LocalDefine {
                ident,
                typ,
                value: self.expr(value),
                span,
            }),
            typed_tree::Stmt::Expr(typed_tree::ExprStmt { expr, span }) => {
                tree::Stmt::Expr(tree::ExprStmt {
                    expr: self.expr(expr),
                    span,
                })
            }
        }
    }
}
