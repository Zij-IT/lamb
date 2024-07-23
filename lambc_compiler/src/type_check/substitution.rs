use std::collections::HashSet;

use lambc_parse::{
    Binary, Block, Call, Define, Else, Expr, ExprStatement, FnDef, Group, If,
    IfCond, Index, List, Statement, Unary,
};

use super::{FnType, TyUniVar, Type, TypeInference};
use crate::type_check::TypedVar;

impl TypeInference {
    pub(super) fn substitute(
        &mut self,
        ty: Type,
    ) -> (HashSet<TyUniVar>, Type) {
        match ty {
            ty @ Type::Con(..) => (HashSet::new(), ty),
            Type::List(elem) => {
                let (unbound, elem) = self.substitute(*elem);
                (unbound, Type::List(Box::new(elem)))
            }
            Type::Var(v) => {
                let root = self.uni_table.find(v);
                match self.uni_table.probe_value(root) {
                    Some(ty) => self.substitute(ty),
                    None => {
                        let mut unbound = HashSet::new();
                        unbound.insert(root);
                        (unbound, Type::Var(root))
                    }
                }
            }
            Type::Fun(fn_ty) => {
                let mut unbound = HashSet::new();
                let args = fn_ty
                    .args
                    .into_iter()
                    .map(|ty| {
                        let (s, ty) = self.substitute(ty);
                        unbound.extend(s);
                        ty
                    })
                    .collect();

                let (ret_s, ret_ty) = self.substitute(*fn_ty.ret_type);
                unbound.extend(ret_s);

                (
                    unbound,
                    Type::Fun(FnType { args, ret_type: Box::new(ret_ty) }),
                )
            }
        }
    }

    pub(super) fn substitute_expr(
        &mut self,
        expr: Expr<TypedVar>,
    ) -> (HashSet<TyUniVar>, Expr<TypedVar>) {
        match expr {
            Expr::Nil(n) => (HashSet::new(), Expr::Nil(n)),
            Expr::I64(i) => (HashSet::new(), Expr::I64(i)),
            Expr::F64(f) => (HashSet::new(), Expr::F64(f)),
            Expr::Char(c) => (HashSet::new(), Expr::Char(c)),
            Expr::Bool(b) => (HashSet::new(), Expr::Bool(b)),
            Expr::String(s) => (HashSet::new(), Expr::String(s)),
            Expr::Ident(TypedVar(var, ty)) => {
                let (unbound, ty) = self.substitute(ty);
                (unbound, Expr::Ident(TypedVar(var, ty)))
            }
            Expr::List(list) => {
                let List { values, span } = list;
                let mut unbound = HashSet::new();
                let values = values
                    .into_iter()
                    .map(|i| {
                        let (un, expr) = self.substitute_expr(i);
                        unbound.extend(un);
                        expr
                    })
                    .collect();

                (unbound, Expr::List(List { values, span }))
            }
            Expr::Group(g) => {
                let Group { value, span } = *g;
                let (unbound, expr) = self.substitute_expr(value);
                (unbound, Expr::Group(Box::new(Group { value: expr, span })))
            }
            Expr::FnDef(fndef) => {
                let FnDef { args, body, recursive, span } = *fndef;
                let mut unbound = HashSet::new();
                let args = args
                    .into_iter()
                    .map(|TypedVar(v, ty)| {
                        let (un, ty) = self.substitute(ty);
                        unbound.extend(un);
                        TypedVar(v, ty)
                    })
                    .collect();

                let (un, body) = self.substitute_expr(body);
                unbound.extend(un);

                (
                    unbound,
                    Expr::FnDef(Box::new(FnDef {
                        args,
                        body,
                        recursive,
                        span,
                    })),
                )
            }
            Expr::Block(block) => {
                let (unbound, block) = self.substitute_block_raw(*block);
                (unbound, Expr::Block(Box::new(block)))
            }
            Expr::If(iff) => {
                let If { cond, elif, els_, span } = *iff;
                let (mut unbound, cond) = self.substitute_ifcond(cond);
                let elif = elif
                    .into_iter()
                    .map(|IfCond { cond, body, span }| {
                        let (un, cond) = self.substitute_expr(cond);
                        unbound.extend(un);
                        let (un, block) = self.substitute_block_raw(body);
                        unbound.extend(un);
                        IfCond { cond, body: block, span }
                    })
                    .collect();

                let els_ = els_.map(|Else { body, span }| {
                    let (un, body) = self.substitute_block_raw(body);
                    unbound.extend(un);
                    Else { body, span }
                });

                (unbound, Expr::If(Box::new(If { cond, elif, els_, span })))
            }
            Expr::Index(idx) => {
                let Index { lhs, rhs, span } = *idx;
                let (mut unbound, idxee) = self.substitute_expr(lhs);
                let (un, index) = self.substitute_expr(rhs);
                unbound.extend(un);
                (
                    unbound,
                    Expr::Index(Box::new(Index {
                        lhs: idxee,
                        rhs: index,
                        span,
                    })),
                )
            }
            Expr::Call(call) => {
                let Call { callee, args, span } = *call;
                let (mut unbound, func) = self.substitute_expr(callee);
                let args = args
                    .into_iter()
                    .map(|a| {
                        let (un, a) = self.substitute_expr(a);
                        unbound.extend(un);
                        a
                    })
                    .collect();

                (
                    unbound,
                    Expr::Call(Box::new(Call { callee: func, args, span })),
                )
            }
            Expr::Unary(unary) => {
                let Unary { rhs, op, span, op_span } = *unary;
                let (unbound, rhs) = self.substitute_expr(rhs);
                (
                    unbound,
                    Expr::Unary(Box::new(Unary { rhs, op, span, op_span })),
                )
            }
            Expr::Binary(binary) => {
                let Binary { lhs, op, rhs, span, op_span } = *binary;
                let (mut unbound, lhs) = self.substitute_expr(lhs);
                let (un, rhs) = self.substitute_expr(rhs);
                unbound.extend(un);

                (
                    unbound,
                    Expr::Binary(Box::new(Binary {
                        lhs,
                        rhs,
                        op,
                        span,
                        op_span,
                    })),
                )
            }
            Expr::Case(_) => todo!(),
            Expr::Path(_) => todo!(),
            Expr::Return(_) => todo!(),
        }
    }

    fn substitute_ifcond(
        &mut self,
        ifcond: IfCond<TypedVar>,
    ) -> (HashSet<TyUniVar>, IfCond<TypedVar>) {
        let IfCond { cond, body, span } = ifcond;
        let (mut unbound, cond) = self.substitute_expr(cond);
        let (un, block) = self.substitute_block_raw(body);
        unbound.extend(un);

        (unbound, IfCond { cond, body: block, span })
    }

    fn substitute_block_raw(
        &mut self,
        block: Block<TypedVar>,
    ) -> (HashSet<TyUniVar>, Block<TypedVar>) {
        let Block { statements, value, span } = block;
        let mut unbound = HashSet::new();

        let statements = statements
            .into_iter()
            .map(|s| {
                let (un, s) = self.substitute_stmt(s);
                unbound.extend(un);
                s
            })
            .collect();

        (unbound, Block { statements, value, span })
    }

    fn substitute_stmt(
        &mut self,
        stmt: Statement<TypedVar>,
    ) -> (HashSet<TyUniVar>, Statement<TypedVar>) {
        match stmt {
            Statement::Define(def) => {
                let Define { ident, typ, value, span } = def;
                let (mut unbound, ty) = self.substitute(ident.1);

                if typ.is_some() {
                    todo!(
                        "Now is the time to handle 'typ' during substitution"
                    )
                }

                let (un, ex) = self.substitute_expr(value);
                unbound.extend(un);

                (
                    unbound,
                    Statement::Define(Define {
                        ident: TypedVar(ident.0, ty),
                        typ,
                        value: ex,
                        span,
                    }),
                )
            }
            Statement::Expr(ex) => {
                let ExprStatement { expr, span } = ex;
                let (unbound, expr) = self.substitute_expr(expr);
                (unbound, Statement::Expr(ExprStatement { expr, span }))
            }
        }
    }
}
