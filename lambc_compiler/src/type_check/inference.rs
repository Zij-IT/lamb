use lambc_parse::{
    Binary, BinaryOp, Block, Call, Define, Else, Expr, ExprStatement, FnDef,
    Group, If, IfCond, Index, List, Return, Statement, Unary, UnaryOp,
};

use super::{
    env::Env, Constraint, FnType, Qualified, TyClass, Type, TypeInference,
    TypedVar, UnifiableVar,
};
use crate::name_res::Var;

impl TypeInference {
    pub(super) fn infer_expr(
        &mut self,
        env: Env,
        expr: Expr<Var>,
    ) -> (Qualified<Expr<TypedVar>>, Type) {
        match expr {
            // The easy cases!
            Expr::Nil(n) => {
                (Qualified::unconstrained(Expr::Nil(n)), Type::NIL)
            }
            Expr::I64(i) => {
                (Qualified::unconstrained(Expr::I64(i)), Type::INT)
            }
            Expr::F64(f) => {
                (Qualified::unconstrained(Expr::F64(f)), Type::DOUBLE)
            }
            Expr::Char(c) => {
                (Qualified::unconstrained(Expr::Char(c)), Type::USV)
            }
            Expr::Bool(b) => {
                (Qualified::unconstrained(Expr::Bool(b)), Type::BOOL)
            }
            Expr::Ident(i) => {
                let ty = env.type_of(i);
                let ty: Qualified<Type> = self.instantiate(ty);
                (
                    Qualified::constrained(
                        Expr::Ident(TypedVar(i, ty.item.clone())),
                        ty.cons,
                    ),
                    ty.item,
                )
            }
            Expr::String(s) => (
                Qualified::unconstrained(Expr::String(s)),
                Type::List(Box::new(Type::USV)),
            ),
            // The harder cases!
            Expr::Case(_) => todo!(),
            Expr::Path(_) => todo!(),
            Expr::If(iff) => self.infer_if(env, *iff),
            Expr::Group(g) => self.infer_group(env, *g),
            Expr::Return(ret) => self.infer_return(env, *ret),
            Expr::Index(idx) => self.infer_idx(env, *idx),
            Expr::List(list) => self.infer_list(env, list),
            Expr::Call(call) => self.infer_call(env, *call),
            Expr::Block(block) => self.infer_block(env, *block),
            Expr::Unary(unary) => self.infer_unary(env, *unary),
            Expr::FnDef(fndef) => self.infer_fndef(env, *fndef),
            Expr::Binary(binary) => self.infer_binary(env, *binary),
        }
    }

    fn infer_fndef(
        &mut self,
        mut env: Env,
        fndef: FnDef<Var>,
    ) -> (Qualified<Expr<TypedVar>>, Type) {
        let FnDef { args, body, recursive, span } = fndef;
        let mut new_args = Vec::with_capacity(args.len());
        let mut typs = Vec::with_capacity(args.len());

        for arg in &args {
            let typ = self.fresh_ty_var();
            typs.push(Type::UnifiableVar(typ));
            new_args.push(TypedVar(*arg, Type::UnifiableVar(typ)));
            env.add_type(
                *arg,
                Qualified::unconstrained(Type::UnifiableVar(typ)),
            );
        }

        // Return types need to have access to the return type, so that the value
        // can be checked against this type.
        let ret_type = Type::UnifiableVar(self.fresh_ty_var());
        self.ret_type.push(ret_type.clone());

        let body_out = self.check_expr(env, body, ret_type);

        (
            Qualified {
                cons: body_out.cons,
                item: Expr::FnDef(Box::new(FnDef {
                    args: new_args,
                    body: body_out.item,
                    recursive,
                    span,
                })),
            },
            Type::Fun(FnType {
                args: typs,
                ret_type: Box::new(
                    self.ret_type
                        .pop()
                        .expect("Function type not-already popped"),
                ),
            }),
        )
    }

    fn infer_call(
        &mut self,
        env: Env,
        call: Call<Var>,
    ) -> (Qualified<Expr<TypedVar>>, Type) {
        let mut cons = Vec::new();
        let mut asts = Vec::new();
        let mut typs = Vec::new();

        for arg in call.args {
            let (out, typ) = self.infer_expr(env.clone(), arg);
            cons.extend(out.cons);
            asts.push(out.item);
            typs.push(typ);
        }

        let ret_typ = Type::UnifiableVar(self.fresh_ty_var());
        let fn_typ = Type::Fun(FnType {
            args: typs,
            ret_type: Box::new(ret_typ.clone()),
        });

        let fn_out = self.check_expr(env, call.callee, fn_typ);
        cons.extend(fn_out.cons);

        (
            Qualified::constrained(
                Expr::Call(Box::new(Call {
                    callee: fn_out.item,
                    args: asts,
                    span: call.span,
                })),
                cons,
            ),
            ret_typ,
        )
    }

    fn infer_idx(
        &mut self,
        env: Env,
        idx: Index<Var>,
    ) -> (Qualified<Expr<TypedVar>>, Type) {
        let list_elem_ty = self.fresh_ty_var();
        let list_ty = Type::List(Box::new(Type::UnifiableVar(list_elem_ty)));

        let (mut idxee_out, indexee_ty) =
            self.infer_expr(env.clone(), idx.lhs);

        let (index_out, index_ty) = self.infer_expr(env, idx.rhs);

        idxee_out.cons.extend(index_out.cons);
        idxee_out.cons.push(Constraint::TypeEqual {
            expected: index_ty,
            got: Type::INT,
        });

        idxee_out.cons.push(Constraint::TypeEqual {
            expected: indexee_ty,
            got: list_ty,
        });

        (
            Qualified::constrained(
                Expr::Index(Box::new(Index {
                    lhs: idxee_out.item,
                    rhs: index_out.item,
                    span: idx.span,
                })),
                idxee_out.cons,
            ),
            Type::UnifiableVar(list_elem_ty),
        )
    }

    fn infer_group(
        &mut self,
        env: Env,
        group: Group<Var>,
    ) -> (Qualified<Expr<TypedVar>>, Type) {
        let (res, ty) = self.infer_expr(env, group.value);
        (
            Qualified::constrained(
                Expr::Group(Box::new(Group {
                    value: res.item,
                    span: group.span,
                })),
                res.cons,
            ),
            ty,
        )
    }

    fn infer_list(
        &mut self,
        env: Env,
        list: List<Var>,
    ) -> (Qualified<Expr<TypedVar>>, Type) {
        let mut values = list.values.into_iter();
        let (mut cons, mut asts, first_ty) = match values.next() {
            Some(e) => {
                let (out, res) = self.infer_expr(env.clone(), e);
                (out.cons, vec![out.item], res)
            }
            None => (vec![], vec![], Type::UnifiableVar(self.fresh_ty_var())),
        };

        for val in values {
            let out = self.check_expr(env.clone(), val, first_ty.clone());
            cons.extend(out.cons);
            asts.push(out.item);
        }

        (
            Qualified::constrained(
                Expr::List(List { values: asts, span: list.span }),
                cons,
            ),
            Type::List(Box::new(first_ty)),
        )
    }

    fn infer_unary(
        &mut self,
        env: Env,
        unary: Unary<Var>,
    ) -> (Qualified<Expr<TypedVar>>, Type) {
        let Unary { rhs, op, span, op_span } = unary;
        let (out, ty) = self.infer_expr(env, rhs);

        let mut cons = out.cons;
        cons.push(match op {
            UnaryOp::Nneg => Constraint::IsIn(TyClass::Num, ty.clone()),
            UnaryOp::Lnot => {
                Constraint::TypeEqual { expected: Type::BOOL, got: ty.clone() }
            }
            UnaryOp::Bneg => {
                Constraint::TypeEqual { expected: Type::INT, got: ty.clone() }
            }
        });

        (
            Qualified::constrained(
                Expr::Unary(Box::new(Unary {
                    rhs: out.item,
                    op,
                    span,
                    op_span,
                })),
                cons,
            ),
            // Each UnaryOp returns the same type as the original type. Each
            // one is roughly `a -> a`
            ty,
        )
    }

    fn infer_binary(
        &mut self,
        env: Env,
        binary: Binary<Var>,
    ) -> (Qualified<Expr<TypedVar>>, Type) {
        let (lhs, rhs, cons, ty) = match binary.op {
            // These operators require that both the `[l|r]hs` is a function type
            // with a singular argument, and `[r|l]hs` is the type of the parameter
            BinaryOp::Appl => {
                let arg = Type::UnifiableVar(self.fresh_ty_var());
                let ret = Type::UnifiableVar(self.fresh_ty_var());
                let fun = Type::Fun(FnType {
                    args: vec![arg.clone()],
                    ret_type: Box::new(ret.clone()),
                });

                let lhs = self.check_expr(env.clone(), binary.lhs, fun);
                let rhs = self.check_expr(env, binary.rhs, arg);
                let mut cons = lhs.cons;
                cons.extend(rhs.cons);

                (lhs.item, rhs.item, cons, ret)
            }
            BinaryOp::Appr => {
                let arg = Type::UnifiableVar(self.fresh_ty_var());
                let ret = Type::UnifiableVar(self.fresh_ty_var());
                let fun = Type::Fun(FnType {
                    args: vec![arg.clone()],
                    ret_type: Box::new(ret.clone()),
                });

                let lhs = self.check_expr(env.clone(), binary.lhs, arg);
                let rhs = self.check_expr(env, binary.rhs, fun);
                let mut cons = lhs.cons;
                cons.extend(rhs.cons);

                (lhs.item, rhs.item, cons, ret)
            }
            // These operators require that both the `lhs` and `rhs` are both
            // unary functions where the output of the `[l|r]hs` is the only
            // parameter of the `[r|l]hs`.
            BinaryOp::Cpsl => {
                let rhs_arg = Type::UnifiableVar(self.fresh_ty_var());
                let rhs_ret = Type::UnifiableVar(self.fresh_ty_var());
                let lhs_ret = Type::UnifiableVar(self.fresh_ty_var());

                let rhs_fn = Type::Fun(FnType {
                    args: vec![rhs_arg],
                    ret_type: Box::new(rhs_ret.clone()),
                });

                let lhs_fn = Type::Fun(FnType {
                    args: vec![rhs_ret],
                    ret_type: Box::new(lhs_ret.clone()),
                });

                let lhs = self.check_expr(env.clone(), binary.lhs, lhs_fn);
                let rhs = self.check_expr(env, binary.rhs, rhs_fn);
                let mut cons = lhs.cons;
                cons.extend(rhs.cons);

                (lhs.item, rhs.item, cons, lhs_ret)
            }
            BinaryOp::Cpsr => {
                let lhs_arg = Type::UnifiableVar(self.fresh_ty_var());
                let lhs_ret = Type::UnifiableVar(self.fresh_ty_var());
                let rhs_ret = Type::UnifiableVar(self.fresh_ty_var());

                let lhs_fn = Type::Fun(FnType {
                    args: vec![lhs_arg],
                    ret_type: Box::new(lhs_ret.clone()),
                });

                let rhs_fn = Type::Fun(FnType {
                    args: vec![lhs_ret],
                    ret_type: Box::new(rhs_ret.clone()),
                });

                let lhs = self.check_expr(env.clone(), binary.lhs, lhs_fn);
                let rhs = self.check_expr(env, binary.rhs, rhs_fn);
                let mut cons = lhs.cons;
                cons.extend(rhs.cons);

                (lhs.item, rhs.item, cons, rhs_ret)
            }
            // These operators always return boolean values, regardless of the
            // type of the `lhs` and `rhs` value
            BinaryOp::Land
            | BinaryOp::Lor
            | BinaryOp::Eq
            | BinaryOp::Ne
            | BinaryOp::Ge
            | BinaryOp::Gt
            | BinaryOp::Le
            | BinaryOp::Lt => {
                let (lhs, lhs_ty) = self.infer_expr(env.clone(), binary.lhs);
                let (rhs, rhs_ty) = self.infer_expr(env, binary.rhs);
                let mut cons = lhs.cons;
                cons.extend(rhs.cons);
                cons.push(Constraint::TypeEqual {
                    expected: lhs_ty,
                    got: rhs_ty,
                });

                (lhs.item, rhs.item, cons, Type::BOOL)
            }
            // These operators require that their values are of the `int` type
            // and they output a value of type `int`.
            BinaryOp::Bor
            | BinaryOp::Bxor
            | BinaryOp::Band
            | BinaryOp::Shr
            | BinaryOp::Shl
            | BinaryOp::Mod => {
                let lhs = self.check_expr(env.clone(), binary.lhs, Type::INT);
                let rhs = self.check_expr(env, binary.rhs, Type::INT);
                let mut cons = lhs.cons;
                cons.extend(rhs.cons);

                (lhs.item, rhs.item, cons, Type::INT)
            }
            // These operators require that their values are of numerical persuasion
            //
            // For type inference of the following binary operators, we assume
            // that the result of the operation is the same type as the expression
            // on the left hand side of the operator.
            //
            // THOUGHT: Perhaps add a `Type::Error` and use a new var with the
            //          constraint output of `Constriant::OutputOfAdd(Type, Type)`
            BinaryOp::Add => {
                let (lhs_out, lhs_ty) =
                    self.infer_expr(env.clone(), binary.lhs);

                let (rhs_out, rhs_ty) = self.infer_expr(env, binary.rhs);

                let mut cons = lhs_out.cons;
                cons.extend(rhs_out.cons);
                cons.push(Constraint::TypeEqual {
                    expected: lhs_ty.clone(),
                    got: rhs_ty,
                });

                cons.push(Constraint::IsIn(TyClass::Addable, lhs_ty.clone()));

                (lhs_out.item, rhs_out.item, cons, lhs_ty)
            }
            BinaryOp::Sub => {
                let (lhs_out, lhs_ty) =
                    self.infer_expr(env.clone(), binary.lhs);

                let (rhs_out, rhs_ty) = self.infer_expr(env, binary.rhs);

                let mut cons = lhs_out.cons;
                cons.extend(rhs_out.cons);
                cons.push(Constraint::TypeEqual {
                    expected: lhs_ty.clone(),
                    got: rhs_ty,
                });

                cons.push(Constraint::IsIn(TyClass::Num, lhs_ty.clone()));
                (lhs_out.item, rhs_out.item, cons, lhs_ty)
            }
            BinaryOp::Div => {
                let (lhs_out, lhs_ty) =
                    self.infer_expr(env.clone(), binary.lhs);

                let (rhs_out, rhs_ty) = self.infer_expr(env, binary.rhs);

                let mut cons = lhs_out.cons;
                cons.extend(rhs_out.cons);
                cons.push(Constraint::TypeEqual {
                    expected: lhs_ty.clone(),
                    got: rhs_ty,
                });

                cons.push(Constraint::IsIn(TyClass::Num, lhs_ty.clone()));
                (lhs_out.item, rhs_out.item, cons, lhs_ty)
            }
            BinaryOp::Mul => {
                let (lhs_out, lhs_ty) =
                    self.infer_expr(env.clone(), binary.lhs);

                let (rhs_out, rhs_ty) = self.infer_expr(env, binary.rhs);

                let mut cons = lhs_out.cons;
                cons.extend(rhs_out.cons);
                cons.push(Constraint::TypeEqual {
                    expected: lhs_ty.clone(),
                    got: rhs_ty,
                });

                cons.push(Constraint::IsIn(TyClass::Num, lhs_ty.clone()));
                (lhs_out.item, rhs_out.item, cons, lhs_ty)
            }
        };

        let bin = Binary {
            lhs,
            rhs,
            op: binary.op,
            span: binary.span,
            op_span: binary.op_span,
        };

        (Qualified::constrained(Expr::Binary(Box::new(bin)), cons), ty)
    }

    fn infer_block(
        &mut self,
        env: Env,
        block: Block<Var>,
    ) -> (Qualified<Expr<TypedVar>>, Type) {
        let (res, ty) = self.infer_block_raw(env, block);
        (
            Qualified::constrained(
                Expr::Block(Box::new(Block {
                    statements: res.item.statements,
                    value: res.item.value,
                    span: res.item.span,
                })),
                res.cons,
            ),
            ty,
        )
    }

    pub(super) fn infer_block_raw(
        &mut self,
        mut env: Env,
        block: Block<Var>,
    ) -> (Qualified<Block<TypedVar>>, Type) {
        let Block { statements, value, span } = block;

        let mut stmts = Vec::with_capacity(statements.len());
        let mut cons = vec![];
        for stmt in statements {
            let out = self.process_stmt(&mut env, stmt);
            stmts.push(out.item);
            cons.extend(out.cons);
        }

        let (val, ty) =
            if let Some((out, ty)) = value.map(|v| self.infer_expr(env, v)) {
                cons.extend(out.cons);
                (Some(out.item), ty)
            } else {
                (None, Type::NIL)
            };

        (
            Qualified::constrained(
                Block { statements: stmts, value: val, span },
                cons,
            ),
            ty,
        )
    }

    pub(super) fn infer_if(
        &mut self,
        env: Env,
        iff: If<Var>,
    ) -> (Qualified<Expr<TypedVar>>, Type) {
        let If { cond, elif, els_, span } = iff;
        let cond_out = self.check_expr(env.clone(), cond.cond, Type::BOOL);
        let (body_out, body_ty) = self.infer_block_raw(env.clone(), cond.body);

        let first = IfCond {
            cond: cond_out.item,
            body: body_out.item,
            span: cond.span,
        };

        let mut cons = vec![];
        cons.extend(cond_out.cons);
        cons.extend(body_out.cons);

        let mut elifs = vec![];
        for IfCond { cond, body, span } in elif {
            let cond_out = self.check_expr(env.clone(), cond, Type::BOOL);
            let (body_out, elif_ty) = self.infer_block_raw(env.clone(), body);

            cons.extend(cond_out.cons);
            cons.extend(body_out.cons);
            cons.push(Constraint::TypeEqual {
                expected: body_ty.clone(),
                got: elif_ty.clone(),
            });

            elifs.push(IfCond {
                cond: cond_out.item,
                body: body_out.item,
                span,
            });
        }

        let els_ = els_.map(|Else { body, span }| {
            let (body_out, else_ty) = self.infer_block_raw(env.clone(), body);
            cons.extend(body_out.cons);
            cons.push(Constraint::TypeEqual {
                expected: body_ty.clone(),
                got: else_ty.clone(),
            });

            Else { body: body_out.item, span }
        });

        (
            Qualified::constrained(
                Expr::If(Box::new(If {
                    cond: first,
                    elif: elifs,
                    els_,
                    span,
                })),
                cons,
            ),
            body_ty,
        )
    }

    fn infer_return(
        &mut self,
        env: Env,
        ret: Return<Var>,
    ) -> (Qualified<Expr<TypedVar>>, Type) {
        let Return { value, span } = ret;
        let ret_ty = self.ret_type.last().cloned().expect(
                "Return outside of a function should have been handled somehow already :/",
            );

        let (value, cons) = match value {
            Some(val) => {
                let res = self.check_expr(env, val, ret_ty);
                (Some(res.item), res.cons)
            }
            None => (
                None,
                vec![Constraint::TypeEqual {
                    expected: ret_ty,
                    got: Type::NIL,
                }],
            ),
        };

        let ret = Expr::Return(Box::new(Return { value, span }));
        (Qualified::constrained(ret, cons), Type::NEVER)
    }

    fn process_stmt(
        &mut self,
        env: &mut Env,
        stmt: Statement<Var>,
    ) -> Qualified<Statement<TypedVar>> {
        match stmt {
            Statement::Define(def) => self.process_def_stmt(env, def),
            Statement::Expr(expr) => self.process_expr_stmt(env.clone(), expr),
        }
    }

    fn process_def_stmt(
        &mut self,
        env: &mut Env,
        def: Define<Var>,
    ) -> Qualified<Statement<TypedVar>> {
        let Define { ident, typ, value, span } = def;

        // Add an entry in for the identifier with a new type
        let recursive = value.is_recursive();
        if recursive {
            let tvar = self.fresh_ty_var();
            env.add_type(
                ident,
                Qualified::unconstrained(Type::UnifiableVar(tvar)),
            );
        }

        let (out, inferred_ty) = self.infer_expr(env.clone(), value);
        let mut cons = out.cons;

        let (ident, typ) = if let Some(typ) = typ {
            let ty = self.parse_ty(typ);
            cons.push(Constraint::TypeEqual {
                expected: ty.clone(),
                got: inferred_ty,
            });

            // TODO: Figure out what to do with `typ` after type inference
            (TypedVar(ident, ty.clone()), None)
        } else {
            (TypedVar(ident, inferred_ty.clone()), None)
        };

        // If recursive, we need to extract out the initial type, and constrain
        // the initial unknown type to the inferred type.
        let old =
            env.add_type(ident.0, Qualified::unconstrained(ident.1.clone()));
        let old = old.map(|scheme| self.instantiate(scheme));

        if recursive {
            let old = old.unwrap();
            cons.push(Constraint::TypeEqual {
                expected: old.item,
                got: ident.1.clone(),
            });
            cons.extend(old.cons);
        }

        Qualified::constrained(
            Statement::Define(Define { ident, typ, value: out.item, span }),
            cons,
        )
    }

    fn process_expr_stmt(
        &mut self,
        env: Env,
        expr: ExprStatement<Var>,
    ) -> Qualified<Statement<TypedVar>> {
        let ExprStatement { expr, span } = expr;
        let (out, _ty) = self.infer_expr(env, expr);
        Qualified::constrained(
            Statement::Expr(ExprStatement { expr: out.item, span }),
            out.cons,
        )
    }

    pub(super) fn fresh_ty_var(&mut self) -> UnifiableVar {
        self.uni_table.new_key(None)
    }

    fn parse_ty(&mut self, _ty: lambc_parse::Type<Var>) -> Type {
        todo!()
    }
}

#[cfg(test)]
mod tests {
    use lambc_parse::{
        Block, BoolLit, Call, CharLit, CharText, Define, Expr, ExprStatement,
        F64Lit, FnDef, Group, I64Base, I64Lit, If, IfCond, Index, List,
        NilLit, Return, Span, Statement, StrLit, StrText, Unary, UnaryOp,
    };
    use pretty_assertions::assert_eq;

    use crate::{
        name_res::Var,
        type_check::{
            env::Env, Constraint, FnType, Qualified, TyClass, Type,
            TypeInference, TypedVar, UnifiableVar,
        },
    };

    const SPAN: Span = Span::new(0, 0);

    fn i64_lit() -> I64Lit {
        I64Lit { base: I64Base::Dec, value: "1".into(), span: SPAN }
    }

    fn f64_lit() -> F64Lit {
        F64Lit { value: "2.".into(), span: SPAN }
    }

    fn str_lit() -> StrLit {
        StrLit {
            text: Some(StrText { inner: "a".into(), span: SPAN }),
            span: SPAN,
        }
    }

    fn nil_lit() -> NilLit {
        NilLit { span: SPAN }
    }

    fn bool_lit() -> BoolLit {
        BoolLit { value: false, span: SPAN }
    }

    fn char_lit() -> CharLit {
        CharLit {
            text: Some(CharText { inner: "f".into(), span: SPAN }),
            span: SPAN,
        }
    }

    #[test]
    fn infers_int() {
        let mut checker = TypeInference::new();

        let lit = i64_lit();

        let out = checker.infer_expr(Env::new(), Expr::I64(lit.clone()));
        assert_eq!(out, (Qualified::unconstrained(Expr::I64(lit)), Type::INT))
    }

    #[test]
    fn infers_double() {
        let mut checker = TypeInference::new();

        let lit = f64_lit();
        let out = checker.infer_expr(Env::new(), Expr::F64(lit.clone()));
        assert_eq!(
            out,
            (Qualified::unconstrained(Expr::F64(lit)), Type::DOUBLE)
        );
    }

    #[test]
    fn infers_usv() {
        let mut checker = TypeInference::new();

        let lit = char_lit();

        let out = checker.infer_expr(Env::new(), Expr::Char(lit.clone()));
        assert_eq!(
            out,
            (Qualified::unconstrained(Expr::Char(lit)), Type::USV)
        );
    }

    #[test]
    fn infers_string() {
        let mut checker = TypeInference::new();

        let lit = str_lit();

        let out = checker.infer_expr(Env::new(), Expr::String(lit.clone()));

        assert_eq!(
            out,
            (
                Qualified::unconstrained(Expr::String(lit)),
                Type::List(Box::new(Type::USV))
            )
        );
    }

    #[test]
    fn infers_var() {
        let mut checker = TypeInference::new();

        let typ = Type::UnifiableVar(UnifiableVar(0));
        let var = Var(0);
        let mut env = Env::new();
        env.add_type(var, Qualified::unconstrained(typ.clone()));
        let out = checker.infer_expr(env, Expr::Ident(var));

        assert_eq!(
            out,
            (
                Qualified::unconstrained(Expr::Ident(TypedVar(
                    var,
                    typ.clone()
                ))),
                typ
            )
        );
    }

    #[test]
    fn infers_fndef() {
        let mut checker = TypeInference::new();

        let def = FnDef {
            args: vec![Var(0), Var(1)],
            body: Expr::Ident(Var(0)),
            recursive: false,
            span: SPAN,
        };

        let typ = Type::Fun(FnType {
            args: vec![
                Type::UnifiableVar(UnifiableVar(0)),
                Type::UnifiableVar(UnifiableVar(1)),
            ],
            ret_type: Box::new(Type::UnifiableVar(UnifiableVar(2))),
        });

        let out = checker.infer_expr(Env::new(), Expr::FnDef(Box::new(def)));

        assert_eq!(
            out,
            (
                Qualified::constrained(
                    Expr::FnDef(Box::new(FnDef {
                        args: vec![
                            TypedVar(
                                Var(0),
                                Type::UnifiableVar(UnifiableVar(0))
                            ),
                            TypedVar(
                                Var(1),
                                Type::UnifiableVar(UnifiableVar(1))
                            )
                        ],
                        body: Expr::Ident(TypedVar(
                            Var(0),
                            Type::UnifiableVar(UnifiableVar(0))
                        )),
                        recursive: false,
                        span: SPAN
                    })),
                    vec![Constraint::TypeEqual {
                        expected: Type::UnifiableVar(UnifiableVar(2)),
                        got: Type::UnifiableVar(UnifiableVar(0))
                    }],
                ),
                typ
            )
        );
    }

    #[test]
    fn infers_idx() {
        let mut checker = TypeInference::new();

        let idx = Index {
            lhs: Expr::Nil(nil_lit()),
            rhs: Expr::Bool(bool_lit()),
            span: SPAN,
        };

        let typ = Type::UnifiableVar(UnifiableVar(0));

        let out =
            checker.check_expr(Env::new(), Expr::Index(Box::new(idx)), typ);

        assert_eq!(
            out,
            Qualified::constrained(
                Expr::Index(Box::new(Index {
                    lhs: Expr::Nil(NilLit { span: SPAN }),
                    rhs: Expr::Bool(BoolLit { value: false, span: SPAN }),
                    span: SPAN,
                })),
                vec![
                    Constraint::TypeEqual {
                        expected: Type::BOOL,
                        got: Type::INT,
                    },
                    Constraint::TypeEqual {
                        expected: Type::NIL,
                        got: Type::List(Box::new(Type::UnifiableVar(
                            UnifiableVar(0)
                        ))),
                    },
                    Constraint::TypeEqual {
                        expected: Type::UnifiableVar(UnifiableVar(0)),
                        got: Type::UnifiableVar(UnifiableVar(0))
                    }
                ],
            ),
        );
    }

    #[test]
    fn infers_call() {
        let mut checker = TypeInference::new();

        let callee_ident = Var(0);
        let callee_typ = Type::UnifiableVar(UnifiableVar(1000));
        let ret_typ = Type::UnifiableVar(UnifiableVar(0));

        let mut env = Env::new();
        env.add_type(
            callee_ident,
            Qualified::unconstrained(callee_typ.clone()),
        );

        let call = Call {
            callee: Expr::Ident(callee_ident),
            args: vec![Expr::I64(i64_lit()), Expr::I64(i64_lit())],
            span: SPAN,
        };

        let out = checker.infer_expr(env, Expr::Call(Box::new(call)));

        assert_eq!(
            out,
            (
                Qualified::constrained(
                    Expr::Call(Box::new(Call {
                        callee: Expr::Ident(TypedVar(
                            callee_ident,
                            callee_typ.clone()
                        )),
                        args: vec![Expr::I64(i64_lit()), Expr::I64(i64_lit())],
                        span: SPAN,
                    })),
                    vec![Constraint::TypeEqual {
                        expected: Type::Fun(FnType {
                            args: vec![Type::INT, Type::INT],
                            ret_type: Box::new(ret_typ.clone())
                        }),
                        got: callee_typ.clone(),
                    }],
                ),
                ret_typ
            ),
        );
    }

    #[test]
    fn infers_group() {
        let mut checker = TypeInference::new();

        let expr = str_lit();
        let group = Group { value: Expr::String(expr.clone()), span: SPAN };
        let out = checker.infer_expr(Env::new(), Expr::Group(Box::new(group)));

        assert_eq!(
            out,
            (
                Qualified::constrained(
                    Expr::Group(Box::new(Group {
                        value: Expr::String(expr),
                        span: SPAN
                    })),
                    vec![],
                ),
                Type::List(Box::new(Type::USV)),
            ),
        );
    }

    #[test]
    fn infers_list() {
        let mut checker = TypeInference::new();

        let list = List {
            values: vec![Expr::String(str_lit()), Expr::Char(char_lit())],
            span: SPAN,
        };

        let out = checker.infer_expr(Env::new(), Expr::List(list));

        assert_eq!(
            out,
            (
                Qualified::constrained(
                    Expr::List(List {
                        values: vec![
                            Expr::String(str_lit()),
                            Expr::Char(char_lit())
                        ],
                        span: SPAN
                    }),
                    vec![Constraint::TypeEqual {
                        expected: Type::List(Box::new(Type::USV)),
                        got: Type::USV,
                    }],
                ),
                Type::List(Box::new(Type::List(Box::new(Type::USV)))),
            ),
        );
    }

    #[test]
    fn infers_unary() {
        fn unary<T>(lit: I64Lit, op: UnaryOp) -> Expr<T> {
            Expr::Unary(Box::new(Unary {
                rhs: Expr::I64::<T>(lit),
                op,
                span: SPAN,
                op_span: SPAN,
            }))
        }

        let mut checker = TypeInference::new();

        let lit = i64_lit();
        let una = unary(lit.clone(), UnaryOp::Nneg);
        let out = checker.infer_expr(Env::new(), una);
        assert_eq!(
            out,
            (
                Qualified::constrained(
                    unary(lit, UnaryOp::Nneg),
                    vec![Constraint::IsIn(TyClass::Num, Type::INT)],
                ),
                Type::INT
            )
        );

        let lit = i64_lit();
        let una = unary(lit.clone(), UnaryOp::Bneg);
        let out = checker.infer_expr(Env::new(), una);
        assert_eq!(
            out,
            (
                Qualified::constrained(
                    unary(lit, UnaryOp::Bneg),
                    vec![Constraint::TypeEqual {
                        expected: Type::INT,
                        got: Type::INT
                    }],
                ),
                Type::INT
            )
        );

        let lit = i64_lit();
        let una = unary(lit.clone(), UnaryOp::Lnot);
        let out = checker.infer_expr(Env::new(), una);
        assert_eq!(
            out,
            (
                Qualified::constrained(
                    unary(lit, UnaryOp::Lnot),
                    vec![Constraint::TypeEqual {
                        expected: Type::BOOL,
                        got: Type::INT,
                    }],
                ),
                Type::INT
            )
        );
    }

    #[test]
    fn infers_block_no_val() {
        let mk_fn = |arg_ty, ret_ty| {
            Type::Fun(FnType {
                args: vec![arg_ty],
                ret_type: Box::new(ret_ty),
            })
        };

        let fn_ty = mk_fn(
            Type::UnifiableVar(UnifiableVar(0)),
            Type::UnifiableVar(UnifiableVar(1)),
        );

        let def_id = Statement::Define(Define {
            ident: Var(0),
            typ: None,
            value: Expr::FnDef(Box::new(FnDef {
                args: vec![Var(1)],
                body: Expr::Ident(Var(1)),
                recursive: false,
                span: SPAN,
            })),
            span: SPAN,
        });

        let def_id_ty = Statement::Define(Define {
            ident: TypedVar(Var(0), fn_ty.clone()),
            typ: None,
            value: Expr::FnDef(Box::new(FnDef {
                args: vec![
                    (TypedVar(Var(1), Type::UnifiableVar(UnifiableVar(0)))),
                ],
                body: Expr::Ident(TypedVar(
                    Var(1),
                    Type::UnifiableVar(UnifiableVar(0)),
                )),
                recursive: false,
                span: SPAN,
            })),
            span: SPAN,
        });

        let call_id = |expr| {
            Statement::Expr(ExprStatement {
                expr: Expr::Call(Box::new(Call {
                    callee: Expr::Ident(Var(0)),
                    args: vec![expr],
                    span: SPAN,
                })),
                span: SPAN,
            })
        };

        let call_id_ty = |expr| {
            Statement::Expr(ExprStatement {
                expr: Expr::Call(Box::new(Call {
                    callee: Expr::Ident(TypedVar(Var(0), fn_ty.clone())),
                    args: vec![expr],
                    span: SPAN,
                })),
                span: SPAN,
            })
        };

        let block = Block {
            statements: vec![
                def_id,
                call_id(Expr::Nil(nil_lit())),
                call_id(Expr::Bool(bool_lit())),
            ],
            value: None,
            span: SPAN,
        };

        let mut checker = TypeInference::new();
        let out = checker.infer_block_raw(Env::new(), block);
        assert_eq!(
            out,
            (
                Qualified::constrained(
                    Block {
                        statements: vec![
                            def_id_ty,
                            call_id_ty(Expr::Nil(nil_lit())),
                            call_id_ty(Expr::Bool(bool_lit())),
                        ],
                        value: None,
                        span: SPAN
                    },
                    vec![
                        Constraint::TypeEqual {
                            expected: Type::UnifiableVar(UnifiableVar(1)),
                            got: Type::UnifiableVar(UnifiableVar(0)),
                        },
                        Constraint::TypeEqual {
                            expected: mk_fn(
                                Type::NIL,
                                Type::UnifiableVar(UnifiableVar(2))
                            ),
                            got: fn_ty.clone(),
                        },
                        Constraint::TypeEqual {
                            expected: mk_fn(
                                Type::BOOL,
                                Type::UnifiableVar(UnifiableVar(3))
                            ),
                            got: fn_ty.clone(),
                        },
                    ],
                ),
                Type::NIL
            )
        )
    }

    #[test]
    fn infers_block_val() {
        let mk_fn = |arg_ty, ret_ty| {
            Type::Fun(FnType {
                args: vec![arg_ty],
                ret_type: Box::new(ret_ty),
            })
        };

        let fn_ty = mk_fn(
            Type::UnifiableVar(UnifiableVar(0)),
            Type::UnifiableVar(UnifiableVar(1)),
        );

        let def_id = Statement::Define(Define {
            ident: Var(0),
            typ: None,
            value: Expr::FnDef(Box::new(FnDef {
                args: vec![Var(1)],
                body: Expr::Ident(Var(1)),
                recursive: false,
                span: SPAN,
            })),
            span: SPAN,
        });

        let def_id_ty = Statement::Define(Define {
            ident: TypedVar(Var(0), fn_ty.clone()),
            typ: None,
            value: Expr::FnDef(Box::new(FnDef {
                args: vec![
                    (TypedVar(Var(1), Type::UnifiableVar(UnifiableVar(0)))),
                ],
                body: Expr::Ident(TypedVar(
                    Var(1),
                    Type::UnifiableVar(UnifiableVar(0)),
                )),
                recursive: false,
                span: SPAN,
            })),
            span: SPAN,
        });

        let call_id = |expr| {
            Statement::Expr(ExprStatement {
                expr: Expr::Call(Box::new(Call {
                    callee: Expr::Ident(Var(0)),
                    args: vec![expr],
                    span: SPAN,
                })),
                span: SPAN,
            })
        };

        let call_id_ty = |expr| {
            Statement::Expr(ExprStatement {
                expr: Expr::Call(Box::new(Call {
                    callee: Expr::Ident(TypedVar(Var(0), fn_ty.clone())),
                    args: vec![expr],
                    span: SPAN,
                })),
                span: SPAN,
            })
        };

        let block = Block {
            statements: vec![def_id, call_id(Expr::Nil(nil_lit()))],
            value: (Some(Expr::Call(Box::new(Call {
                callee: Expr::Ident(Var(0)),
                args: vec![Expr::Bool(bool_lit())],
                span: SPAN,
            })))),
            span: SPAN,
        };

        let mut checker = TypeInference::new();
        let out = checker.infer_block_raw(Env::new(), block);
        assert_eq!(
            out,
            (
                Qualified::constrained(
                    Block {
                        statements: vec![
                            def_id_ty,
                            call_id_ty(Expr::Nil(nil_lit())),
                        ],
                        value: Some(Expr::Call(Box::new(Call {
                            callee: Expr::Ident(TypedVar(
                                Var(0),
                                fn_ty.clone()
                            )),
                            args: vec![Expr::Bool(bool_lit())],
                            span: SPAN,
                        }))),
                        span: SPAN
                    },
                    vec![
                        Constraint::TypeEqual {
                            expected: Type::UnifiableVar(UnifiableVar(1)),
                            got: Type::UnifiableVar(UnifiableVar(0))
                        },
                        Constraint::TypeEqual {
                            expected: mk_fn(
                                Type::NIL,
                                Type::UnifiableVar(UnifiableVar(2))
                            ),
                            got: fn_ty.clone(),
                        },
                        Constraint::TypeEqual {
                            expected: mk_fn(
                                Type::BOOL,
                                Type::UnifiableVar(UnifiableVar(3))
                            ),
                            got: fn_ty.clone(),
                        },
                    ],
                ),
                Type::UnifiableVar(UnifiableVar(3))
            )
        )
    }

    #[test]
    fn infers_block_rec_val() {
        let mk_fn = |arg_ty, ret_ty| {
            Type::Fun(FnType {
                args: vec![arg_ty],
                ret_type: Box::new(ret_ty),
            })
        };

        let fn_ty = mk_fn(
            Type::UnifiableVar(UnifiableVar(1)),
            Type::UnifiableVar(UnifiableVar(2)),
        );

        // defines a function: fn x(a) { return x(a); }
        let def_rec = Statement::Define(Define {
            ident: Var(0),
            typ: None,
            value: Expr::FnDef(Box::new(FnDef {
                args: vec![Var(1)],
                body: Expr::Call(Box::new(Call {
                    callee: Expr::Ident(Var(0)),
                    args: vec![Expr::Ident(Var(1))],
                    span: SPAN,
                })),
                recursive: true,
                span: SPAN,
            })),
            span: SPAN,
        });

        let def_rec_ty = Statement::Define(Define {
            ident: TypedVar(Var(0), fn_ty.clone()),
            typ: None,
            value: Expr::FnDef(Box::new(FnDef {
                args: vec![
                    (TypedVar(Var(1), Type::UnifiableVar(UnifiableVar(1)))),
                ],
                body: Expr::Call(Box::new(Call {
                    callee: Expr::Ident(TypedVar(
                        Var(0),
                        Type::UnifiableVar(UnifiableVar(0)),
                    )),
                    args: vec![Expr::Ident(TypedVar(
                        Var(1),
                        Type::UnifiableVar(UnifiableVar(1)),
                    ))],
                    span: SPAN,
                })),
                recursive: true,
                span: SPAN,
            })),
            span: SPAN,
        });

        let block = Block {
            statements: vec![def_rec],
            value: Some(Expr::Call(Box::new(Call {
                callee: Expr::Ident(Var(0)),
                args: vec![Expr::Bool(bool_lit())],
                span: SPAN,
            }))),
            span: SPAN,
        };

        let mut checker = TypeInference::new();
        let out = checker.infer_block_raw(Env::new(), block);
        assert_eq!(
            out,
            (
                Qualified::constrained(
                    Block {
                        statements: vec![def_rec_ty],
                        value: Some(Expr::Call(Box::new(Call {
                            callee: Expr::Ident(TypedVar(
                                Var(0),
                                fn_ty.clone(),
                            )),
                            args: vec![Expr::Bool(bool_lit())],
                            span: SPAN,
                        }))),
                        span: SPAN
                    },
                    vec![
                        Constraint::TypeEqual {
                            expected: mk_fn(
                                Type::UnifiableVar(UnifiableVar(1)),
                                Type::UnifiableVar(UnifiableVar(3))
                            ),
                            got: Type::UnifiableVar(UnifiableVar(0)),
                        },
                        Constraint::TypeEqual {
                            expected: Type::UnifiableVar(UnifiableVar(2)),
                            got: Type::UnifiableVar(UnifiableVar(3)),
                        },
                        Constraint::TypeEqual {
                            expected: Type::UnifiableVar(UnifiableVar(0)),
                            got: fn_ty.clone(),
                        },
                        Constraint::TypeEqual {
                            expected: mk_fn(
                                Type::BOOL,
                                Type::UnifiableVar(UnifiableVar(4))
                            ),
                            got: mk_fn(
                                Type::UnifiableVar(UnifiableVar(1)),
                                Type::UnifiableVar(UnifiableVar(2))
                            ),
                        },
                    ],
                ),
                Type::UnifiableVar(UnifiableVar(4))
            )
        )
    }

    #[test]
    fn infers_if() {
        fn make_block<T>(t: Expr<T>) -> Block<T> {
            Block { statements: vec![], value: Some(t), span: SPAN }
        }

        fn make_ifcond<T>(cond: Expr<T>, body: Expr<T>) -> IfCond<T> {
            IfCond { cond, body: make_block(body), span: SPAN }
        }

        fn make_if<T>() -> If<T> {
            If {
                cond: make_ifcond(
                    Expr::Nil(nil_lit()),
                    Expr::Bool(bool_lit()),
                ),
                elif: vec![
                    make_ifcond(Expr::Bool(bool_lit()), Expr::I64(i64_lit())),
                    make_ifcond(Expr::I64(i64_lit()), Expr::Bool(bool_lit())),
                ],
                els_: None,
                span: SPAN,
            }
        }

        let iff = make_if::<Var>();
        let mut checker = TypeInference::new();
        let out = checker.infer_if(Env::new(), iff);

        assert_eq!(
            out,
            (
                Qualified::constrained(
                    Expr::If(Box::new(make_if::<TypedVar>())),
                    vec![
                        // If condition is required to be a boolean
                        Constraint::TypeEqual {
                            expected: Type::BOOL,
                            got: Type::NIL,
                        },
                        // All following conditions are required to be of tpye bool
                        Constraint::TypeEqual {
                            expected: Type::BOOL,
                            got: Type::INT,
                        },
                        // If condition is required to be a boolean
                        Constraint::TypeEqual {
                            expected: Type::BOOL,
                            got: Type::INT,
                        },
                        // All following conditions are required to be of type bool
                        Constraint::TypeEqual {
                            expected: Type::BOOL,
                            got: Type::BOOL,
                        },
                    ],
                ),
                Type::BOOL
            )
        )
    }

    #[test]
    fn infers_fn_def_with_return() {
        fn fndef<T>(args: Vec<T>, body: Expr<T>) -> Expr<T> {
            Expr::FnDef(Box::new(FnDef {
                args,
                body,
                recursive: false,
                span: SPAN,
            }))
        }

        let x = Var(u32::MAX);

        // fn(x) -> { return true; x };
        let Expr::FnDef(test) = fndef(
            vec![x],
            Expr::Block(Box::new(Block {
                statements: vec![Statement::Expr(ExprStatement {
                    expr: Expr::Return(Box::new(Return {
                        value: Some(Expr::Bool(bool_lit())),
                        span: SPAN,
                    })),
                    span: SPAN,
                })],
                value: Some(Expr::Ident(x)),
                span: SPAN,
            })),
        ) else {
            panic!()
        };

        let mut checker = TypeInference::new();
        let (res, ty) = checker.infer_fndef(Env::new(), *test);
        assert_eq!(
            res,
            Qualified::constrained(
                fndef(
                    vec![TypedVar(x, Type::UnifiableVar(UnifiableVar(0)))],
                    Expr::Block(Box::new(Block {
                        statements: vec![Statement::Expr(ExprStatement {
                            expr: Expr::Return(Box::new(Return {
                                value: Some(Expr::Bool(bool_lit())),
                                span: SPAN,
                            })),
                            span: SPAN,
                        })],
                        value: Some(Expr::Ident(TypedVar(
                            x,
                            Type::UnifiableVar(UnifiableVar(0))
                        ))),
                        span: SPAN,
                    })),
                ),
                vec![
                    Constraint::TypeEqual {
                        expected: Type::UnifiableVar(UnifiableVar(1)),
                        got: Type::BOOL,
                    },
                    Constraint::TypeEqual {
                        expected: Type::UnifiableVar(UnifiableVar(1)),
                        got: Type::UnifiableVar(UnifiableVar(0)),
                    },
                ],
            )
        );

        assert_eq!(
            ty,
            Type::fun(
                vec![Type::UnifiableVar(UnifiableVar(0))],
                Type::UnifiableVar(UnifiableVar(1))
            ),
        );
    }

    #[test]
    fn infers_fn_def_returning_never() {
        fn fndef<T>(args: Vec<T>, body: Expr<T>) -> Expr<T> {
            Expr::FnDef(Box::new(FnDef {
                args,
                body,
                recursive: false,
                span: SPAN,
            }))
        }

        let x = Var(u32::MAX - 0);
        let y = Var(u32::MAX - 1);

        // fn(x) -> { return true; x };
        let Expr::FnDef(test) = fndef(
            vec![x],
            Expr::Block(Box::new(Block {
                statements: vec![Statement::Define(Define {
                    ident: y,
                    typ: None,
                    value: Expr::Return(Box::new(Return {
                        value: Some(Expr::Bool(bool_lit())),
                        span: SPAN,
                    })),
                    span: SPAN,
                })],
                value: Some(Expr::Ident(y)),
                span: SPAN,
            })),
        ) else {
            panic!()
        };

        let mut checker = TypeInference::new();
        let (res, ty) = checker.infer_fndef(Env::new(), *test);
        assert_eq!(
            res,
            Qualified::constrained(
                fndef(
                    vec![TypedVar(x, Type::UnifiableVar(UnifiableVar(0)))],
                    Expr::Block(Box::new(Block {
                        statements: vec![Statement::Define(Define {
                            ident: TypedVar(y, Type::NEVER),
                            typ: None,
                            value: Expr::Return(Box::new(Return {
                                value: Some(Expr::Bool(bool_lit())),
                                span: SPAN,
                            })),
                            span: SPAN,
                        })],
                        value: Some(Expr::Ident(TypedVar(y, Type::NEVER))),
                        span: SPAN,
                    })),
                ),
                vec![
                    Constraint::TypeEqual {
                        expected: Type::UnifiableVar(UnifiableVar(1)),
                        got: Type::BOOL,
                    },
                    Constraint::TypeEqual {
                        expected: Type::UnifiableVar(UnifiableVar(1)),
                        got: Type::NEVER,
                    },
                ],
            )
        );

        assert_eq!(
            ty,
            Type::fun(
                vec![Type::UnifiableVar(UnifiableVar(0))],
                Type::UnifiableVar(UnifiableVar(1))
            ),
        );
    }

    #[test]
    fn infers_fn_def_chained_return() {
        fn fndef<T>(args: Vec<T>, body: Expr<T>) -> Expr<T> {
            Expr::FnDef(Box::new(FnDef {
                args,
                body,
                recursive: false,
                span: SPAN,
            }))
        }

        let x = Var(u32::MAX - 0);

        // fn(x) -> return return;
        let Expr::FnDef(test) = fndef(
            vec![x],
            Expr::Return(Box::new(Return {
                value: Some(Expr::Return(Box::new(Return {
                    value: None,
                    span: SPAN,
                }))),
                span: SPAN,
            })),
        ) else {
            panic!()
        };

        let mut checker = TypeInference::new();
        let (res, ty) = checker.infer_fndef(Env::new(), *test);
        assert_eq!(
            res,
            Qualified::constrained(
                fndef(
                    vec![TypedVar(x, Type::UnifiableVar(UnifiableVar(0)))],
                    Expr::Return(Box::new(Return {
                        value: Some(Expr::Return(Box::new(Return {
                            value: None,
                            span: SPAN,
                        }))),
                        span: SPAN,
                    })),
                ),
                vec![
                    Constraint::TypeEqual {
                        expected: Type::UnifiableVar(UnifiableVar(1)),
                        got: Type::NIL,
                    },
                    Constraint::TypeEqual {
                        expected: Type::UnifiableVar(UnifiableVar(1)),
                        got: Type::NEVER,
                    },
                    Constraint::TypeEqual {
                        expected: Type::UnifiableVar(UnifiableVar(1)),
                        got: Type::NEVER,
                    },
                ],
            )
        );

        assert_eq!(
            ty,
            Type::fun(
                vec![Type::UnifiableVar(UnifiableVar(0))],
                Type::UnifiableVar(UnifiableVar(1))
            ),
        );
    }
}
