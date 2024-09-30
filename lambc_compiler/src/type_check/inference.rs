use lambc_parse::{
    ArrayPattern, Binary, BinaryOp, Block, Call, Case, CaseArm, Define, Else,
    Expr, ExprStatement, FnDef, Group, IdentPattern, If, IfCond, Index,
    InnerPattern, List, LiteralPattern, Pattern, Return, Statement, Unary,
    UnaryOp,
};

use super::{
    env::VarEnv,
    instantiate::{Instantiate, InstantiationContext},
    parsing::ParserContext,
    substitution::SubstitutionContext,
    unification::UnificationContext,
    Constraint, Error, FnType, Qualified, TyClass, Type, TypedVar,
    UnifiableVar,
};
use crate::{name_res::Var, type_check::parsing::TypeParser};

pub trait InferenceContext:
    UnificationContext + SubstitutionContext + ParserContext + InstantiationContext
{
    fn vars_mut(&mut self) -> &mut VarEnv;
    fn new_unif_var(&mut self) -> UnifiableVar;
    fn add_error(&mut self, err: Error);
}

pub struct TypeInference<'ctx, C> {
    pub ctx: &'ctx mut C,
    pub ret_type: Vec<Type>,
}

impl<'ctx, C> TypeInference<'ctx, C>
where
    C: InferenceContext,
{
    pub fn new(ctx: &'ctx mut C) -> Self {
        Self { ctx, ret_type: Default::default() }
    }

    /// Checks the type of an expression, returning an [`Expr`] with the
    /// requirements for the expression to type-check.
    pub fn check_expr(
        &mut self,
        expr: Expr<Var>,
        typ: Type,
    ) -> Qualified<Expr<TypedVar>> {
        match (expr, typ) {
            (Expr::Nil(n), Type::NIL) => {
                Qualified::unconstrained(Expr::Nil(n))
            }
            (Expr::I64(i), Type::INT) => {
                Qualified::unconstrained(Expr::I64(i))
            }
            (Expr::Char(c), Type::USV) => {
                Qualified::unconstrained(Expr::Char(c))
            }
            (Expr::Bool(b), Type::BOOL) => {
                Qualified::unconstrained(Expr::Bool(b))
            }
            (Expr::F64(f), Type::DOUBLE) => {
                Qualified::unconstrained(Expr::F64(f))
            }
            (Expr::String(s), Type::List(e)) if *e == Type::USV => {
                Qualified::unconstrained(Expr::String(s))
            }
            (expr, expected_ty) => {
                let (mut out, actual_ty) = self.infer_expr(expr);
                out.cons.push(Constraint::TypeEqual {
                    expected: expected_ty,
                    got: actual_ty,
                });

                out
            }
        }
    }

    /// Infers the type of `expr` given the environment `Env`. Returns a tuple
    /// of a constrained expression, and the type of that expression.
    pub(super) fn infer_expr(
        &mut self,
        expr: Expr<Var>,
        // todo: The type is what is actually constrained here, so move `Qualified`
        // over to the type.
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
                let ty = self.ctx.vars_mut().type_of(i);
                let ty = Instantiate::new(self.ctx).scheme(ty);
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
            Expr::Case(case) => self.infer_case(*case),
            Expr::Path(_) => todo!(),
            Expr::If(iff) => self.infer_if(*iff),
            Expr::Group(g) => self.infer_group(*g),
            Expr::Return(ret) => self.infer_return(*ret),
            Expr::Index(idx) => self.infer_idx(*idx),
            Expr::List(list) => self.infer_list(list),
            Expr::Call(call) => self.infer_call(*call),
            Expr::Block(block) => self.infer_block(*block),
            Expr::Unary(unary) => self.infer_unary(*unary),
            Expr::FnDef(fndef) => self.infer_fndef(*fndef),
            Expr::Binary(binary) => self.infer_binary(*binary),
        }
    }

    fn infer_fndef(
        &mut self,
        fndef: FnDef<Var>,
    ) -> (Qualified<Expr<TypedVar>>, Type) {
        let FnDef { args, body, recursive, span } = fndef;
        let mut new_args = Vec::with_capacity(args.len());
        let mut typs = Vec::with_capacity(args.len());

        for arg in &args {
            let typ = self.fresh_ty_var();
            typs.push(Type::UnifiableVar(typ));
            new_args.push(TypedVar(*arg, Type::UnifiableVar(typ)));
            self.ctx.vars_mut().add_type(
                *arg,
                Qualified::unconstrained(Type::UnifiableVar(typ)),
            );
        }

        // Return types need to have access to the return type, so that the value
        // can be checked against this type.
        let ret_type = Type::UnifiableVar(self.fresh_ty_var());
        self.ret_type.push(ret_type.clone());

        let body_out = self.check_expr(body, ret_type);

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
        call: Call<Var>,
    ) -> (Qualified<Expr<TypedVar>>, Type) {
        let mut cons = Vec::new();
        let mut asts = Vec::new();
        let mut typs = Vec::new();

        for arg in call.args {
            let (out, typ) = self.infer_expr(arg);
            cons.extend(out.cons);
            asts.push(out.item);
            typs.push(typ);
        }

        let ret_typ = Type::UnifiableVar(self.fresh_ty_var());
        let fn_typ = Type::Fun(FnType {
            args: typs,
            ret_type: Box::new(ret_typ.clone()),
        });

        let fn_out = self.check_expr(call.callee, fn_typ);
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
        idx: Index<Var>,
    ) -> (Qualified<Expr<TypedVar>>, Type) {
        let list_elem_ty = self.fresh_ty_var();
        let list_ty = Type::List(Box::new(Type::UnifiableVar(list_elem_ty)));

        let (mut idxee_out, indexee_ty) = self.infer_expr(idx.lhs);
        let (index_out, index_ty) = self.infer_expr(idx.rhs);

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
        group: Group<Var>,
    ) -> (Qualified<Expr<TypedVar>>, Type) {
        let (res, ty) = self.infer_expr(group.value);
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
        list: List<Var>,
    ) -> (Qualified<Expr<TypedVar>>, Type) {
        let mut values = list.values.into_iter();
        let (mut cons, mut asts, first_ty) = match values.next() {
            Some(e) => {
                let (out, res) = self.infer_expr(e);
                (out.cons, vec![out.item], res)
            }
            None => (vec![], vec![], Type::UnifiableVar(self.fresh_ty_var())),
        };

        for val in values {
            let out = self.check_expr(val, first_ty.clone());
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
        unary: Unary<Var>,
    ) -> (Qualified<Expr<TypedVar>>, Type) {
        let Unary { rhs, op, span, op_span } = unary;
        let (out, ty) = self.infer_expr(rhs);

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

                let lhs = self.check_expr(binary.lhs, fun);
                let rhs = self.check_expr(binary.rhs, arg);
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

                let lhs = self.check_expr(binary.lhs, arg);
                let rhs = self.check_expr(binary.rhs, fun);
                let mut cons = lhs.cons;
                cons.extend(rhs.cons);

                (lhs.item, rhs.item, cons, ret)
            }
            // lhs <. rhs
            // (fn(t2) -> t3) <. (fn(t1) -> t2)
            // => (fn(t1) -> t3)
            BinaryOp::Cpsl => {
                let t1 = Type::UnifiableVar(self.fresh_ty_var());
                let t2 = Type::UnifiableVar(self.fresh_ty_var());
                let t3 = Type::UnifiableVar(self.fresh_ty_var());

                let lhs_fn = Type::fun(vec![t2.clone()], t3.clone());
                let rhs_fn = Type::fun(vec![t1.clone()], t2.clone());
                let ret_fn = Type::fun(vec![t1.clone()], t3.clone());

                let lhs = self.check_expr(binary.lhs, lhs_fn);
                let rhs = self.check_expr(binary.rhs, rhs_fn);

                let mut cons = vec![];
                cons.extend(lhs.cons);
                cons.extend(rhs.cons);

                (lhs.item, rhs.item, cons, ret_fn)
            }
            // lhs <. rhs
            // (fn(t1) -> t2) .> (fn(t2) -> t3)
            // => (fn(t1) -> t3)
            BinaryOp::Cpsr => {
                let t1 = Type::UnifiableVar(self.fresh_ty_var());
                let t2 = Type::UnifiableVar(self.fresh_ty_var());
                let t3 = Type::UnifiableVar(self.fresh_ty_var());

                let lhs_fn = Type::fun(vec![t1.clone()], t2.clone());
                let rhs_fn = Type::fun(vec![t2.clone()], t3.clone());
                let ret_fn = Type::fun(vec![t1.clone()], t3.clone());

                let lhs = self.check_expr(binary.lhs, lhs_fn);
                let rhs = self.check_expr(binary.rhs, rhs_fn);

                let mut cons = vec![];
                cons.extend(lhs.cons);
                cons.extend(rhs.cons);

                (lhs.item, rhs.item, cons, ret_fn)
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
                let (lhs, lhs_ty) = self.infer_expr(binary.lhs);
                let (rhs, rhs_ty) = self.infer_expr(binary.rhs);
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
                let lhs = self.check_expr(binary.lhs, Type::INT);
                let rhs = self.check_expr(binary.rhs, Type::INT);
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
                let (lhs_out, lhs_ty) = self.infer_expr(binary.lhs);

                let (rhs_out, rhs_ty) = self.infer_expr(binary.rhs);

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
                let (lhs_out, lhs_ty) = self.infer_expr(binary.lhs);

                let (rhs_out, rhs_ty) = self.infer_expr(binary.rhs);

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
                let (lhs_out, lhs_ty) = self.infer_expr(binary.lhs);

                let (rhs_out, rhs_ty) = self.infer_expr(binary.rhs);

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
                let (lhs_out, lhs_ty) = self.infer_expr(binary.lhs);

                let (rhs_out, rhs_ty) = self.infer_expr(binary.rhs);

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
        block: Block<Var>,
    ) -> (Qualified<Expr<TypedVar>>, Type) {
        let (res, ty) = self.infer_block_raw(block);
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
        block: Block<Var>,
    ) -> (Qualified<Block<TypedVar>>, Type) {
        let returns = does_block_unconditionally_return(&block);
        let Block { statements, value, span } = block;

        let mut stmts = Vec::with_capacity(statements.len());
        let mut cons = vec![];
        for stmt in statements {
            let out = self.process_stmt(stmt);
            stmts.push(out.item);
            cons.extend(out.cons);
        }

        let (val, ty) =
            if let Some((out, ty)) = value.map(|v| self.infer_expr(v)) {
                cons.extend(out.cons);
                (Some(out.item), ty)
            } else {
                // If a block returns, then it never produces a value
                let typ = if returns { Type::NEVER } else { Type::NIL };
                (None, typ)
            };

        (
            Qualified::constrained(
                Block { statements: stmts, value: val, span },
                cons,
            ),
            ty,
        )
    }

    fn infer_case(
        &mut self,
        case: Case<Var>,
    ) -> (Qualified<Expr<TypedVar>>, Type) {
        let Case { scrutinee, arms, span } = case;
        let (scrut, scrut_ty) = self.infer_expr(scrutinee);
        let mut cons = scrut.cons;

        let mut ty_arms = Vec::with_capacity(arms.len());
        let mut arm_tys = Vec::with_capacity(arms.len());
        for arm in arms {
            let (arm, arm_ty) = self.infer_case_arm(arm, scrut_ty.clone());
            ty_arms.push(arm.item);
            arm_tys.push(arm_ty);
            cons.extend(arm.cons);
        }

        // All types are required to be equal to the first, and the first determines
        // the expected type.
        //
        // todo: if there are no arms, then the type is never, as the code
        //       will never progress past that point. The behavior of case
        //       expressions with no arms should be specified in #25.
        let case_ty = arm_tys
            .into_iter()
            .reduce(|l, r| {
                cons.push(Constraint::TypeEqual {
                    expected: l.clone(),
                    got: r,
                });
                l
            })
            .unwrap_or(Type::NEVER);

        (
            Qualified::constrained(
                Expr::Case(Box::new(Case {
                    scrutinee: scrut.item,
                    arms: ty_arms,
                    span,
                })),
                cons,
            ),
            case_ty,
        )
    }

    fn infer_case_arm(
        &mut self,
        arm: CaseArm<Var>,
        scrut_ty: Type,
    ) -> (Qualified<CaseArm<TypedVar>>, Type) {
        let CaseArm { pattern, body, span } = arm;
        let (pat, pat_ty) = self.infer_pattern(pattern);
        let (body, body_ty) = self.infer_expr(body);

        let mut cons =
            vec![Constraint::TypeEqual { expected: scrut_ty, got: pat_ty }];
        cons.extend(pat.cons);
        cons.extend(body.cons);

        (
            Qualified::constrained(
                CaseArm { pattern: pat.item, body: body.item, span },
                cons,
            ),
            body_ty,
        )
    }

    fn infer_pattern(
        &mut self,
        pat: Pattern<Var>,
    ) -> (Qualified<Pattern<TypedVar>>, Type) {
        let mut idents: Vec<Var> =
            pat.binding_names().into_iter().copied().collect();
        idents.sort_by_key(|v| v.0);
        idents.dedup();

        for ident in &idents {
            let unif = self.fresh_ty_var();
            self.ctx.vars_mut().add_type(
                *ident,
                Qualified::unconstrained(Type::UnifiableVar(unif)),
            );
        }

        let Pattern { inner, span } = pat;

        let mut cons = vec![];
        let mut inner_tys = vec![];
        let mut ty_inners = vec![];
        let mut envs = vec![];
        for inner in inner {
            let env = self.ctx.vars_mut().clone();
            let (ty_inner, inner_ty) = self.infer_inner_pattern(inner);

            ty_inners.push(ty_inner.item);
            inner_tys.push(inner_ty);
            cons.extend(ty_inner.cons);
            envs.push(std::mem::replace(self.ctx.vars_mut(), env));
        }

        // Add constraints so that all patterns must declare all variables to have
        // the same types.
        if let Some(first_env) = envs.first() {
            let rest = envs.iter().skip(1);
            for env in rest {
                let first = idents.iter().map(|v| first_env.type_of(*v).ty);
                let next = idents.iter().map(|v| env.type_of(*v).ty);
                for (l, r) in first.zip(next) {
                    cons.push(Constraint::TypeEqual { expected: l, got: r });
                }
            }

            std::mem::swap(self.ctx.vars_mut(), &mut envs[0]);
        }

        let ty = inner_tys
            .into_iter()
            .reduce(|l, r| {
                cons.push(Constraint::TypeEqual {
                    expected: l.clone(),
                    got: r,
                });
                l
            })
            .unwrap();

        (Qualified::constrained(Pattern { inner: ty_inners, span }, cons), ty)
    }

    fn infer_inner_pattern(
        &mut self,
        pat: InnerPattern<Var>,
    ) -> (Qualified<InnerPattern<TypedVar>>, Type) {
        use InnerPattern as Ip;
        match pat {
            Ip::Literal(lit) => self.infer_literal_pattern(*lit),
            Ip::Array(arr) => self.infer_array_pattern(*arr),
            Ip::Ident(id) => self.infer_ident_pattern(*id),
            Ip::Rest(r) => (
                Qualified::unconstrained(Ip::Rest(r)),
                Type::List(Box::new(Type::UnifiableVar(self.fresh_ty_var()))),
            ),
        }
    }

    fn infer_literal_pattern(
        &mut self,
        lit: LiteralPattern,
    ) -> (Qualified<InnerPattern<TypedVar>>, Type) {
        use LiteralPattern as Lp;
        let (lit, ty) = match lit {
            Lp::String(s) => (Lp::String(s), Type::List(Box::new(Type::USV))),
            Lp::Bool(b) => (Lp::Bool(b), Type::BOOL),
            Lp::Char(c) => (Lp::Char(c), Type::USV),
            Lp::I64(i) => (Lp::I64(i), Type::INT),
            Lp::Nil(n) => (Lp::Nil(n), Type::NIL),
        };

        (Qualified::unconstrained(InnerPattern::Literal(Box::new(lit))), ty)
    }

    fn infer_ident_pattern(
        &mut self,
        id: IdentPattern<Var>,
    ) -> (Qualified<InnerPattern<TypedVar>>, Type) {
        let IdentPattern { ident, bound, span } = id;
        let (bound, ty, mut cons) = match bound {
            Some(pat) => {
                let (ty_pat, pat_ty) = self.infer_inner_pattern(*pat);
                (Some(Box::new(ty_pat.item)), pat_ty, ty_pat.cons)
            }
            None => {
                let t = Type::UnifiableVar(self.fresh_ty_var());
                (None, t, vec![])
            }
        };

        let is_ty = self.ctx.vars_mut().type_of(ident).ty;
        cons.push(Constraint::TypeEqual { expected: is_ty, got: ty.clone() });

        (
            Qualified::constrained(
                InnerPattern::Ident(Box::new(IdentPattern {
                    ident: TypedVar(ident, ty.clone()),
                    bound,
                    span,
                })),
                cons,
            ),
            ty,
        )
    }

    fn infer_array_pattern(
        &mut self,
        arr: ArrayPattern<Var>,
    ) -> (Qualified<InnerPattern<TypedVar>>, Type) {
        let ArrayPattern { patterns, span } = arr;

        let mut constraints = vec![];
        let mut ty_patterns = vec![];
        let mut element_tys = vec![];
        let mut rest_tys = vec![];

        for pat in patterns {
            let (ty_pat, pat_ty) = self.infer_pattern(pat);
            if ty_pat.item.inner.iter().all(|i| i.is_rest_pattern()) {
                rest_tys.push(pat_ty)
            } else {
                element_tys.push(pat_ty);
            }

            constraints.extend(ty_pat.cons);
            ty_patterns.push(ty_pat.item);
        }

        let elem_typ = element_tys
            .into_iter()
            .reduce(|l, r| {
                constraints.push(Constraint::TypeEqual {
                    expected: l.clone(),
                    got: r,
                });
                l
            })
            .unwrap_or_else(|| Type::UnifiableVar(self.fresh_ty_var()));

        // There should only ever be one. Before this but after name resolution
        // there should be a syntactic analysis step which transforms the AST so
        // that the fact there should only be one is built into the type.
        for rest in rest_tys {
            constraints.push(Constraint::TypeEqual {
                expected: Type::List(Box::new(elem_typ.clone())),
                got: rest.clone(),
            });
        }

        (
            Qualified::constrained(
                InnerPattern::Array(Box::new(ArrayPattern {
                    patterns: ty_patterns,
                    span,
                })),
                constraints,
            ),
            Type::List(Box::new(elem_typ)),
        )
    }

    pub(super) fn infer_if(
        &mut self,
        iff: If<Var>,
    ) -> (Qualified<Expr<TypedVar>>, Type) {
        let If { cond, elif, els_, span } = iff;
        let cond_out = self.check_expr(cond.cond, Type::BOOL);
        let (body_out, body_ty) = self.infer_block_raw(cond.body);

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
            let cond_out = self.check_expr(cond, Type::BOOL);
            let (body_out, elif_ty) = self.infer_block_raw(body);

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
            let (body_out, else_ty) = self.infer_block_raw(body);
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
        ret: Return<Var>,
    ) -> (Qualified<Expr<TypedVar>>, Type) {
        let Return { value, span } = ret;
        let ret_ty = self.ret_type.last().cloned().unwrap_or_else(|| {
            self.ctx.add_error(Error::ReturnOutOfFunction);
            Type::Error
        });

        let (value, cons) = match value {
            Some(val) => {
                let res = self.check_expr(val, ret_ty);
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
        stmt: Statement<Var>,
    ) -> Qualified<Statement<TypedVar>> {
        match stmt {
            Statement::Define(def) => self.process_def_stmt(def),
            Statement::Expr(expr) => self.process_expr_stmt(expr),
        }
    }

    fn process_def_stmt(
        &mut self,
        def: Define<Var>,
    ) -> Qualified<Statement<TypedVar>> {
        let Define { ident, typ, value, span } = def;

        // Add an entry in for the identifier with a new type
        let recursive = value.is_recursive();
        if recursive {
            let tvar = self.fresh_ty_var();
            self.ctx.vars_mut().add_type(
                ident,
                Qualified::unconstrained(Type::UnifiableVar(tvar)),
            );
        }

        let (out, inferred_ty) = self.infer_expr(value);
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
        let old = self
            .ctx
            .vars_mut()
            .add_type(ident.0, Qualified::unconstrained(ident.1.clone()));

        let old = old.map(|scheme| Instantiate::new(self.ctx).scheme(scheme));

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
        expr: ExprStatement<Var>,
    ) -> Qualified<Statement<TypedVar>> {
        let ExprStatement { expr, span } = expr;
        let (out, _ty) = self.infer_expr(expr);
        Qualified::constrained(
            Statement::Expr(ExprStatement { expr: out.item, span }),
            out.cons,
        )
    }

    pub(super) fn fresh_ty_var(&mut self) -> UnifiableVar {
        self.ctx.new_unif_var()
    }

    fn parse_ty(&mut self, ty: lambc_parse::Type<Var>) -> Type {
        let mut parser = TypeParser::new(self.ctx);
        let sc = match parser.parse_scheme(&ty) {
            Ok(sc) => sc,
            Err(err) => {
                self.ctx.add_error(err);
                return Type::Error;
            }
        };

        assert_eq!(
            sc.unbound.len(),
            0,
            "A statement type definition can't have unbound variables"
        );

        sc.ty
    }
}

fn does_stmt_return<T>(s: &Statement<T>) -> bool {
    match s {
        Statement::Define(Define { value, .. }) => {
            does_expr_unconditionally_return(value)
        }
        Statement::Expr(e) => does_expr_unconditionally_return(&e.expr),
    }
}

fn does_expr_unconditionally_return<T>(e: &Expr<T>) -> bool {
    match e {
        Expr::Nil(_) => false,
        Expr::I64(_) => false,
        Expr::F64(_) => false,
        Expr::Bool(_) => false,
        Expr::Char(_) => false,
        Expr::Ident(_) => false,
        Expr::String(_) => false,
        Expr::FnDef(_) => false,
        Expr::Return(_) => true,
        Expr::List(li) => {
            li.values.iter().any(does_expr_unconditionally_return)
        }
        Expr::Group(g) => does_expr_unconditionally_return(&g.value),
        Expr::Block(b) => b.statements.iter().any(does_stmt_return),
        Expr::If(i) => {
            does_ifcond_unconditionally_return(&i.cond)
                && i.elif.iter().all(does_ifcond_unconditionally_return)
                && i.els_.as_ref().map_or(false, |e| {
                    does_block_unconditionally_return(&e.body)
                })
        }
        // todo: this is unsound if the case is missing arms
        Expr::Case(c) => {
            does_expr_unconditionally_return(&c.scrutinee)
                || c.arms
                    .iter()
                    .all(|arm| does_expr_unconditionally_return(&arm.body))
        }
        Expr::Index(i) => {
            does_expr_unconditionally_return(&i.lhs)
                || does_expr_unconditionally_return(&i.rhs)
        }
        Expr::Call(c) => {
            does_expr_unconditionally_return(&c.callee)
                || c.args.iter().any(does_expr_unconditionally_return)
        }
        Expr::Unary(u) => does_expr_unconditionally_return(&u.rhs),
        Expr::Binary(b) => {
            does_expr_unconditionally_return(&b.lhs)
                || does_expr_unconditionally_return(&b.rhs)
        }
        Expr::Path(_) => todo!(),
    }
}

fn does_block_unconditionally_return<T>(b: &Block<T>) -> bool {
    b.statements.iter().any(does_stmt_return)
        || b.value.as_ref().map_or(false, does_expr_unconditionally_return)
}

fn does_ifcond_unconditionally_return<T>(i: &IfCond<T>) -> bool {
    does_expr_unconditionally_return(&i.cond)
        || does_block_unconditionally_return(&i.body)
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
            context::Context, scheme::TypeScheme, substitution::Substitute,
            unification::Unifier, Constraint, Error, FnType, Qualified,
            RigidVar, TyClass, Type, TypeInference, TypedVar, UnifiableVar,
        },
        State,
    };

    use crate::type_check::Result;
    use std::collections::HashSet;

    impl<'ctx, 'state> TypeInference<'ctx, Context<'state>> {
        fn infer(
            &mut self,
            expr: Expr<Var>,
        ) -> Result<(Expr<TypedVar>, TypeScheme)> {
            self.infer_with_env(expr)
        }

        fn infer_with_env(
            &mut self,
            expr: Expr<Var>,
        ) -> Result<(Expr<TypedVar>, TypeScheme)> {
            let (out, ty) = self.infer_expr(expr);
            Unifier::new(self.ctx).unify(out.cons.clone())?;

            let mut sub = Substitute::new(self.ctx);
            let (mut unbound, ty) = sub.rigidify(ty);
            let (ast_unbound, expr) = sub.rigidify_expr(out.item);
            unbound.extend(ast_unbound);

            let (con_unbound, cons) = sub.rigidify_constraints(out.cons);
            let ambiguities = con_unbound.difference(&unbound).count();
            assert_eq!(ambiguities, 0);

            let reduced = self.reduce_constraints(&unbound, cons);
            Ok((expr, TypeScheme { unbound, constraints: reduced, ty }))
        }

        fn reduce_constraints(
            &self,
            unbound: &HashSet<RigidVar>,
            cons: Vec<Constraint>,
        ) -> Vec<Constraint> {
            cons.into_iter()
                .filter(|c| match c {
                    Constraint::IsIn(_, t1) => self.has_unbound(unbound, t1),
                    // This is post unification, and any `TypeEqual` would have resulted
                    // in a type being inferred to this type. Thus, we don't need to keep
                    // these restraints.
                    Constraint::TypeEqual { expected: _, got: _ } => false,
                })
                .collect()
        }

        fn has_unbound(&self, unbound: &HashSet<RigidVar>, ty: &Type) -> bool {
            match ty {
                Type::Error | Type::Con(_) => false,
                Type::RigidVar(rig) => unbound.contains(rig),
                Type::List(elem_ty) => self.has_unbound(unbound, &elem_ty),
                Type::Fun(f) => {
                    f.args.iter().any(|t| self.has_unbound(unbound, t))
                        || self.has_unbound(unbound, &f.ret_type)
                }
                Type::UnifiableVar(_) => {
                    unreachable!("Should not occur post unification")
                }
            }
        }
    }

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

    fn fndef<T>(args: Vec<T>, body: Expr<T>) -> Expr<T> {
        Expr::FnDef(Box::new(FnDef {
            args,
            body,
            recursive: false,
            span: SPAN,
        }))
    }

    fn call<T>(callee: Expr<T>, args: Vec<Expr<T>>) -> Expr<T> {
        Expr::Call(Box::new(Call { callee, args, span: SPAN }))
    }

    macro_rules! set {
        ($($expr:expr),*$(,)?) => {{
            #[allow(unused_mut)]
            let mut x = std::collections::HashSet::new();
            $(x.insert($expr);)*
            x
        }};
    }

    #[test]
    fn infers_int() {
        let mut state = State::new();
        let mut ctx = Context::new(&mut state);
        let mut inferer = TypeInference::new(&mut ctx);

        let lit = i64_lit();

        let out = inferer.infer_expr(Expr::I64(lit.clone()));
        assert_eq!(out, (Qualified::unconstrained(Expr::I64(lit)), Type::INT))
    }

    #[test]
    fn infers_double() {
        let mut state = State::new();
        let mut ctx = Context::new(&mut state);
        let mut inferer = TypeInference::new(&mut ctx);

        let lit = f64_lit();
        let out = inferer.infer_expr(Expr::F64(lit.clone()));
        assert_eq!(
            out,
            (Qualified::unconstrained(Expr::F64(lit)), Type::DOUBLE)
        );
    }

    #[test]
    fn infers_usv() {
        let mut state = State::new();
        let mut ctx = Context::new(&mut state);
        let mut inferer = TypeInference::new(&mut ctx);

        let lit = char_lit();

        let out = inferer.infer_expr(Expr::Char(lit.clone()));
        assert_eq!(
            out,
            (Qualified::unconstrained(Expr::Char(lit)), Type::USV)
        );
    }

    #[test]
    fn infers_string() {
        let state = &mut State::new();
        let mut ctx = Context::new(state);
        let mut inferer = TypeInference::new(&mut ctx);

        let lit = str_lit();

        let out = inferer.infer_expr(Expr::String(lit.clone()));

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
        let state = &mut State::new();
        let mut ctx = Context::new(state);
        let typ = Type::UnifiableVar(UnifiableVar(0));
        let var = Var(0);
        ctx.vars_mut().add_type(var, Qualified::unconstrained(typ.clone()));

        let mut inferer = TypeInference::new(&mut ctx);
        let out = inferer.infer_expr(Expr::Ident(var));

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
        let state = &mut State::new();
        let mut ctx = Context::new(state);
        let mut inferer = TypeInference::new(&mut ctx);

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

        let out = inferer.infer_expr(Expr::FnDef(Box::new(def)));

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
        let state = &mut State::new();
        let mut ctx = Context::new(state);
        let mut inferer = TypeInference::new(&mut ctx);

        let idx = Index {
            lhs: Expr::Nil(nil_lit()),
            rhs: Expr::Bool(bool_lit()),
            span: SPAN,
        };

        let typ = Type::UnifiableVar(UnifiableVar(0));

        let out = inferer.check_expr(Expr::Index(Box::new(idx)), typ);

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
        let mut state = State::new();
        let mut ctx = Context::new(&mut state);
        let callee_ident = Var(0);
        let callee_typ = Type::UnifiableVar(UnifiableVar(1000));
        let ret_typ = Type::UnifiableVar(UnifiableVar(0));

        ctx.vars_mut().add_type(
            callee_ident,
            Qualified::unconstrained(callee_typ.clone()),
        );

        let mut inferer = TypeInference::new(&mut ctx);

        let call = Call {
            callee: Expr::Ident(callee_ident),
            args: vec![Expr::I64(i64_lit()), Expr::I64(i64_lit())],
            span: SPAN,
        };

        let out = inferer.infer_expr(Expr::Call(Box::new(call)));

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
        let state = &mut State::new();
        let mut ctx = Context::new(state);
        let mut inferer = TypeInference::new(&mut ctx);

        let expr = str_lit();
        let group = Group { value: Expr::String(expr.clone()), span: SPAN };
        let out = inferer.infer_expr(Expr::Group(Box::new(group)));

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
        let mut state = State::new();
        let mut ctx = Context::new(&mut state);
        let mut inferer = TypeInference::new(&mut ctx);

        let list = List {
            values: vec![Expr::String(str_lit()), Expr::Char(char_lit())],
            span: SPAN,
        };

        let out = inferer.infer_expr(Expr::List(list));

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

        let state = &mut State::new();
        let mut ctx = Context::new(state);
        let mut inferer = TypeInference::new(&mut ctx);

        let lit = i64_lit();
        let una = unary(lit.clone(), UnaryOp::Nneg);
        let out = inferer.infer_expr(una);
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
        let out = inferer.infer_expr(una);
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
        let out = inferer.infer_expr(una);
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

        let state = &mut State::new();
        let mut ctx = Context::new(state);
        let mut inferer = TypeInference::new(&mut ctx);
        let out = inferer.infer_block_raw(block);
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

        let state = &mut State::new();
        let mut ctx = Context::new(state);
        let mut inferer = TypeInference::new(&mut ctx);
        let out = inferer.infer_block_raw(block);
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

        let state = &mut State::new();
        let mut ctx = Context::new(state);
        let mut inferer = TypeInference::new(&mut ctx);
        let out = inferer.infer_block_raw(block);
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
        let state = &mut State::new();
        let mut ctx = Context::new(state);
        let mut inferer = TypeInference::new(&mut ctx);
        let out = inferer.infer_if(iff);

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

        let state = &mut State::new();
        let mut ctx = Context::new(state);
        let mut inferer = TypeInference::new(&mut ctx);
        let (res, ty) = inferer.infer_fndef(*test);
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

        let state = &mut State::new();
        let mut ctx = Context::new(state);
        let mut inferer = TypeInference::new(&mut ctx);
        let (res, ty) = inferer.infer_fndef(*test);
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

        let state = &mut State::new();
        let mut ctx = Context::new(state);
        let mut inferer = TypeInference::new(&mut ctx);
        let (res, ty) = inferer.infer_fndef(*test);
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

    #[test]
    fn infers_id() {
        let x = Var(u32::MAX);
        let ast = Expr::FnDef(Box::new(FnDef {
            args: vec![x],
            body: Expr::Ident(x),
            recursive: false,
            span: SPAN,
        }));

        let state = &mut State::new();
        let mut ctx = Context::new(state);
        let (expr, scheme) = TypeInference::new(&mut ctx)
            .infer(ast)
            .expect("Inference to succeed");

        let a = RigidVar(0);

        assert_eq!(
            expr,
            Expr::FnDef(Box::new(FnDef {
                args: vec![TypedVar(x, Type::RigidVar(a))],
                body: Expr::Ident(TypedVar(x, Type::RigidVar(a))),
                recursive: false,
                span: SPAN,
            }))
        );

        assert_eq!(
            scheme,
            TypeScheme {
                constraints: vec![],
                unbound: set![a],
                ty: Type::Fun(FnType {
                    args: vec![Type::RigidVar(a)],
                    ret_type: Box::new(Type::RigidVar(a))
                })
            }
        );
    }

    #[test]
    fn infers_k_combinator() {
        let x = Var(u32::MAX);
        let y = Var(u32::MAX - 1);

        let ast = Expr::FnDef(Box::new(FnDef {
            args: vec![x],
            body: Expr::FnDef(Box::new(FnDef {
                args: vec![y],
                body: Expr::Ident(x),
                recursive: false,
                span: SPAN,
            })),
            recursive: false,
            span: SPAN,
        }));

        let state = &mut State::new();
        let mut ctx = Context::new(state);
        let (expr, scheme) = TypeInference::new(&mut ctx)
            .infer(ast)
            .expect("Inference to succeed");

        let a = RigidVar(0);
        let b = RigidVar(1);

        assert_eq!(
            expr,
            Expr::FnDef(Box::new(FnDef {
                args: vec![TypedVar(x, Type::RigidVar(a))],
                body: Expr::FnDef(Box::new(FnDef {
                    args: vec![TypedVar(y, Type::RigidVar(b))],
                    body: Expr::Ident(TypedVar(x, Type::RigidVar(a))),
                    recursive: false,
                    span: SPAN
                })),
                recursive: false,
                span: SPAN,
            }))
        );

        assert_eq!(
            scheme,
            TypeScheme {
                constraints: vec![],
                unbound: set![a, b],
                ty: Type::Fun(FnType {
                    args: vec![Type::RigidVar(a)],
                    ret_type: Box::new(Type::Fun(FnType {
                        args: vec![Type::RigidVar(b)],
                        ret_type: Box::new(Type::RigidVar(a))
                    }))
                })
            }
        );
    }

    #[test]
    fn infers_s_combinator() {
        let x = Var(u32::MAX);
        let y = Var(u32::MAX - 1);
        let z = Var(u32::MAX - 2);

        let s_comb = fndef(
            vec![x],
            fndef(
                vec![y],
                fndef(
                    vec![z],
                    call(
                        call(Expr::Ident(x), vec![Expr::Ident(z)]),
                        vec![call(Expr::Ident(y), vec![Expr::Ident(z)])],
                    ),
                ),
            ),
        );

        let state = &mut State::new();
        let mut ctx = Context::new(state);
        let (_expr, scheme) = TypeInference::new(&mut ctx)
            .infer(s_comb)
            .expect("Inference to succeed");

        let a = RigidVar(0);
        let b = RigidVar(1);
        let c = RigidVar(2);

        let x_ty = Type::fun(
            vec![Type::RigidVar(a)],
            Type::fun(vec![Type::RigidVar(b)], Type::RigidVar(c)),
        );

        let y_ty = Type::fun(vec![Type::RigidVar(a)], Type::RigidVar(b));

        assert_eq!(
            scheme,
            TypeScheme {
                constraints: vec![],
                unbound: set![a, b, c],
                ty: Type::fun(
                    vec![x_ty],
                    Type::fun(
                        vec![y_ty],
                        Type::fun(vec![Type::RigidVar(a)], Type::RigidVar(c))
                    )
                )
            }
        )
    }

    #[test]
    fn infer_fails() {
        let x = Var(0);
        let ast = call(
            fndef(
                vec![x],
                call(Expr::Ident(x), vec![Expr::Nil(NilLit { span: SPAN })]),
            ),
            vec![Expr::Nil(NilLit { span: SPAN })],
        );

        let state = &mut State::new();
        let mut ctx = Context::new(state);
        let res = TypeInference::new(&mut ctx).infer(ast);
        assert_eq!(
            res,
            Err(Error::TypeNotEqual {
                expected: Type::fun(
                    vec![Type::NIL],
                    Type::UnifiableVar(UnifiableVar(0))
                ),
                got: Type::fun(
                    vec![Type::fun(
                        vec![Type::NIL],
                        Type::UnifiableVar(UnifiableVar(3))
                    )],
                    Type::UnifiableVar(UnifiableVar(2))
                )
            })
        )
    }

    #[test]
    fn infers_generalized_def() {
        let id = Var(0);
        let x = Var(1);
        let idexpr = fndef(vec![x], Expr::Ident(x));
        let type_expr_pair = vec![
            (Expr::Bool(bool_lit()), Expr::Bool(bool_lit()), Type::BOOL),
            (Expr::Nil(nil_lit()), Expr::Nil(nil_lit()), Type::NIL),
        ];

        let state = &mut State::new();
        let mut ctx = Context::new(state);
        ctx.vars_mut().add_scheme(
            id,
            TypeScheme {
                unbound: set![RigidVar(3)],
                constraints: vec![],
                ty: Type::fun(
                    vec![Type::RigidVar(RigidVar(0))],
                    Type::RigidVar(RigidVar(0)),
                ),
            },
        );

        let mut inferer = TypeInference::new(&mut ctx);

        for (var_expr, ty_expr, ty) in type_expr_pair {
            let idcall = call(idexpr.clone(), vec![var_expr]);
            let (expr, scheme) =
                inferer.infer_with_env(idcall).expect("Inference to succeed");

            let typed_x = TypedVar(x, ty.clone());
            let typed_id = fndef(vec![typed_x.clone()], Expr::Ident(typed_x));

            assert_eq!(expr, call(typed_id, vec![ty_expr]));
            assert_eq!(
                scheme,
                TypeScheme { unbound: set![], constraints: vec![], ty: ty }
            );
        }
    }

    #[test]
    fn infers_partial_scheme() {
        // func(a, b) -> nil;
        //
        // This `Var` and the rigid variables aren't inferred, but are set by the scheme,
        // and thus there value is customizable.
        let func = Var(u32::MAX);
        let rig_a = RigidVar(u32::MAX);
        let rig_b = RigidVar(u32::MAX - 1);
        let func_type = TypeScheme {
            unbound: set![rig_a, rig_b],
            constraints: vec![],
            ty: Type::fun(
                vec![Type::RigidVar(rig_a), Type::RigidVar(rig_b)],
                Type::NIL,
            ),
        };

        let state = &mut State::new();
        let mut ctx = Context::new(state);
        ctx.vars_mut().add_scheme(func, func_type);

        // fn(a) -> func(a, 1);
        //
        // This `Var` also doesn't matter, but cannot conflict with any other vars, but
        // is also customizable.
        let param_a = Var(u32::MAX - 1);
        let def = fndef(
            vec![param_a],
            call(
                Expr::Ident(func),
                vec![Expr::Ident(param_a), Expr::Nil(nil_lit())],
            ),
        );

        let mut inferer = TypeInference::new(&mut ctx);
        let (expr, scheme) =
            inferer.infer_with_env(def).expect("Inference to succeed");

        // This rigid variables is inferred, and thus starts at 0
        let rigvar_a = RigidVar(0);
        let typeof_a = Type::RigidVar(rigvar_a);
        let typed_a = TypedVar(param_a, typeof_a.clone());

        let typed_func =
            Type::fun(vec![typeof_a.clone(), Type::NIL], Type::NIL);

        assert_eq!(
            expr,
            fndef(
                vec![typed_a.clone()],
                call(
                    Expr::Ident(TypedVar(func, typed_func)),
                    vec![Expr::Ident(typed_a), Expr::Nil(nil_lit())],
                ),
            )
        );

        assert_eq!(
            scheme,
            TypeScheme {
                unbound: set![rigvar_a],
                constraints: vec![],
                ty: Type::fun(vec![Type::RigidVar(rigvar_a)], Type::NIL),
            }
        );
    }

    #[test]
    fn checks_int() {
        let state = &mut State::new();
        let mut ctx = Context::new(state);
        let mut checker = TypeInference::new(&mut ctx);

        let lit = i64_lit();

        let out = checker.check_expr(Expr::I64(lit.clone()), Type::INT);

        assert_eq!(out, Qualified::unconstrained(Expr::I64(lit)))
    }

    #[test]
    fn checks_double() {
        let state = &mut State::new();
        let mut ctx = Context::new(state);
        let mut checker = TypeInference::new(&mut ctx);

        let lit = f64_lit();

        let out = checker.check_expr(Expr::F64(lit.clone()), Type::DOUBLE);

        assert_eq!(out, Qualified::unconstrained(Expr::F64(lit)))
    }

    #[test]
    fn checks_usv() {
        let state = &mut State::new();
        let mut ctx = Context::new(state);
        let mut checker = TypeInference::new(&mut ctx);

        let lit = char_lit();

        let out = checker.check_expr(Expr::Char(lit.clone()), Type::USV);

        assert_eq!(out, Qualified::unconstrained(Expr::Char(lit)))
    }

    #[test]
    fn checks_string() {
        let state = &mut State::new();
        let mut ctx = Context::new(state);
        let mut checker = TypeInference::new(&mut ctx);

        let lit = str_lit();

        let out = checker.check_expr(
            Expr::String(lit.clone()),
            Type::List(Box::new(Type::USV)),
        );

        assert_eq!(out, Qualified::unconstrained(Expr::String(lit)),);
    }

    #[test]
    fn checks_fndef() {
        let state = &mut State::new();
        let mut ctx = Context::new(state);
        let mut checker = TypeInference::new(&mut ctx);

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
            ret_type: Box::new(Type::UnifiableVar(UnifiableVar(0))),
        });

        let out = checker.check_expr(Expr::FnDef(Box::new(def)), typ);

        let uvar = |n| Type::UnifiableVar(UnifiableVar(n));

        assert_eq!(
            out,
            Qualified::constrained(
                Expr::FnDef(Box::new(FnDef {
                    args: vec![
                        TypedVar(Var(0), Type::UnifiableVar(UnifiableVar(0))),
                        TypedVar(Var(1), Type::UnifiableVar(UnifiableVar(1)))
                    ],
                    body: Expr::Ident(TypedVar(
                        Var(0),
                        Type::UnifiableVar(UnifiableVar(0))
                    )),
                    recursive: false,
                    span: SPAN
                })),
                vec![
                    Constraint::TypeEqual { expected: uvar(2), got: uvar(0) },
                    Constraint::TypeEqual {
                        expected: Type::fun(vec![uvar(0), uvar(1)], uvar(0)),
                        got: Type::fun(vec![uvar(0), uvar(1)], uvar(2)),
                    },
                ],
            ),
        );
    }

    #[test]
    fn checks_fndef_refined() {
        let state = &mut State::new();
        let mut ctx = Context::new(state);
        let mut checker = TypeInference::new(&mut ctx);

        let def = FnDef {
            args: vec![Var(0), Var(1)],
            body: Expr::Ident(Var(0)),
            recursive: false,
            span: SPAN,
        };

        let typ = Type::Fun(FnType {
            args: vec![Type::INT, Type::DOUBLE],
            ret_type: Box::new(Type::INT),
        });

        let out = checker.check_expr(Expr::FnDef(Box::new(def)), typ);

        let uvar = |n| Type::UnifiableVar(UnifiableVar(n));

        assert_eq!(
            out,
            Qualified::constrained(
                Expr::FnDef(Box::new(FnDef {
                    args: vec![
                        TypedVar(Var(0), uvar(0)),
                        TypedVar(Var(1), uvar(1))
                    ],
                    body: Expr::Ident(TypedVar(Var(0), uvar(0))),
                    recursive: false,
                    span: SPAN
                })),
                vec![
                    Constraint::TypeEqual { expected: uvar(2), got: uvar(0) },
                    Constraint::TypeEqual {
                        expected: Type::fun(
                            vec![Type::INT, Type::DOUBLE],
                            Type::INT
                        ),
                        got: Type::fun(vec![uvar(0), uvar(1)], uvar(2))
                    }
                ],
            ),
        );
    }
}
