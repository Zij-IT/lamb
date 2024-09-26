use lambc_parse::Expr;

use super::{env::Env, Constraint, Qualified, Type, TypeInference, TypedVar};
use crate::name_res::Var;

impl TypeInference {
    /// Checks the type of an expression, returning an [`Expr`] with the
    /// requirements for the expression to type-check.
    pub(super) fn check_expr(
        &mut self,
        env: Env,
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
                let (mut out, actual_ty) = self.infer_expr(env, expr);
                out.cons.push(Constraint::TypeEqual {
                    expected: expected_ty,
                    got: actual_ty,
                });

                out
            }
        }
    }
}
#[cfg(test)]
mod tests {
    use lambc_parse::{
        CharLit, CharText, Expr, F64Lit, FnDef, I64Base, I64Lit, Span, StrLit,
        StrText,
    };
    use pretty_assertions::assert_eq;

    use super::{Type, TypeInference};
    use crate::{
        name_res::Var,
        type_check::{
            env::Env, Constraint, FnType, Qualified as GenWith, TypedVar,
            UnifiableVar,
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

    fn char_lit() -> CharLit {
        CharLit {
            text: Some(CharText { inner: "f".into(), span: SPAN }),
            span: SPAN,
        }
    }

    #[test]
    fn checks_int() {
        let mut checker = TypeInference::new();

        let lit = i64_lit();

        let out =
            checker.check_expr(Env::new(), Expr::I64(lit.clone()), Type::INT);

        assert_eq!(out, GenWith::unconstrained(Expr::I64(lit)))
    }

    #[test]
    fn checks_double() {
        let mut checker = TypeInference::new();

        let lit = f64_lit();

        let out = checker.check_expr(
            Env::new(),
            Expr::F64(lit.clone()),
            Type::DOUBLE,
        );

        assert_eq!(out, GenWith::unconstrained(Expr::F64(lit)))
    }

    #[test]
    fn checks_usv() {
        let mut checker = TypeInference::new();

        let lit = char_lit();

        let out =
            checker.check_expr(Env::new(), Expr::Char(lit.clone()), Type::USV);

        assert_eq!(out, GenWith::unconstrained(Expr::Char(lit)))
    }

    #[test]
    fn checks_string() {
        let mut checker = TypeInference::new();

        let lit = str_lit();

        let out = checker.check_expr(
            Env::new(),
            Expr::String(lit.clone()),
            Type::List(Box::new(Type::USV)),
        );

        assert_eq!(out, GenWith::unconstrained(Expr::String(lit)),);
    }

    #[test]
    fn checks_fndef() {
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
            ret_type: Box::new(Type::UnifiableVar(UnifiableVar(0))),
        });

        let out =
            checker.check_expr(Env::new(), Expr::FnDef(Box::new(def)), typ);

        let uvar = |n| Type::UnifiableVar(UnifiableVar(n));

        assert_eq!(
            out,
            GenWith::constrained(
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
        let mut checker = TypeInference::new();

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

        let out =
            checker.check_expr(Env::new(), Expr::FnDef(Box::new(def)), typ);

        let uvar = |n| Type::UnifiableVar(UnifiableVar(n));

        assert_eq!(
            out,
            GenWith::constrained(
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
