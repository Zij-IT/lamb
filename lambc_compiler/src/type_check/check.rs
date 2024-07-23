use lambc_parse::{Expr, FnDef};

use super::{
    env::Env, CheckRes, Constraint, FnType, Type, TypeInference, TypedVar,
};
use crate::name_res::Var;

impl TypeInference {
    pub(super) fn check_expr(
        &mut self,
        env: Env,
        expr: Expr<Var>,
        typ: Type,
    ) -> CheckRes<Expr<TypedVar>> {
        match (expr, typ) {
            (Expr::Nil(n), Type::NIL) => CheckRes::empty(Expr::Nil(n)),
            (Expr::I64(i), Type::INT) => CheckRes::empty(Expr::I64(i)),
            (Expr::Char(c), Type::USV) => CheckRes::empty(Expr::Char(c)),
            (Expr::Bool(b), Type::BOOL) => CheckRes::empty(Expr::Bool(b)),
            (Expr::F64(f), Type::DOUBLE) => CheckRes::empty(Expr::F64(f)),
            (Expr::String(s), Type::List(e)) if *e == Type::USV => {
                CheckRes::empty(Expr::String(s))
            }
            (Expr::FnDef(def), Type::Fun(typ)) => {
                self.check_fndef(env, *def, typ)
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

    pub(super) fn check_fndef(
        &mut self,
        mut env: Env,
        def: FnDef<Var>,
        typ: FnType,
    ) -> CheckRes<Expr<TypedVar>> {
        // TODO: Issue type error instead of panic if arg count doesn't match
        assert_eq!(def.args.len(), typ.args.len());
        let mut new_args = Vec::with_capacity(typ.args.len());
        for (ty, arg) in typ.args.into_iter().zip(def.args.into_iter()) {
            env.add_type(arg, ty.clone());
            new_args.push(TypedVar(arg, ty));
        }
        let bodyres = self.check_expr(env, def.body, *typ.ret_type);
        CheckRes::new(
            bodyres.cons,
            Expr::FnDef(Box::new(FnDef {
                args: new_args,
                body: bodyres.ast,
                recursive: def.recursive,
                span: def.span,
            })),
        )
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
            env::Env, CheckRes as GenWith, Constraint, FnType, TyUniVar,
            TypedVar,
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

        assert_eq!(out, GenWith::empty(Expr::I64(lit)))
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

        assert_eq!(out, GenWith::empty(Expr::F64(lit)))
    }

    #[test]
    fn checks_usv() {
        let mut checker = TypeInference::new();

        let lit = char_lit();

        let out =
            checker.check_expr(Env::new(), Expr::Char(lit.clone()), Type::USV);

        assert_eq!(out, GenWith::empty(Expr::Char(lit)))
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

        assert_eq!(out, GenWith::empty(Expr::String(lit)),);
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
            args: vec![Type::Var(TyUniVar(0)), Type::Var(TyUniVar(1))],
            ret_type: Box::new(Type::Var(TyUniVar(0))),
        });

        let out =
            checker.check_expr(Env::new(), Expr::FnDef(Box::new(def)), typ);

        assert_eq!(
            out,
            GenWith::new(
                vec![Constraint::TypeEqual {
                    expected: Type::Var(TyUniVar(0)),
                    got: Type::Var(TyUniVar(0))
                }],
                Expr::FnDef(Box::new(FnDef {
                    args: vec![
                        TypedVar(Var(0), Type::Var(TyUniVar(0))),
                        TypedVar(Var(1), Type::Var(TyUniVar(1)))
                    ],
                    body: Expr::Ident(TypedVar(
                        Var(0),
                        Type::Var(TyUniVar(0))
                    )),
                    recursive: false,
                    span: SPAN
                })),
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

        assert_eq!(
            out,
            GenWith::new(
                vec![Constraint::TypeEqual {
                    expected: Type::INT,
                    got: Type::INT
                }],
                Expr::FnDef(Box::new(FnDef {
                    args: vec![
                        TypedVar(Var(0), Type::INT),
                        TypedVar(Var(1), Type::DOUBLE)
                    ],
                    body: Expr::Ident(TypedVar(Var(0), Type::INT)),
                    recursive: false,
                    span: SPAN
                })),
            ),
        );
    }
}
