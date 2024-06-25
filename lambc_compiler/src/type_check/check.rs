use im::HashMap;
use lambc_parse::{Expr, FnDef};

use crate::name_res::Var;

use super::{CheckRes, Constraint, FnType, Type, TypeInference, TypedVar};

impl TypeInference {
    pub(super) fn check_expr(
        &mut self,
        env: HashMap<Var, Type>,
        expr: Expr<Var>,
        typ: Type,
    ) -> CheckRes<Expr<TypedVar>> {
        match (expr, typ) {
            (Expr::Nil(n), Type::Nil) => CheckRes::empty(Expr::Nil(n)),
            (Expr::I64(i), Type::Int) => CheckRes::empty(Expr::I64(i)),
            (Expr::Char(c), Type::Usv) => CheckRes::empty(Expr::Char(c)),
            (Expr::Bool(b), Type::Bool) => CheckRes::empty(Expr::Bool(b)),
            (Expr::F64(f), Type::Double) => CheckRes::empty(Expr::F64(f)),
            (Expr::String(s), Type::List(e)) if *e == Type::Usv => {
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
        mut env: HashMap<Var, Type>,
        def: FnDef<Var>,
        typ: FnType,
    ) -> CheckRes<Expr<TypedVar>> {
        // TODO: Issue type error instead of panic if arg count doesn't match
        assert_eq!(def.args.len(), typ.args.len());
        let mut new_args = Vec::with_capacity(typ.args.len());
        for (ty, arg) in typ.args.into_iter().zip(def.args.into_iter()) {
            env.insert(arg, ty.clone());
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
    use im::HashMap;
    use pretty_assertions::assert_eq;

    use lambc_parse::{
        CharLit, CharText, Expr, F64Lit, FnDef, I64Base, I64Lit, Span, StrLit,
        StrText,
    };

    use super::{Type, TypeInference};
    use crate::{
        name_res::Var,
        type_check::{
            CheckRes as GenWith, Constraint, FnType, TypeVar, TypedVar,
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

        let out = checker.check_expr(
            HashMap::new(),
            Expr::I64(lit.clone()),
            Type::Int,
        );

        assert_eq!(out, GenWith::empty(Expr::I64(lit)))
    }

    #[test]
    fn checks_double() {
        let mut checker = TypeInference::new();

        let lit = f64_lit();

        let out = checker.check_expr(
            HashMap::new(),
            Expr::F64(lit.clone()),
            Type::Double,
        );

        assert_eq!(out, GenWith::empty(Expr::F64(lit)))
    }

    #[test]
    fn checks_usv() {
        let mut checker = TypeInference::new();

        let lit = char_lit();

        let out = checker.check_expr(
            HashMap::new(),
            Expr::Char(lit.clone()),
            Type::Usv,
        );

        assert_eq!(out, GenWith::empty(Expr::Char(lit)))
    }

    #[test]
    fn checks_string() {
        let mut checker = TypeInference::new();

        let lit = str_lit();

        let out = checker.check_expr(
            HashMap::new(),
            Expr::String(lit.clone()),
            Type::List(Box::new(Type::Usv)),
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
            args: vec![Type::Var(TypeVar(0)), Type::Var(TypeVar(1))],
            ret_type: Box::new(Type::Var(TypeVar(0))),
        });

        let out = checker.check_expr(
            HashMap::new(),
            Expr::FnDef(Box::new(def)),
            typ,
        );

        assert_eq!(
            out,
            GenWith::new(
                vec![Constraint::TypeEqual {
                    expected: Type::Var(TypeVar(0)),
                    got: Type::Var(TypeVar(0))
                }],
                Expr::FnDef(Box::new(FnDef {
                    args: vec![
                        TypedVar(Var(0), Type::Var(TypeVar(0))),
                        TypedVar(Var(1), Type::Var(TypeVar(1)))
                    ],
                    body: Expr::Ident(TypedVar(Var(0), Type::Var(TypeVar(0)))),
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
            args: vec![Type::Int, Type::Double],
            ret_type: Box::new(Type::Int),
        });

        let out = checker.check_expr(
            HashMap::new(),
            Expr::FnDef(Box::new(def)),
            typ,
        );

        assert_eq!(
            out,
            GenWith::new(
                vec![Constraint::TypeEqual {
                    expected: Type::Int,
                    got: Type::Int
                }],
                Expr::FnDef(Box::new(FnDef {
                    args: vec![
                        TypedVar(Var(0), Type::Int),
                        TypedVar(Var(1), Type::Double)
                    ],
                    body: Expr::Ident(TypedVar(Var(0), Type::Int)),
                    recursive: false,
                    span: SPAN
                })),
            ),
        );
    }
}
