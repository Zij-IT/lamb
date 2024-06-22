use im::HashMap;
use lambc_parse::{Expr, FnDef, Module};

use crate::{name_res::Var, PathRef, State};

#[derive(Debug, PartialEq, Clone, Eq)]
pub struct TypedVar(Var, Type);

#[derive(Debug, PartialEq, Clone, Copy, Eq)]
pub struct TypeVar(u32);

#[derive(Debug, PartialEq, Clone, Eq)]
pub enum Type {
    Var(TypeVar),
    Int,
    Nil,
    Usv,
    Bool,
    Double,
    List(Box<Self>),
    Module(Vec<TypedVar>),
    Fun(FnType),
}

#[derive(Debug, PartialEq, Clone, Eq)]
struct FnType {
    args: Vec<Type>,
    ret_type: Box<Type>,
}

type Mod<V> = Module<V, PathRef>;

type Env = HashMap<Var, Type>;

pub struct TypeChecker<'s> {
    state: &'s mut State,
    types: u32,
}

impl<'s> TypeChecker<'s> {
    pub fn new(state: &'s mut State) -> Self {
        Self { state, types: 0 }
    }

    pub fn check_modules(
        &mut self,
        _modules: Vec<Mod<Var>>,
    ) -> Vec<Mod<TypedVar>> {
        todo!()
    }

    fn check_expr(
        &mut self,
        env: Env,
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
                self.check_fndef(env, def, typ)
            }

            (expr, expected_ty) => {
                let (mut out, actual_ty) = self.infer_expr(env, expr);
                out.cons.push(Constraint::TypeEqual(expected_ty, actual_ty));
                out
            }
        }
    }

    fn infer_expr(
        &mut self,
        env: Env,
        expr: Expr<Var>,
    ) -> (CheckRes<Expr<TypedVar>>, Type) {
        match expr {
            // The easy cases!
            Expr::Nil(n) => (CheckRes::empty(Expr::Nil(n)), Type::Nil),
            Expr::I64(i) => (CheckRes::empty(Expr::I64(i)), Type::Int),
            Expr::F64(f) => (CheckRes::empty(Expr::F64(f)), Type::Double),
            Expr::Char(c) => (CheckRes::empty(Expr::Char(c)), Type::Usv),
            Expr::Bool(b) => (CheckRes::empty(Expr::Bool(b)), Type::Bool),
            Expr::Ident(i) => {
                let ty = env[&i].clone();
                (CheckRes::empty(Expr::Ident(TypedVar(i, ty.clone()))), ty)
            }
            Expr::String(s) => (
                CheckRes::empty(Expr::String(s)),
                Type::List(Box::new(Type::Usv)),
            ),
            Expr::FnDef(fndef) => self.infer_fndef(env, *fndef),
            _ => todo!(),
        }
    }

    fn infer_fndef(
        &mut self,
        mut env: HashMap<Var, Type>,
        fndef: FnDef<Var>,
    ) -> (CheckRes<Expr<TypedVar>>, Type) {
        let FnDef { args, body, recursive, span } = fndef;
        let mut new_args = Vec::with_capacity(args.len());
        let mut typs = Vec::with_capacity(args.len());

        for arg in &args {
            let typ = self.fresh_ty_var();
            typs.push(Type::Var(typ));
            new_args.push(TypedVar(*arg, Type::Var(typ)));
            env.insert(*arg, Type::Var(typ));
        }

        let (body_out, body_typ) = self.infer_expr(env, body);
        (
            CheckRes {
                cons: body_out.cons,
                ast: Expr::FnDef(Box::new(FnDef {
                    args: new_args,
                    body: body_out.ast,
                    recursive,
                    span,
                })),
            },
            Type::Fun(FnType { args: typs, ret_type: Box::new(body_typ) }),
        )
    }

    fn check_fndef(
        &mut self,
        mut env: HashMap<Var, Type>,
        def: Box<FnDef<Var>>,
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

    fn fresh_ty_var(&mut self) -> TypeVar {
        self.types += 1;
        TypeVar(self.types - 1)
    }
}

#[derive(Debug, Eq, PartialEq)]
struct CheckRes<T> {
    cons: Vec<Constraint>,
    ast: T,
}

impl<T> CheckRes<T> {
    fn new(cons: Vec<Constraint>, ast: T) -> Self {
        Self { cons, ast }
    }

    fn empty(t: T) -> Self {
        Self { ast: t, cons: vec![] }
    }
}

#[derive(Debug, Eq, PartialEq)]
enum Constraint {
    TypeEqual(Type, Type),
}

#[cfg(test)]
mod tests {
    use im::HashMap;
    use pretty_assertions::assert_eq;

    use lambc_parse::{
        CharLit, CharText, Expr, F64Lit, FnDef, I64Base, I64Lit, Span, StrLit,
        StrText,
    };

    use super::{Type, TypeChecker};
    use crate::{
        name_res::Var,
        type_check::{
            CheckRes as GenWith, Constraint, FnType, TypeVar, TypedVar,
        },
        State,
    };

    #[test]
    fn infers_int() {
        let mut state = State::default();
        let mut checker = TypeChecker::new(&mut state);

        let lit = I64Lit {
            base: I64Base::Dec,
            value: "2".into(),
            span: Span::new(0, 0),
        };

        let out = checker.infer_expr(HashMap::new(), Expr::I64(lit.clone()));
        assert_eq!(out, (GenWith::empty(Expr::I64(lit)), Type::Int))
    }

    #[test]
    fn checks_int() {
        let mut state = State::default();
        let mut checker = TypeChecker::new(&mut state);

        let lit = I64Lit {
            base: I64Base::Dec,
            value: "2".into(),
            span: Span::new(0, 0),
        };

        let out = checker.check_expr(
            HashMap::new(),
            Expr::I64(lit.clone()),
            Type::Int,
        );

        assert_eq!(out, GenWith::empty(Expr::I64(lit)))
    }

    #[test]
    fn infers_double() {
        let mut state = State::default();
        let mut checker = TypeChecker::new(&mut state);

        let lit = F64Lit { value: "2.0".into(), span: Span::new(0, 0) };
        let out = checker.infer_expr(HashMap::new(), Expr::F64(lit.clone()));
        assert_eq!(out, (GenWith::empty(Expr::F64(lit)), Type::Double));
    }

    #[test]
    fn checks_double() {
        let mut state = State::default();
        let mut checker = TypeChecker::new(&mut state);

        let lit = F64Lit { value: "2.0".into(), span: Span::new(0, 0) };

        let out = checker.check_expr(
            HashMap::new(),
            Expr::F64(lit.clone()),
            Type::Double,
        );

        assert_eq!(out, GenWith::empty(Expr::F64(lit)))
    }

    #[test]
    fn infers_usv() {
        let mut state = State::default();
        let mut checker = TypeChecker::new(&mut state);

        let lit = CharLit {
            text: Some(CharText { inner: "f".into(), span: Span::new(0, 0) }),
            span: Span::new(0, 0),
        };

        let out = checker.infer_expr(HashMap::new(), Expr::Char(lit.clone()));
        assert_eq!(out, (GenWith::empty(Expr::Char(lit)), Type::Usv));
    }

    #[test]
    fn checks_usv() {
        let mut state = State::default();
        let mut checker = TypeChecker::new(&mut state);

        let lit = CharLit {
            text: Some(CharText { inner: "f".into(), span: Span::new(0, 0) }),
            span: Span::new(0, 0),
        };

        let out = checker.check_expr(
            HashMap::new(),
            Expr::Char(lit.clone()),
            Type::Usv,
        );

        assert_eq!(out, GenWith::empty(Expr::Char(lit)))
    }

    #[test]
    fn infers_string() {
        let mut state = State::default();
        let mut checker = TypeChecker::new(&mut state);

        let lit = StrLit {
            text: Some(StrText { inner: "a".into(), span: Span::new(0, 0) }),
            span: Span::new(0, 0),
        };

        let out =
            checker.infer_expr(HashMap::new(), Expr::String(lit.clone()));

        assert_eq!(
            out,
            (
                GenWith::empty(Expr::String(lit)),
                Type::List(Box::new(Type::Usv))
            )
        );
    }

    #[test]
    fn checks_string() {
        let mut state = State::default();
        let mut checker = TypeChecker::new(&mut state);

        let lit = StrLit {
            text: Some(StrText { inner: "a".into(), span: Span::new(0, 0) }),
            span: Span::new(0, 0),
        };

        let out = checker.check_expr(
            HashMap::new(),
            Expr::String(lit.clone()),
            Type::List(Box::new(Type::Usv)),
        );

        assert_eq!(out, GenWith::empty(Expr::String(lit)),);
    }

    #[test]
    fn infers_var() {
        let mut state = State::default();
        let mut checker = TypeChecker::new(&mut state);

        let typ = Type::Var(TypeVar(0));
        let var = Var(0);
        let env = HashMap::from([(var, typ.clone())].as_slice());
        let out = checker.infer_expr(env, Expr::Ident(var));

        assert_eq!(
            out,
            (GenWith::empty(Expr::Ident(TypedVar(var, typ.clone()))), typ)
        );
    }

    #[test]
    fn infers_fndef() {
        let mut state = State::default();
        let mut checker = TypeChecker::new(&mut state);

        let def = FnDef {
            args: vec![Var(0), Var(1)],
            body: Expr::Ident(Var(0)),
            recursive: false,
            span: Span::new(0, 0),
        };

        let typ = Type::Fun(FnType {
            args: vec![Type::Var(TypeVar(0)), Type::Var(TypeVar(1))],
            ret_type: Box::new(Type::Var(TypeVar(0))),
        });

        let out =
            checker.infer_expr(HashMap::new(), Expr::FnDef(Box::new(def)));

        assert_eq!(
            out,
            (
                GenWith::empty(Expr::FnDef(Box::new(FnDef {
                    args: vec![
                        TypedVar(Var(0), Type::Var(TypeVar(0))),
                        TypedVar(Var(1), Type::Var(TypeVar(1)))
                    ],
                    body: Expr::Ident(TypedVar(Var(0), Type::Var(TypeVar(0)))),
                    recursive: false,
                    span: Span::new(0, 0)
                }))),
                typ
            )
        );
    }

    #[test]
    fn checks_fndef() {
        let mut state = State::default();
        let mut checker = TypeChecker::new(&mut state);

        let def = FnDef {
            args: vec![Var(0), Var(1)],
            body: Expr::Ident(Var(0)),
            recursive: false,
            span: Span::new(0, 0),
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
                vec![Constraint::TypeEqual(
                    Type::Var(TypeVar(0)),
                    Type::Var(TypeVar(0))
                )],
                Expr::FnDef(Box::new(FnDef {
                    args: vec![
                        TypedVar(Var(0), Type::Var(TypeVar(0))),
                        TypedVar(Var(1), Type::Var(TypeVar(1)))
                    ],
                    body: Expr::Ident(TypedVar(Var(0), Type::Var(TypeVar(0)))),
                    recursive: false,
                    span: Span::new(0, 0)
                })),
            ),
        );
    }

    #[test]
    fn checks_fndef_refined() {
        let mut state = State::default();
        let mut checker = TypeChecker::new(&mut state);

        let def = FnDef {
            args: vec![Var(0), Var(1)],
            body: Expr::Ident(Var(0)),
            recursive: false,
            span: Span::new(0, 0),
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
                vec![Constraint::TypeEqual(Type::Int, Type::Int)],
                Expr::FnDef(Box::new(FnDef {
                    args: vec![
                        TypedVar(Var(0), Type::Int),
                        TypedVar(Var(1), Type::Double)
                    ],
                    body: Expr::Ident(TypedVar(Var(0), Type::Int)),
                    recursive: false,
                    span: Span::new(0, 0)
                })),
            ),
        );
    }
}
