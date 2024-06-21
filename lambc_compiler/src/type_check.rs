use im::HashMap;
use lambc_parse::{Expr, Module};

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
    Fun { args: Vec<Self>, ret_type: Box<Self> },
}

type Mod<V> = Module<V, PathRef>;

type Env = HashMap<Var, Type>;

pub struct TypeChecker<'s> {
    state: &'s mut State,
}

impl<'s> TypeChecker<'s> {
    pub fn new(state: &'s mut State) -> Self {
        Self { state }
    }

    pub fn check_modules(
        &mut self,
        _modules: Vec<Mod<Var>>,
    ) -> Vec<Mod<TypedVar>> {
        todo!()
    }

    fn check_expr(
        &mut self,
        _env: Env,
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
            _ => todo!(),
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
            _ => todo!(),
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
struct CheckRes<T> {
    cons: Vec<Constraint>,
    ast: T,
}

impl<T> CheckRes<T> {
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

    use lambc_parse::{
        CharLit, CharText, Expr, F64Lit, I64Base, I64Lit, Span, StrLit,
        StrText,
    };

    use super::{Type, TypeChecker};
    use crate::{
        name_res::Var,
        type_check::{CheckRes as GenWith, TypeVar, TypedVar},
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
}
