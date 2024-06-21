#![allow(dead_code)]
use std::collections::HashMap;
mod constraint;

use lambc_parse::{Block, Else, Expr, If, IfCond, Item, Module};

use self::constraint::Constraint;
use crate::{name_res::Var, PathRef, State};

#[derive(Debug, PartialEq, Clone)]
pub struct TypedVar(Var, Typ);

#[derive(Debug, PartialEq, Clone, Copy)]
pub struct TypeVar(u32);

#[derive(Debug, PartialEq, Clone)]
pub enum Typ {
    Var(TypeVar),
    Int,
    Nil,
    Usv,
    Bool,
    Double,
    List(Box<Self>),
    Module(Vec<TypedVar>),
    Fun { args: Vec<Typ>, ret_type: Box<Typ> },
}

pub struct TypeChecker<'s> {
    state: &'s mut State,
}

impl<'s> TypeChecker<'s> {
    pub fn new(state: &'s mut State) -> Self {
        Self { state }
    }

    pub fn check_modules(
        &mut self,
        modules: Vec<Mod<Var>>,
    ) -> Vec<Mod<TypedVar>> {
        let mut exported = self.build_module_envs(&modules);
        modules
            .into_iter()
            .map(|i| self.check_module(exported.get_mut(&i.path).unwrap(), i))
            .collect()
    }

    fn build_module_envs(
        &mut self,
        _modules: &[Mod<Var>],
    ) -> HashMap<PathRef, Environment> {
        todo!()
    }

    fn check_module(
        &mut self,
        _envs: &mut Environment,
        _module: Mod<Var>,
    ) -> Mod<TypedVar> {
        todo!()
    }

    fn check(
        &mut self,
        _env: &mut Environment,
        _expr: Expr<Var>,
        _typ: Typ,
    ) -> GenWith<Expr<TypedVar>> {
        todo!()
    }

    fn infer_expr(
        &mut self,
        env: &mut Environment,
        expr: Expr<Var>,
    ) -> (GenWith<Expr<TypedVar>>, Typ) {
        match expr {
            // The easy cases!
            Expr::Nil(n) => (GenWith::empty(Expr::Nil(n)), Typ::Nil),
            Expr::I64(i) => (GenWith::empty(Expr::I64(i)), Typ::Int),
            Expr::F64(f) => (GenWith::empty(Expr::F64(f)), Typ::Double),
            Expr::Char(c) => (GenWith::empty(Expr::Char(c)), Typ::Usv),
            Expr::Bool(b) => (GenWith::empty(Expr::Bool(b)), Typ::Bool),
            Expr::Ident(i) => {
                let ty = env[&i].clone();
                (GenWith::empty(Expr::Ident(TypedVar(i, ty.clone()))), ty)
            }
            Expr::String(s) => (
                GenWith::empty(Expr::String(s)),
                Typ::List(Box::new(Typ::Usv)),
            ),
            // The less easy cases!
            Expr::If(i) => self.infer_if(env, *i),
            Expr::Call(_) => todo!(),
            Expr::Case(_) => todo!(),
            Expr::Path(_) => todo!(),
            Expr::List(_) => todo!(),
            Expr::Index(_) => todo!(),
            Expr::Group(_) => todo!(),
            Expr::FnDef(_) => todo!(),
            Expr::Block(b) => {
                let (gen, typ) = self.infer_block(env, *b);
                (gen.map(|b| Expr::Block(Box::new(b))), typ)
            }
            Expr::Unary(_) => todo!(),
            Expr::Binary(_) => todo!(),
            Expr::Return(_) => todo!(),
        }
    }

    fn infer_item(
        &mut self,
        _env: &mut HashMap<Var, Typ>,
        _item: Item<Var>,
    ) -> (GenWith<Expr<TypedVar>>, Typ) {
        todo!()
    }

    fn infer_if(
        &mut self,
        env: &mut HashMap<Var, Typ>,
        iff: If<Var>,
    ) -> (GenWith<Expr<TypedVar>>, Typ) {
        let If { cond, elif, els_, span } = iff;
        let mut constraints = Vec::new();

        let cond_out = self.check(env, cond.cond, Typ::Bool);
        constraints.extend(cond_out.constraints);
        let (GenWith { constraints: cons, typed_ast: block }, ty) =
            self.infer_block(env, cond.body);

        constraints.extend(cons);

        let mut elifs = Vec::with_capacity(elif.len());
        for ifcond in elif {
            let (cons, ifcond) = self.infer_ifcond(env, ifcond, ty.clone());

            elifs.push(ifcond);
            constraints.extend(cons);
        }

        let els_ = els_.map(|Else { body, span }| {
            let (GenWith { constraints: cons, typed_ast: block }, else_ty) =
                self.infer_block(env, body);

            constraints.extend(cons);
            constraints.push(Constraint::Eq(ty.clone(), else_ty));
            Else { body: block, span }
        });

        let iff = If {
            cond: IfCond { cond: cond_out.typed_ast, body: block, span },
            elif: elifs,
            els_,
            span,
        };

        (GenWith { constraints, typed_ast: Expr::If(Box::new(iff)) }, ty)
    }

    fn infer_ifcond(
        &mut self,
        env: &mut Environment,
        iff: IfCond<Var>,
        parent_ty: Typ,
    ) -> (Vec<Constraint>, IfCond<TypedVar>) {
        let IfCond { cond, body, span } = iff;

        let mut constraints = Vec::new();

        let cond_out = self.check(env, cond, Typ::Bool);
        constraints.extend(cond_out.constraints);

        let (GenWith { constraints: cons, typed_ast: block }, ty) =
            self.infer_block(env, body);

        constraints.extend(cons);
        constraints.push(Constraint::Eq(parent_ty.clone(), ty));

        let iff = IfCond { cond: cond_out.typed_ast, body: block, span };
        (constraints, iff)
    }

    fn infer_block(
        &mut self,
        _env: &mut Environment,
        _block: Block<Var>,
    ) -> (GenWith<Block<TypedVar>>, Typ) {
        todo!()
    }
}

type Mod<V> = Module<V, PathRef>;

type Environment = HashMap<Var, Typ>;

#[derive(Debug, PartialEq)]
struct GenWith<T> {
    constraints: Vec<Constraint>,
    typed_ast: T,
}

impl<T> GenWith<T> {
    pub fn new(constraints: Vec<Constraint>, ast: T) -> Self {
        Self { constraints, typed_ast: ast }
    }

    pub fn empty(ast: T) -> Self {
        Self { constraints: vec![], typed_ast: ast }
    }

    pub fn map<F, U>(self, f: F) -> GenWith<U>
    where
        F: FnOnce(T) -> U,
    {
        GenWith { constraints: self.constraints, typed_ast: f(self.typed_ast) }
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use lambc_parse::{
        CharLit, CharText, Expr, F64Lit, I64Base, I64Lit, Span, StrLit,
        StrText,
    };

    use super::{Typ, TypeChecker};
    use crate::{type_check::GenWith, State};

    #[test]
    fn infers_int() {
        let mut state = State::default();
        let mut checker = TypeChecker::new(&mut state);

        let lit = I64Lit {
            base: I64Base::Dec,
            value: "2".into(),
            span: Span::new(0, 0),
        };

        let out =
            checker.infer_expr(&mut HashMap::new(), Expr::I64(lit.clone()));
        assert_eq!(out, (GenWith::empty(Expr::I64(lit)), Typ::Int))
    }

    #[test]
    fn infers_double() {
        let mut state = State::default();
        let mut checker = TypeChecker::new(&mut state);

        let lit = F64Lit { value: "2.0".into(), span: Span::new(0, 0) };
        let out =
            checker.infer_expr(&mut HashMap::new(), Expr::F64(lit.clone()));
        assert_eq!(out, (GenWith::empty(Expr::F64(lit)), Typ::Double));
    }

    #[test]
    fn infers_usv() {
        let mut state = State::default();
        let mut checker = TypeChecker::new(&mut state);

        let lit = CharLit {
            text: Some(CharText { inner: "f".into(), span: Span::new(0, 0) }),
            span: Span::new(0, 0),
        };

        let out =
            checker.infer_expr(&mut HashMap::new(), Expr::Char(lit.clone()));
        assert_eq!(out, (GenWith::empty(Expr::Char(lit)), Typ::Usv));
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
            checker.infer_expr(&mut HashMap::new(), Expr::String(lit.clone()));

        assert_eq!(
            out,
            (GenWith::empty(Expr::String(lit)), Typ::List(Box::new(Typ::Usv)))
        );
    }
}
