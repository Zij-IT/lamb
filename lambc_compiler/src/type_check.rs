mod check;
mod inference;
mod substitution;
mod unification;

use ena::unify::InPlaceUnificationTable;

use crate::{name_res::Var, State};

#[derive(Debug, PartialEq, Clone, Eq)]
pub struct TypedVar(Var, Type);

#[derive(Debug, PartialEq, Clone, Copy, Eq, Hash)]
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
pub struct FnType {
    args: Vec<Type>,
    ret_type: Box<Type>,
}

pub struct TypeChecker<'s> {
    uni_table: InPlaceUnificationTable<TypeVar>,
    state: &'s mut State,
}

impl<'s> TypeChecker<'s> {
    pub fn new(state: &'s mut State) -> Self {
        Self { uni_table: Default::default(), state }
    }

    pub fn dostuff(&mut self) {
        self.check_expr(todo(), todo(), todo());
        _ = self.unification(todo());
        _ = self.substitute_expr(todo());
    }
}

fn todo<T>() -> T {
    todo!()
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
    ImplAdd(Type),
    ImplSub(Type),
    ImplDiv(Type),
    ImplMul(Type),
    ImplNegate(Type),
    TypeEqual { expected: Type, got: Type },
}
