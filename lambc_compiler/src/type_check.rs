use im::HashMap;
use lambc_parse::{
    Binary, BinaryOp, Block, Call, Define, Else, Expr, ExprStatement, FnDef,
    Group, If, IfCond, Index, List, Module, Statement, Unary, UnaryOp,
};

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
pub struct FnType {
    args: Vec<Type>,
    ret_type: Box<Type>,
}

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
        _modules: Vec<Module<Var, PathRef>>,
    ) -> Vec<Module<TypedVar, PathRef>> {
        todo!()
    }

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
    ImplAdd(Type),
    ImplSub(Type),
    ImplDiv(Type),
    ImplMul(Type),
    ImplNegate(Type),
    TypeEqual { expected: Type, got: Type },
}
