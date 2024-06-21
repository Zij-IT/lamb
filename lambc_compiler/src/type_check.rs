use lambc_parse::Module;

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

type Mod<V> = Module<V, PathRef>;

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
}
