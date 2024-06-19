use std::collections::HashMap;

use lambc_parse::{Expr, Module};

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
        modules: &[Mod<Var>],
    ) -> HashMap<PathRef, Environment> {
        todo!()
    }

    fn check_module(
        &mut self,
        envs: &mut Environment,
        module: Mod<Var>,
    ) -> Mod<TypedVar> {
        let Mod { exports, imports, items, path, span } = module;
        todo!()
    }

}

type Mod<V> = Module<V, PathRef>;

type Environment = HashMap<Var, Typ>;
