use crate::name_res::Var;

use super::{Qualified, Type};

#[derive(Clone)]
pub struct Env {
    inner: im::HashMap<Var, Type>,
}

impl Env {
    pub fn new() -> Self {
        Self { inner: Default::default() }
    }

    pub fn type_of(&self, v: Var) -> Type {
        self.inner[&v].clone()
    }

    pub fn add_type(&mut self, v: Var, t: Qualified<Type>) -> Option<Type> {
        assert!(t.cons.is_empty());
        self.inner.insert(v, t.item)
    }
}
