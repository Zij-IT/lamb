use crate::name_res::Var;

use super::Type;

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

    pub fn add_type(&mut self, v: Var, t: Type) -> Option<Type> {
        self.inner.insert(v, t)
    }
}
