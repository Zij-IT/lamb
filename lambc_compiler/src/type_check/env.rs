use crate::name_res::Var;

use super::{Qualified, Type, TypeScheme};

#[derive(Clone)]
pub struct Env {
    inner: im::HashMap<Var, TypeScheme>,
}

impl Env {
    pub fn new() -> Self {
        Self { inner: Default::default() }
    }

    pub fn type_of(&self, v: Var) -> TypeScheme {
        self.inner[&v].clone()
    }

    pub fn add_type(
        &mut self,
        v: Var,
        t: Qualified<Type>,
    ) -> Option<TypeScheme> {
        let scheme = TypeScheme {
            unbound: std::collections::HashSet::new(),
            constraints: t.cons,
            ty: t.item,
        };

        self.inner.insert(v, scheme)
    }

    pub fn add_scheme(
        &mut self,
        v: Var,
        scheme: TypeScheme,
    ) -> Option<TypeScheme> {
        self.inner.insert(v, scheme)
    }
}
