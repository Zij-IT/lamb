use crate::name_res::Var;

use super::{Qualified, Type, TypeScheme};

/// A map from `Var` to their `Type(Scheme)`.
#[derive(Clone)]
pub struct Env {
    inner: im::HashMap<Var, TypeScheme>,
}

impl Env {
    /// Constructs a new empty `Env`
    pub fn new() -> Self {
        Self { inner: Default::default() }
    }

    /// Returns the `TypeScheme` of `Var`
    pub fn type_of(&self, v: Var) -> TypeScheme {
        self.inner[&v].clone()
    }

    /// Adds a `Var` to the map which itself has no unbound variables.
    /// This is used for local variables, which are not permitted to have
    /// let-generalization.
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

    /// Adds a `Var` to the map which may have unbound variables. This is
    /// used on top-level variables, which are permitted to be generic over
    /// types.
    pub fn add_scheme(
        &mut self,
        v: Var,
        scheme: TypeScheme,
    ) -> Option<TypeScheme> {
        self.inner.insert(v, scheme)
    }
}
