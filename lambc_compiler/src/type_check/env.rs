use crate::name_res::Var;

use super::{Error, Qualified, Result, Type, TypeScheme};

/// A map from `Var` to their `Type(Scheme)`.
#[derive(Clone)]
pub struct VarEnv {
    inner: im::HashMap<Var, TypeScheme>,
}

impl VarEnv {
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

/// A map from `Var` to the actual Lamb `Type` being referred to.
#[derive(Clone, Default)]
pub struct TypeEnv {
    inner: im::HashMap<Var, Type>,
}

impl TypeEnv {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn add_type(&mut self, var: Var, ty: Type) {
        assert!(
            self.inner.insert(var, ty).is_none(),
            "A var had it's type redefined... this should not be possible"
        );
    }

    pub fn get_type(&self, var: Var) -> Result<Type> {
        self.inner.get(&var).cloned().ok_or(Error::UnknownType)
    }
}
