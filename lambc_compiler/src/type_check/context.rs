use ena::unify::InPlaceUnificationTable;

use crate::name_res::Var;

use super::{
    inference::InferenceContext, instantiate::InstantiationContext,
    parsing::ParserContext, substitution::SubstitutionContext,
    unification::UnificationContext, Error, Result, RigidVar, Type, TypeEnv,
    UnifiableVar,
};

pub struct Context {
    types: TypeEnv,
    uni_table: InPlaceUnificationTable<UnifiableVar>,
    next_tyvar: u32,
}

impl Context {
    pub fn new() -> Self {
        Self {
            types: TypeEnv::new(),
            uni_table: InPlaceUnificationTable::new(),
            next_tyvar: 0,
        }
    }

    pub fn new_unif_var(&mut self) -> UnifiableVar {
        self.uni_table.new_key(None)
    }

    pub fn new_rigid_var(&mut self) -> RigidVar {
        self.next_tyvar += 1;
        RigidVar(self.next_tyvar - 1)
    }
}

impl InferenceContext for Context {
    fn new_unif_var(&mut self) -> UnifiableVar {
        Context::new_unif_var(self)
    }
}

impl InferenceContext for &mut Context {
    fn new_unif_var(&mut self) -> UnifiableVar {
        Context::new_unif_var(self)
    }
}

impl UnificationContext for Context {
    fn unify_var_var(
        &mut self,
        v1: UnifiableVar,
        v2: UnifiableVar,
    ) -> Result<()> {
        self.uni_table
            .unify_var_var(v1, v2)
            .map_err(|(e, g)| Error::TypeNotEqual { expected: e, got: g })
    }

    fn unify_var_value(
        &mut self,
        key: UnifiableVar,
        ty: Option<Type>,
    ) -> Result<()> {
        self.uni_table
            .unify_var_value(key, ty)
            .map_err(|(e, g)| Error::TypeNotEqual { expected: e, got: g })
    }

    fn get(&mut self, key: UnifiableVar) -> Option<Type> {
        self.uni_table.probe_value(key)
    }
}

impl UnificationContext for &mut Context {
    fn unify_var_var(
        &mut self,
        v1: UnifiableVar,
        v2: UnifiableVar,
    ) -> Result<()> {
        self.uni_table
            .unify_var_var(v1, v2)
            .map_err(|(e, g)| Error::TypeNotEqual { expected: e, got: g })
    }

    fn unify_var_value(
        &mut self,
        key: UnifiableVar,
        ty: Option<Type>,
    ) -> Result<()> {
        self.uni_table
            .unify_var_value(key, ty)
            .map_err(|(e, g)| Error::TypeNotEqual { expected: e, got: g })
    }

    fn get(&mut self, key: UnifiableVar) -> Option<Type> {
        self.uni_table.probe_value(key)
    }
}

impl SubstitutionContext for Context {
    fn get_value(&mut self, var: UnifiableVar) -> Option<Type> {
        self.uni_table.probe_value(var)
    }

    fn get_root(&mut self, var: UnifiableVar) -> UnifiableVar {
        self.uni_table.find(var)
    }

    fn gen_rigid_var(&mut self) -> RigidVar {
        self.new_rigid_var()
    }
}

impl SubstitutionContext for &mut Context {
    fn get_value(&mut self, var: UnifiableVar) -> Option<Type> {
        self.uni_table.probe_value(var)
    }

    fn get_root(&mut self, var: UnifiableVar) -> UnifiableVar {
        self.uni_table.find(var)
    }

    fn gen_rigid_var(&mut self) -> RigidVar {
        self.new_rigid_var()
    }
}

impl ParserContext for Context {
    fn get_type(&mut self, var: Var) -> Result<Type> {
        self.types.get_type(var)
    }

    fn add_type(&mut self, var: Var, ty: Type) {
        self.types.add_type(var, ty)
    }

    fn new_rigid_var(&mut self) -> RigidVar {
        Context::new_rigid_var(self)
    }
}

impl ParserContext for &mut Context {
    fn get_type(&mut self, var: Var) -> Result<Type> {
        self.types.get_type(var)
    }

    fn add_type(&mut self, var: Var, ty: Type) {
        self.types.add_type(var, ty)
    }

    fn new_rigid_var(&mut self) -> RigidVar {
        Context::new_rigid_var(self)
    }
}

impl InstantiationContext for Context {
    fn gen_uni_var(&mut self) -> UnifiableVar {
        self.new_unif_var()
    }
}

impl InstantiationContext for &mut Context {
    fn gen_uni_var(&mut self) -> UnifiableVar {
        self.new_unif_var()
    }
}
