use crate::{name_res::Var, State};

use super::{
    inference::InferenceContext, instantiate::InstantiationContext,
    parsing::ParserContext, substitution::SubstitutionContext,
    unification::UnificationContext, Error, Result, RigidVar, Type, TypeEnv,
    UnifiableVar, VarEnv,
};

pub struct Context<'s> {
    pub state: &'s mut State,
    vars: VarEnv,
    types: TypeEnv,
}

impl<'s> Context<'s> {
    pub fn new(state: &'s mut State) -> Self {
        Self { state, vars: VarEnv::new(), types: TypeEnv::new() }
    }

    pub fn vars_mut(&mut self) -> &mut VarEnv {
        &mut self.vars
    }
}

impl InferenceContext for Context<'_> {
    fn new_unif_var(&mut self) -> UnifiableVar {
        self.types.new_unif_var()
    }

    fn vars_mut(&mut self) -> &mut VarEnv {
        Context::vars_mut(self)
    }

    fn add_error(&mut self, err: Error) {
        self.state.add_error(err, None)
    }
}

impl UnificationContext for Context<'_> {
    fn unify_var_var(
        &mut self,
        v1: UnifiableVar,
        v2: UnifiableVar,
    ) -> Result<()> {
        self.types
            .uni_table
            .unify_var_var(v1, v2)
            .map_err(|(e, g)| Error::TypeNotEqual { expected: e, got: g })
    }

    fn unify_var_value(
        &mut self,
        key: UnifiableVar,
        ty: Option<Type>,
    ) -> Result<()> {
        self.types
            .uni_table
            .unify_var_value(key, ty)
            .map_err(|(e, g)| Error::TypeNotEqual { expected: e, got: g })
    }

    fn get(&mut self, key: UnifiableVar) -> Option<Type> {
        self.types.uni_table.probe_value(key)
    }
}

impl SubstitutionContext for Context<'_> {
    fn get_value(&mut self, var: UnifiableVar) -> Option<Type> {
        self.types.uni_table.probe_value(var)
    }

    fn get_root(&mut self, var: UnifiableVar) -> UnifiableVar {
        self.types.uni_table.find(var)
    }

    fn gen_rigid_var(&mut self) -> RigidVar {
        self.new_rigid_var()
    }
}

impl ParserContext for Context<'_> {
    fn get_type(&mut self, var: Var) -> Result<Type> {
        self.types.get_type(var)
    }

    fn add_type(&mut self, var: Var, ty: Type) {
        self.types.add_type(var, ty)
    }

    fn new_rigid_var(&mut self) -> RigidVar {
        self.types.new_rigid_var()
    }
}

impl InstantiationContext for Context<'_> {
    fn gen_uni_var(&mut self) -> UnifiableVar {
        self.types.new_unif_var()
    }
}
