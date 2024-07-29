use std::collections::HashMap;

use super::{
    FnType, Qualified, TyRigVar, Type, TypeInference, TypeScheme, UnifiableVar,
};

impl TypeInference {
    pub fn instantiate(&mut self, scheme: TypeScheme) -> Qualified<Type> {
        let tyvar_to_unifiers = scheme
            .unbound
            .iter()
            .map(|var| (*var, self.fresh_ty_var()))
            .collect::<HashMap<_, _>>();

        let inst = Instantiate::new(&tyvar_to_unifiers);
        inst.scheme(scheme)
    }
}

pub struct Instantiate<'a> {
    tyvars_to_unif: &'a HashMap<TyRigVar, UnifiableVar>,
}

impl<'a> Instantiate<'a> {
    pub fn new(tyvars: &'a HashMap<TyRigVar, UnifiableVar>) -> Self {
        Self { tyvars_to_unif: tyvars }
    }

    pub fn scheme(&self, scheme: TypeScheme) -> Qualified<Type> {
        let TypeScheme { unbound: _, ty, constraints } = scheme;
        Qualified::constrained(self.ty(ty), constraints)
    }

    fn ty(&self, ty: Type) -> Type {
        match ty {
            Type::RigidVar(rig) => self
                .tyvars_to_unif
                .get(&rig)
                .copied()
                .map(Type::UnifiableVar)
                .expect("Unbound variable not found in type-scheme"),
            Type::UnifiableVar(_) | Type::Con(_) => ty,
            Type::List(lt) => Type::List(Box::new(self.ty(*lt))),
            Type::Fun(FnType { args, ret_type }) => {
                let args = args.into_iter().map(|t| self.ty(t)).collect();
                let ret_type = Box::new(self.ty(*ret_type));
                Type::Fun(FnType { args, ret_type })
            }
        }
    }
}
