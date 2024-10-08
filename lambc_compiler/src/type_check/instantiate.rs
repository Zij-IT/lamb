use std::collections::HashMap;

use super::{FnType, Qualified, RigidVar, Type, TypeScheme, UnifiableVar};

pub trait InstantiationContext {
    fn gen_uni_var(&mut self) -> UnifiableVar;
}

pub struct Instantiate<'ctx, C> {
    ctx: &'ctx mut C,
    rigid_to_unif: HashMap<RigidVar, UnifiableVar>,
}

impl<'ctx, C: InstantiationContext> Instantiate<'ctx, C> {
    pub fn new(ctx: &'ctx mut C) -> Self {
        Self { ctx, rigid_to_unif: HashMap::new() }
    }

    pub fn scheme(&mut self, scheme: TypeScheme) -> Qualified<Type> {
        self.rigid_to_unif.extend(
            scheme.unbound.iter().map(|var| (*var, self.ctx.gen_uni_var())),
        );

        let TypeScheme { unbound: _, ty, constraints } = scheme;
        Qualified::constrained(self.ty(ty), constraints)
    }

    fn ty(&self, ty: Type) -> Type {
        match ty {
            Type::Error | Type::UnifiableVar(_) | Type::Con(_) => ty,
            Type::RigidVar(rig) => self
                .rigid_to_unif
                .get(&rig)
                .copied()
                .map(Type::UnifiableVar)
                .unwrap_or(ty),
            Type::List(lt) => Type::List(Box::new(self.ty(*lt))),
            Type::Fun(FnType { args, ret_type }) => {
                let args = args.into_iter().map(|t| self.ty(t)).collect();
                let ret_type = Box::new(self.ty(*ret_type));
                Type::Fun(FnType { args, ret_type })
            }
        }
    }
}
