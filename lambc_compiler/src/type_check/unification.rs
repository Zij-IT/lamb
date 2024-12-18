use ena::unify::{EqUnifyValue, UnifyKey};

use super::{Constraint, Error, FnType, Result, Type, UnifiableVar};

impl EqUnifyValue for Type {}

impl UnifyKey for UnifiableVar {
    type Value = Option<Type>;

    fn index(&self) -> u32 {
        self.0
    }

    fn from_index(u: u32) -> Self {
        Self(u)
    }

    fn tag() -> &'static str {
        "TypeVar"
    }
}

pub trait UnificationContext {
    /// Unifies two `UnifiableVar`, returning an error if the two aren't able to be unified
    fn unify_var_var(
        &mut self,
        v1: UnifiableVar,
        v2: UnifiableVar,
    ) -> Result<()>;

    /// Attempts to set the value of the `key` to `Value` by merging with the previous value if present
    fn unify_var_value(
        &mut self,
        key: UnifiableVar,
        ty: Option<Type>,
    ) -> Result<()>;

    /// Returns the current value of the `key`
    fn get(&mut self, key: UnifiableVar) -> Option<Type>;

    /// Returns the root of the `key`
    fn get_root(&mut self, key: UnifiableVar) -> UnifiableVar;
}

pub struct Unifier<'ctx, C> {
    ctx: &'ctx mut C,
}

impl<'ctx, C: UnificationContext> Unifier<'ctx, C> {
    pub fn new(ctx: &'ctx mut C) -> Self {
        Self { ctx }
    }

    pub(super) fn unify(&mut self, cons: Vec<Constraint>) -> Result<()> {
        let (teq, isin): (Vec<_>, Vec<_>) = cons
            .into_iter()
            .partition(|c| matches!(c, Constraint::TypeEqual { .. }));

        for con in teq {
            match con {
                Constraint::TypeEqual { expected, got } => {
                    self.unify_ty_ty(expected, got)?
                }
                Constraint::IsIn(..) => unreachable!(),
            }
        }

        for con in isin {
            match con {
                Constraint::TypeEqual { .. } => unreachable!(),
                Constraint::IsIn(_, Type::Error) => (),
                Constraint::IsIn(class, ty) => {
                    let ty = self.normalize_ty(ty);
                    if !class.impld_by(&ty) {
                        return Err(Error::NotImpld { class, ty });
                    }
                }
            }
        }

        Ok(())
    }

    fn unify_ty_ty(&mut self, l: Type, r: Type) -> Result<()> {
        let left = self.normalize_ty(l);
        let right = self.normalize_ty(r);
        match (left, right) {
            (Type::NEVER, ..) | (.., Type::NEVER) => Ok(()),
            (Type::Con(c1), Type::Con(c2)) if c1 == c2 => Ok(()),
            (Type::List(l), Type::List(r)) => self.unify_ty_ty(*l, *r),
            (Type::UnifiableVar(l), Type::UnifiableVar(r)) => {
                self.ctx.unify_var_var(l, r)?;
                Ok(())
            }
            (Type::Fun(l), Type::Fun(r)) => {
                if l.args.len() != r.args.len() {
                    return Err(Error::TypeNotEqual {
                        expected: Type::Fun(l),
                        got: Type::Fun(r),
                    });
                }

                for (l_arg, r_arg) in l.args.iter().zip(r.args.iter()) {
                    if self.unify_ty_ty(l_arg.clone(), r_arg.clone()).is_err()
                    {
                        return Err(Error::TypeNotEqual {
                            expected: Type::Fun(l),
                            got: Type::Fun(r),
                        });
                    }
                }

                if self
                    .unify_ty_ty(
                        Type::clone(&l.ret_type),
                        Type::clone(&r.ret_type),
                    )
                    .is_err()
                {
                    return Err(Error::TypeNotEqual {
                        expected: Type::Fun(l),
                        got: Type::Fun(r),
                    });
                }

                Ok(())
            }
            (Type::UnifiableVar(v), ty) | (ty, Type::UnifiableVar(v)) => {
                self.occurs_check(&ty, v)?;
                self.ctx.unify_var_value(v, Some(ty))?;
                Ok(())
            }
            (Type::RigidVar(a), Type::RigidVar(b)) if a == b => Ok(()),
            (Type::Error, ..) | (.., Type::Error) => Ok(()),
            // Modules are never equal... I would like them to be structurally
            // equal at one point, such that exports are normal anonymous structs.
            (l, r) => Err(Error::TypeNotEqual { expected: l, got: r }),
        }
    }

    fn normalize_ty(&mut self, ty: Type) -> Type {
        match ty {
            t @ (Type::Con(..) | Type::RigidVar(..) | Type::Error) => t,
            Type::List(t) => Type::List(Box::new(self.normalize_ty(*t))),
            Type::Fun(FnType { args, ret_type }) => Type::Fun(FnType {
                args: args.into_iter().map(|a| self.normalize_ty(a)).collect(),
                ret_type: Box::new(self.normalize_ty(*ret_type)),
            }),
            Type::UnifiableVar(v) => match self.ctx.get(v) {
                Some(ty) => self.normalize_ty(ty),
                None => Type::UnifiableVar(self.ctx.get_root(v)),
            },
        }
    }

    fn occurs_check(&self, ty: &Type, tyvar: UnifiableVar) -> Result<()> {
        match ty {
            Type::Con(..) | Type::RigidVar(..) | Type::Error => Ok(()),
            Type::UnifiableVar(v) => {
                if *v == tyvar {
                    Err(Error::InfiniteType {
                        repr_var: *v,
                        ty: Type::UnifiableVar(*v),
                    })
                } else {
                    Ok(())
                }
            }
            Type::Fun(f) => {
                for arg in &f.args {
                    self.occurs_check(arg, tyvar)?;
                }

                self.occurs_check(&f.ret_type, tyvar)
            }
            Type::List(l) => self.occurs_check(l.as_ref(), tyvar),
        }
    }
}
