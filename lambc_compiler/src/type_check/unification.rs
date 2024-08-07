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

impl super::TypeInference {
    pub(super) fn unification(&mut self, cons: Vec<Constraint>) -> Result<()> {
        for con in cons {
            match con {
                Constraint::IsIn(class, t) => {
                    let t = self.normalize_ty(t);
                    if !class.impld_by(&t) {
                        return Err(Error::NotImpld { class, ty: t });
                    }
                }
                Constraint::TypeEqual { expected, got } => {
                    self.unify_ty_ty(expected, got)?
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
            (Type::UnifiableVar(l), Type::UnifiableVar(r)) => self
                .uni_table
                .unify_var_var(l, r)
                .map_err(|(l, r)| Error::TypeNotEqual { expected: l, got: r }),
            (Type::Fun(l), Type::Fun(r)) => {
                if l.args.len() != r.args.len() {
                    return Err(Error::TypeNotEqual {
                        expected: Type::Fun(l),
                        got: Type::Fun(r),
                    });
                }

                for (l_arg, r_arg) in l.args.into_iter().zip(r.args) {
                    self.unify_ty_ty(l_arg, r_arg)?;
                }

                self.unify_ty_ty(*l.ret_type, *r.ret_type)
            }
            (Type::UnifiableVar(v), ty) | (ty, Type::UnifiableVar(v)) => {
                self.occurs_check(&ty, v)?;
                self.uni_table.unify_var_value(v, Some(ty)).map_err(
                    |(l, r)| Error::TypeNotEqual { expected: l, got: r },
                )
            }
            (Type::RigidVar(a), Type::RigidVar(b)) if a == b => Ok(()),
            // Modules are never equal... I would like them to be structurally
            // equal at one point, such that exports are normal anonymous structs.
            (l, r) => Err(Error::TypeNotEqual { expected: l, got: r }),
        }
    }

    fn normalize_ty(&mut self, ty: Type) -> Type {
        match ty {
            t @ (Type::Con(..) | Type::RigidVar(..)) => t,
            Type::List(t) => Type::List(Box::new(self.normalize_ty(*t))),
            Type::Fun(FnType { args, ret_type }) => Type::Fun(FnType {
                args: args.into_iter().map(|a| self.normalize_ty(a)).collect(),
                ret_type: Box::new(self.normalize_ty(*ret_type)),
            }),
            Type::UnifiableVar(v) => match self.uni_table.probe_value(v) {
                Some(ty) => self.normalize_ty(ty),
                None => Type::UnifiableVar(v),
            },
        }
    }

    fn occurs_check(&self, ty: &Type, tyvar: UnifiableVar) -> Result<()> {
        match ty {
            Type::Con(..) | Type::RigidVar(..) => Ok(()),
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
