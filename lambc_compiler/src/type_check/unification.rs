use ena::unify::{EqUnifyValue, UnifyKey};

use super::{Constraint, FnType, Type, TypedVar, Tyvar};

impl EqUnifyValue for Type {}

impl UnifyKey for Tyvar {
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

pub enum TypeError {
    AddNotImpld(Type),
    SubNotImpld(Type),
    DivNotImpld(Type),
    MulNotImpld(Type),
    NegNotImpld(Type),
    TypeNotEqual(Type, Type),
    InfiniteType(Tyvar, Type),
}

impl super::TypeInference {
    pub(super) fn unification(
        &mut self,
        cons: Vec<Constraint>,
    ) -> Result<(), TypeError> {
        for con in cons {
            match con {
                Constraint::ImplAdd(t) => {
                    let t = self.normalize_ty(t);
                    if !matches!(t, Type::Int | Type::Double | Type::List(_)) {
                        return Err(TypeError::AddNotImpld(t));
                    }
                }
                Constraint::ImplSub(t) => {
                    let t = self.normalize_ty(t);
                    if !matches!(t, Type::Int | Type::Double) {
                        return Err(TypeError::SubNotImpld(t));
                    }
                }
                Constraint::ImplDiv(t) => {
                    let t = self.normalize_ty(t);
                    if !matches!(t, Type::Int | Type::Double) {
                        return Err(TypeError::DivNotImpld(t));
                    }
                }
                Constraint::ImplMul(t) => {
                    let t = self.normalize_ty(t);
                    if !matches!(t, Type::Int | Type::Double) {
                        return Err(TypeError::MulNotImpld(t));
                    }
                }
                Constraint::ImplNegate(t) => {
                    let t = self.normalize_ty(t);
                    if !matches!(t, Type::Int | Type::Double) {
                        return Err(TypeError::NegNotImpld(t));
                    }
                }
                Constraint::TypeEqual { expected, got } => {
                    self.unify_ty_ty(expected, got)?
                }
            }
        }

        Ok(())
    }

    fn unify_ty_ty(&mut self, l: Type, r: Type) -> Result<(), TypeError> {
        let left = self.normalize_ty(l);
        let right = self.normalize_ty(r);
        match (left, right) {
            (Type::Int, Type::Int)
            | (Type::Nil, Type::Nil)
            | (Type::Usv, Type::Usv)
            | (Type::Bool, Type::Bool)
            | (Type::Double, Type::Double) => Ok(()),
            (Type::List(l), Type::List(r)) => self.unify_ty_ty(*l, *r),
            (Type::Var(l), Type::Var(r)) => self
                .uni_table
                .unify_var_var(l, r)
                .map_err(|(l, r)| TypeError::TypeNotEqual(l, r)),
            (Type::Fun(l), Type::Fun(r)) => {
                if l.args.len() != r.args.len() {
                    return Err(TypeError::TypeNotEqual(
                        Type::Fun(l),
                        Type::Fun(r),
                    ));
                }

                for (l_arg, r_arg) in l.args.into_iter().zip(r.args) {
                    self.unify_ty_ty(l_arg, r_arg)?;
                }

                self.unify_ty_ty(*l.ret_type, *r.ret_type)
            }
            (Type::Var(v), ty) | (ty, Type::Var(v)) => {
                self.occurs_check(&ty, v)?;
                self.uni_table
                    .unify_var_value(v, Some(ty))
                    .map_err(|(l, r)| TypeError::TypeNotEqual(l, r))
            }
            // Modules are never equal... I would like them to be structurally
            // equal at one point, such that exports are normal anonymous structs.
            (l, r) => Err(TypeError::TypeNotEqual(l, r)),
        }
    }

    fn normalize_ty(&mut self, ty: Type) -> Type {
        match ty {
            t @ (Type::Int
            | Type::Nil
            | Type::Usv
            | Type::Bool
            | Type::Double) => t,
            Type::List(t) => Type::List(Box::new(self.normalize_ty(*t))),
            Type::Fun(FnType { args, ret_type }) => Type::Fun(FnType {
                args: args.into_iter().map(|a| self.normalize_ty(a)).collect(),
                ret_type: Box::new(self.normalize_ty(*ret_type)),
            }),
            Type::Var(v) => match self.uni_table.probe_value(v) {
                Some(ty) => self.normalize_ty(ty),
                None => Type::Var(v),
            },
        }
    }

    fn occurs_check(&self, ty: &Type, tyvar: Tyvar) -> Result<(), TypeError> {
        match ty {
            Type::Int | Type::Nil | Type::Usv | Type::Bool | Type::Double => {
                Ok(())
            }
            Type::Var(v) => {
                if *v == tyvar {
                    Err(TypeError::InfiniteType(*v, Type::Var(*v)))
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
