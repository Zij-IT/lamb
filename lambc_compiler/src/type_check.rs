mod check;
mod inference;
mod substitution;
mod unification;

use std::collections::HashSet;

use ena::unify::InPlaceUnificationTable;
use lambc_parse::Expr;

use self::unification::TypeError;
use crate::{name_res::Var, State};

#[derive(Debug, PartialEq, Clone, Eq)]
pub struct TypedVar(Var, Type);

#[derive(Debug, PartialEq, Clone, Copy, Eq, Hash)]
pub struct Tyvar(u32);

#[derive(Debug, PartialEq, Clone, Eq)]
pub enum Type {
    Var(Tyvar),
    Con(Tycon),
    List(Box<Self>),
    Fun(FnType),
}

impl Type {
    pub const INT: Self = Self::Con(Tycon { id: Var::INT });
    pub const NIL: Self = Self::Con(Tycon { id: Var::NIL });
    pub const USV: Self = Self::Con(Tycon { id: Var::USV });
    pub const BOOL: Self = Self::Con(Tycon { id: Var::BOOL });
    pub const DOUBLE: Self = Self::Con(Tycon { id: Var::DOUBLE });
}

/// A type constructor, like the nulllary (int, nil, usv)
/// or the more complicated, like `List` and `Fun`
#[derive(Debug, PartialEq, Clone, Eq)]
pub struct Tycon {
    id: Var,
}

#[derive(Debug, PartialEq, Clone, Eq)]
pub struct FnType {
    args: Vec<Type>,
    ret_type: Box<Type>,
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct TypeScheme {
    unbound: HashSet<Tyvar>,
    ty: Type,
}

pub struct TypeChecker<'s> {
    state: &'s mut State,
}

impl<'s> TypeChecker<'s> {
    pub fn new(state: &'s mut State) -> Self {
        Self { state }
    }

    pub fn infer(
        &self,
        expr: Expr<Var>,
    ) -> Result<(Expr<TypedVar>, TypeScheme), TypeError> {
        let mut inf = TypeInference::new();

        let (out, ty) = inf.infer_expr(Default::default(), expr);
        inf.unification(out.cons)?;

        let (mut unbound, ty) = inf.substitute(ty);
        let (ast_unbound, expr) = inf.substitute_expr(out.ast);
        unbound.extend(ast_unbound);

        Ok((expr, TypeScheme { unbound, ty }))
    }
}

struct TypeInference {
    uni_table: InPlaceUnificationTable<Tyvar>,
}

impl TypeInference {
    fn new() -> Self {
        Self { uni_table: Default::default() }
    }
}

#[derive(Debug, Eq, PartialEq)]
struct CheckRes<T> {
    cons: Vec<Constraint>,
    ast: T,
}

impl<T> CheckRes<T> {
    fn new(cons: Vec<Constraint>, ast: T) -> Self {
        Self { cons, ast }
    }

    fn empty(t: T) -> Self {
        Self { ast: t, cons: vec![] }
    }
}

#[derive(Debug, Eq, PartialEq)]
enum Constraint {
    IsIn(TyClass, Type),
    TypeEqual { expected: Type, got: Type },
}

/// Represents a type-class (think trait) which are builtin
/// There probably won't be a way to add these for a while
#[derive(Debug, Eq, PartialEq)]
enum TyClass {
    Addable,
    Num,
}

impl TyClass {
    pub fn impld_by(&self, t: &Type) -> bool {
        match self {
            TyClass::Addable => {
                matches!(t, &Type::INT | &Type::DOUBLE | &Type::List(..))
            }
            TyClass::Num => {
                matches!(t, &Type::INT | &Type::DOUBLE)
            }
        }
    }
}

#[cfg(test)]
mod test {
    use crate::type_check::{TyClass, Type};

    #[test]
    fn int_impls() {
        let int = Type::INT.clone();
        assert!(TyClass::Addable.impld_by(&int));
        assert!(TyClass::Num.impld_by(&int));
    }

    #[test]
    fn double_impls() {
        let dl = Type::DOUBLE.clone();
        assert!(TyClass::Addable.impld_by(&dl));
        assert!(TyClass::Num.impld_by(&dl));
    }
}
