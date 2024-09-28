use crate::type_check::Type;

#[derive(Debug, Eq, PartialEq)]
pub struct Qualified<T> {
    pub cons: Vec<Constraint>,
    pub item: T,
}

impl<T> Qualified<T> {
    pub fn constrained(item: T, cons: Vec<Constraint>) -> Self {
        Self { cons, item }
    }

    pub fn unconstrained(t: T) -> Self {
        Self { item: t, cons: vec![] }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Constraint {
    IsIn(TyClass, Type),
    TypeEqual { expected: Type, got: Type },
}

/// Represents a type-class (think trait) which are builtin
/// There probably won't be a way to add these for a while
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum TyClass {
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

    pub fn name(&self) -> &'static str {
        match self {
            TyClass::Addable => "Add",
            TyClass::Num => "Num",
        }
    }
}
