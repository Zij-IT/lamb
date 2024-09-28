use crate::name_res::Var;

#[derive(Debug, PartialEq, Clone, Eq)]
pub struct TypedVar(pub Var, pub Type);

/// The representation of a type within the compiler
#[derive(Debug, PartialEq, Clone, Eq)]
pub enum Type {
    UnifiableVar(UnifiableVar),
    RigidVar(RigidVar),
    Con(Tycon),
    List(Box<Self>),
    Fun(FnType),
    Error,
}

impl Type {
    /// the `int` type
    pub const INT: Self = Self::Con(Tycon { id: Var::INT });
    /// the `nil` type
    pub const NIL: Self = Self::Con(Tycon { id: Var::NIL });
    /// the `usv` type
    pub const USV: Self = Self::Con(Tycon { id: Var::USV });
    /// the `bool` type
    pub const BOOL: Self = Self::Con(Tycon { id: Var::BOOL });
    /// the `double` type
    pub const DOUBLE: Self = Self::Con(Tycon { id: Var::DOUBLE });
    /// the `never` type
    pub const NEVER: Self = Self::Con(Tycon { id: Var::NEVER });

    pub fn fun(args: Vec<Self>, ret: Self) -> Self {
        Self::Fun(FnType { args, ret_type: Box::new(ret) })
    }

    pub fn name(&self) -> String {
        match self {
            &Type::Error => "^error^".into(),
            &Type::NIL => "nil".into(),
            &Type::INT => "int".into(),
            &Type::USV => "usv".into(),
            &Type::BOOL => "bool".into(),
            &Type::DOUBLE => "double".into(),
            Type::UnifiableVar(u) => format!("Uv{}", u.0),
            Type::RigidVar(r) => format!("Rv{}", r.0),
            Type::List(l) => format!("list[{}]", l.name()),
            Type::Fun(f) => format!(
                "fn({}) -> {}",
                f.args
                    .iter()
                    .map(Type::name)
                    .reduce(|a, b| a + ", " + b.as_str())
                    .unwrap_or_default(),
                f.ret_type.name()
            ),
            Type::Con(_) => todo!(),
        }
    }
}
/// A type variable which is allowed to be unified with another type
#[derive(Debug, PartialEq, Clone, Copy, Eq, Hash)]
pub struct UnifiableVar(pub u32);

/// A type variable which is *not* allowed to be unified with another type.
#[derive(Debug, PartialEq, Clone, Copy, Eq, Hash)]
pub struct RigidVar(pub u32);

/// A type constructor, like the nulllary (int, nil, usv)
/// or the more complicated, like `List` and `Fun`
#[derive(Debug, PartialEq, Clone, Eq)]
pub struct Tycon {
    id: Var,
}

#[derive(Debug, PartialEq, Clone, Eq)]
pub struct FnType {
    pub args: Vec<Type>,
    pub ret_type: Box<Type>,
}
