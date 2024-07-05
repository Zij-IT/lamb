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
pub struct TypeVar(u32);

#[derive(Debug, PartialEq, Clone, Eq)]
pub enum Type {
    Var(TypeVar),
    Int,
    Nil,
    Usv,
    Bool,
    Double,
    List(Box<Self>),
    Fun(FnType),
}

#[derive(Debug, PartialEq, Clone, Eq)]
pub struct FnType {
    args: Vec<Type>,
    ret_type: Box<Type>,
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct TypeScheme {
    unbound: HashSet<TypeVar>,
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
    uni_table: InPlaceUnificationTable<TypeVar>,
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
    ImplAdd(Type),
    ImplSub(Type),
    ImplDiv(Type),
    ImplMul(Type),
    ImplNegate(Type),
    TypeEqual { expected: Type, got: Type },
}
