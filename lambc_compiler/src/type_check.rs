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

    #[cfg(test)]
    pub fn fun(args: Vec<Self>, ret: Self) -> Self {
        Self::Fun(FnType { args, ret_type: Box::new(ret) })
    }
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
}

#[cfg(test)]
mod test {
    use std::collections::HashSet;

    use lambc_parse::{Call, Expr, FnDef, I64Base, I64Lit, NilLit, Span};
    use pretty_assertions::assert_eq;

    use super::TypeChecker;
    use crate::{
        name_res::Var,
        type_check::{
            unification::TypeError, FnType, TyClass, Type, TypeScheme,
            TypedVar, Tyvar,
        },
        State,
    };

    const SPAN: Span = Span::new(0, 0);

    macro_rules! set {
        ($($expr:expr),*$(,)?) => {{
            let mut x = HashSet::new();
            $(x.insert($expr);)*
            x
        }};
    }

    fn fndef<T>(args: Vec<T>, body: Expr<T>) -> Expr<T> {
        Expr::FnDef(Box::new(FnDef {
            args,
            body,
            recursive: false,
            span: SPAN,
        }))
    }

    fn call<T>(callee: Expr<T>, args: Vec<Expr<T>>) -> Expr<T> {
        Expr::Call(Box::new(Call { callee, args, span: SPAN }))
    }

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

    #[test]
    fn infers_id() {
        let x = Var(u32::MAX);
        let a = Tyvar(0);
        let ast = Expr::FnDef(Box::new(FnDef {
            args: vec![x],
            body: Expr::Ident(x),
            recursive: false,
            span: SPAN,
        }));

        let (expr, scheme) = TypeChecker::new(&mut State::default())
            .infer(ast)
            .expect("Inference to succeed");

        assert_eq!(
            expr,
            Expr::FnDef(Box::new(FnDef {
                args: vec![TypedVar(x, Type::Var(a))],
                body: Expr::Ident(TypedVar(x, Type::Var(a))),
                recursive: false,
                span: SPAN,
            }))
        );

        assert_eq!(
            scheme,
            TypeScheme {
                unbound: set![a],
                ty: Type::Fun(FnType {
                    args: vec![Type::Var(a)],
                    ret_type: Box::new(Type::Var(a))
                })
            }
        );
    }

    #[test]
    fn infers_k_combinator() {
        let x = Var(u32::MAX);
        let y = Var(u32::MAX - 1);

        let a = Tyvar(0);
        let b = Tyvar(1);

        let ast = Expr::FnDef(Box::new(FnDef {
            args: vec![x],
            body: Expr::FnDef(Box::new(FnDef {
                args: vec![y],
                body: Expr::Ident(x),
                recursive: false,
                span: SPAN,
            })),
            recursive: false,
            span: SPAN,
        }));

        let (expr, scheme) = TypeChecker::new(&mut State::default())
            .infer(ast)
            .expect("Inference to succeed");

        assert_eq!(
            expr,
            Expr::FnDef(Box::new(FnDef {
                args: vec![TypedVar(x, Type::Var(a))],
                body: Expr::FnDef(Box::new(FnDef {
                    args: vec![TypedVar(y, Type::Var(b))],
                    body: Expr::Ident(TypedVar(x, Type::Var(a))),
                    recursive: false,
                    span: SPAN
                })),
                recursive: false,
                span: SPAN,
            }))
        );

        assert_eq!(
            scheme,
            TypeScheme {
                unbound: set![a, b],
                ty: Type::Fun(FnType {
                    args: vec![Type::Var(a)],
                    ret_type: Box::new(Type::Fun(FnType {
                        args: vec![Type::Var(b)],
                        ret_type: Box::new(Type::Var(a))
                    }))
                })
            }
        );
    }

    #[test]
    fn infers_s_combinator() {
        let x = Var(u32::MAX);
        let y = Var(u32::MAX - 1);
        let z = Var(u32::MAX - 2);

        let s_comb = fndef(
            vec![x],
            fndef(
                vec![y],
                fndef(
                    vec![z],
                    call(
                        call(Expr::Ident(x), vec![Expr::Ident(z)]),
                        vec![call(Expr::Ident(y), vec![Expr::Ident(z)])],
                    ),
                ),
            ),
        );

        let (_expr, scheme) = TypeChecker::new(&mut State::default())
            .infer(s_comb)
            .expect("Inference to succeed");

        let a = Tyvar(2);
        let b = Tyvar(3);
        let c = Tyvar(4);

        let x_ty = Type::fun(
            vec![Type::Var(a)],
            Type::fun(vec![Type::Var(b)], Type::Var(c)),
        );

        let y_ty = Type::fun(vec![Type::Var(a)], Type::Var(b));

        assert_eq!(
            scheme,
            TypeScheme {
                unbound: set![a, b, c],
                ty: Type::fun(
                    vec![x_ty],
                    Type::fun(
                        vec![y_ty],
                        Type::fun(vec![Type::Var(a)], Type::Var(c))
                    )
                )
            }
        )
    }

    #[test]
    fn infer_fails() {
        let x = Var(0);
        let ast = call(
            fndef(
                vec![x],
                call(Expr::Ident(x), vec![Expr::Nil(NilLit { span: SPAN })]),
            ),
            vec![Expr::Nil(NilLit { span: SPAN })],
        );

        let res = TypeChecker::new(&mut State::default()).infer(ast);
        assert_eq!(
            res,
            Err(TypeError::TypeNotEqual(
                Type::fun(vec![Type::NIL], Type::Var(Tyvar(1))),
                Type::NIL,
            ))
        )
    }
}
