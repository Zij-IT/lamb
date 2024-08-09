mod check;
mod env;
mod inference;
mod instantiate;
mod parsing;
mod substitution;
mod unification;

use std::collections::{HashMap, HashSet};

use ena::unify::InPlaceUnificationTable;
use lambc_parse::{
    Define, Export, ExportItem, Import, ImportItem, Item, Module,
};

#[cfg(test)]
use lambc_parse::Expr;
use miette::Diagnostic;

use self::env::Env;
use crate::{
    name_res::Var,
    type_check::parsing::{TypeEnv, TypeParser},
    PathRef, State,
};

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Diagnostic, thiserror::Error, Debug, Clone, PartialEq)]
pub enum Error {
    #[diagnostic(code("type-checking::no-impl"))]
    #[error("no impl of `{}` found for type `..`", .class.name())]
    NotImpld { class: TyClass, ty: Type },
    #[diagnostic(code("type-checking::expected-found"))]
    #[error("found `..`, but expected `..`")]
    TypeNotEqual { expected: Type, got: Type },
    #[diagnostic(code("type-checking::infinite-type"))]
    #[error("the inferred type `..` is infinite")]
    InfiniteType { repr_var: UnifiableVar, ty: Type },
    #[diagnostic(code("type-checking::new-unbound-vars"))]
    #[error("the inferred type introduces new unbound variables")]
    NewUnboundTypes,
    #[diagnostic(code("type-checking::no-such-type"))]
    #[error("no type found with the name `..`")]
    UnknownType,
}

#[derive(Debug, PartialEq, Clone, Eq)]
pub struct TypedVar(Var, Type);

#[derive(Debug, PartialEq, Clone, Copy, Eq, Hash)]
pub struct UnifiableVar(u32);

#[derive(Debug, PartialEq, Clone, Copy, Eq, Hash)]
pub struct RigidVar(u32);

#[derive(Debug, PartialEq, Clone, Eq)]
pub enum Type {
    UnifiableVar(UnifiableVar),
    RigidVar(RigidVar),
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
    pub const NEVER: Self = Self::Con(Tycon { id: Var::NEVER });

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
    unbound: HashSet<RigidVar>,
    constraints: Vec<Constraint>,
    ty: Type,
}

pub struct TypeChecker<'s> {
    #[allow(dead_code)]
    state: &'s mut State,
}

impl<'s> TypeChecker<'s> {
    pub fn new(state: &'s mut State) -> Self {
        Self { state }
    }

    pub fn check_modules(
        &mut self,
        mut modules: Vec<Module<Var, PathRef>>,
    ) -> Vec<Module<TypedVar, PathRef>> {
        let mut inf = TypeInference::new();
        let globals = self.build_env(
            &mut inf,
            modules.iter().flat_map(|i| i.items.as_slice()),
        );

        let mut item_map = HashMap::new();
        for module in modules.iter_mut() {
            let items = std::mem::take(&mut module.items);
            // If this errors we can't properly build the import map because the types
            // of all the variables aren't able to be used.
            let items = self
                .check_items(&mut inf, globals.clone(), module.path, items)
                .unwrap_or_default();

            item_map.insert(module.path, items);
        }

        // If we have an error from type-checking such that we can't rebuild the types
        // for each of the variables for the input map, then we need to bail before we try
        // to build the import map.
        if self.state.has_errors() {
            return Default::default();
        }

        let typemap = item_map
            .values()
            .flatten()
            .map(|i| match i {
                Item::Def(def) => (def.ident.0, &def.ident),
            })
            .collect();

        let mut import_map = HashMap::new();
        let mut export_map = HashMap::new();
        for module in modules.iter_mut() {
            let imports = std::mem::take(&mut module.imports);
            import_map
                .insert(module.path, self.add_import_types(&typemap, imports));

            let exports = std::mem::take(&mut module.exports);
            export_map
                .insert(module.path, self.add_export_types(&typemap, exports));
        }

        modules
            .into_iter()
            .map(|m| Module {
                exports: export_map.remove(&m.path).unwrap(),
                imports: import_map.remove(&m.path).unwrap(),
                items: item_map.remove(&m.path).unwrap(),
                path: m.path,
                span: m.span,
            })
            .collect()
    }

    fn check_items(
        &mut self,
        inf: &mut TypeInference,
        env: Env,
        path: PathRef,
        items: Vec<Item<Var>>,
    ) -> std::result::Result<Vec<Item<TypedVar>>, ()> {
        let mut typed_items = Vec::with_capacity(items.len());
        for item in items {
            match item {
                Item::Def(def) => {
                    let scheme = env.type_of(def.ident);
                    let res =
                        self.check_toplevel_def(inf, env.clone(), def, scheme);

                    match res {
                        Ok(ast) => typed_items.push(Item::Def(ast)),
                        Err(er) => self.state.add_error(er, Some(path)),
                    }
                }
            }
        }

        if self.state.has_errors() {
            return Err(());
        }

        Ok(typed_items)
    }

    fn build_env<'a, I: Iterator<Item = &'a Item<Var>>>(
        &self,
        inf: &mut TypeInference,
        items: I,
    ) -> Env {
        let mut env = Env::new();
        let mut ty_env = TypeEnv::default();
        ty_env.add_type(Var::INT, Type::INT);
        ty_env.add_type(Var::NIL, Type::NIL);
        ty_env.add_type(Var::USV, Type::USV);
        ty_env.add_type(Var::BOOL, Type::BOOL);
        ty_env.add_type(Var::NEVER, Type::NEVER);
        ty_env.add_type(Var::DOUBLE, Type::DOUBLE);

        let mut parser = TypeParser::new(&mut ty_env, || inf.gen_rigidvar());

        for item in items {
            match item {
                Item::Def(id) => {
                    let Define { ident, typ, value: _, span: _ } = id;
                    let scheme = parser
                        .parse_scheme(
                            typ.as_ref()
                                .expect("Top level decls to have typs"),
                        )
                        .expect("Type to be known");

                    env.add_scheme(*ident, scheme);
                }
            }
        }

        env
    }

    fn check_toplevel_def(
        &self,
        inf: &mut TypeInference,
        env: Env,
        def: Define<Var>,
        scheme: TypeScheme,
    ) -> Result<Define<TypedVar>> {
        if def.value.is_recursive() {
            // Nothing to do in the case of a recursive bound because top-level definitions
            // now require types.
        }

        let mut qual_value = inf.check_expr(env, def.value, scheme.ty.clone());
        qual_value.cons.extend(scheme.constraints.clone());

        inf.unification(qual_value.cons.clone())?;

        let (mut unbound, ty) = inf.substitute(scheme.ty);
        let (ast_unbound, expr) = inf.substitute_expr(qual_value.item);
        unbound.extend(ast_unbound);

        let (con_unbound, _cons) = inf.substitute_constraints(qual_value.cons);
        let ambiguities = con_unbound.difference(&unbound).count();
        assert_eq!(ambiguities, 0);

        let new_unbound = con_unbound.difference(&unbound);
        if new_unbound.count() != 0 {
            // Handle new generic types being added
            return Err(Error::NewUnboundTypes);
        }

        // let reduced = inf.reduce_constraints(&unbound, cons);

        Ok(Define {
            ident: TypedVar(def.ident, ty),
            typ: None,
            value: expr,
            span: def.span,
        })
    }

    fn add_import_types(
        &self,
        globals: &HashMap<Var, &TypedVar>,
        imports: Vec<Import<Var, PathRef>>,
    ) -> Vec<Import<TypedVar, PathRef>> {
        imports
            .into_iter()
            .map(|Import { file, name: _, items, star, span, path_span }| {
                Import {
                    file,
                    // TODO: Add a type for `name`. This should be done in `build_env`
                    name: None,
                    items: items
                        .into_iter()
                        .map(|i| self.add_import_item_type(globals, i))
                        .collect(),
                    star,
                    span,
                    path_span,
                }
            })
            .collect()
    }

    fn add_import_item_type(
        &self,
        globals: &HashMap<Var, &TypedVar>,
        i: ImportItem<Var>,
    ) -> ImportItem<TypedVar> {
        ImportItem {
            item: globals[&i.item].clone(),
            alias: i.alias.map(|a| globals[&a].clone()),
            span: i.span,
        }
    }

    fn add_export_types(
        &self,
        globals: &HashMap<Var, &TypedVar>,
        exports: Vec<Export<Var>>,
    ) -> Vec<Export<TypedVar>> {
        exports
            .into_iter()
            .map(|Export { items, span }| Export {
                items: items
                    .into_iter()
                    .map(|e| self.add_export_item_type(globals, e))
                    .collect(),
                span,
            })
            .collect()
    }

    fn add_export_item_type(
        &self,
        globals: &HashMap<Var, &TypedVar>,
        i: ExportItem<Var>,
    ) -> ExportItem<TypedVar> {
        ExportItem {
            item: globals[&i.item].clone(),
            alias: i.alias.map(|a| globals[&a].clone()),
            span: i.span,
        }
    }

    #[cfg(test)]
    fn infer(&self, expr: Expr<Var>) -> Result<(Expr<TypedVar>, TypeScheme)> {
        self.infer_with_env(Env::new(), expr)
    }

    #[cfg(test)]
    fn infer_with_env(
        &self,
        env: Env,
        expr: Expr<Var>,
    ) -> Result<(Expr<TypedVar>, TypeScheme)> {
        let mut inf = TypeInference::new();
        let (out, ty) = inf.infer_expr(env, expr);
        inf.unification(out.cons.clone())?;

        let (mut unbound, ty) = inf.substitute(ty);
        let (ast_unbound, expr) = inf.substitute_expr(out.item);
        unbound.extend(ast_unbound);

        let (con_unbound, cons) = inf.substitute_constraints(out.cons);
        let ambiguities = con_unbound.difference(&unbound).count();
        assert_eq!(ambiguities, 0);

        let reduced = inf.reduce_constraints(&unbound, cons);
        Ok((expr, TypeScheme { unbound, constraints: reduced, ty }))
    }
}

struct TypeInference {
    uni_table: InPlaceUnificationTable<UnifiableVar>,
    subst_unifiers_to_tyvars: HashMap<UnifiableVar, RigidVar>,
    next_tyvar: u32,
    ret_type: Vec<Type>,
}

impl TypeInference {
    fn new() -> Self {
        Self {
            uni_table: Default::default(),
            subst_unifiers_to_tyvars: Default::default(),
            next_tyvar: 0,
            ret_type: Default::default(),
        }
    }

    #[cfg(test)]
    fn reduce_constraints(
        &self,
        unbound: &HashSet<RigidVar>,
        cons: Vec<Constraint>,
    ) -> Vec<Constraint> {
        cons.into_iter()
            .filter(|c| match c {
                Constraint::IsIn(_, t1) => self.has_unbound(unbound, t1),
                // This is post unification, and any `TypeEqual` would have resulted
                // in a type being inferred to this type. Thus, we don't need to keep
                // these restraints.
                Constraint::TypeEqual { expected: _, got: _ } => false,
            })
            .collect()
    }

    #[cfg(test)]
    fn has_unbound(&self, unbound: &HashSet<RigidVar>, ty: &Type) -> bool {
        match ty {
            Type::Con(_) => false,
            Type::RigidVar(rig) => unbound.contains(rig),
            Type::List(elem_ty) => self.has_unbound(unbound, &elem_ty),
            Type::Fun(f) => {
                f.args.iter().any(|t| self.has_unbound(unbound, t))
                    || self.has_unbound(unbound, &f.ret_type)
            }
            Type::UnifiableVar(_) => {
                unreachable!("Should not occur post unification")
            }
        }
    }

    fn gen_rigidvar(&mut self) -> RigidVar {
        self.next_tyvar += 1;
        RigidVar(self.next_tyvar - 1)
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct Qualified<T> {
    cons: Vec<Constraint>,
    item: T,
}

impl<T> Qualified<T> {
    fn constrained(item: T, cons: Vec<Constraint>) -> Self {
        Self { cons, item }
    }

    fn unconstrained(t: T) -> Self {
        Self { item: t, cons: vec![] }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
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

    pub fn name(&self) -> &'static str {
        match self {
            TyClass::Addable => "Add",
            TyClass::Num => "Num",
        }
    }
}

#[cfg(test)]
mod test {
    use std::collections::HashSet;

    use lambc_parse::{
        Binary, BinaryOp, BoolLit, Call, Define, Expr, FnDef, NilLit, Span,
    };
    use pretty_assertions::assert_eq;

    use super::{env::Env, Error, TypeChecker};
    use crate::{
        name_res::Var,
        type_check::{
            FnType, RigidVar, TyClass, Type, TypeInference, TypeScheme,
            TypedVar, UnifiableVar,
        },
        State,
    };

    const SPAN: Span = Span::new(0, 0);

    macro_rules! set {
        ($($expr:expr),*$(,)?) => {{
            #[allow(unused_mut)]
            let mut x = HashSet::new();
            $(x.insert($expr);)*
            x
        }};
    }

    fn nil_lit() -> NilLit {
        NilLit { span: SPAN }
    }

    fn bool_lit() -> BoolLit {
        BoolLit { value: false, span: SPAN }
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
        let ast = Expr::FnDef(Box::new(FnDef {
            args: vec![x],
            body: Expr::Ident(x),
            recursive: false,
            span: SPAN,
        }));

        let (expr, scheme) = TypeChecker::new(&mut State::default())
            .infer(ast)
            .expect("Inference to succeed");

        let a = RigidVar(0);

        assert_eq!(
            expr,
            Expr::FnDef(Box::new(FnDef {
                args: vec![TypedVar(x, Type::RigidVar(a))],
                body: Expr::Ident(TypedVar(x, Type::RigidVar(a))),
                recursive: false,
                span: SPAN,
            }))
        );

        assert_eq!(
            scheme,
            TypeScheme {
                constraints: vec![],
                unbound: set![a],
                ty: Type::Fun(FnType {
                    args: vec![Type::RigidVar(a)],
                    ret_type: Box::new(Type::RigidVar(a))
                })
            }
        );
    }

    #[test]
    fn infers_k_combinator() {
        let x = Var(u32::MAX);
        let y = Var(u32::MAX - 1);

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

        let a = RigidVar(0);
        let b = RigidVar(1);

        assert_eq!(
            expr,
            Expr::FnDef(Box::new(FnDef {
                args: vec![TypedVar(x, Type::RigidVar(a))],
                body: Expr::FnDef(Box::new(FnDef {
                    args: vec![TypedVar(y, Type::RigidVar(b))],
                    body: Expr::Ident(TypedVar(x, Type::RigidVar(a))),
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
                constraints: vec![],
                unbound: set![a, b],
                ty: Type::Fun(FnType {
                    args: vec![Type::RigidVar(a)],
                    ret_type: Box::new(Type::Fun(FnType {
                        args: vec![Type::RigidVar(b)],
                        ret_type: Box::new(Type::RigidVar(a))
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

        let a = RigidVar(0);
        let b = RigidVar(1);
        let c = RigidVar(2);

        let x_ty = Type::fun(
            vec![Type::RigidVar(a)],
            Type::fun(vec![Type::RigidVar(b)], Type::RigidVar(c)),
        );

        let y_ty = Type::fun(vec![Type::RigidVar(a)], Type::RigidVar(b));

        assert_eq!(
            scheme,
            TypeScheme {
                constraints: vec![],
                unbound: set![a, b, c],
                ty: Type::fun(
                    vec![x_ty],
                    Type::fun(
                        vec![y_ty],
                        Type::fun(vec![Type::RigidVar(a)], Type::RigidVar(c))
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
            Err(Error::TypeNotEqual {
                expected: Type::fun(
                    vec![Type::NIL],
                    Type::UnifiableVar(UnifiableVar(1))
                ),
                got: Type::NIL
            })
        )
    }

    #[test]
    fn infers_generalized_def() {
        let id = Var(0);
        let mut env = Env::new();
        env.add_scheme(
            id,
            TypeScheme {
                unbound: set![RigidVar(3)],
                constraints: vec![],
                ty: Type::fun(
                    vec![Type::RigidVar(RigidVar(0))],
                    Type::RigidVar(RigidVar(0)),
                ),
            },
        );

        let x = Var(1);
        let idexpr = fndef(vec![x], Expr::Ident(x));
        let type_expr_pair = vec![
            (Expr::Bool(bool_lit()), Expr::Bool(bool_lit()), Type::BOOL),
            (Expr::Nil(nil_lit()), Expr::Nil(nil_lit()), Type::NIL),
        ];

        let mut state = State::default();
        let type_checker = TypeChecker::new(&mut state);

        for (var_expr, ty_expr, ty) in type_expr_pair {
            let idcall = call(idexpr.clone(), vec![var_expr]);
            let (expr, scheme) = type_checker
                .infer_with_env(env.clone(), idcall)
                .expect("Inference to succeed");

            let typed_x = TypedVar(x, ty.clone());
            let typed_id = fndef(vec![typed_x.clone()], Expr::Ident(typed_x));

            assert_eq!(expr, call(typed_id, vec![ty_expr]));
            assert_eq!(
                scheme,
                TypeScheme { unbound: set![], constraints: vec![], ty: ty }
            );
        }
    }

    #[test]
    fn infers_partial_scheme() {
        // func(a, b) -> nil;
        //
        // This `Var` and the rigid variables aren't inferred, but are set by the scheme,
        // and thus there value is customizable.
        let func = Var(u32::MAX);
        let rig_a = RigidVar(u32::MAX);
        let rig_b = RigidVar(u32::MAX - 1);
        let func_type = TypeScheme {
            unbound: set![rig_a, rig_b],
            constraints: vec![],
            ty: Type::fun(
                vec![Type::RigidVar(rig_a), Type::RigidVar(rig_b)],
                Type::NIL,
            ),
        };

        let mut env = Env::new();
        env.add_scheme(func, func_type);

        // fn(a) -> func(a, 1);
        //
        // This `Var` also doesn't matter, but cannot conflict with any other vars, but
        // is also customizable.
        let param_a = Var(u32::MAX - 1);
        let def = fndef(
            vec![param_a],
            call(
                Expr::Ident(func),
                vec![Expr::Ident(param_a), Expr::Nil(nil_lit())],
            ),
        );

        let mut state = State::default();
        let type_checker = TypeChecker::new(&mut state);
        let (expr, scheme) = type_checker
            .infer_with_env(env.clone(), def)
            .expect("Inference to succeed");

        // This rigid variables is inferred, and thus starts at 0
        let rigvar_a = RigidVar(0);
        let typeof_a = Type::RigidVar(rigvar_a);
        let typed_a = TypedVar(param_a, typeof_a.clone());

        let typed_func =
            Type::fun(vec![typeof_a.clone(), Type::NIL], Type::NIL);

        assert_eq!(
            expr,
            fndef(
                vec![typed_a.clone()],
                call(
                    Expr::Ident(TypedVar(func, typed_func)),
                    vec![Expr::Ident(typed_a), Expr::Nil(nil_lit())],
                ),
            )
        );

        assert_eq!(
            scheme,
            TypeScheme {
                unbound: set![rigvar_a],
                constraints: vec![],
                ty: Type::fun(vec![Type::RigidVar(rigvar_a)], Type::NIL),
            }
        );
    }

    #[test]
    fn checks_toplevel_def() {
        let a = u32::MAX;
        let scheme = TypeScheme {
            unbound: set![RigidVar(a)],
            constraints: vec![],
            ty: Type::fun(
                vec![Type::RigidVar(RigidVar(a))],
                Type::RigidVar(RigidVar(a)),
            ),
        };

        let id = Var(u32::MAX - 2);
        let x = Var(u32::MAX - 1);
        let id_value = fndef(vec![x], Expr::Ident(x));

        let def = Define { ident: id, typ: None, value: id_value, span: SPAN };

        let mut inf = TypeInference::new();
        let typed_id = TypeChecker::new(&mut State::default())
            .check_toplevel_def(&mut inf, Env::new(), def, scheme)
            .expect("Type checking to succeed");

        let rigid_x = RigidVar(0);
        let typed_x = TypedVar(x, Type::RigidVar(rigid_x));
        let id_value = fndef(vec![typed_x.clone()], Expr::Ident(typed_x));

        assert_eq!(
            typed_id,
            Define {
                ident: TypedVar(
                    id,
                    Type::fun(
                        vec![Type::RigidVar(rigid_x)],
                        Type::RigidVar(rigid_x)
                    )
                ),
                typ: None,
                value: id_value,
                span: SPAN
            }
        )
    }

    #[test]
    fn allows_given_type_to_restrict_actual() {
        let scheme = TypeScheme {
            unbound: set![],
            constraints: vec![],
            ty: Type::fun(vec![Type::BOOL], Type::BOOL),
        };

        let id = Var(u32::MAX - 2);
        let x = Var(u32::MAX - 1);
        let id_value = fndef(vec![x], Expr::Ident(x));

        let def = Define { ident: id, typ: None, value: id_value, span: SPAN };

        let mut inf = TypeInference::new();
        let typed_id = TypeChecker::new(&mut State::default())
            .check_toplevel_def(&mut inf, Env::new(), def, scheme)
            .expect("Type checking to succeed");

        let typed_x = TypedVar(x, Type::BOOL);
        let id_value = fndef(vec![typed_x.clone()], Expr::Ident(typed_x));

        assert_eq!(
            typed_id,
            Define {
                ident: TypedVar(id, Type::fun(vec![Type::BOOL], Type::BOOL)),
                typ: None,
                value: id_value,
                span: SPAN
            }
        )
    }

    #[test]
    fn forbids_inferred_type_from_being_more_general() {
        let a = RigidVar(u32::MAX);
        let scheme = TypeScheme {
            unbound: set![a],
            constraints: vec![],
            ty: Type::fun(vec![Type::RigidVar(a)], Type::RigidVar(a)),
        };

        let id = Var(u32::MAX - 2);
        let x = Var(u32::MAX - 1);
        let id_value = fndef(
            vec![x],
            Expr::Binary(Box::new(Binary {
                lhs: Expr::Ident(x),
                op: BinaryOp::Add,
                rhs: Expr::Ident(x),
                span: SPAN,
                op_span: SPAN,
            })),
        );

        let def = Define { ident: id, typ: None, value: id_value, span: SPAN };

        let mut inf = TypeInference::new();
        let err = TypeChecker::new(&mut State::default()).check_toplevel_def(
            &mut inf,
            Env::new(),
            def,
            scheme,
        );

        assert_eq!(
            err,
            Err(Error::NotImpld {
                class: TyClass::Addable,
                ty: Type::UnifiableVar(UnifiableVar(0))
            }),
        );
    }
}
