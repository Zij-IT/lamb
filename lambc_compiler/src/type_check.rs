mod constraints;
mod context;
mod env;
mod inference;
mod instantiate;
mod parsing;
mod scheme;
mod substitution;
mod types;
mod unification;

use std::collections::{HashMap, HashSet};

use context::Context;
use lambc_parse::{
    Define, Export, ExportItem, Import, ImportItem, Item, Module,
};

use miette::Diagnostic;
use parsing::ParserContext;
use substitution::{Substitute, SubstitutionContext};
use unification::Unifier;

pub use self::{
    constraints::{Constraint, Qualified, TyClass},
    env::{TypeEnv, VarEnv},
    inference::TypeInference,
    scheme::TypeScheme,
    types::*,
};
use crate::{name_res::Var, type_check::parsing::TypeParser, PathRef, State};

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Diagnostic, thiserror::Error, Debug, Clone, PartialEq)]
pub enum Error {
    #[diagnostic(code("type-checking::no-impl"))]
    #[error("no impl of `{}` found for type `{}`", .class.name(), .ty.name())]
    NotImpld { class: TyClass, ty: Type },
    #[diagnostic(code("type-checking::expected-found"))]
    #[error("found `{}`, but expected `{}`", .got.name(), .expected.name())]
    TypeNotEqual { expected: Type, got: Type },
    #[diagnostic(code("type-checking::infinite-type"))]
    #[error("the inferred type `{}` is infinite", .ty.name())]
    InfiniteType { repr_var: UnifiableVar, ty: Type },
    #[diagnostic(code("type-checking::new-unbound-vars"))]
    #[error("the inferred type introduces new unbound variables")]
    NewUnboundTypes,
    #[diagnostic(code("type-checking::no-such-type"))]
    #[error("no type found with the name `..`")]
    UnknownType,
    #[diagnostic(code("type-checking::new-type-params"))]
    #[error("new generics cannot be introduced here")]
    NewTypeNotAllowed,
    #[diagnostic(code("type-checking::type-param-count"))]
    #[error("The type takes {} type parameters, but received {} instead", .expected, .got)]
    TypeParamCountMismatch { got: usize, expected: i32 },
    #[diagnostic(code("type-checking::missing-type"))]
    #[error("top-level definitions require a type, but none was provided")]
    MissingTypeAscription,
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
        let mut ctx =
            self.build_ctx(modules.iter().flat_map(|i| i.items.as_slice()));

        let mut item_map = HashMap::new();
        for module in modules.iter_mut() {
            let items = std::mem::take(&mut module.items);
            // If this errors we can't properly build the import map because the types
            // of all the variables aren't able to be used.
            let items = self
                .check_items(&mut ctx, module.path, items)
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
        ctx: &mut Context,
        path: PathRef,
        items: Vec<Item<Var>>,
    ) -> std::result::Result<Vec<Item<TypedVar>>, ()> {
        let mut typed_items = Vec::with_capacity(items.len());
        for item in items {
            match item {
                Item::Def(def) => {
                    let scheme = ctx.vars_mut().type_of(def.ident);
                    let res = self.check_toplevel_def(ctx, def, scheme);

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

    fn check_toplevel_def(
        &self,
        ctx: &mut Context,
        def: Define<Var>,
        scheme: TypeScheme,
    ) -> Result<Define<TypedVar>> {
        if def.value.is_recursive() {
            // Nothing to do in the case of a recursive bound because top-level definitions
            // now require types.
        }

        let mut inf = TypeInference::new(ctx);
        let mut qual_value = inf.check_expr(def.value, scheme.ty.clone());
        qual_value.cons.extend(scheme.constraints.clone());
        Unifier::new(inf.ctx).unify(qual_value.cons.clone())?;

        let mut sub = Substitute::new(inf.ctx);
        let (mut unbound, ty) = sub.rigidify(scheme.ty);
        let (ast_unbound, expr) = sub.rigidify_expr(qual_value.item);
        unbound.extend(ast_unbound);

        let (con_unbound, _cons) = sub.rigidify_constraints(qual_value.cons);
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

    fn build_ctx<'a, I: Iterator<Item = &'a Item<Var>>>(
        &mut self,
        items: I,
    ) -> Context {
        let mut ctx = Context::new();
        Self::add_builtin_functions(&mut ctx);
        Self::add_builtin_types(&mut ctx);

        for item in items {
            match item {
                Item::Def(id) => {
                    let Define { ident, typ, value: _, span: _ } = id;
                    let res = match typ
                        .as_ref()
                        .map(|sc| TypeParser::new(&mut ctx).parse_scheme(sc))
                    {
                        Some(res) => res,
                        None => Err(Error::MissingTypeAscription),
                    };

                    let scheme = res.unwrap_or_else(|err| {
                        self.state.add_error(err, None);
                        TypeScheme {
                            unbound: Default::default(),
                            constraints: Default::default(),
                            ty: Type::Error,
                        }
                    });

                    ctx.vars_mut().add_scheme(*ident, scheme);
                }
            }
        }

        ctx
    }

    fn add_builtin_functions(ctx: &mut Context) {
        let to_simple_scheme = |t| TypeScheme {
            unbound: Default::default(),
            constraints: vec![],
            ty: t,
        };

        let assert_ty = Type::fun(vec![Type::BOOL], Type::NIL);
        ctx.vars_mut().add_scheme(Var::ASSERT, to_simple_scheme(assert_ty));

        let user_char_ty = Type::fun(vec![], Type::USV);
        ctx.vars_mut()
            .add_scheme(Var::USER_CHAR, to_simple_scheme(user_char_ty));

        let user_int_ty = Type::fun(vec![], Type::INT);
        ctx.vars_mut()
            .add_scheme(Var::USER_INT, to_simple_scheme(user_int_ty));

        let rand_ty = Type::fun(vec![], Type::INT);
        ctx.vars_mut().add_scheme(Var::RAND, to_simple_scheme(rand_ty));

        let to_one_gen_scheme =
            |rigid: RigidVar, make_ty: fn(RigidVar) -> Type| TypeScheme {
                unbound: HashSet::from([rigid]),
                constraints: vec![],
                ty: make_ty(rigid),
            };

        let print_ty = |rig| Type::fun(vec![Type::RigidVar(rig)], Type::NIL);
        let print_ty = to_one_gen_scheme(ctx.gen_rigid_var(), print_ty);
        ctx.vars_mut().add_scheme(Var::PRINT, print_ty);

        let println_ty = |rig| Type::fun(vec![Type::RigidVar(rig)], Type::NIL);
        let println_ty = to_one_gen_scheme(ctx.gen_rigid_var(), println_ty);
        ctx.vars_mut().add_scheme(Var::PRINTLN, println_ty);
    }

    fn add_builtin_types(ctx: &mut Context) {
        ctx.add_type(Var::INT, Type::INT);
        ctx.add_type(Var::NIL, Type::NIL);
        ctx.add_type(Var::USV, Type::USV);
        ctx.add_type(Var::BOOL, Type::BOOL);
        ctx.add_type(Var::NEVER, Type::NEVER);
        ctx.add_type(Var::DOUBLE, Type::DOUBLE);
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
}

#[cfg(test)]
mod test {
    use std::collections::HashSet;

    use lambc_parse::{Binary, BinaryOp, Define, Expr, FnDef, Span};
    use pretty_assertions::assert_eq;

    use super::{Error, TypeChecker};
    use crate::{
        name_res::Var,
        type_check::{
            context::Context, RigidVar, TyClass, Type, TypeScheme, TypedVar,
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

    fn fndef<T>(args: Vec<T>, body: Expr<T>) -> Expr<T> {
        Expr::FnDef(Box::new(FnDef {
            args,
            body,
            recursive: false,
            span: SPAN,
        }))
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

        let mut ctx = Context::new();
        let typed_id = TypeChecker::new(&mut State::default())
            .check_toplevel_def(&mut ctx, def, scheme.clone())
            .expect("Type checking to succeed");

        let rigid_x = RigidVar(a);
        let typed_x = TypedVar(x, Type::RigidVar(rigid_x));
        let id_value = fndef(vec![typed_x.clone()], Expr::Ident(typed_x));

        assert_eq!(
            typed_id,
            Define {
                ident: TypedVar(id, scheme.ty.clone()),
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

        let mut ctx = Context::new();
        let typed_id = TypeChecker::new(&mut State::default())
            .check_toplevel_def(&mut ctx, def, scheme)
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

        let mut ctx = Context::new();
        let err = TypeChecker::new(&mut State::default())
            .check_toplevel_def(&mut ctx, def, scheme);

        assert_eq!(
            err,
            Err(Error::NotImpld {
                class: TyClass::Addable,
                ty: Type::RigidVar(a)
            }),
        );
    }
}
