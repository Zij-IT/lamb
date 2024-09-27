//! Name Resolution
//!
//! This module is responsible for performing the matching of variable references
//! to the correct definition. This is performed across files in the events of
//! imports and exports. During this step aliases are resolved, and variables
//! are pinned to certain definitions. Additionally, this step connects type
//! references to the definition of that type, so that it can be known what
//! `int` the user can be referring to.
//!
//! It should be known that there are two namespaces in which names are resolved:
//! + type names
//! + variable names
//!
//! This allows the user to make the use of a type name for a variable name, without
//! a conflict arising. This enables the user to write (though this isn't encouraged)
//! the following:
//!
//! ```text
//! def int: int = 0;
//! ```
//!
//! Finally, this step is responsible for requiring that patterns are well formed,
//! and bind an identifier in all sub-patterns.

mod pattern;
use std::collections::HashMap;

use lambc_parse::{
    ArrayPattern, Binary, Block, Call, Case, CaseArm, Define, Else, Export,
    ExportItem, Expr, ExprStatement, FnDef, FnType, Generic, Generics, Group,
    Ident, IdentPattern, If, IfCond, Import, ImportItem, Index, InnerPattern,
    Item, List, Module, NamedType, Pattern, Return, SimpleGeneric,
    SimpleGenerics, Span, Statement, Type, Unary,
};
use miette::{Diagnostic, LabeledSpan};

use self::pattern::PatternChecker;
use crate::{PathRef, State};

/// The error type for errors encountered during name-resolution
#[derive(Diagnostic, thiserror::Error, Debug)]
pub enum Error {
    #[diagnostic(code("name-res::not-found"))]
    #[error("cannot find `{}` in this scope", .name)]
    /// A reference is being made to a variable or type which does not exist.
    NotFound {
        name: String,
        #[label]
        span: Span,
    },
    #[diagnostic(code("name-res::not-exported"))]
    #[error("`{}` not found", .name)]
    /// An attempt is being made to import a variable from a file which doesn't
    /// export a variable by that name.
    NotExported {
        name: String,
        #[label]
        span: Span,
        #[label("this module doesn't export `{}`", .name)]
        import_span: Span,
    },
    #[diagnostic(code("name-res::too-many-defs"))]
    #[error("`{}` is defined multiple times", .name)]
    /// A variable is being defined too many times in a conflicting manner, such
    /// as in an argument list, or a pattern.
    MultipleDefinitions {
        name: String,
        #[label(collection)]
        locations: Vec<LabeledSpan>,
    },
    #[diagnostic(code("name-res::missing-bindings"))]
    #[error("`{}` isn't bound in all cases", .binding)]
    /// A patten does not bind an identifier in all sub-patterns.
    MissingBinding {
        binding: String,
        #[label]
        pat_span: Span,
    },
}

/// A struct representing a variable or type reference within a program.
/// In the example `def x: int = 0` both `x` and `int` are converted to
/// `Var`, though `int` will later be converted to the proper type.
#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub struct Var(pub u32);

impl Var {
    /// The `Var` representing the `int` type
    pub const INT: Self = Self(0);
    /// The `Var` representing the `nil` type
    pub const NIL: Self = Self(1);
    /// The `Var` representing the `usv` type
    pub const USV: Self = Self(2);
    /// The `Var` representing the `bool` type
    pub const BOOL: Self = Self(3);
    /// The `Var` representing the `double` type
    pub const DOUBLE: Self = Self(4);
    /// The `Var` representing the `list` type
    pub const LIST: Self = Self(5);
    /// The `Var` representing the `never` type
    pub const NEVER: Self = Self(6);
    /// The `print` builtin
    pub const PRINT: Self = Self(7);
    /// The `println` builtin
    pub const PRINTLN: Self = Self(8);
    /// The `assert` builtin
    pub const ASSERT: Self = Self(9);
    /// The `rand` builtin
    pub const RAND: Self = Self(10);
    /// The `user_char` builtin
    pub const USER_CHAR: Self = Self(11);
    /// The `user_int` builtin
    pub const USER_INT: Self = Self(12);
    #[allow(nonstandard_style)]
    // This must be the last item in the list of vars, and must be updated
    // to not include the next var.
    pub(self) const __BASE: u32 = 13;
}

/// The struct responsible for performing name resolution over a compilation unit.
pub struct Resolver<'s> {
    /// the state to which errors will be reported
    pub state: &'s mut State,
    /// a number representing the current `Var` count.
    // todo: change this name
    pub start: u32,
}

impl<'s> Resolver<'s> {
    /// Constructs a new `Resolver` which reports errors through `state`.
    pub fn new(state: &'s mut State) -> Self {
        Self { state, start: Var::__BASE }
    }

    /// Performs name resolution over all the modules, reporting any errors,
    /// such as missing variable definitions or duplicate definitions to the
    /// state.
    ///
    /// Note: The length of the output is guarunteed to be the same as the length
    /// of `modules`.
    pub fn resolve_modules(
        &mut self,
        modules: Vec<Module<Ident, PathRef>>,
    ) -> Vec<Module<Var, PathRef>> {
        let exportmap = modules
            .iter()
            .map(|md| (md.path, self.create_exportmap(&md.exports)))
            .collect::<HashMap<_, _>>();

        modules
            .into_iter()
            .map(|module| {
                self.report_duplicates_in_module(&module);

                self.resolve_module(&exportmap, module)
            })
            .collect()
    }

    fn resolve_module(
        &mut self,
        exportmap: &HashMap<PathRef, ExportMap>,
        module: Module<Ident, PathRef>,
    ) -> Module<Var, PathRef> {
        let mut scope = self.new_module_scope(&module);

        self.forward_declare_items(&module, exportmap, &mut scope);

        let exports = exportmap.get(&module.path).expect("module removed?");

        let imports = self.resolve_imports(&mut scope, &module, exportmap);

        let exports =
            self.resolve_exports(&mut scope, &module.exports, exports);

        let items = self.resolve_items(&mut scope, module.items);

        Module {
            exports,
            imports,
            items,
            path: module.path,
            span: module.span,
        }
    }

    fn resolve_imports(
        &mut self,
        scope: &mut Scope,
        md: &Module<Ident, PathRef>,
        exports: &HashMap<PathRef, ExportMap>,
    ) -> Vec<Import<Var, PathRef>> {
        md.imports
            .iter()
            .map(|i| self.resolve_import(i, scope, exports))
            .collect()
    }

    fn resolve_import(
        &mut self,
        import: &Import<Ident, PathRef>,
        scope: &mut Scope,
        exports: &HashMap<PathRef, ExportMap>,
    ) -> Import<Var, PathRef> {
        let import_name =
            import.name.as_ref().map(|i| (&i.raw, self.fresh_var()));

        if let Some((name, var)) = import_name {
            scope.add_var(name, var);
        }

        let mut items = Vec::new();
        let export = &exports[&import.file];
        for item in import.items.iter() {
            let name = &item.alias.as_ref().unwrap_or(&item.item).raw;
            let var = if let Some(var) = export.get_from_within(&item.item) {
                scope.add_var(name, var);
                var
            } else {
                self.state.add_error(
                    Error::NotExported {
                        name: item.item.raw.clone(),
                        span: item.item.span,
                        import_span: import.path_span,
                    },
                    Some(scope.module),
                );

                self.define_new_var(
                    scope,
                    item.alias.as_ref().unwrap_or(&item.item),
                )
            };

            items.push(ImportItem {
                item: var,
                alias: item.alias.as_ref().map(|_| var),
                span: item.span,
            })
        }

        Import {
            file: import.file,
            name: import_name.map(|i| i.1),
            items,
            star: import.star,
            span: import.span,
            path_span: import.path_span,
        }
    }

    fn resolve_exports(
        &mut self,
        scope: &mut Scope,
        exports: &[Export<Ident>],
        exportmap: &ExportMap,
    ) -> Vec<Export<Var>> {
        exports
            .iter()
            .map(|Export { items, span }| {
                let exports = items
                    .iter()
                    .map(|ExportItem { item, alias, span }| ExportItem {
                        // The export-map is the first thing that is created, which creates
                        // `Var` entries for each exported item. When attempting to define
                        // an item in the global scope, it's checked if the item is exported,
                        // and if so, that var is used.
                        //
                        // If we use: `exportmap.get_from_within(..)` it's possible that
                        // a variable that is exported doesn't exist in the global scope.
                        // By using `self.find_var(..)` we check that each exported item is
                        // locatable in the global scope of that module.
                        item: self.find_var(scope, item),
                        alias: alias.as_ref().map(|i| {
                            exportmap.get_from_outside(i).expect(
                                "Exports should all be found within the map",
                            )
                        }),
                        span: *span,
                    })
                    .collect();

                Export { items: exports, span: *span }
            })
            .collect()
    }

    fn resolve_items(
        &mut self,
        scope: &mut Scope,
        items: Vec<Item<Ident>>,
    ) -> Vec<Item<Var>> {
        items.into_iter().map(|i| self.resolve_item(scope, i)).collect()
    }

    fn resolve_item(
        &mut self,
        scope: &mut Scope,
        item: Item<Ident>,
    ) -> Item<Var> {
        match item {
            Item::Def(def) => {
                let ident = self.find_var(scope, &def.ident);
                let value = self.resolve_expr(scope, def.value);
                Item::Def(Define {
                    ident,
                    value,
                    typ: def.typ.map(|t| self.resolve_type(scope, t)),
                    span: def.span,
                })
            }
        }
    }

    fn resolve_stmt(
        &mut self,
        scope: &mut Scope,
        stmt: Statement<Ident>,
    ) -> Statement<Var> {
        match stmt {
            Statement::Define(def) => {
                let (ident, value) = if def.value.is_recursive() {
                    let ident = self.define_new_var(scope, &def.ident);
                    let value = self.resolve_expr(scope, def.value);
                    (ident, value)
                } else {
                    let value = self.resolve_expr(scope, def.value);
                    let ident = self.define_new_var(scope, &def.ident);
                    (ident, value)
                };

                let typ = def.typ.map(|typ| self.resolve_type(scope, typ));

                Statement::Define(Define { ident, value, typ, span: def.span })
            }
            Statement::Expr(ExprStatement { expr, span }) => {
                Statement::Expr(ExprStatement {
                    expr: self.resolve_expr(scope, expr),
                    span,
                })
            }
        }
    }

    fn resolve_expr(
        &mut self,
        scope: &mut Scope,
        expr: Expr<Ident>,
    ) -> Expr<Var> {
        match expr {
            Expr::Nil(n) => Expr::Nil(n),
            Expr::I64(i) => Expr::I64(i),
            Expr::F64(f) => Expr::F64(f),
            Expr::Path(p) => Expr::Path(p),
            Expr::Char(c) => Expr::Char(c),
            Expr::Bool(b) => Expr::Bool(b),
            Expr::String(s) => Expr::String(s),
            Expr::Ident(i) => Expr::Ident(self.find_var(scope, &i)),
            Expr::If(i) => Expr::If(self.resolve_if(scope, *i)),
            Expr::List(l) => Expr::List(self.resolve_list(scope, l)),
            Expr::Case(c) => Expr::Case(self.resolve_case(scope, *c)),
            Expr::Call(c) => Expr::Call(self.resolve_call(scope, *c)),
            Expr::Group(g) => Expr::Group(self.resolve_group(scope, *g)),
            Expr::Index(i) => Expr::Index(self.resolve_index(scope, *i)),
            Expr::Unary(u) => Expr::Unary(self.resolve_unary(scope, *u)),
            Expr::Binary(b) => Expr::Binary(self.resolve_binary(scope, *b)),
            Expr::Return(r) => Expr::Return(self.resolve_return(scope, *r)),
            Expr::Block(b) => {
                Expr::Block(Box::new(self.resolve_block(scope, *b)))
            }
            Expr::FnDef(fndef) => {
                Expr::FnDef(self.resolve_fndef(scope, *fndef))
            }
        }
    }

    fn resolve_fndef(
        &mut self,
        scope: &mut Scope,
        fndef: FnDef<Ident>,
    ) -> Box<FnDef<Var>> {
        let FnDef { args, body, recursive, span } = fndef;
        scope.begin();

        self.report_if_duplicates(
            scope.module,
            args.iter().map(|a| (a.raw.as_str(), a.span)),
        );

        let args =
            args.into_iter().map(|i| self.define_new_var(scope, &i)).collect();

        let body = self.resolve_expr(scope, body);
        scope.end();

        Box::new(FnDef { args, body, recursive, span })
    }

    fn resolve_block(
        &mut self,
        scope: &mut Scope,
        b: Block<Ident>,
    ) -> Block<Var> {
        scope.begin();
        let statements = b
            .statements
            .into_iter()
            .map(|i| self.resolve_stmt(scope, i))
            .collect();

        let value = b.value.map(|v| self.resolve_expr(scope, v));
        scope.end();

        Block { statements, value, span: b.span }
    }

    fn resolve_if(
        &mut self,
        scope: &mut Scope,
        if_: If<Ident>,
    ) -> Box<If<Var>> {
        let If { cond, elif, els_, span } = if_;
        let cond = self.resolve_ifcond(scope, cond);

        let elif = elif
            .into_iter()
            .map(|cond| self.resolve_ifcond(scope, cond))
            .collect();

        let els_ = els_.map(|e| Else {
            body: self.resolve_block(scope, e.body),
            span: e.span,
        });

        Box::new(If { cond, elif, els_, span })
    }

    fn resolve_ifcond(
        &mut self,
        scope: &mut Scope,
        cond: IfCond<Ident>,
    ) -> IfCond<Var> {
        IfCond {
            cond: self.resolve_expr(scope, cond.cond),
            body: self.resolve_block(scope, cond.body),
            span: cond.span,
        }
    }

    fn resolve_case(
        &mut self,
        scope: &mut Scope,
        c: Case<Ident>,
    ) -> Box<Case<Var>> {
        let Case { scrutinee, arms, span } = c;
        let scrutinee = self.resolve_expr(scope, scrutinee);
        let arms = arms
            .into_iter()
            .map(|CaseArm { pattern, body, span }| {
                scope.begin();
                let pattern = self.resolve_pattern(scope, pattern);
                let body = self.resolve_expr(scope, body);
                scope.end();

                CaseArm { pattern, body, span }
            })
            .collect();

        Box::new(Case { scrutinee, arms, span })
    }

    fn resolve_return(
        &mut self,
        scope: &mut Scope,
        r: Return<Ident>,
    ) -> Box<Return<Var>> {
        Box::new(Return {
            value: r.value.map(|v| self.resolve_expr(scope, v)),
            span: r.span,
        })
    }

    fn resolve_binary(
        &mut self,
        scope: &mut Scope,
        b: Binary<Ident>,
    ) -> Box<Binary<Var>> {
        Box::new(Binary {
            lhs: self.resolve_expr(scope, b.lhs),
            op: b.op,
            rhs: self.resolve_expr(scope, b.rhs),
            span: b.span,
            op_span: b.op_span,
        })
    }

    fn resolve_unary(
        &mut self,
        scope: &mut Scope,
        u: Unary<Ident>,
    ) -> Box<Unary<Var>> {
        Box::new(Unary {
            rhs: self.resolve_expr(scope, u.rhs),
            op: u.op,
            span: u.span,
            op_span: u.op_span,
        })
    }

    fn resolve_call(
        &mut self,
        scope: &mut Scope,
        c: Call<Ident>,
    ) -> Box<Call<Var>> {
        Box::new(Call {
            callee: self.resolve_expr(scope, c.callee),
            args: c
                .args
                .into_iter()
                .map(|a| self.resolve_expr(scope, a))
                .collect(),
            span: c.span,
        })
    }

    fn resolve_group(
        &mut self,
        scope: &mut Scope,
        g: Group<Ident>,
    ) -> Box<Group<Var>> {
        Box::new(Group {
            value: self.resolve_expr(scope, g.value),
            span: g.span,
        })
    }

    fn resolve_list(
        &mut self,
        scope: &mut Scope,
        list: List<Ident>,
    ) -> List<Var> {
        List {
            values: list
                .values
                .into_iter()
                .map(|i| self.resolve_expr(scope, i))
                .collect(),
            span: list.span,
        }
    }

    fn resolve_index(
        &mut self,
        scope: &mut Scope,
        i: Index<Ident>,
    ) -> Box<Index<Var>> {
        Box::new(Index {
            lhs: self.resolve_expr(scope, i.lhs),
            rhs: self.resolve_expr(scope, i.rhs),
            span: i.span,
        })
    }

    fn resolve_pattern(
        &mut self,
        scope: &mut Scope,
        pattern: Pattern<Ident>,
    ) -> Pattern<Var> {
        self.report_duplicates_in_pattern(scope.module, &pattern);

        // Add all names from this pattern into scope, so that all names use the same `Var`
        let mut names = pattern.binding_names();
        names.sort_by_key(|k| k.raw.as_str());
        names.dedup();

        for name in names {
            self.define_new_var(scope, name);
        }

        let Pattern { inner, span } = pattern;
        let inner = inner
            .into_iter()
            .map(|i| self.resolve_inner_pattern(scope, i))
            .collect();

        Pattern { inner, span }
    }

    fn resolve_inner_pattern(
        &mut self,
        scope: &mut Scope,
        i: InnerPattern<Ident>,
    ) -> InnerPattern<Var> {
        match i {
            InnerPattern::Rest(r) => InnerPattern::Rest(r),
            InnerPattern::Literal(l) => InnerPattern::Literal(l),
            InnerPattern::Ident(i) => {
                InnerPattern::Ident(self.resolve_ident_pattern(scope, *i))
            }
            InnerPattern::Array(arr) => {
                InnerPattern::Array(self.resolve_array_pattern(*arr, scope))
            }
        }
    }

    fn resolve_ident_pattern(
        &mut self,
        scope: &mut Scope,
        i: IdentPattern<Ident>,
    ) -> Box<IdentPattern<Var>> {
        Box::new(IdentPattern {
            ident: scope
                .get_var(&i.ident.raw)
                .expect("The outermost pattern should have define all names"),
            bound: i
                .bound
                .map(|i| Box::new(self.resolve_inner_pattern(scope, *i))),
            span: i.span,
        })
    }

    fn resolve_array_pattern(
        &mut self,
        arr: ArrayPattern<Ident>,
        scope: &mut Scope,
    ) -> Box<ArrayPattern<Var>> {
        Box::new(ArrayPattern {
            patterns: arr
                .patterns
                .into_iter()
                .map(|p| self.resolve_pattern(scope, p))
                .collect(),
            span: arr.span,
        })
    }

    fn resolve_type(
        &mut self,
        scope: &mut Scope,
        t: Type<Ident>,
    ) -> Type<Var> {
        match t {
            Type::Fn(f) => {
                let FnType { args, gens, ret_type, span } = *f;
                let gens = gens.map(|SimpleGenerics { params, span }| {
                    let params = params
                        .into_iter()
                        .map(|i| SimpleGeneric {
                            id: self.define_new_type(scope, &i.id),
                            span: i.span,
                        })
                        .collect();

                    SimpleGenerics { params, span }
                });

                let args = args
                    .into_iter()
                    .map(|t| self.resolve_type(scope, t))
                    .collect();

                let ret_type = ret_type.map(|i| self.resolve_type(scope, i));
                Type::Fn(Box::new(FnType { args, gens, ret_type, span }))
            }
            Type::Named(named) => {
                let NamedType { name, gens, span } = *named;
                let name = self.find_type(scope, &name);
                let gens = gens.map(|Generics { params, span }| {
                    let params = params
                        .into_iter()
                        .map(|i| Generic {
                            id: self.resolve_type(scope, i.id),
                            span: i.span,
                        })
                        .collect();

                    Generics { params, span }
                });

                Type::Named(Box::new(NamedType { name, gens, span }))
            }
        }
    }

    fn define_new_var(&mut self, scope: &mut Scope, i: &Ident) -> Var {
        let ident = self.fresh_var();
        scope.add_var(&i.raw, ident);
        ident
    }

    fn define_new_type(&mut self, scope: &mut Scope, i: &Ident) -> Var {
        let ident = self.fresh_var();
        scope.add_type(&i.raw, ident);
        ident
    }

    fn find_var(&mut self, scope: &mut Scope, i: &Ident) -> Var {
        match scope.get_var(&i.raw) {
            Some(v) => v,
            None => {
                self.state.add_error(
                    Error::NotFound { name: i.raw.clone(), span: i.span },
                    Some(scope.module),
                );

                self.fresh_var()
            }
        }
    }

    fn find_type(&mut self, scope: &mut Scope, i: &Ident) -> Var {
        match scope.get_type(&i.raw) {
            Some(v) => v,
            None => {
                self.state.add_error(
                    Error::NotFound { name: i.raw.clone(), span: i.span },
                    Some(scope.module),
                );

                self.fresh_var()
            }
        }
    }

    fn fresh_var(&mut self) -> Var {
        self.start = self.start.checked_add(1).expect("Too many names.");
        Var(self.start - 1)
    }

    fn create_exportmap(&mut self, exports: &[Export<Ident>]) -> ExportMap {
        let mut map = ExportMap::new();
        for export in exports.iter().flat_map(|e| &e.items) {
            map.insert(export, self.fresh_var());
        }

        map
    }

    fn new_module_scope(&mut self, module: &Module<Ident, PathRef>) -> Scope {
        let mut scope = Scope::new(module.path);
        scope.add_builtin_vars().add_builtin_types();
        scope
    }

    fn forward_declare_items(
        &mut self,
        module: &Module<Ident, PathRef>,
        exportmap: &HashMap<PathRef, ExportMap>,
        scope: &mut Scope,
    ) {
        for item in &module.items {
            match item {
                Item::Def(def) => {
                    if let Some(var) = exportmap
                        .get(&module.path)
                        .expect("Module to exist")
                        .get_from_within(&def.ident)
                    {
                        scope.add_var(&def.ident.raw, var);
                    } else {
                        self.define_new_var(scope, &def.ident);
                    }
                }
            };
        }
    }

    fn report_duplicates_in_module(&mut self, md: &Module<Ident, PathRef>) {
        let global_defs = md
            .imports
            .iter()
            .flat_map(|i| {
                i.items
                    .iter()
                    .map(|ii| {
                        let name =
                            ii.alias.as_ref().unwrap_or(&ii.item).raw.as_str();
                        (name, ii.span)
                    })
                    .chain(
                        i.name
                            .as_ref()
                            .map(|name| (name.raw.as_str(), name.span)),
                    )
            })
            .chain(md.items.iter().map(|item| match item {
                Item::Def(def) => (def.ident.raw.as_str(), def.span),
            }));

        self.report_if_duplicates(md.path, global_defs);

        let exported_names = md.exports.iter().flat_map(|e| {
            e.items.iter().map(|item| {
                let name = item.alias.as_ref().unwrap_or(&item.item);
                (name.raw.as_str(), item.span)
            })
        });

        self.report_if_duplicates(md.path, exported_names);
    }

    fn report_duplicates_in_pattern(
        &mut self,
        module: PathRef,
        pattern: &Pattern<Ident>,
    ) {
        PatternChecker::new(module, self).check(pattern)
    }

    fn report_if_duplicates<'a>(
        &mut self,
        module: PathRef,
        iter: impl Iterator<Item = (&'a str, Span)>,
    ) {
        let mut map: HashMap<_, Vec<_>> = HashMap::new();
        for (s, span) in iter {
            map.entry(s)
                .or_default()
                .push(LabeledSpan::new_with_span(Some("here".into()), span));
        }

        for (s, spans) in map {
            if spans.len() > 1 {
                self.state.add_error(
                    Error::MultipleDefinitions {
                        name: s.into(),
                        locations: spans,
                    },
                    Some(module),
                )
            }
        }
    }
}

#[derive(Default)]
struct Scope {
    vars: HashMap<String, Var>,
    types: HashMap<String, Var>,
    parent: Option<Box<Self>>,
    module: PathRef,
}

impl Scope {
    pub fn new(module: PathRef) -> Self {
        Self { module, ..Default::default() }
    }

    pub fn add_builtin_vars(&mut self) -> &mut Self {
        self.add_var("assert", Var::ASSERT);
        self.add_var("print", Var::PRINT);
        self.add_var("println", Var::PRINTLN);
        self.add_var("rand", Var::RAND);
        self.add_var("user_int", Var::USER_INT);
        self.add_var("user_char", Var::USER_CHAR);
        self
    }

    pub fn add_builtin_types(&mut self) -> &mut Self {
        self.add_type("int", Var::INT);
        self.add_type("usv", Var::USV);
        self.add_type("bool", Var::BOOL);
        self.add_type("nil", Var::NIL);
        self.add_type("list", Var::LIST);
        self.add_type("double", Var::DOUBLE);
        self
    }

    pub fn add_var<S: Into<String>>(&mut self, item: S, var: Var) {
        self.vars.insert(item.into(), var);
    }

    pub fn get_var(&self, item: &String) -> Option<Var> {
        match self.vars.get(item).copied() {
            Some(v) => Some(v),
            None => self.parent.as_ref().and_then(|p| p.get_var(item)),
        }
    }

    pub fn add_type<S: Into<String>>(&mut self, item: S, t: Var) {
        self.types.insert(item.into(), t);
    }

    pub fn get_type(&self, item: &String) -> Option<Var> {
        match self.types.get(item).copied() {
            Some(v) => Some(v),
            None => self.parent.as_ref().and_then(|p| p.get_type(item)),
        }
    }

    pub fn begin(&mut self) {
        let parent = std::mem::replace(self, Self::new(self.module));
        self.parent = Some(Box::new(parent));
    }

    pub fn end(&mut self) {
        *self = *self.parent.take().expect("Closing the global scope, eh?")
    }
}

struct ExportMap {
    within: HashMap<String, Var>,
    outside: HashMap<String, Var>,
}

impl ExportMap {
    pub fn new() -> Self {
        Self { within: HashMap::new(), outside: HashMap::new() }
    }

    pub fn insert(&mut self, export: &ExportItem<Ident>, v: Var) {
        let within = export.item.raw.clone();
        let outside =
            export.alias.as_ref().unwrap_or(&export.item).raw.clone();

        self.within.insert(within, v);
        self.outside.insert(outside, v);
    }

    pub fn get_from_within(&self, ident: &Ident) -> Option<Var> {
        self.within.get(&ident.raw).copied()
    }

    pub fn get_from_outside(&self, ident: &Ident) -> Option<Var> {
        self.outside.get(&ident.raw).copied()
    }
}

#[cfg(test)]
mod tests {
    use lambc_parse::{Expr, Ident, Span};

    use super::{Resolver, Scope};
    use crate::State;

    const DUMMY_SPAN: Span = Span::new(0, 0);

    #[test]
    fn resolve_ident() {
        let mut scope = Scope::default();
        let mut state = State::new();
        let mut resolver = Resolver::new(&mut state);
        let ident = Ident { raw: "a".into(), span: DUMMY_SPAN };
        resolver.define_new_var(&mut scope, &ident);
        resolver.resolve_expr(&mut scope, Expr::Ident(ident));
    }

    #[test]
    fn resolve_ident_in_parent() {
        let mut scope = Scope::default();
        let mut state = State::new();
        let mut resolver = Resolver::new(&mut state);
        let ident = Ident { raw: "a".into(), span: DUMMY_SPAN };
        let var = resolver.define_new_var(&mut scope, &ident);
        scope.begin();

        assert_eq!(scope.get_var(&ident.raw), Some(var));
        resolver.resolve_expr(&mut scope, Expr::Ident(ident));
    }

    #[test]
    fn resolve_ident_in_grandparent() {
        let mut scope = Scope::default();
        let mut state = State::new();
        let mut resolver = Resolver::new(&mut state);
        let ident = Ident { raw: "a".into(), span: DUMMY_SPAN };
        let var = resolver.define_new_var(&mut scope, &ident);
        scope.begin();
        scope.begin();

        assert_eq!(scope.get_var(&ident.raw), Some(var));
        resolver.resolve_expr(&mut scope, Expr::Ident(ident));
    }

    #[test]
    fn cant_resolve_ident_in_closed_scope() {
        let mut scope = Scope::default();
        let mut state = State::new();
        let mut resolver = Resolver::new(&mut state);

        scope.begin();
        let ident = Ident { raw: "a".into(), span: DUMMY_SPAN };
        resolver.define_new_var(&mut scope, &ident);
        scope.end();

        assert_eq!(scope.get_var(&ident.raw), None);
    }

    #[test]
    fn resolves_to_most_recent_ident() {
        let mut scope = Scope::default();
        let mut state = State::new();
        let mut resolver = Resolver::new(&mut state);
        let ident = Ident { raw: "a".into(), span: DUMMY_SPAN };

        scope.begin();
        let first = resolver.define_new_var(&mut scope, &ident);
        let last = resolver.define_new_var(&mut scope, &ident);

        assert_ne!(scope.get_var(&ident.raw), Some(first),);
        assert_eq!(scope.get_var(&ident.raw), Some(last),);
    }

    #[test]
    fn resolves_ident_and_intermediate_defines_dont_interfere() {
        let mut scope = Scope::default();
        let mut state = State::new();
        let mut resolver = Resolver::new(&mut state);
        let a = Ident { raw: "a".into(), span: DUMMY_SPAN };
        let b = Ident { raw: "b".into(), span: DUMMY_SPAN };
        let avar = resolver.define_new_var(&mut scope, &a);

        scope.begin();
        for x in 0..10 {
            scope.begin();
            resolver.define_new_var(
                &mut scope,
                &Ident { raw: format!("x{x}"), span: DUMMY_SPAN },
            );
            scope.end();
        }

        let bvar = resolver.define_new_var(&mut scope, &b);

        assert_ne!(Some(avar), scope.get_var(&b.raw));
        assert_eq!(Some(bvar), scope.get_var(&b.raw));
    }
}
