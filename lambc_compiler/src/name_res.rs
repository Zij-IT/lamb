use std::collections::HashMap;

use lambc_parse::{
    ArrayPattern, Binary, Block, Call, Case, CaseArm, Define, Else, Export,
    ExportItem, Expr, ExprStatement, FnDef, Group, Ident, IdentPattern, If,
    IfCond, Import, ImportItem, Index, InnerPattern, Item, List, Module,
    Pattern, Return, Span, Statement, Unary,
};
use miette::Diagnostic;

use crate::{PathRef, State};

#[derive(Diagnostic, thiserror::Error, Debug)]
pub enum Error {
    #[error("cannot find `{}` in this scope", .name)]
    NotFound {
        name: String,
        #[label]
        span: Span,
    },
    #[error("`{}` not found", .name)]
    NotExported {
        name: String,
        #[label]
        span: Span,
        #[label("this module doesn't export `{}`", .name)]
        import_span: Span,
    },
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct Var(u32);

pub struct Resolver<'s> {
    pub state: &'s mut State,
    pub start: u32,
}

impl<'s> Resolver<'s> {
    pub fn new(state: &'s mut State) -> Self {
        Self { state, start: 0 }
    }

    pub fn resolve(
        &mut self,
        modules: Vec<Module<Ident, PathRef>>,
    ) -> Vec<Module<Var, PathRef>> {
        let mut mapped = Vec::new();
        let mut importmap = HashMap::new();
        let mut scopemap = HashMap::new();
        let exportmap = modules
            .iter()
            .map(|md| (md.path, self.create_exportmap(&md.exports)))
            .collect::<HashMap<_, _>>();

        for module in &modules {
            let (imports, scope) =
                self.create_scope_and_resolve_imports(module, &exportmap);

            importmap.insert(module.path, imports);
            scopemap.insert(module.path, scope);
        }

        for module in modules {
            let exports = self
                .resolve_exports(&module.exports, &exportmap[&module.path]);

            let items = self.resolve_items(
                scopemap.get_mut(&module.path).expect("module removed?"),
                module.items,
            );

            mapped.push(Module {
                exports,
                imports: std::mem::take(
                    importmap
                        .get_mut(&module.path)
                        .expect("module was removed?"),
                ),
                items,
                path: module.path,
                span: module.span,
            })
        }

        mapped
    }

    fn create_exportmap(
        &mut self,
        exports: &[Export<Ident>],
    ) -> HashMap<String, Var> {
        let mut map = HashMap::new();
        for export in exports {
            for item in &export.items {
                let name =
                    item.alias.as_ref().unwrap_or(&item.item).raw.clone();

                map.insert(name, self.fresh());
            }
        }

        map
    }

    fn create_scope_and_resolve_imports(
        &mut self,
        md: &Module<Ident, PathRef>,
        exports: &HashMap<PathRef, HashMap<String, Var>>,
    ) -> (Vec<Import<Var, PathRef>>, Scope) {
        let mut scope = Scope::new(md.path);
        scope.add_builtin_vars(|| self.fresh());

        let mut imports = Vec::new();
        for import in &md.imports {
            let import_name =
                import.name.as_ref().map(|i| (&i.raw, self.fresh()));

            if let Some((name, var)) = import_name {
                scope.add_var(name, var);
            }

            let mut items = Vec::new();
            let export = &exports[&import.file];
            for item in import.items.iter() {
                let name = &item.alias.as_ref().unwrap_or(&item.item).raw;
                let exported = &item.item.raw;
                let var = if let Some(var) = export.get(exported) {
                    scope.add_var(name, *var);
                    *var
                } else {
                    let path = self.state.resolve_path(scope.module);
                    self.state.add_error(
                        Error::NotExported {
                            name: item.item.raw.clone(),
                            span: item.item.span,
                            import_span: import.path_span,
                        },
                        std::fs::read_to_string(path).ok(),
                    );

                    self.define_new_var(
                        &mut scope,
                        item.alias.as_ref().unwrap_or(&item.item),
                    )
                };

                items.push(ImportItem {
                    item: var,
                    alias: item.alias.as_ref().map(|_| var),
                    span: item.span,
                })
            }

            let import = Import {
                file: import.file,
                name: import_name.map(|i| i.1),
                items,
                star: import.star,
                span: import.span,
                path_span: import.path_span,
            };

            imports.push(import);
        }

        for item in &md.items {
            match item {
                Item::Def(def) => self.define_new_var(&mut scope, &def.ident),
            };
        }

        (imports, scope)
    }

    fn resolve_exports(
        &self,
        exports: &[Export<Ident>],
        exportmap: &HashMap<String, Var>,
    ) -> Vec<Export<Var>> {
        exports
            .iter()
            .map(|Export { items, span }| {
                let exports = items
                    .iter()
                    .map(|ExportItem { item, alias, span }| ExportItem {
                        item: exportmap[&item.raw],
                        // We want the alias to point to the original name, so we use the
                        // the actual name for the alias
                        alias: alias.as_ref().map(|_| exportmap[&item.raw]),
                        span: span.clone(),
                    })
                    .collect();

                Export { items: exports, span: span.clone() }
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
                    // TODO: Add type information in `Scope`
                    typ: None,
                    span: def.span.clone(),
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
                let value = if def.value.is_recursive() {
                    self.define_new_var(scope, &def.ident);
                    self.resolve_expr(scope, def.value)
                } else {
                    let value = self.resolve_expr(scope, def.value);
                    self.define_new_var(scope, &def.ident);
                    value
                };

                Statement::Define(Define {
                    // This is incorrect for recursive bindings
                    ident: self.define_new_var(scope, &def.ident),
                    value,
                    typ: None,
                    span: def.span.clone(),
                })
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
                InnerPattern::Array(self.array_pattern(arr, scope))
            }
        }
    }

    fn resolve_ident_pattern(
        &mut self,
        scope: &mut Scope,
        i: IdentPattern<Ident>,
    ) -> Box<IdentPattern<Var>> {
        Box::new(IdentPattern {
            ident: self.define_new_var(scope, &i.ident),
            bound: i
                .bound
                .map(|i| Box::new(self.resolve_inner_pattern(scope, *i))),
            span: i.span,
        })
    }

    fn array_pattern(
        &mut self,
        arr: Box<ArrayPattern<Ident>>,
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

    fn define_new_var(&mut self, scope: &mut Scope, i: &Ident) -> Var {
        let ident = self.fresh();
        scope.add_var(&i.raw, ident);
        ident
    }

    fn find_var(&mut self, scope: &mut Scope, i: &Ident) -> Var {
        match scope.get_var(&i.raw) {
            Some(v) => v,
            None => {
                let path = self.state.resolve_path(scope.module);
                self.state.add_error(
                    Error::NotFound { name: i.raw.clone(), span: i.span },
                    std::fs::read_to_string(path).ok(),
                );

                self.fresh()
            }
        }
    }

    fn fresh(&mut self) -> Var {
        self.start = self.start.checked_add(1).expect("Too many names.");
        Var(self.start - 1)
    }
}

#[derive(Default)]
struct Scope {
    vars: HashMap<String, Var>,
    parent: Option<Box<Self>>,
    module: PathRef,
}

impl Scope {
    pub fn new(module: PathRef) -> Self {
        Self { module, ..Default::default() }
    }

    pub fn add_builtin_vars(
        &mut self,
        mut new_id: impl FnMut() -> Var,
    ) -> &mut Self {
        self.add_var("assert", new_id());
        self.add_var("print", new_id());
        self.add_var("println", new_id());
        self.add_var("rand", new_id());
        self.add_var("user_int", new_id());
        self.add_var("user_char", new_id());
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

    pub fn begin(&mut self) {
        let parent = std::mem::replace(self, Self::new(self.module));
        self.parent = Some(Box::new(parent));
    }

    pub fn end(&mut self) {
        *self = *self.parent.take().expect("Closing the global scope, eh?")
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
