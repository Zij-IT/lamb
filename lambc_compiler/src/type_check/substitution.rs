use std::collections::{HashMap, HashSet};

use super::{
    tree::{
        ArrayPattern, Binary, Block, Call, Case, CaseArm, Else, Expr,
        ExprStmt, FnDef, Group, IdentPattern, If, IfCond, Index, InnerPattern,
        List, LocalDefine, Pattern, Return, Stmt, Unary,
    },
    Constraint, FnType, RigidVar, Type, UnifiableVar,
};
use crate::type_check::TypedVar;

pub trait SubstitutionContext {
    /// Returns the current value of `var`
    fn get_value(&mut self, var: UnifiableVar) -> Option<Type>;

    /// Given a `UnifiableVar`, returns the current root
    fn get_root(&mut self, var: UnifiableVar) -> UnifiableVar;

    /// Returns a new `RigidVar`
    fn gen_rigid_var(&mut self) -> RigidVar;
}

pub struct Substitute<'ctx, C> {
    ctx: &'ctx mut C,
    unif_to_rigid: HashMap<UnifiableVar, RigidVar>,
}

impl<'ctx, C: SubstitutionContext> Substitute<'ctx, C> {
    pub fn new(ctx: &'ctx mut C) -> Self {
        Self { ctx, unif_to_rigid: Default::default() }
    }
    /// Performs substitution of `UnifiableVar` for `RigidVar`. This is used
    /// after unification to create a type with unbound type variables.
    pub(super) fn rigidify(&mut self, ty: Type) -> (HashSet<RigidVar>, Type) {
        match ty {
            ty @ (Type::Con(..) | Type::RigidVar(..) | Type::Error) => {
                (HashSet::new(), ty)
            }
            Type::List(elem) => {
                let (unbound, elem) = self.rigidify(*elem);
                (unbound, Type::List(Box::new(elem)))
            }
            Type::UnifiableVar(v) => {
                let root = self.ctx.get_root(v);
                match self.ctx.get_value(root) {
                    Some(ty) => self.rigidify(ty),
                    None => {
                        let tyvar = self.tyvar_for_unifier(root);
                        let mut unbound = HashSet::new();
                        unbound.insert(tyvar);
                        (unbound, Type::RigidVar(tyvar))
                    }
                }
            }
            Type::Fun(fn_ty) => {
                let mut unbound = HashSet::new();
                let args = fn_ty
                    .args
                    .into_iter()
                    .map(|ty| {
                        let (s, ty) = self.rigidify(ty);
                        unbound.extend(s);
                        ty
                    })
                    .collect();

                let (ret_s, ret_ty) = self.rigidify(*fn_ty.ret_type);
                unbound.extend(ret_s);

                (
                    unbound,
                    Type::Fun(FnType { args, ret_type: Box::new(ret_ty) }),
                )
            }
        }
    }

    /// Substitutes the [`UnifiableVar`](`UnifiableVar`) within a [`Constraint`] for the [`RigidVar`]
    pub(super) fn rigidify_constraints(
        &mut self,
        constraints: Vec<Constraint>,
    ) -> (HashSet<RigidVar>, Vec<Constraint>) {
        constraints
            .into_iter()
            .map(|con| match con {
                Constraint::IsIn(tc, ty) => {
                    let (hs, ty) = self.rigidify(ty);
                    (hs, Constraint::IsIn(tc, ty))
                }
                Constraint::TypeEqual { expected, got } => {
                    let (mut hse, expected) = self.rigidify(expected);
                    let (hsg, got) = self.rigidify(got);
                    hse.extend(hsg);
                    (hse, Constraint::TypeEqual { expected, got })
                }
            })
            .fold(
                Default::default(),
                |(mut hs, mut vec), (next_hs, next_vec)| {
                    hs.extend(next_hs);
                    vec.push(next_vec);
                    (hs, vec)
                },
            )
    }

    /// Substitutes the [`UnifiableVar`](`UnifiableVar`) within an [`Expr`] for the [`RigidVar`]
    pub(super) fn rigidify_expr(
        &mut self,
        expr: Expr,
    ) -> (HashSet<RigidVar>, Expr) {
        match expr {
            Expr::Nil(n) => (HashSet::new(), Expr::Nil(n)),
            Expr::Int(i) => (HashSet::new(), Expr::Int(i)),
            Expr::Double(f) => (HashSet::new(), Expr::Double(f)),
            Expr::Usv(c) => (HashSet::new(), Expr::Usv(c)),
            Expr::Bool(b) => (HashSet::new(), Expr::Bool(b)),
            Expr::String(s) => (HashSet::new(), Expr::String(s)),
            Expr::Ident(TypedVar(var, ty)) => {
                let (unbound, ty) = self.rigidify(ty);
                (unbound, Expr::Ident(TypedVar(var, ty)))
            }
            Expr::List(list) => {
                let List { values, span } = *list;
                let mut unbound = HashSet::new();
                let values = values
                    .into_iter()
                    .map(|i| {
                        let (un, expr) = self.rigidify_expr(i);
                        unbound.extend(un);
                        expr
                    })
                    .collect();

                (unbound, Expr::List(Box::new(List { values, span })))
            }
            Expr::Group(g) => {
                let Group { value, span } = *g;
                let (unbound, expr) = self.rigidify_expr(value);
                (unbound, Expr::Group(Box::new(Group { value: expr, span })))
            }
            Expr::FnDef(fndef) => {
                let FnDef { args, body, recursive, span } = *fndef;
                let mut unbound = HashSet::new();
                let args = args
                    .into_iter()
                    .map(|TypedVar(v, ty)| {
                        let (un, ty) = self.rigidify(ty);
                        unbound.extend(un);
                        TypedVar(v, ty)
                    })
                    .collect();

                let (un, body) = self.rigidify_expr(body);
                unbound.extend(un);

                (
                    unbound,
                    Expr::FnDef(Box::new(FnDef {
                        args,
                        body,
                        recursive,
                        span,
                    })),
                )
            }
            Expr::Block(block) => {
                let (unbound, block) = self.rigidify_block_raw(*block);
                (unbound, Expr::Block(Box::new(block)))
            }
            Expr::If(iff) => {
                let If { cond, elif, els_, span } = *iff;
                let (mut unbound, cond) = self.rigidify_ifcond(cond);
                let elif = elif
                    .into_iter()
                    .map(|IfCond { cond, body, span }| {
                        let (un, cond) = self.rigidify_expr(cond);
                        unbound.extend(un);
                        let (un, block) = self.rigidify_block_raw(body);
                        unbound.extend(un);
                        IfCond { cond, body: block, span }
                    })
                    .collect();

                let els_ = els_.map(|Else { body, span }| {
                    let (un, body) = self.rigidify_block_raw(body);
                    unbound.extend(un);
                    Else { body, span }
                });

                (unbound, Expr::If(Box::new(If { cond, elif, els_, span })))
            }
            Expr::Index(idx) => {
                let Index { lhs, rhs, span } = *idx;
                let (mut unbound, idxee) = self.rigidify_expr(lhs);
                let (un, index) = self.rigidify_expr(rhs);
                unbound.extend(un);
                (
                    unbound,
                    Expr::Index(Box::new(Index {
                        lhs: idxee,
                        rhs: index,
                        span,
                    })),
                )
            }
            Expr::Call(call) => {
                let Call { callee, args, span } = *call;
                let (mut unbound, func) = self.rigidify_expr(callee);
                let args = args
                    .into_iter()
                    .map(|a| {
                        let (un, a) = self.rigidify_expr(a);
                        unbound.extend(un);
                        a
                    })
                    .collect();

                (
                    unbound,
                    Expr::Call(Box::new(Call { callee: func, args, span })),
                )
            }
            Expr::Unary(unary) => {
                let Unary { rhs, op, span, op_span } = *unary;
                let (unbound, rhs) = self.rigidify_expr(rhs);
                (
                    unbound,
                    Expr::Unary(Box::new(Unary { rhs, op, span, op_span })),
                )
            }
            Expr::Binary(binary) => {
                let Binary { lhs, op, rhs, span, op_span } = *binary;
                let (mut unbound, lhs) = self.rigidify_expr(lhs);
                let (un, rhs) = self.rigidify_expr(rhs);
                unbound.extend(un);

                (
                    unbound,
                    Expr::Binary(Box::new(Binary {
                        lhs,
                        rhs,
                        op,
                        span,
                        op_span,
                    })),
                )
            }
            Expr::Case(c) => self.rigidify_case(*c),
            Expr::Return(e) => {
                let Return { value, span } = *e;
                if let Some(value) = value {
                    let (unbound, value) = self.rigidify_expr(value);
                    (
                        unbound,
                        Expr::Return(Box::new(Return {
                            value: Some(value),
                            span,
                        })),
                    )
                } else {
                    (
                        Default::default(),
                        Expr::Return(Box::new(Return { value, span })),
                    )
                }
            }
        }
    }

    fn rigidify_ifcond(
        &mut self,
        ifcond: IfCond,
    ) -> (HashSet<RigidVar>, IfCond) {
        let IfCond { cond, body, span } = ifcond;
        let (mut unbound, cond) = self.rigidify_expr(cond);
        let (un, block) = self.rigidify_block_raw(body);
        unbound.extend(un);

        (unbound, IfCond { cond, body: block, span })
    }

    fn rigidify_block_raw(
        &mut self,
        block: Block,
    ) -> (HashSet<RigidVar>, Block) {
        let Block { statements, value, span } = block;
        let mut unbound = HashSet::new();

        let statements = statements
            .into_iter()
            .map(|s| {
                let (un, s) = self.rigidify_stmt(s);
                unbound.extend(un);
                s
            })
            .collect();

        (unbound, Block { statements, value, span })
    }

    fn rigidify_stmt(&mut self, stmt: Stmt) -> (HashSet<RigidVar>, Stmt) {
        match stmt {
            Stmt::Def(def) => {
                let LocalDefine { ident, typ, value, span } = def;
                let (mut unbound, ty) = self.rigidify(ident.1);
                let (un, ex) = self.rigidify_expr(value);
                unbound.extend(un);

                (
                    unbound,
                    Stmt::Def(LocalDefine {
                        ident: TypedVar(ident.0, ty),
                        typ,
                        value: ex,
                        span,
                    }),
                )
            }
            Stmt::Expr(ex) => {
                let ExprStmt { expr, span } = ex;
                let (unbound, expr) = self.rigidify_expr(expr);
                (unbound, Stmt::Expr(ExprStmt { expr, span }))
            }
        }
    }

    fn rigidify_case(&mut self, c: Case) -> (HashSet<RigidVar>, Expr) {
        let Case { scrutinee, arms, span } = c;
        let (mut un, scrutinee) = self.rigidify_expr(scrutinee);
        let arms = arms
            .into_iter()
            .map(|arm| {
                let (u, arm) = self.rigidify_case_arm(arm);
                un.extend(u);
                arm
            })
            .collect();

        (un, Expr::Case(Box::new(Case { scrutinee, arms, span })))
    }

    fn rigidify_case_arm(
        &mut self,
        arm: CaseArm,
    ) -> (HashSet<RigidVar>, CaseArm) {
        let CaseArm { pattern, body, span } = arm;
        let (mut un, pattern) = self.rigidify_pattern(pattern);
        let (u, body) = self.rigidify_expr(body);
        un.extend(u);

        (un, CaseArm { pattern, body, span })
    }

    fn rigidify_pattern(
        &mut self,
        pattern: Pattern,
    ) -> (HashSet<RigidVar>, Pattern) {
        let Pattern { inner, span } = pattern;
        let mut un = HashSet::default();
        let inner = inner
            .into_iter()
            .map(|ip| {
                let (u, ip) = self.rigidify_inner_pattern(ip);
                un.extend(u);
                ip
            })
            .collect();

        (un, Pattern { inner, span })
    }

    fn rigidify_inner_pattern(
        &mut self,
        ip: InnerPattern,
    ) -> (HashSet<RigidVar>, InnerPattern) {
        match ip {
            InnerPattern::Rest(_) => (HashSet::default(), ip),
            InnerPattern::Literal(..) => (HashSet::default(), ip),
            InnerPattern::Array(arr) => {
                let ArrayPattern { patterns, span } = *arr;
                let mut un = HashSet::default();
                let patterns = patterns
                    .into_iter()
                    .map(|p| {
                        let (u, p) = self.rigidify_pattern(p);
                        un.extend(u);
                        p
                    })
                    .collect();

                let arr = ArrayPattern { patterns, span };
                (un, InnerPattern::Array(Box::new(arr)))
            }
            InnerPattern::Ident(i) => {
                let IdentPattern { ident: TypedVar(v, ty), bound, span } = *i;
                let (mut un, ty) = self.rigidify(ty);
                let ident = TypedVar(v, ty);

                let bound = match bound {
                    Some(ip) => {
                        let (u, ip) = self.rigidify_inner_pattern(*ip);
                        un.extend(u);
                        Some(Box::new(ip))
                    }
                    None => None,
                };

                (
                    un,
                    InnerPattern::Ident(Box::new(IdentPattern {
                        ident,
                        bound,
                        span,
                    })),
                )
            }
        }
    }

    fn tyvar_for_unifier(&mut self, var: UnifiableVar) -> RigidVar {
        *self
            .unif_to_rigid
            .entry(var)
            .or_insert_with(|| self.ctx.gen_rigid_var())
    }
}
