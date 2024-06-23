use im::HashMap;
use lambc_parse::{
    Binary, BinaryOp, Block, Call, Define, Expr, ExprStatement, FnDef, Group,
    Index, List, Module, Statement, Unary, UnaryOp,
};

use crate::{name_res::Var, PathRef, State};

#[derive(Debug, PartialEq, Clone, Eq)]
pub struct TypedVar(Var, Type);

#[derive(Debug, PartialEq, Clone, Copy, Eq)]
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
    Module(Vec<TypedVar>),
    Fun(FnType),
}

#[derive(Debug, PartialEq, Clone, Eq)]
struct FnType {
    args: Vec<Type>,
    ret_type: Box<Type>,
}

pub struct TypeChecker<'s> {
    state: &'s mut State,
    types: u32,
}

impl<'s> TypeChecker<'s> {
    pub fn new(state: &'s mut State) -> Self {
        Self { state, types: 0 }
    }

    pub fn check_modules(
        &mut self,
        _modules: Vec<Module<Var, PathRef>>,
    ) -> Vec<Module<TypedVar, PathRef>> {
        todo!()
    }

    fn check_expr(
        &mut self,
        env: HashMap<Var, Type>,
        expr: Expr<Var>,
        typ: Type,
    ) -> CheckRes<Expr<TypedVar>> {
        match (expr, typ) {
            (Expr::Nil(n), Type::Nil) => CheckRes::empty(Expr::Nil(n)),
            (Expr::I64(i), Type::Int) => CheckRes::empty(Expr::I64(i)),
            (Expr::Char(c), Type::Usv) => CheckRes::empty(Expr::Char(c)),
            (Expr::Bool(b), Type::Bool) => CheckRes::empty(Expr::Bool(b)),
            (Expr::F64(f), Type::Double) => CheckRes::empty(Expr::F64(f)),
            (Expr::String(s), Type::List(e)) if *e == Type::Usv => {
                CheckRes::empty(Expr::String(s))
            }
            (Expr::FnDef(def), Type::Fun(typ)) => {
                self.check_fndef(env, def, typ)
            }

            (expr, expected_ty) => {
                let (mut out, actual_ty) = self.infer_expr(env, expr);
                out.cons.push(Constraint::TypeEqual(expected_ty, actual_ty));
                out
            }
        }
    }

    fn infer_expr(
        &mut self,
        env: HashMap<Var, Type>,
        expr: Expr<Var>,
    ) -> (CheckRes<Expr<TypedVar>>, Type) {
        match expr {
            // The easy cases!
            Expr::Nil(n) => (CheckRes::empty(Expr::Nil(n)), Type::Nil),
            Expr::I64(i) => (CheckRes::empty(Expr::I64(i)), Type::Int),
            Expr::F64(f) => (CheckRes::empty(Expr::F64(f)), Type::Double),
            Expr::Char(c) => (CheckRes::empty(Expr::Char(c)), Type::Usv),
            Expr::Bool(b) => (CheckRes::empty(Expr::Bool(b)), Type::Bool),
            Expr::Ident(i) => {
                let ty = env[&i].clone();
                (CheckRes::empty(Expr::Ident(TypedVar(i, ty.clone()))), ty)
            }
            Expr::String(s) => (
                CheckRes::empty(Expr::String(s)),
                Type::List(Box::new(Type::Usv)),
            ),
            Expr::FnDef(fndef) => self.infer_fndef(env, *fndef),
            Expr::Call(call) => self.infer_call(env, *call),
            Expr::Index(idx) => self.infer_idx(env, *idx),
            Expr::Group(g) => self.infer_group(env, *g),
            Expr::List(list) => self.infer_list(env, list),
            Expr::Block(block) => self.infer_block(env, *block),
            Expr::If(_) => todo!(),
            Expr::Case(_) => todo!(),
            Expr::Unary(unary) => self.infer_unary(env, *unary),
            Expr::Binary(binary) => self.infer_binary(env, *binary),
            Expr::Return(_) => todo!(),
            Expr::Path(_) => todo!(),
        }
    }

    fn infer_fndef(
        &mut self,
        mut env: HashMap<Var, Type>,
        fndef: FnDef<Var>,
    ) -> (CheckRes<Expr<TypedVar>>, Type) {
        let FnDef { args, body, recursive, span } = fndef;
        let mut new_args = Vec::with_capacity(args.len());
        let mut typs = Vec::with_capacity(args.len());

        for arg in &args {
            let typ = self.fresh_ty_var();
            typs.push(Type::Var(typ));
            new_args.push(TypedVar(*arg, Type::Var(typ)));
            env.insert(*arg, Type::Var(typ));
        }

        let (body_out, body_typ) = self.infer_expr(env, body);
        (
            CheckRes {
                cons: body_out.cons,
                ast: Expr::FnDef(Box::new(FnDef {
                    args: new_args,
                    body: body_out.ast,
                    recursive,
                    span,
                })),
            },
            Type::Fun(FnType { args: typs, ret_type: Box::new(body_typ) }),
        )
    }

    fn check_fndef(
        &mut self,
        mut env: HashMap<Var, Type>,
        def: Box<FnDef<Var>>,
        typ: FnType,
    ) -> CheckRes<Expr<TypedVar>> {
        // TODO: Issue type error instead of panic if arg count doesn't match
        assert_eq!(def.args.len(), typ.args.len());
        let mut new_args = Vec::with_capacity(typ.args.len());
        for (ty, arg) in typ.args.into_iter().zip(def.args.into_iter()) {
            env.insert(arg, ty.clone());
            new_args.push(TypedVar(arg, ty));
        }
        let bodyres = self.check_expr(env, def.body, *typ.ret_type);
        CheckRes::new(
            bodyres.cons,
            Expr::FnDef(Box::new(FnDef {
                args: new_args,
                body: bodyres.ast,
                recursive: def.recursive,
                span: def.span,
            })),
        )
    }

    fn infer_call(
        &mut self,
        env: HashMap<Var, Type>,
        call: Call<Var>,
    ) -> (CheckRes<Expr<TypedVar>>, Type) {
        let mut cons = Vec::new();
        let mut asts = Vec::new();
        let mut typs = Vec::new();

        for arg in call.args {
            let (out, typ) = self.infer_expr(env.clone(), arg);
            cons.extend(out.cons);
            asts.push(out.ast);
            typs.push(typ);
        }

        let ret_typ = Type::Var(self.fresh_ty_var());
        let fn_typ = Type::Fun(FnType {
            args: typs,
            ret_type: Box::new(ret_typ.clone()),
        });

        let fn_out = self.check_expr(env, call.callee, fn_typ);
        cons.extend(fn_out.cons);

        (
            CheckRes::new(
                cons,
                Expr::Call(Box::new(Call {
                    callee: fn_out.ast,
                    args: asts,
                    span: call.span,
                })),
            ),
            ret_typ,
        )
    }

    fn infer_idx(
        &mut self,
        env: HashMap<Var, Type>,
        idx: Index<Var>,
    ) -> (CheckRes<Expr<TypedVar>>, Type) {
        let list_elem_ty = self.fresh_ty_var();
        let list_ty = Type::List(Box::new(Type::Var(list_elem_ty)));

        let (mut idxee_out, indexee_ty) =
            self.infer_expr(env.clone(), idx.lhs);

        let (index_out, index_ty) = self.infer_expr(env, idx.rhs);

        idxee_out.cons.extend(index_out.cons);
        idxee_out.cons.push(Constraint::TypeEqual(index_ty, Type::Int));
        idxee_out.cons.push(Constraint::TypeEqual(indexee_ty, list_ty));

        (
            CheckRes::new(
                idxee_out.cons,
                Expr::Index(Box::new(Index {
                    lhs: idxee_out.ast,
                    rhs: index_out.ast,
                    span: idx.span,
                })),
            ),
            Type::Var(list_elem_ty),
        )
    }

    fn infer_group(
        &mut self,
        env: HashMap<Var, Type>,
        group: Group<Var>,
    ) -> (CheckRes<Expr<TypedVar>>, Type) {
        let (res, ty) = self.infer_expr(env, group.value);
        (
            CheckRes::new(
                res.cons,
                Expr::Group(Box::new(Group {
                    value: res.ast,
                    span: group.span,
                })),
            ),
            ty,
        )
    }

    fn infer_list(
        &mut self,
        env: HashMap<Var, Type>,
        list: List<Var>,
    ) -> (CheckRes<Expr<TypedVar>>, Type) {
        let mut values = list.values.into_iter();
        let (mut cons, mut asts, first_ty) = match values.next() {
            Some(e) => {
                let (out, res) = self.infer_expr(env.clone(), e);
                (out.cons, vec![out.ast], res)
            }
            None => (vec![], vec![], Type::Var(self.fresh_ty_var())),
        };

        for val in values {
            let out = self.check_expr(env.clone(), val, first_ty.clone());
            cons.extend(out.cons);
            asts.push(out.ast);
        }

        (
            CheckRes::new(
                cons,
                Expr::List(List { values: asts, span: list.span }),
            ),
            Type::List(Box::new(first_ty)),
        )
    }

    fn infer_unary(
        &mut self,
        env: HashMap<Var, Type>,
        unary: Unary<Var>,
    ) -> (CheckRes<Expr<TypedVar>>, Type) {
        let Unary { rhs, op, span, op_span } = unary;
        let (out, ty) = self.infer_expr(env, rhs);

        let mut cons = out.cons;
        cons.push(match op {
            UnaryOp::Nneg => Constraint::ImplNegate(ty.clone()),
            UnaryOp::Lnot => Constraint::TypeEqual(ty.clone(), Type::Bool),
            UnaryOp::Bneg => Constraint::TypeEqual(ty.clone(), Type::Int),
        });

        (
            CheckRes::new(
                cons,
                Expr::Unary(Box::new(Unary {
                    rhs: out.ast,
                    op,
                    span,
                    op_span,
                })),
            ),
            // Each UnaryOp returns the same type as the original type. Each
            // one is roughly `a -> a`
            ty,
        )
    }

    fn infer_binary(
        &mut self,
        env: HashMap<Var, Type>,
        binary: Binary<Var>,
    ) -> (CheckRes<Expr<TypedVar>>, Type) {
        let (lhs, rhs, cons, ty) = match binary.op {
            // These operators require that both the `[l|r]hs` is a function type
            // with a singular argument, and `[r|l]hs` is the type of the parameter
            BinaryOp::Appl => {
                let arg = Type::Var(self.fresh_ty_var());
                let ret = Type::Var(self.fresh_ty_var());
                let fun = Type::Fun(FnType {
                    args: vec![arg.clone()],
                    ret_type: Box::new(ret.clone()),
                });

                let lhs = self.check_expr(env.clone(), binary.lhs, fun);
                let rhs = self.check_expr(env, binary.rhs, arg);
                let mut cons = lhs.cons;
                cons.extend(rhs.cons);

                (lhs.ast, rhs.ast, cons, ret)
            }
            BinaryOp::Appr => {
                let arg = Type::Var(self.fresh_ty_var());
                let ret = Type::Var(self.fresh_ty_var());
                let fun = Type::Fun(FnType {
                    args: vec![arg.clone()],
                    ret_type: Box::new(ret.clone()),
                });

                let lhs = self.check_expr(env.clone(), binary.lhs, arg);
                let rhs = self.check_expr(env, binary.rhs, fun);
                let mut cons = lhs.cons;
                cons.extend(rhs.cons);

                (lhs.ast, rhs.ast, cons, ret)
            }
            // These operators require that both the `lhs` and `rhs` are both
            // unary functions where the output of the `[l|r]hs` is the only
            // parameter of the `[r|l]hs`.
            BinaryOp::Cpsl => {
                let rhs_arg = Type::Var(self.fresh_ty_var());
                let rhs_ret = Type::Var(self.fresh_ty_var());
                let lhs_ret = Type::Var(self.fresh_ty_var());

                let rhs_fn = Type::Fun(FnType {
                    args: vec![rhs_arg],
                    ret_type: Box::new(rhs_ret.clone()),
                });

                let lhs_fn = Type::Fun(FnType {
                    args: vec![rhs_ret],
                    ret_type: Box::new(lhs_ret.clone()),
                });

                let lhs = self.check_expr(env.clone(), binary.lhs, lhs_fn);
                let rhs = self.check_expr(env, binary.rhs, rhs_fn);
                let mut cons = lhs.cons;
                cons.extend(rhs.cons);

                (lhs.ast, rhs.ast, cons, lhs_ret)
            }
            BinaryOp::Cpsr => {
                let lhs_arg = Type::Var(self.fresh_ty_var());
                let lhs_ret = Type::Var(self.fresh_ty_var());
                let rhs_ret = Type::Var(self.fresh_ty_var());

                let lhs_fn = Type::Fun(FnType {
                    args: vec![lhs_arg],
                    ret_type: Box::new(lhs_ret.clone()),
                });

                let rhs_fn = Type::Fun(FnType {
                    args: vec![lhs_ret],
                    ret_type: Box::new(rhs_ret.clone()),
                });

                let lhs = self.check_expr(env.clone(), binary.lhs, lhs_fn);
                let rhs = self.check_expr(env, binary.rhs, rhs_fn);
                let mut cons = lhs.cons;
                cons.extend(rhs.cons);

                (lhs.ast, rhs.ast, cons, rhs_ret)
            }
            // These operators always return boolean values, regardless of the
            // type of the `lhs` and `rhs` value
            BinaryOp::Land
            | BinaryOp::Lor
            | BinaryOp::Eq
            | BinaryOp::Ne
            | BinaryOp::Ge
            | BinaryOp::Gt
            | BinaryOp::Le
            | BinaryOp::Lt => {
                let (lhs, lhs_ty) = self.infer_expr(env.clone(), binary.lhs);
                let (rhs, rhs_ty) = self.infer_expr(env, binary.rhs);
                let mut cons = lhs.cons;
                cons.extend(rhs.cons);
                cons.push(Constraint::TypeEqual(lhs_ty, rhs_ty));

                (lhs.ast, rhs.ast, cons, Type::Bool)
            }
            // These operators require that their values are of the `int` type
            // and they output a value of type `int`.
            BinaryOp::Bor
            | BinaryOp::Bxor
            | BinaryOp::Band
            | BinaryOp::Shr
            | BinaryOp::Shl
            | BinaryOp::Mod => {
                let lhs = self.check_expr(env.clone(), binary.lhs, Type::Int);
                let rhs = self.check_expr(env, binary.rhs, Type::Int);
                let mut cons = lhs.cons;
                cons.extend(rhs.cons);

                (lhs.ast, rhs.ast, cons, Type::Int)
            }
            // These operators require that their values are of numerical persuasion
            //
            // For type inference of the following binary operators, we assume
            // that the result of the operation is the same type as the expression
            // on the left hand side of the operator.
            //
            // THOUGHT: Perhaps add a `Type::Error` and use a new var with the
            //          constraint output of `Constriant::OutputOfAdd(Type, Type)`
            BinaryOp::Add => {
                let (lhs_out, lhs_ty) =
                    self.infer_expr(env.clone(), binary.lhs);

                let (rhs_out, rhs_ty) = self.infer_expr(env, binary.rhs);

                let mut cons = lhs_out.cons;
                cons.extend(rhs_out.cons);
                cons.push(Constraint::TypeEqual(lhs_ty.clone(), rhs_ty));
                cons.push(Constraint::ImplAdd(lhs_ty.clone()));

                (lhs_out.ast, rhs_out.ast, cons, lhs_ty)
            }
            BinaryOp::Sub => {
                let (lhs_out, lhs_ty) =
                    self.infer_expr(env.clone(), binary.lhs);

                let (rhs_out, rhs_ty) = self.infer_expr(env, binary.rhs);

                let mut cons = lhs_out.cons;
                cons.extend(rhs_out.cons);
                cons.push(Constraint::TypeEqual(lhs_ty.clone(), rhs_ty));
                cons.push(Constraint::ImplSub(lhs_ty.clone()));
                (lhs_out.ast, rhs_out.ast, cons, lhs_ty)
            }
            BinaryOp::Div => {
                let (lhs_out, lhs_ty) =
                    self.infer_expr(env.clone(), binary.lhs);

                let (rhs_out, rhs_ty) = self.infer_expr(env, binary.rhs);

                let mut cons = lhs_out.cons;
                cons.extend(rhs_out.cons);
                cons.push(Constraint::TypeEqual(lhs_ty.clone(), rhs_ty));
                cons.push(Constraint::ImplDiv(lhs_ty.clone()));
                (lhs_out.ast, rhs_out.ast, cons, lhs_ty)
            }
            BinaryOp::Mul => {
                let (lhs_out, lhs_ty) =
                    self.infer_expr(env.clone(), binary.lhs);

                let (rhs_out, rhs_ty) = self.infer_expr(env, binary.rhs);

                let mut cons = lhs_out.cons;
                cons.extend(rhs_out.cons);
                cons.push(Constraint::TypeEqual(lhs_ty.clone(), rhs_ty));
                cons.push(Constraint::ImplMul(lhs_ty.clone()));
                (lhs_out.ast, rhs_out.ast, cons, lhs_ty)
            }
        };

        let bin = Binary {
            lhs,
            rhs,
            op: binary.op,
            span: binary.span,
            op_span: binary.op_span,
        };

        (CheckRes::new(cons, Expr::Binary(Box::new(bin))), ty)
    }

    fn infer_block(
        &mut self,
        mut env: HashMap<Var, Type>,
        block: Block<Var>,
    ) -> (CheckRes<Expr<TypedVar>>, Type) {
        let (res, ty) = self.infer_block_raw(env, block);
        (
            CheckRes::new(
                res.cons,
                Expr::Block(Box::new(Block {
                    statements: res.ast.statements,
                    value: res.ast.value,
                    span: res.ast.span,
                })),
            ),
            ty,
        )
    }

    fn infer_block_raw(
        &mut self,
        mut env: HashMap<Var, Type>,
        block: Block<Var>,
    ) -> (CheckRes<Block<TypedVar>>, Type) {
        let Block { statements, value, span } = block;

        let mut stmts = Vec::with_capacity(statements.len());
        let mut cons = vec![];
        for stmt in statements {
            let out = self.process_stmt(&mut env, stmt);
            stmts.push(out.ast);
            cons.extend(out.cons);
        }

        let (val, ty) =
            if let Some((out, ty)) = value.map(|v| self.infer_expr(env, v)) {
                cons.extend(out.cons);
                (Some(out.ast), ty)
            } else {
                (None, Type::Nil)
            };

        (
            CheckRes::new(cons, Block { statements: stmts, value: val, span }),
            ty,
        )
    }

    fn process_stmt(
        &mut self,
        env: &mut HashMap<Var, Type>,
        stmt: Statement<Var>,
    ) -> CheckRes<Statement<TypedVar>> {
        match stmt {
            Statement::Define(def) => self.process_def_stmt(env, def),
            Statement::Expr(expr) => self.process_expr_stmt(env.clone(), expr),
        }
    }

    fn process_def_stmt(
        &mut self,
        env: &mut HashMap<Var, Type>,
        def: Define<Var>,
    ) -> CheckRes<Statement<TypedVar>> {
        let Define { ident, typ, value, span } = def;
        let (out, inferred_ty) = self.infer_expr(env.clone(), value);
        let mut cons = out.cons;

        let (ident, typ) = if let Some(typ) = typ {
            let ty = self.parse_ty(typ);
            cons.push(Constraint::TypeEqual(inferred_ty, ty.clone()));
            // TODO: Figure out what to do with `typ` after type inference
            (TypedVar(ident, ty.clone()), None)
        } else {
            (TypedVar(ident, inferred_ty.clone()), None)
        };

        CheckRes::new(
            cons,
            Statement::Define(Define { ident, typ, value: out.ast, span }),
        )
    }

    fn process_expr_stmt(
        &mut self,
        env: HashMap<Var, Type>,
        expr: ExprStatement<Var>,
    ) -> CheckRes<Statement<TypedVar>> {
        let ExprStatement { expr, span } = expr;
        let (out, _ty) = self.infer_expr(env, expr);
        CheckRes::new(
            out.cons,
            Statement::Expr(ExprStatement { expr: out.ast, span }),
        )
    }

    fn fresh_ty_var(&mut self) -> TypeVar {
        self.types += 1;
        TypeVar(self.types - 1)
    }

    fn parse_ty(&mut self, ty: lambc_parse::Type<Var>) -> Type {
        todo!()
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
    TypeEqual(Type, Type),
}

#[cfg(test)]
mod tests {
    use im::HashMap;
    use pretty_assertions::assert_eq;

    use lambc_parse::{
        BoolLit, Call, CharLit, CharText, Expr, F64Lit, FnDef, Group, I64Base,
        I64Lit, Ident, Index, List, NilLit, Span, StrLit, StrText, Unary,
        UnaryOp,
    };

    use super::{Type, TypeChecker};
    use crate::{
        name_res::Var,
        type_check::{
            CheckRes as GenWith, Constraint, FnType, TypeVar, TypedVar,
        },
        State,
    };

    const SPAN: Span = Span::new(0, 0);

    fn i64_lit() -> I64Lit {
        I64Lit { base: I64Base::Dec, value: "1".into(), span: SPAN }
    }

    fn f64_lit() -> F64Lit {
        F64Lit { value: "2.".into(), span: SPAN }
    }

    fn str_lit() -> StrLit {
        StrLit {
            text: Some(StrText { inner: "a".into(), span: SPAN }),
            span: SPAN,
        }
    }

    fn nil_lit() -> NilLit {
        NilLit { span: SPAN }
    }

    fn bool_lit() -> BoolLit {
        BoolLit { value: false, span: SPAN }
    }

    fn char_lit() -> CharLit {
        CharLit {
            text: Some(CharText { inner: "f".into(), span: SPAN }),
            span: SPAN,
        }
    }

    #[test]
    fn infers_int() {
        let mut state = State::default();
        let mut checker = TypeChecker::new(&mut state);

        let lit = i64_lit();

        let out = checker.infer_expr(HashMap::new(), Expr::I64(lit.clone()));
        assert_eq!(out, (GenWith::empty(Expr::I64(lit)), Type::Int))
    }

    #[test]
    fn checks_int() {
        let mut state = State::default();
        let mut checker = TypeChecker::new(&mut state);

        let lit = i64_lit();

        let out = checker.check_expr(
            HashMap::new(),
            Expr::I64(lit.clone()),
            Type::Int,
        );

        assert_eq!(out, GenWith::empty(Expr::I64(lit)))
    }

    #[test]
    fn infers_double() {
        let mut state = State::default();
        let mut checker = TypeChecker::new(&mut state);

        let lit = f64_lit();
        let out = checker.infer_expr(HashMap::new(), Expr::F64(lit.clone()));
        assert_eq!(out, (GenWith::empty(Expr::F64(lit)), Type::Double));
    }

    #[test]
    fn checks_double() {
        let mut state = State::default();
        let mut checker = TypeChecker::new(&mut state);

        let lit = f64_lit();

        let out = checker.check_expr(
            HashMap::new(),
            Expr::F64(lit.clone()),
            Type::Double,
        );

        assert_eq!(out, GenWith::empty(Expr::F64(lit)))
    }

    #[test]
    fn infers_usv() {
        let mut state = State::default();
        let mut checker = TypeChecker::new(&mut state);

        let lit = char_lit();

        let out = checker.infer_expr(HashMap::new(), Expr::Char(lit.clone()));
        assert_eq!(out, (GenWith::empty(Expr::Char(lit)), Type::Usv));
    }

    #[test]
    fn checks_usv() {
        let mut state = State::default();
        let mut checker = TypeChecker::new(&mut state);

        let lit = char_lit();

        let out = checker.check_expr(
            HashMap::new(),
            Expr::Char(lit.clone()),
            Type::Usv,
        );

        assert_eq!(out, GenWith::empty(Expr::Char(lit)))
    }

    #[test]
    fn infers_string() {
        let mut state = State::default();
        let mut checker = TypeChecker::new(&mut state);

        let lit = str_lit();

        let out =
            checker.infer_expr(HashMap::new(), Expr::String(lit.clone()));

        assert_eq!(
            out,
            (
                GenWith::empty(Expr::String(lit)),
                Type::List(Box::new(Type::Usv))
            )
        );
    }

    #[test]
    fn checks_string() {
        let mut state = State::default();
        let mut checker = TypeChecker::new(&mut state);

        let lit = str_lit();

        let out = checker.check_expr(
            HashMap::new(),
            Expr::String(lit.clone()),
            Type::List(Box::new(Type::Usv)),
        );

        assert_eq!(out, GenWith::empty(Expr::String(lit)),);
    }

    #[test]
    fn infers_var() {
        let mut state = State::default();
        let mut checker = TypeChecker::new(&mut state);

        let typ = Type::Var(TypeVar(0));
        let var = Var(0);
        let env = HashMap::from([(var, typ.clone())].as_slice());
        let out = checker.infer_expr(env, Expr::Ident(var));

        assert_eq!(
            out,
            (GenWith::empty(Expr::Ident(TypedVar(var, typ.clone()))), typ)
        );
    }

    #[test]
    fn infers_fndef() {
        let mut state = State::default();
        let mut checker = TypeChecker::new(&mut state);

        let def = FnDef {
            args: vec![Var(0), Var(1)],
            body: Expr::Ident(Var(0)),
            recursive: false,
            span: SPAN,
        };

        let typ = Type::Fun(FnType {
            args: vec![Type::Var(TypeVar(0)), Type::Var(TypeVar(1))],
            ret_type: Box::new(Type::Var(TypeVar(0))),
        });

        let out =
            checker.infer_expr(HashMap::new(), Expr::FnDef(Box::new(def)));

        assert_eq!(
            out,
            (
                GenWith::empty(Expr::FnDef(Box::new(FnDef {
                    args: vec![
                        TypedVar(Var(0), Type::Var(TypeVar(0))),
                        TypedVar(Var(1), Type::Var(TypeVar(1)))
                    ],
                    body: Expr::Ident(TypedVar(Var(0), Type::Var(TypeVar(0)))),
                    recursive: false,
                    span: SPAN
                }))),
                typ
            )
        );
    }

    #[test]
    fn checks_fndef() {
        let mut state = State::default();
        let mut checker = TypeChecker::new(&mut state);

        let def = FnDef {
            args: vec![Var(0), Var(1)],
            body: Expr::Ident(Var(0)),
            recursive: false,
            span: SPAN,
        };

        let typ = Type::Fun(FnType {
            args: vec![Type::Var(TypeVar(0)), Type::Var(TypeVar(1))],
            ret_type: Box::new(Type::Var(TypeVar(0))),
        });

        let out = checker.check_expr(
            HashMap::new(),
            Expr::FnDef(Box::new(def)),
            typ,
        );

        assert_eq!(
            out,
            GenWith::new(
                vec![Constraint::TypeEqual(
                    Type::Var(TypeVar(0)),
                    Type::Var(TypeVar(0))
                )],
                Expr::FnDef(Box::new(FnDef {
                    args: vec![
                        TypedVar(Var(0), Type::Var(TypeVar(0))),
                        TypedVar(Var(1), Type::Var(TypeVar(1)))
                    ],
                    body: Expr::Ident(TypedVar(Var(0), Type::Var(TypeVar(0)))),
                    recursive: false,
                    span: SPAN
                })),
            ),
        );
    }

    #[test]
    fn checks_fndef_refined() {
        let mut state = State::default();
        let mut checker = TypeChecker::new(&mut state);

        let def = FnDef {
            args: vec![Var(0), Var(1)],
            body: Expr::Ident(Var(0)),
            recursive: false,
            span: SPAN,
        };

        let typ = Type::Fun(FnType {
            args: vec![Type::Int, Type::Double],
            ret_type: Box::new(Type::Int),
        });

        let out = checker.check_expr(
            HashMap::new(),
            Expr::FnDef(Box::new(def)),
            typ,
        );

        assert_eq!(
            out,
            GenWith::new(
                vec![Constraint::TypeEqual(Type::Int, Type::Int)],
                Expr::FnDef(Box::new(FnDef {
                    args: vec![
                        TypedVar(Var(0), Type::Int),
                        TypedVar(Var(1), Type::Double)
                    ],
                    body: Expr::Ident(TypedVar(Var(0), Type::Int)),
                    recursive: false,
                    span: SPAN
                })),
            ),
        );
    }

    #[test]
    fn infers_idx() {
        let mut state = State::default();
        let mut checker = TypeChecker::new(&mut state);

        let idx = Index {
            lhs: Expr::Nil(nil_lit()),
            rhs: Expr::Bool(bool_lit()),
            span: SPAN,
        };

        let typ = Type::Var(TypeVar(0));

        let out = checker.check_expr(
            HashMap::new(),
            Expr::Index(Box::new(idx)),
            typ,
        );

        assert_eq!(
            out,
            GenWith::new(
                vec![
                    Constraint::TypeEqual(Type::Bool, Type::Int),
                    Constraint::TypeEqual(
                        Type::Nil,
                        Type::List(Box::new(Type::Var(TypeVar(0))))
                    ),
                    Constraint::TypeEqual(
                        Type::Var(TypeVar(0)),
                        Type::Var(TypeVar(0))
                    )
                ],
                Expr::Index(Box::new(Index {
                    lhs: Expr::Nil(NilLit { span: SPAN }),
                    rhs: Expr::Bool(BoolLit { value: false, span: SPAN }),
                    span: SPAN,
                })),
            ),
        );
    }

    #[test]
    fn infers_call() {
        let mut state = State::default();
        let mut checker = TypeChecker::new(&mut state);

        let callee_ident = Var(0);
        let callee_typ = Type::Var(TypeVar(1000));
        let ret_typ = Type::Var(TypeVar(0));

        let env =
            HashMap::from([(callee_ident, callee_typ.clone())].as_slice());

        let call = Call {
            callee: Expr::Ident(callee_ident),
            args: vec![Expr::I64(i64_lit()), Expr::I64(i64_lit())],
            span: SPAN,
        };

        let out = checker.infer_expr(env, Expr::Call(Box::new(call)));

        assert_eq!(
            out,
            (
                GenWith::new(
                    vec![Constraint::TypeEqual(
                        Type::Fun(FnType {
                            args: vec![Type::Int, Type::Int],
                            ret_type: Box::new(ret_typ.clone())
                        }),
                        callee_typ.clone()
                    )],
                    Expr::Call(Box::new(Call {
                        callee: Expr::Ident(TypedVar(
                            callee_ident,
                            callee_typ.clone()
                        )),
                        args: vec![Expr::I64(i64_lit()), Expr::I64(i64_lit())],
                        span: SPAN,
                    })),
                ),
                ret_typ
            ),
        );
    }

    #[test]
    fn infers_group() {
        let mut state = State::default();
        let mut checker = TypeChecker::new(&mut state);

        let expr = str_lit();
        let group = Group { value: Expr::String(expr.clone()), span: SPAN };
        let out =
            checker.infer_expr(HashMap::new(), Expr::Group(Box::new(group)));

        assert_eq!(
            out,
            (
                GenWith::new(
                    vec![],
                    Expr::Group(Box::new(Group {
                        value: Expr::String(expr),
                        span: SPAN
                    })),
                ),
                Type::List(Box::new(Type::Usv)),
            ),
        );
    }

    #[test]
    fn infers_list() {
        let mut state = State::default();
        let mut checker = TypeChecker::new(&mut state);

        let list = List {
            values: vec![Expr::String(str_lit()), Expr::Char(char_lit())],
            span: SPAN,
        };

        let out = checker.infer_expr(HashMap::new(), Expr::List(list));

        assert_eq!(
            out,
            (
                GenWith::new(
                    vec![Constraint::TypeEqual(
                        Type::List(Box::new(Type::Usv)),
                        Type::Usv,
                    )],
                    Expr::List(List {
                        values: vec![
                            Expr::String(str_lit()),
                            Expr::Char(char_lit())
                        ],
                        span: SPAN
                    }),
                ),
                Type::List(Box::new(Type::List(Box::new(Type::Usv)))),
            ),
        );
    }

    #[test]
    fn infers_unary() {
        fn unary<T>(lit: I64Lit, op: UnaryOp) -> Expr<T> {
            Expr::Unary(Box::new(Unary {
                rhs: Expr::I64::<T>(lit),
                op,
                span: SPAN,
                op_span: SPAN,
            }))
        }

        let mut state = State::default();
        let mut checker = TypeChecker::new(&mut state);

        let lit = i64_lit();
        let una = unary(lit.clone(), UnaryOp::Nneg);
        let out = checker.infer_expr(HashMap::new(), una);
        assert_eq!(
            out,
            (
                GenWith::new(
                    vec![Constraint::ImplNegate(Type::Int)],
                    unary(lit, UnaryOp::Nneg)
                ),
                Type::Int
            )
        );

        let lit = i64_lit();
        let una = unary(lit.clone(), UnaryOp::Bneg);
        let out = checker.infer_expr(HashMap::new(), una);
        assert_eq!(
            out,
            (
                GenWith::new(
                    vec![Constraint::TypeEqual(Type::Int, Type::Int)],
                    unary(lit, UnaryOp::Bneg)
                ),
                Type::Int
            )
        );

        let lit = i64_lit();
        let una = unary(lit.clone(), UnaryOp::Lnot);
        let out = checker.infer_expr(HashMap::new(), una);
        assert_eq!(
            out,
            (
                GenWith::new(
                    vec![Constraint::TypeEqual(Type::Int, Type::Bool)],
                    unary(lit, UnaryOp::Lnot)
                ),
                Type::Int
            )
        );
    }
}
