use crate::ffi::{
    Assign, Ast, Atom, Binary, Block, Case, CaseArm, Either, Elif, Else, Expr, FuncCall, FuncDef,
    Ident, If, Index, Literal, Statement, Unary,
};

pub trait Optimize {
    fn optimize(&mut self) -> bool;
}

impl<T: Optimize> Optimize for Box<T> {
    fn optimize(&mut self) -> bool {
        T::optimize(self)
    }
}

impl<T: Optimize> Optimize for Vec<T> {
    fn optimize(&mut self) -> bool {
        self.iter_mut().fold(false, |acc, e| acc | e.optimize())
    }
}

impl<L: Optimize, R: Optimize> Optimize for Either<L, R> {
    fn optimize(&mut self) -> bool {
        match self {
            Either::Left(l) => l.optimize(),
            Either::Right(r) => r.optimize(),
        }
    }
}

impl Optimize for Ast {
    fn optimize(&mut self) -> bool {
        match self {
            Ast::Expr(e) => e.optimize(),
            Ast::Statement(s) => s.optimize(),
        }
    }
}

impl Optimize for Statement {
    fn optimize(&mut self) -> bool {
        match self {
            Statement::Assign(Assign { value, .. }) => value.optimize(),
            Statement::Expr(e) | Statement::Return(Some(e)) => e.optimize(),
            Statement::Return(None) => false,
        }
    }
}

impl Optimize for Expr {
    fn optimize(&mut self) -> bool {
        match self {
            Expr::Binary(b) => {
                let change = b.optimize();
                if let Some(e) = b.constant_fold() {
                    *self = e;
                    true
                } else {
                    change
                }
            }
            Expr::Unary(u) => {
                let change = u.optimize();
                if let Some(e) = u.constant_fold() {
                    *self = e;
                    true
                } else {
                    change
                }
            }
            Expr::FuncCall(f) => f.optimize(),
            Expr::Index(i) => {
                let change = i.optimize();
                match (&mut *i.indexee, &mut *i.index) {
                    (Expr::Atom(Atom::Array(arr)), Expr::Atom(Atom::Literal(Literal::Num(n)))) => {
                        if *n >= 0 && (*n as usize) < arr.len() {
                            *self = arr.swap_remove(*n as usize);
                            true
                        } else {
                            change
                        }
                    }
                    _ => change,
                }
            }
            Expr::If(i) => i.optimize(),
            Expr::Case(c) => c.optimize(),
            Expr::FuncDef(f) => f.optimize(),
            Expr::Block(b) => b.optimize(),
            Expr::Atom(a) => a.optimize(),
        }
    }
}

impl Optimize for Binary {
    fn optimize(&mut self) -> bool {
        self.lhs.optimize() | self.rhs.optimize()
    }
}

impl Optimize for Unary {
    fn optimize(&mut self) -> bool {
        self.rhs.optimize()
    }
}

impl Optimize for FuncCall {
    fn optimize(&mut self) -> bool {
        self.callee.optimize() | self.args.optimize()
    }
}

impl Optimize for Index {
    fn optimize(&mut self) -> bool {
        self.indexee.optimize() | self.index.optimize()
    }
}

impl Optimize for If {
    fn optimize(&mut self) -> bool {
        self.cond.optimize()
            | self.block.optimize()
            | self.elifs.optimize()
            | match self.els {
                Some(ref mut e) => e.optimize(),
                None => false,
            }
    }
}

impl Optimize for Elif {
    fn optimize(&mut self) -> bool {
        self.cond.optimize() | self.block.optimize()
    }
}

impl Optimize for Else {
    fn optimize(&mut self) -> bool {
        self.block.optimize()
    }
}

impl Optimize for Block {
    fn optimize(&mut self) -> bool {
        self.stats.optimize()
            | match self.value {
                Some(ref mut e) => e.optimize(),
                None => false,
            }
    }
}

impl Optimize for FuncDef {
    fn optimize(&mut self) -> bool {
        self.body.optimize() | self.args.optimize()
    }
}

impl Optimize for Case {
    fn optimize(&mut self) -> bool {
        self.arms.optimize() | self.value.optimize()
    }
}

impl Optimize for CaseArm {
    fn optimize(&mut self) -> bool {
        self.pattern.optimize() | self.on_match.optimize()
    }
}

impl Optimize for Literal {
    fn optimize(&mut self) -> bool {
        false
    }
}

impl Optimize for Ident {
    fn optimize(&mut self) -> bool {
        false
    }
}

impl Optimize for Atom {
    fn optimize(&mut self) -> bool {
        match self {
            Atom::Array(a) => a.optimize(),
            Atom::Literal(_) | Atom::Ident(_) => false,
        }
    }
}

mod extra_impls {
    use crate::ffi::{Atom, Binary, BinaryOp, Expr, Literal, Unary, UnaryOp};

    impl Expr {
        pub fn as_literal_mut(&mut self) -> Option<&mut Literal> {
            match self {
                Expr::Atom(Atom::Literal(l)) => Some(l),
                _ => None,
            }
        }
    }

    impl Binary {
        pub fn constant_fold(&mut self) -> Option<Expr> {
            let (l, op, r) = self.as_literal_tuple_mut()?;
            match op {
                BinaryOp::LApply | BinaryOp::RApply | BinaryOp::LCompose | BinaryOp::RCompose => {
                    None
                }
                BinaryOp::Add => match (l, r) {
                    (Literal::Num(l), Literal::Num(r)) => Some(Expr::from(Literal::Num(*l + *r))),
                    (Literal::Str(l), Literal::Str(r)) => {
                        Some(Expr::from(Literal::Str(format!("{l}{r}"))))
                    }
                    _ => None,
                },
                BinaryOp::Sub => match (l, r) {
                    (Literal::Num(l), Literal::Num(r)) => Some(Expr::from(Literal::Num(*l - *r))),
                    _ => None,
                },
                BinaryOp::Div => match (l, r) {
                    (Literal::Num(l), Literal::Num(r)) => Some(Expr::from(Literal::Num(*l / *r))),
                    _ => None,
                },
                BinaryOp::Mul => match (l, r) {
                    (Literal::Num(l), Literal::Num(r)) => Some(Expr::from(Literal::Num(*l * *r))),
                    _ => None,
                },
                BinaryOp::Mod => match (l, r) {
                    (Literal::Num(l), Literal::Num(r)) => Some(Expr::from(Literal::Num(*l % *r))),
                    _ => None,
                },
                BinaryOp::Gt => match (l, r) {
                    (Literal::Num(l), Literal::Num(r)) => Some(Expr::from(Literal::Bool(l > r))),
                    (Literal::Str(l), Literal::Str(r)) => Some(Expr::from(Literal::Bool(l > r))),
                    (Literal::Char(l), Literal::Char(r)) => Some(Expr::from(Literal::Bool(l > r))),
                    (Literal::Bool(l), Literal::Bool(r)) => Some(Expr::from(Literal::Bool(l > r))),
                    (Literal::Nil, Literal::Nil) => Some(Expr::from(Literal::Bool(false))),
                    _ => None,
                },
                BinaryOp::Ge => match (l, r) {
                    (Literal::Num(l), Literal::Num(r)) => Some(Expr::from(Literal::Bool(l >= r))),
                    (Literal::Str(l), Literal::Str(r)) => Some(Expr::from(Literal::Bool(l >= r))),
                    (Literal::Char(l), Literal::Char(r)) => Some(Expr::from(Literal::Bool(l >= r))),
                    (Literal::Bool(l), Literal::Bool(r)) => Some(Expr::from(Literal::Bool(l > r))),
                    (Literal::Nil, Literal::Nil) => Some(Expr::from(Literal::Bool(false))),
                    _ => None,
                },
                BinaryOp::Lt => match (l, r) {
                    (Literal::Num(l), Literal::Num(r)) => Some(Expr::from(Literal::Bool(l < r))),
                    (Literal::Str(l), Literal::Str(r)) => Some(Expr::from(Literal::Bool(l < r))),
                    (Literal::Char(l), Literal::Char(r)) => Some(Expr::from(Literal::Bool(l < r))),
                    (Literal::Bool(l), Literal::Bool(r)) => Some(Expr::from(Literal::Bool(l < r))),
                    (Literal::Nil, Literal::Nil) => Some(Expr::from(Literal::Bool(false))),
                    _ => None,
                },
                BinaryOp::Le => match (l, r) {
                    (Literal::Num(l), Literal::Num(r)) => Some(Expr::from(Literal::Bool(l <= r))),
                    (Literal::Str(l), Literal::Str(r)) => Some(Expr::from(Literal::Bool(l <= r))),
                    (Literal::Char(l), Literal::Char(r)) => Some(Expr::from(Literal::Bool(l <= r))),
                    (Literal::Bool(l), Literal::Bool(r)) => Some(Expr::from(Literal::Bool(l <= r))),
                    (Literal::Nil, Literal::Nil) => Some(Expr::from(Literal::Bool(false))),
                    _ => None,
                },
                BinaryOp::Eq => match (l, r) {
                    (Literal::Num(l), Literal::Num(r)) => Some(Expr::from(Literal::Bool(l == r))),
                    (Literal::Str(l), Literal::Str(r)) => Some(Expr::from(Literal::Bool(l == r))),
                    (Literal::Char(l), Literal::Char(r)) => Some(Expr::from(Literal::Bool(l == r))),
                    (Literal::Bool(l), Literal::Bool(r)) => Some(Expr::from(Literal::Bool(l == r))),
                    (Literal::Nil, Literal::Nil) => Some(Expr::from(Literal::Bool(true))),
                    _ => None,
                },
                BinaryOp::Ne => match (l, r) {
                    (Literal::Num(l), Literal::Num(r)) => Some(Expr::from(Literal::Bool(l != r))),
                    (Literal::Str(l), Literal::Str(r)) => Some(Expr::from(Literal::Bool(l != r))),
                    (Literal::Char(l), Literal::Char(r)) => Some(Expr::from(Literal::Bool(l != r))),
                    (Literal::Bool(l), Literal::Bool(r)) => Some(Expr::from(Literal::Bool(l != r))),
                    (Literal::Nil, Literal::Nil) => Some(Expr::from(Literal::Bool(false))),
                    _ => None,
                },
                BinaryOp::LogOr => match (l, r) {
                    (Literal::Bool(l), Literal::Bool(r)) => {
                        Some(Expr::from(Literal::Bool(*l || *r)))
                    }
                    _ => None,
                },
                BinaryOp::LogAnd => match (l, r) {
                    (Literal::Bool(l), Literal::Bool(r)) => {
                        Some(Expr::from(Literal::Bool(*l && *r)))
                    }
                    _ => None,
                },
                BinaryOp::BinOr => match (l, r) {
                    (Literal::Num(l), Literal::Num(r)) => Some(Expr::from(Literal::Num(*l | *r))),
                    _ => None,
                },
                BinaryOp::BinAnd => match (l, r) {
                    (Literal::Num(l), Literal::Num(r)) => Some(Expr::from(Literal::Num(*l & *r))),
                    _ => None,
                },
                BinaryOp::BinXor => match (l, r) {
                    (Literal::Num(l), Literal::Num(r)) => Some(Expr::from(Literal::Num(*l ^ *r))),
                    _ => None,
                },
                BinaryOp::RShift => match (l, r) {
                    (Literal::Num(l), Literal::Num(r)) => Some(Expr::from(Literal::Num(*l >> *r))),
                    _ => None,
                },
                BinaryOp::LShift => match (l, r) {
                    (Literal::Num(l), Literal::Num(r)) => Some(Expr::from(Literal::Num(*l << *r))),
                    _ => None,
                },
            }
        }

        pub fn as_literal_tuple_mut(
            &mut self,
        ) -> Option<(&mut Literal, &mut BinaryOp, &mut Literal)> {
            let lhs = self.lhs.as_literal_mut()?;
            let rhs = self.rhs.as_literal_mut()?;

            Some((lhs, &mut self.op, rhs))
        }
    }

    impl Unary {
        pub fn constant_fold(&mut self) -> Option<Expr> {
            let Unary { rhs, op } = self;
            match (rhs.as_literal_mut()?, op) {
                (Literal::Num(n), UnaryOp::NumNeg) => Some(Expr::from(Literal::Num(-*n))),
                (Literal::Num(n), UnaryOp::BinNot) => Some(Expr::from(Literal::Num(!*n))),
                (Literal::Bool(b), UnaryOp::LogNot) => Some(Expr::from(Literal::Bool(!*b))),
                _ => None,
            }
        }
    }
}
