use crate::ast::{
    Assign, Atom, Binary, Block, Case, CaseArm, Either, Elif, Else, Expr, FuncCall, FuncDef, Ident,
    If, Index, Literal, Statement, Unary,
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
                        match usize::try_from(*n) {
                            Ok(idx) => {
                                *self = arr.swap_remove(idx);
                                true
                            }
                            Err(_) => change,
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
            | self.els.as_deref_mut().is_some_and(|e| e.optimize())
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
        self.stats.optimize() | self.value.as_deref_mut().is_some_and(|v| v.optimize())
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
    use crate::ast::{Atom, Binary, BinaryOp, Expr, Literal, Unary, UnaryOp};

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
            match self.as_literal_tuple_mut()? {
                (Literal::Nil, op, Literal::Nil) => op.apply_to_nils(),
                (Literal::Str(l), op, Literal::Str(r)) => op.apply_to_strs(l, r),
                (Literal::Num(l), op, Literal::Num(r)) => op.apply_to_nums(*l, *r),
                (Literal::Bool(l), op, Literal::Bool(r)) => op.apply_to_bools(*l, *r),
                (Literal::Char(l), op, Literal::Char(r)) => op.apply_to_chars(*l, *r),
                _ => None,
            }
        }

        fn as_literal_tuple_mut(&mut self) -> Option<(&mut Literal, &mut BinaryOp, &mut Literal)> {
            let lhs = self.lhs.as_literal_mut()?;
            let rhs = self.rhs.as_literal_mut()?;

            Some((lhs, &mut self.op, rhs))
        }
    }

    impl BinaryOp {
        fn apply_to_nums(self, l: i64, r: i64) -> Option<Expr> {
            Some(Expr::Atom(Atom::Literal(match self {
                BinaryOp::Add => Literal::Num(l + r),
                BinaryOp::Sub => Literal::Num(l - r),
                BinaryOp::Div => Literal::Num(l / r),
                BinaryOp::Mul => Literal::Num(l * r),
                BinaryOp::Mod => Literal::Num(l % r),
                BinaryOp::BinOr => Literal::Num(l | r),
                BinaryOp::BinAnd => Literal::Num(l & r),
                BinaryOp::BinXor => Literal::Num(l ^ r),
                BinaryOp::RShift => Literal::Num(l >> r),
                BinaryOp::LShift => Literal::Num(l << r),
                BinaryOp::Gt => Literal::Bool(l > r),
                BinaryOp::Ge => Literal::Bool(l >= r),
                BinaryOp::Lt => Literal::Bool(l < r),
                BinaryOp::Le => Literal::Bool(l <= r),
                BinaryOp::Eq => Literal::Bool(l == r),
                BinaryOp::Ne => Literal::Bool(l != r),
                _ => return None,
            })))
        }

        fn apply_to_strs(self, l: &String, r: &String) -> Option<Expr> {
            Some(Expr::Atom(Atom::Literal(match self {
                BinaryOp::Add => Literal::Str(format!("{l}{r}")),
                BinaryOp::Gt => Literal::Bool(l > r),
                BinaryOp::Ge => Literal::Bool(l >= r),
                BinaryOp::Lt => Literal::Bool(l < r),
                BinaryOp::Le => Literal::Bool(l <= r),
                BinaryOp::Eq => Literal::Bool(l == r),
                BinaryOp::Ne => Literal::Bool(l != r),
                _ => return None,
            })))
        }

        fn apply_to_chars(self, l: char, r: char) -> Option<Expr> {
            Some(Expr::Atom(Atom::Literal(match self {
                BinaryOp::Gt => Literal::Bool(l > r),
                BinaryOp::Ge => Literal::Bool(l >= r),
                BinaryOp::Lt => Literal::Bool(l < r),
                BinaryOp::Le => Literal::Bool(l <= r),
                BinaryOp::Eq => Literal::Bool(l == r),
                BinaryOp::Ne => Literal::Bool(l != r),
                _ => return None,
            })))
        }

        fn apply_to_bools(self, l: bool, r: bool) -> Option<Expr> {
            Some(Expr::Atom(Atom::Literal(match self {
                BinaryOp::LogOr => Literal::Bool(l | r),
                BinaryOp::LogAnd => Literal::Bool(l & r),
                BinaryOp::Gt => Literal::Bool(l & !r),
                BinaryOp::Ge => Literal::Bool(l >= r),
                BinaryOp::Lt => Literal::Bool(!l & r),
                BinaryOp::Le => Literal::Bool(l <= r),
                BinaryOp::Eq => Literal::Bool(l == r),
                BinaryOp::Ne => Literal::Bool(l != r),
                _ => return None,
            })))
        }

        fn apply_to_nils(self) -> Option<Expr> {
            Some(Expr::Atom(Atom::Literal(match self {
                BinaryOp::Ge | BinaryOp::Le | BinaryOp::Eq => Literal::Bool(true),
                BinaryOp::Gt | BinaryOp::Lt | BinaryOp::Ne => Literal::Bool(false),
                _ => return None,
            })))
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
