use std::ffi::{CStr, CString};

use crate::ast::{
    Assign, Atom, Binary, BinaryOp, Block, Case, Either, Else, Expr, FuncCall, FuncDef, Ident, If,
    Index, Literal, Script, Statement, Unary, UnaryOp,
};

use super::{
    bindings::{
        self, AstNodeType_AstntArray, AstNodeType_AstntArrayIndex, AstNodeType_AstntAssignStmt,
        AstNodeType_AstntBinaryAdd, AstNodeType_AstntBinaryAnd, AstNodeType_AstntBinaryDiv,
        AstNodeType_AstntBinaryEq, AstNodeType_AstntBinaryGe, AstNodeType_AstntBinaryGt,
        AstNodeType_AstntBinaryLApply, AstNodeType_AstntBinaryLCompose,
        AstNodeType_AstntBinaryLShift, AstNodeType_AstntBinaryLe, AstNodeType_AstntBinaryLogAnd,
        AstNodeType_AstntBinaryLogOr, AstNodeType_AstntBinaryLt, AstNodeType_AstntBinaryMod,
        AstNodeType_AstntBinaryMul, AstNodeType_AstntBinaryNe, AstNodeType_AstntBinaryOr,
        AstNodeType_AstntBinaryRApply, AstNodeType_AstntBinaryRCompose,
        AstNodeType_AstntBinaryRShift, AstNodeType_AstntBinarySub, AstNodeType_AstntBinaryXor,
        AstNodeType_AstntBlock, AstNodeType_AstntBoolLit, AstNodeType_AstntCase,
        AstNodeType_AstntCaseArm, AstNodeType_AstntCharLit, AstNodeType_AstntExprStmt,
        AstNodeType_AstntFuncCall, AstNodeType_AstntFuncDef, AstNodeType_AstntIdent,
        AstNodeType_AstntIf, AstNodeType_AstntNilLit, AstNodeType_AstntNodeList,
        AstNodeType_AstntNumLit, AstNodeType_AstntReturn, AstNodeType_AstntStrLit,
        AstNodeType_AstntUnaryBitNot, AstNodeType_AstntUnaryLogNot, AstNodeType_AstntUnaryNeg,
    },
    AstNode_T,
};

pub(super) trait Convert {
    fn convert(&self) -> *mut AstNode_T;
}

impl<L, R> Convert for Either<L, R>
where
    L: Convert,
    R: Convert,
{
    fn convert(&self) -> *mut AstNode_T {
        match self {
            Self::Left(l) => l.convert(),
            Self::Right(r) => r.convert(),
        }
    }
}

impl Convert for Script {
    fn convert(&self) -> *mut AstNode_T {
        let block = self.block.convert();
        unsafe {
            let kid = (*block).kids[0];
            (*block).kids[0] = std::ptr::null_mut();
            bindings::free_ast(block);
            kid
        }
    }
}

impl Convert for Statement {
    fn convert(&self) -> *mut AstNode_T {
        match self {
            Statement::Assign(Assign { assignee, value }) => unsafe {
                let this = new_node(NodeKind::AssignStmt);
                (*this).kids[0] = assignee.convert();
                (*this).kids[1] = value.convert();
                this
            },
            Statement::Expr(e) => unsafe {
                let this = new_node(NodeKind::ExprStmt);
                (*this).kids[0] = e.convert();
                this
            },
            Statement::Return(r) => unsafe {
                let this = new_node(NodeKind::Return);
                (*this).kids[0] = r.as_ref().map_or(std::ptr::null_mut(), Expr::convert);
                this
            },
        }
    }
}

impl Convert for Expr {
    fn convert(&self) -> *mut AstNode_T {
        match self {
            Expr::Binary(b) => b.convert(),
            Expr::Unary(u) => u.convert(),
            Expr::FuncCall(f) => f.convert(),
            Expr::Index(i) => i.convert(),
            Expr::If(i) => i.convert(),
            Expr::Case(c) => c.convert(),
            Expr::FuncDef(f) => f.convert(),
            Expr::Block(b) => b.convert(),
            Expr::Atom(a) => a.convert(),
        }
    }
}

impl Convert for Binary {
    fn convert(&self) -> *mut AstNode_T {
        let Binary { lhs, rhs, op } = self;
        let node_type = match op {
            BinaryOp::Add => NodeKind::BinaryAdd,
            BinaryOp::Sub => NodeKind::BinarySub,
            BinaryOp::Div => NodeKind::BinaryDiv,
            BinaryOp::Mul => NodeKind::BinaryMul,
            BinaryOp::Mod => NodeKind::BinaryMod,
            BinaryOp::LApply => NodeKind::BinaryLApply,
            BinaryOp::RApply => NodeKind::BinaryRApply,
            BinaryOp::LCompose => NodeKind::BinaryLCompose,
            BinaryOp::RCompose => NodeKind::BinaryRCompose,
            BinaryOp::Gt => NodeKind::BinaryGt,
            BinaryOp::Ge => NodeKind::BinaryGe,
            BinaryOp::Lt => NodeKind::BinaryLt,
            BinaryOp::Le => NodeKind::BinaryLe,
            BinaryOp::Eq => NodeKind::BinaryEq,
            BinaryOp::Ne => NodeKind::BinaryNe,
            BinaryOp::LogOr => NodeKind::BinaryLogOr,
            BinaryOp::LogAnd => NodeKind::BinaryLogAnd,
            BinaryOp::BinOr => NodeKind::BinaryOr,
            BinaryOp::BinAnd => NodeKind::BinaryAnd,
            BinaryOp::BinXor => NodeKind::BinaryXor,
            BinaryOp::RShift => NodeKind::BinaryRShift,
            BinaryOp::LShift => NodeKind::BinaryLShift,
        };

        unsafe {
            let node = new_node(node_type);
            (*node).kids[0] = lhs.convert();
            (*node).kids[1] = rhs.convert();
            node
        }
    }
}

impl Convert for Unary {
    fn convert(&self) -> *mut AstNode_T {
        let Unary { rhs, op } = self;
        let node_type = match op {
            UnaryOp::NumNeg => NodeKind::UnaryNeg,
            UnaryOp::LogNot => NodeKind::UnaryLogNot,
            UnaryOp::BinNot => NodeKind::UnaryBitNot,
        };

        unsafe {
            let node = new_node(node_type);
            (*node).kids[0] = rhs.convert();
            node
        }
    }
}

impl Convert for FuncCall {
    fn convert(&self) -> *mut AstNode_T {
        let FuncCall { callee, args } = self;

        unsafe {
            let node = new_node(NodeKind::FuncCall);
            (*node).kids[0] = callee.convert();
            (*node).kids[1] = args.iter().map(Convert::convert).convert();
            node
        }
    }
}

impl Convert for Index {
    fn convert(&self) -> *mut AstNode_T {
        let Index { indexee, index } = self;
        unsafe {
            let node = new_node(NodeKind::ArrayIndex);
            (*node).kids[0] = indexee.convert();
            (*node).kids[1] = index.convert();
            node
        }
    }
}

impl Convert for If {
    fn convert(&self) -> *mut AstNode_T {
        let If {
            cond,
            block,
            elifs,
            els,
        } = self;

        unsafe {
            let node = new_node(NodeKind::If);
            (*node).kids[0] = cond.convert();
            (*node).kids[1] = block.convert();
            let els_node = els.as_ref().map_or(std::ptr::null_mut(), |x| x.convert());
            (*node).kids[2] = elifs.iter().rev().fold(els_node, |acc, i| {
                let node = new_node(NodeKind::If);
                (*node).kids[2] = acc;
                (*node).kids[1] = i.block.convert();
                (*node).kids[0] = i.cond.convert();
                node
            });

            node
        }
    }
}

impl Convert for Else {
    fn convert(&self) -> *mut AstNode_T {
        self.block.convert()
    }
}

impl Convert for Case {
    fn convert(&self) -> *mut AstNode_T {
        let arms = self
            .arms
            .iter()
            .rev()
            .fold(std::ptr::null_mut(), |acc, arm| unsafe {
                let node = new_node(NodeKind::CaseArm);
                (*node).kids[2] = acc;
                (*node).kids[1] = arm.on_match.convert();
                (*node).kids[0] = arm.pattern.convert();
                node
            });

        let value = self.value.convert();

        unsafe {
            let node = new_node(NodeKind::Case);
            (*node).kids[0] = value;
            (*node).kids[1] = arms;
            node
        }
    }
}

impl Convert for Literal {
    fn convert(&self) -> *mut AstNode_T {
        match self {
            Self::Num(n) => unsafe {
                let node = new_node(NodeKind::NumLit);
                (*node).val.n = *n;
                node
            },
            Self::Str(s) => unsafe {
                let node = new_node(NodeKind::StrLit);
                let cstr = CString::new(s.clone()).expect("String can not contain internal 0 byte");
                (*node).val.s = c_duplicate(&cstr).as_ptr().cast_mut();
                node
            },
            Self::Bool(b) => unsafe {
                let node = new_node(NodeKind::BoolLit);
                (*node).val.b = *b;
                node
            },
            Self::Char(c) => unsafe {
                let node = new_node(NodeKind::CharLit);
                (*node).val.c = *c as u8 as _;
                node
            },
            Self::Nil => new_node(NodeKind::NilLit),
        }
    }
}

impl Convert for FuncDef {
    fn convert(&self) -> *mut AstNode_T {
        let args = self.args.iter().map(Convert::convert).convert();
        let body = self.body.convert();
        let is_rec = Literal::Bool(self.is_recursive).convert();

        unsafe {
            let node = new_node(NodeKind::FuncDef);
            (*node).kids[0] = args;
            (*node).kids[1] = body;
            (*node).kids[2] = is_rec;
            node
        }
    }
}

impl Convert for Atom {
    fn convert(&self) -> *mut AstNode_T {
        match self {
            Atom::Ident(i) => i.convert(),
            Atom::Literal(l) => l.convert(),
            Atom::Array(a) => unsafe {
                let node = new_node(NodeKind::Array);
                (*node).kids[0] = a.iter().map(Convert::convert).convert();
                node
            },
        }
    }
}

impl Convert for Block {
    fn convert(&self) -> *mut AstNode_T {
        let this = new_node(NodeKind::Block);

        // DO *NOT* try to inline this with `Convert::convert` on the iterator.
        // The iterator version wraps every element in `NodeList`, but because
        // `BLOCK_STMTS` is a special snowflake that doesn't wrap the final
        // expression, that breaks the parsing. So fix `BLOCK_STMTS` please dear god.
        let expr_node = self
            .value
            .as_ref()
            .map_or(std::ptr::null_mut(), |x| x.convert());

        let block =
            self.stats
                .iter()
                .map(Convert::convert)
                .rev()
                .fold(expr_node, |acc, i| unsafe {
                    let node = new_node(NodeKind::NodeList);
                    (*node).kids[1] = acc;
                    (*node).kids[0] = i;
                    node
                });

        unsafe {
            (*this).kids[0] = block;
        }

        this
    }
}

impl Convert for Ident {
    fn convert(&self) -> *mut AstNode_T {
        let node = new_node(NodeKind::Ident);
        let cstr = CString::new(self.0.clone()).expect("Ident can not contain internal 0 byte");
        unsafe {
            (*node).val.i = c_duplicate(&cstr).as_ptr().cast_mut();
        }
        node
    }
}

impl<T> Convert for T
where
    T: Iterator<Item = *mut AstNode_T> + Clone + DoubleEndedIterator,
{
    fn convert(&self) -> *mut AstNode_T {
        // Build the list backwards:
        //
        // Given the list:
        //   a -> b -> c
        //
        // Steps for list:
        //
        //   (null, c)
        //   let c_node = new_node();
        //   c_node.kids[1] = NULL;
        //   c_node.kids[0] = c;
        //   c_node
        //
        //   (c_node, b)
        //   let b_node = new_node();
        //   b_node.kids[1] = c_node;
        //   b_node.kids[0] = b;
        //   b_node
        //
        //   (b_node, a)
        //   let a_node = new_node();
        //   a_node.kids[1] = b_node;
        //   a_node.kids[0] = a;
        //   a_node
        //
        // Generalized:
        //
        //   (acc, new_item)
        //   let node = new_node();
        //   node.kids[1] = acc;
        //   node.kids[0] = new_item;
        //   node
        //

        self.clone()
            .rev()
            .fold(std::ptr::null_mut(), |acc, i| unsafe {
                let node = new_node(NodeKind::NodeList);
                (*node).kids[1] = acc;
                (*node).kids[0] = i;
                node
            })
    }
}

fn new_node(kind: NodeKind) -> *mut AstNode_T {
    let ptr = unsafe { bindings::new_astnode(kind.as_u32()) };
    assert!(!ptr.is_null(), "Memory allocation failed. Rough day buddy.");

    for x in 0..bindings::MAX_AST_KID_COUNT {
        // Safety
        // `bindings::new_astnode` returns a non-null ptr to a valid allocation of
        // `AstNode_T`. It is not guarunteed that the values of kids are `null` so
        // we fix that ourselves
        unsafe {
            (*ptr).kids[x as usize] = std::ptr::null_mut();
        }
    }

    ptr
}

fn c_duplicate<'a>(c: &CStr) -> &'a CStr {
    // Safety
    // + A (properly created) CStr is guarunteed to point to a valid *const c_char
    let dup = unsafe { bindings::strdup(c.as_ptr()) };
    assert!(!dup.is_null(), "Memory allocation failed. Rough day buddy.");

    // Safety
    // + Guarunteed by `strdup` when it doesn't return NULL
    unsafe { CStr::from_ptr(dup) }
}

#[derive(Copy, Clone)]
enum NodeKind {
    StrLit,
    NumLit,
    CharLit,
    BoolLit,
    NilLit,
    Ident,
    UnaryNeg,
    UnaryLogNot,
    UnaryBitNot,
    BinaryAdd,
    BinarySub,
    BinaryMul,
    BinaryDiv,
    BinaryMod,
    BinaryLCompose,
    BinaryRCompose,
    BinaryLApply,
    BinaryRApply,
    BinaryLogAnd,
    BinaryLogOr,
    BinaryEq,
    BinaryNe,
    BinaryGt,
    BinaryGe,
    BinaryLt,
    BinaryLe,
    BinaryOr,
    BinaryXor,
    BinaryAnd,
    BinaryRShift,
    BinaryLShift,
    If,
    Case,
    CaseArm,
    Array,
    FuncDef,
    FuncCall,
    ArrayIndex,
    Return,
    ExprStmt,
    AssignStmt,
    Block,
    NodeList,
}

impl NodeKind {
    pub fn as_u32(self) -> u32 {
        match self {
            NodeKind::StrLit => AstNodeType_AstntStrLit,
            NodeKind::NumLit => AstNodeType_AstntNumLit,
            NodeKind::CharLit => AstNodeType_AstntCharLit,
            NodeKind::BoolLit => AstNodeType_AstntBoolLit,
            NodeKind::NilLit => AstNodeType_AstntNilLit,
            NodeKind::Ident => AstNodeType_AstntIdent,
            NodeKind::UnaryNeg => AstNodeType_AstntUnaryNeg,
            NodeKind::UnaryLogNot => AstNodeType_AstntUnaryLogNot,
            NodeKind::UnaryBitNot => AstNodeType_AstntUnaryBitNot,
            NodeKind::BinaryAdd => AstNodeType_AstntBinaryAdd,
            NodeKind::BinarySub => AstNodeType_AstntBinarySub,
            NodeKind::BinaryMul => AstNodeType_AstntBinaryMul,
            NodeKind::BinaryDiv => AstNodeType_AstntBinaryDiv,
            NodeKind::BinaryMod => AstNodeType_AstntBinaryMod,
            NodeKind::BinaryLCompose => AstNodeType_AstntBinaryLCompose,
            NodeKind::BinaryRCompose => AstNodeType_AstntBinaryRCompose,
            NodeKind::BinaryLApply => AstNodeType_AstntBinaryLApply,
            NodeKind::BinaryRApply => AstNodeType_AstntBinaryRApply,
            NodeKind::BinaryLogAnd => AstNodeType_AstntBinaryLogAnd,
            NodeKind::BinaryLogOr => AstNodeType_AstntBinaryLogOr,
            NodeKind::BinaryEq => AstNodeType_AstntBinaryEq,
            NodeKind::BinaryNe => AstNodeType_AstntBinaryNe,
            NodeKind::BinaryGt => AstNodeType_AstntBinaryGt,
            NodeKind::BinaryGe => AstNodeType_AstntBinaryGe,
            NodeKind::BinaryLt => AstNodeType_AstntBinaryLt,
            NodeKind::BinaryLe => AstNodeType_AstntBinaryLe,
            NodeKind::BinaryOr => AstNodeType_AstntBinaryOr,
            NodeKind::BinaryXor => AstNodeType_AstntBinaryXor,
            NodeKind::BinaryAnd => AstNodeType_AstntBinaryAnd,
            NodeKind::BinaryRShift => AstNodeType_AstntBinaryRShift,
            NodeKind::BinaryLShift => AstNodeType_AstntBinaryLShift,
            NodeKind::If => AstNodeType_AstntIf,
            NodeKind::Case => AstNodeType_AstntCase,
            NodeKind::CaseArm => AstNodeType_AstntCaseArm,
            NodeKind::Array => AstNodeType_AstntArray,
            NodeKind::FuncDef => AstNodeType_AstntFuncDef,
            NodeKind::FuncCall => AstNodeType_AstntFuncCall,
            NodeKind::ArrayIndex => AstNodeType_AstntArrayIndex,
            NodeKind::Return => AstNodeType_AstntReturn,
            NodeKind::ExprStmt => AstNodeType_AstntExprStmt,
            NodeKind::AssignStmt => AstNodeType_AstntAssignStmt,
            NodeKind::Block => AstNodeType_AstntBlock,
            NodeKind::NodeList => AstNodeType_AstntNodeList,
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        ast::{
            Assign, Atom, Binary, BinaryOp, Block, Expr, Ident, Literal, Script, Statement, Unary,
            UnaryOp,
        },
        ffi::convert::Convert,
        ffi::Ast,
    };

    #[test]
    fn test_atoms() {
        let atoms = [
            Atom::Literal(Literal::Num(2)),
            Atom::Literal(Literal::Char('c')),
            Atom::Literal(Literal::Bool(true)),
            Atom::Literal(Literal::Nil),
            Atom::Literal(Literal::Str("hi".into())),
            Atom::Ident(Ident("hi".into())),
            Atom::Array(vec![
                Expr::Atom(Atom::Literal(Literal::Num(1))),
                Expr::Atom(Atom::Literal(Literal::Num(2))),
                Expr::Atom(Atom::Literal(Literal::Num(3))),
                Expr::Atom(Atom::Literal(Literal::Num(4))),
            ]),
        ];

        for atom in atoms {
            let clone: Expr = atom.clone().into();
            let at = match unsafe { Ast::from_ptr(clone.convert()) } {
                Ok(Ast::Expr(Expr::Atom(at))) => at,
                t => panic!("Expected Atom, got: {t:#?}"),
            };
            assert_eq!(atom, at);
        }
    }

    #[test]
    fn test_unaries() {
        let unaries = [
            Expr::Unary(Unary {
                rhs: Box::new(Expr::Atom(Atom::Literal(Literal::Num(2)))),
                op: UnaryOp::NumNeg,
            }),
            Expr::Unary(Unary {
                rhs: Box::new(Expr::Atom(Atom::Literal(Literal::Num(1)))),
                op: UnaryOp::BinNot,
            }),
            Expr::Unary(Unary {
                rhs: Box::new(Expr::Atom(Atom::Literal(Literal::Bool(false)))),
                op: UnaryOp::LogNot,
            }),
        ];

        for unary in unaries {
            let clone = unary.clone();
            let un = match unsafe { Ast::from_ptr(clone.convert()) } {
                Ok(Ast::Expr(un @ Expr::Unary(..))) => un,
                t => panic!("Expected Unary, got: {t:#?}"),
            };
            assert_eq!(unary, un);
        }
    }

    #[test]
    fn test_binaries() {
        let binaries = [
            Expr::Binary(Binary {
                lhs: Box::new(Expr::Atom(Atom::Literal(Literal::Num(2)))),
                rhs: Box::new(Expr::Atom(Atom::Literal(Literal::Num(2)))),
                op: BinaryOp::Add,
            }),
            Expr::Binary(Binary {
                lhs: Box::new(Expr::Atom(Atom::Array(vec![
                    Expr::Atom(Atom::Literal(Literal::Num(1))),
                    Expr::Atom(Atom::Literal(Literal::Num(2))),
                    Expr::Atom(Atom::Literal(Literal::Num(3))),
                    Expr::Atom(Atom::Literal(Literal::Num(4))),
                ]))),
                rhs: Box::new(Expr::Atom(Atom::Literal(Literal::Char('C')))),
                op: BinaryOp::Mul,
            }),
            Expr::Binary(Binary {
                lhs: Box::new(Expr::Binary(Binary {
                    lhs: Box::new(Expr::Atom(Atom::Array(vec![
                        Expr::Atom(Atom::Literal(Literal::Num(1))),
                        Expr::Atom(Atom::Literal(Literal::Num(2))),
                        Expr::Atom(Atom::Literal(Literal::Num(3))),
                        Expr::Atom(Atom::Literal(Literal::Num(4))),
                    ]))),
                    rhs: Box::new(Expr::Binary(Binary {
                        lhs: Box::new(Expr::Atom(Atom::Literal(Literal::Num(2)))),
                        rhs: Box::new(Expr::Atom(Atom::Literal(Literal::Num(2)))),
                        op: BinaryOp::Add,
                    })),
                    op: BinaryOp::Mul,
                })),
                rhs: Box::new(Expr::Atom(Atom::Literal(Literal::Char('C')))),
                op: BinaryOp::Sub,
            }),
        ];

        for binary in binaries {
            let clone = binary.clone();
            let bin = match unsafe { Ast::from_ptr(clone.convert()) } {
                Ok(Ast::Expr(bin @ Expr::Binary(..))) => bin,
                t => panic!("Expected Binary, got: {t:#?}"),
            };
            assert_eq!(binary, bin);
        }
    }

    #[test]
    fn test_statements() {
        let statements = [
            Statement::Assign(Assign {
                assignee: Ident("hi".into()),
                value: Expr::Binary(Binary {
                    lhs: Box::new(Expr::Atom(Atom::Literal(Literal::Num(2)))),
                    rhs: Box::new(Expr::Atom(Atom::Literal(Literal::Num(2)))),
                    op: BinaryOp::Add,
                }),
            }),
            Statement::Expr(Expr::Binary(Binary {
                lhs: Box::new(Expr::Atom(Atom::Literal(Literal::Num(2)))),
                rhs: Box::new(Expr::Atom(Atom::Literal(Literal::Num(2)))),
                op: BinaryOp::Add,
            })),
            Statement::Return(None),
            Statement::Return(Some(Expr::Binary(Binary {
                lhs: Box::new(Expr::Atom(Atom::Literal(Literal::Num(2)))),
                rhs: Box::new(Expr::Atom(Atom::Literal(Literal::Num(2)))),
                op: BinaryOp::Add,
            }))),
        ];

        for statement in statements {
            let clone = statement.clone();
            let stat = match unsafe { Ast::from_ptr(clone.convert()) } {
                Ok(Ast::Statement(stat)) => stat,
                t => panic!("Expected Statement, got: {t:#?}"),
            };

            assert_eq!(statement, stat);
        }
    }

    #[test]
    fn test_blocks() {
        let blocks = [
            Expr::Block(Block {
                stats: vec![],
                value: None,
            }),
            Expr::Block(Block {
                stats: vec![],
                value: Some(Box::new(Expr::Atom(Atom::Literal(Literal::Num(0))))),
            }),
            Expr::Block(Block {
                stats: vec![
                    Statement::Expr(Expr::Block(Block {
                        stats: vec![],
                        value: None,
                    })),
                    Statement::Return(None),
                    Statement::Return(Some(Expr::Binary(Binary {
                        lhs: Box::new(Expr::Atom(Atom::Literal(Literal::Num(2)))),
                        rhs: Box::new(Expr::Atom(Atom::Literal(Literal::Num(2)))),
                        op: BinaryOp::Add,
                    }))),
                ],
                value: None,
            }),
            Expr::Block(Block {
                stats: vec![
                    Statement::Expr(Expr::Block(Block {
                        stats: vec![],
                        value: None,
                    })),
                    Statement::Return(None),
                    Statement::Return(Some(Expr::Binary(Binary {
                        lhs: Box::new(Expr::Atom(Atom::Literal(Literal::Num(2)))),
                        rhs: Box::new(Expr::Atom(Atom::Literal(Literal::Num(2)))),
                        op: BinaryOp::Add,
                    }))),
                ],
                value: Some(Box::new(Expr::Binary(Binary {
                    lhs: Box::new(Expr::Atom(Atom::Literal(Literal::Num(2)))),
                    rhs: Box::new(Expr::Atom(Atom::Literal(Literal::Num(2)))),
                    op: BinaryOp::Add,
                }))),
            }),
        ];

        for block in blocks {
            let clone = block.clone();
            let blk = match unsafe { Ast::from_ptr(clone.convert()) } {
                Ok(Ast::Expr(blk @ Expr::Block(..))) => blk,
                t => panic!("Expected Block, got: {t:#?}"),
            };

            assert_eq!(block, blk);
        }
    }

    #[test]
    fn test_scripts() {
        let scripts = [
            Block {
                stats: vec![],
                value: None,
            },
            Block {
                stats: vec![],
                value: Some(Box::new(Expr::Atom(Atom::Literal(Literal::Num(0))))),
            },
            Block {
                stats: vec![
                    Statement::Expr(Expr::Block(Block {
                        stats: vec![],
                        value: None,
                    })),
                    Statement::Return(None),
                    Statement::Return(Some(Expr::Binary(Binary {
                        lhs: Box::new(Expr::Atom(Atom::Literal(Literal::Num(2)))),
                        rhs: Box::new(Expr::Atom(Atom::Literal(Literal::Num(2)))),
                        op: BinaryOp::Add,
                    }))),
                ],
                value: None,
            },
            Block {
                stats: vec![
                    Statement::Expr(Expr::Block(Block {
                        stats: vec![],
                        value: None,
                    })),
                    Statement::Return(None),
                    Statement::Return(Some(Expr::Binary(Binary {
                        lhs: Box::new(Expr::Atom(Atom::Literal(Literal::Num(2)))),
                        rhs: Box::new(Expr::Atom(Atom::Literal(Literal::Num(2)))),
                        op: BinaryOp::Add,
                    }))),
                ],
                value: Some(Box::new(Expr::Binary(Binary {
                    lhs: Box::new(Expr::Atom(Atom::Literal(Literal::Num(2)))),
                    rhs: Box::new(Expr::Atom(Atom::Literal(Literal::Num(2)))),
                    op: BinaryOp::Add,
                }))),
            },
        ]
        .map(|block| Script { block });

        for script in scripts {
            let clone = script.clone();
            let scr = unsafe { Script::from_ptr(clone.convert()).unwrap() };
            assert_eq!(script, scr);
        }
    }
}
