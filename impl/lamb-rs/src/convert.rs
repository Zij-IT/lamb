use std::ffi::CString;

use crate::wrap::{
    ffi, Assign, Ast, AstNode_T, Atom, Binary, BinaryOp, Block, Case, Either, Else, Expr, FuncCall,
    FuncDef, Ident, If, Index, Literal, Script, Statement, Unary, UnaryOp,
};

pub fn test_all() {
    test_atoms();
    test_unaries();
    test_binaries();
    test_statements();
    test_blocks();
    test_scripts();
}

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

    println!("test_atoms: PASS");
}

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

    println!("test_unary: PASS");
}

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

    println!("test_binary: PASS");
}

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

    println!("test_statements: PASS");
}

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

    println!("test_blocks: PASS");
}

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

    println!("test_scripts: PASS");
}

unsafe fn nullify_kids(node: &*mut AstNode_T) {
    if node.is_null() {
        return;
    }

    for kid in 0..ffi::MAX_AST_KID_COUNT as usize {
        unsafe {
            (**node).kids[kid] = std::ptr::null_mut();
        }
    }
}

pub trait Convert {
    fn convert(self) -> *mut AstNode_T;
}

impl<L, R> Convert for Either<L, R>
where
    L: Convert,
    R: Convert,
{
    fn convert(self) -> *mut AstNode_T {
        match self {
            Self::Left(l) => l.convert(),
            Self::Right(r) => r.convert(),
        }
    }
}

impl Convert for Script {
    fn convert(self) -> *mut AstNode_T {
        let block = self.block.convert();
        unsafe {
            let kid = (*block).kids[0];
            nullify_kids(&block);
            ffi::free_ast(block);
            kid
        }
    }
}

impl Convert for Statement {
    fn convert(self) -> *mut AstNode_T {
        match self {
            Statement::Assign(Assign { assignee, value }) => unsafe {
                let this = ffi::new_astnode(ffi::AstNodeType_AstntAssignStmt);
                nullify_kids(&this);
                (*this).kids[0] = assignee.convert();
                (*this).kids[1] = value.convert();
                this
            },
            Statement::Expr(e) => unsafe {
                let this = ffi::new_astnode(ffi::AstNodeType_AstntExprStmt);
                nullify_kids(&this);
                (*this).kids[0] = e.convert();

                this
            },
            Statement::Return(r) => unsafe {
                let this = ffi::new_astnode(ffi::AstNodeType_AstntReturn);
                nullify_kids(&this);

                (*this).kids[0] = r.map_or(std::ptr::null_mut(), Expr::convert);
                this
            },
        }
    }
}

impl Convert for Expr {
    fn convert(self) -> *mut AstNode_T {
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
    fn convert(self) -> *mut AstNode_T {
        let Binary { lhs, rhs, op } = self;
        let node_type = match op {
            BinaryOp::Add => ffi::AstNodeType_AstntBinaryAdd,
            BinaryOp::Sub => ffi::AstNodeType_AstntBinarySub,
            BinaryOp::Div => ffi::AstNodeType_AstntBinaryDiv,
            BinaryOp::Mul => ffi::AstNodeType_AstntBinaryMul,
            BinaryOp::Mod => ffi::AstNodeType_AstntBinaryMod,
            BinaryOp::LApply => ffi::AstNodeType_AstntBinaryLApply,
            BinaryOp::RApply => ffi::AstNodeType_AstntBinaryRApply,
            BinaryOp::LCompose => ffi::AstNodeType_AstntBinaryLCompose,
            BinaryOp::RCompose => ffi::AstNodeType_AstntBinaryRCompose,
            BinaryOp::Gt => ffi::AstNodeType_AstntBinaryGt,
            BinaryOp::Ge => ffi::AstNodeType_AstntBinaryGe,
            BinaryOp::Lt => ffi::AstNodeType_AstntBinaryLt,
            BinaryOp::Le => ffi::AstNodeType_AstntBinaryLe,
            BinaryOp::Eq => ffi::AstNodeType_AstntBinaryEq,
            BinaryOp::Ne => ffi::AstNodeType_AstntBinaryNe,
            BinaryOp::LogOr => ffi::AstNodeType_AstntBinaryLogOr,
            BinaryOp::LogAnd => ffi::AstNodeType_AstntBinaryLogAnd,
            BinaryOp::BinOr => ffi::AstNodeType_AstntBinaryOr,
            BinaryOp::BinAnd => ffi::AstNodeType_AstntBinaryAnd,
            BinaryOp::BinXor => ffi::AstNodeType_AstntBinaryXor,
            BinaryOp::RShift => ffi::AstNodeType_AstntBinaryRShift,
            BinaryOp::LShift => ffi::AstNodeType_AstntBinaryLShift,
        };

        let node = unsafe { ffi::new_astnode(node_type) };
        unsafe { nullify_kids(&node) };
        unsafe {
            (*node).kids[0] = lhs.convert();
        }
        unsafe {
            (*node).kids[1] = rhs.convert();
        }

        node
    }
}

impl Convert for Unary {
    fn convert(self) -> *mut AstNode_T {
        let Unary { rhs, op } = self;
        let node_type = match op {
            UnaryOp::NumNeg => ffi::AstNodeType_AstntUnaryNeg,
            UnaryOp::LogNot => ffi::AstNodeType_AstntUnaryLogNot,
            UnaryOp::BinNot => ffi::AstNodeType_AstntUnaryBitNot,
        };

        let node = unsafe { ffi::new_astnode(node_type) };
        unsafe { nullify_kids(&node) };
        unsafe {
            (*node).kids[0] = rhs.convert();
        }

        node
    }
}

impl Convert for FuncCall {
    fn convert(self) -> *mut AstNode_T {
        let FuncCall { callee, args } = self;

        let node = unsafe { ffi::new_astnode(ffi::AstNodeType_AstntFuncCall) };
        unsafe { nullify_kids(&node) };
        unsafe {
            (*node).kids[0] = callee.convert();
        }
        unsafe {
            (*node).kids[1] = args.into_iter().map(Convert::convert).convert();
        }

        node
    }
}

impl Convert for Index {
    fn convert(self) -> *mut AstNode_T {
        let Index { indexee, index } = self;

        let node = unsafe { ffi::new_astnode(ffi::AstNodeType_AstntArrayIndex) };
        unsafe { nullify_kids(&node) };
        unsafe {
            (*node).kids[0] = indexee.convert();
        }
        unsafe {
            (*node).kids[1] = index.convert();
        }

        node
    }
}

impl Convert for If {
    fn convert(self) -> *mut AstNode_T {
        let If {
            cond,
            block,
            elifs,
            els,
        } = self;

        let node = unsafe { ffi::new_astnode(ffi::AstNodeType_AstntIf) };
        unsafe { nullify_kids(&node) };
        unsafe {
            (*node).kids[0] = cond.convert();
        }
        unsafe {
            (*node).kids[1] = block.convert();
        }
        unsafe {
            let els_node = els.map_or(std::ptr::null_mut(), |x| x.convert());
            (*node).kids[2] = elifs.into_iter().rev().fold(els_node, |acc, i| {
                let node = ffi::new_astnode(ffi::AstNodeType_AstntIf);
                (*node).kids[2] = acc;
                (*node).kids[1] = i.block.convert();
                (*node).kids[0] = i.cond.convert();
                node
            });
        }

        node
    }
}

impl Convert for Else {
    fn convert(self) -> *mut AstNode_T {
        self.block.convert()
    }
}

impl Convert for Case {
    fn convert(self) -> *mut AstNode_T {
        let arms = self
            .arms
            .into_iter()
            .rev()
            .fold(std::ptr::null_mut(), |acc, arm| unsafe {
                let node = ffi::new_astnode(ffi::AstNodeType_AstntCaseArm);
                (*node).kids[2] = acc;
                (*node).kids[1] = arm.on_match.convert();
                (*node).kids[0] = arm.pattern.convert();
                node
            });

        let value = self.value.convert();

        unsafe {
            let node = ffi::new_astnode(ffi::AstNodeType_AstntCase);
            nullify_kids(&node);
            (*node).kids[0] = value;
            (*node).kids[1] = arms;
            node
        }
    }
}

impl Convert for Literal {
    fn convert(self) -> *mut AstNode_T {
        match self {
            Self::Num(n) => unsafe {
                let node = ffi::new_astnode(ffi::AstNodeType_AstntNumLit);
                (*node).val.n = n;
                node
            },
            Self::Str(s) => unsafe {
                let node = ffi::new_astnode(ffi::AstNodeType_AstntStrLit);
                let cstr = CString::new(s).expect("String should not contain internal 0 byte");
                (*node).val.s = ffi::strdup(cstr.as_ptr());
                node
            },
            Self::Bool(b) => unsafe {
                let node = ffi::new_astnode(ffi::AstNodeType_AstntBoolLit);
                (*node).val.b = b;
                node
            },
            Self::Char(c) => unsafe {
                let node = ffi::new_astnode(ffi::AstNodeType_AstntCharLit);
                (*node).val.c = c as u8 as _;
                node
            },
            Self::Nil => unsafe { ffi::new_astnode(ffi::AstNodeType_AstntNilLit) },
        }
    }
}

impl Convert for FuncDef {
    fn convert(self) -> *mut AstNode_T {
        let args = self.args.into_iter().map(Convert::convert).convert();
        let body = self.body.convert();
        let is_rec = Literal::Bool(self.is_recursive).convert();

        unsafe {
            let node = ffi::new_astnode(ffi::AstNodeType_AstntFuncDef);
            (*node).kids[0] = args;
            (*node).kids[1] = body;
            (*node).kids[2] = is_rec;
            node
        }
    }
}

impl Convert for Atom {
    fn convert(self) -> *mut AstNode_T {
        match self {
            Atom::Ident(i) => i.convert(),
            Atom::Literal(l) => l.convert(),
            Atom::Array(a) => unsafe {
                let node = ffi::new_astnode(ffi::AstNodeType_AstntArray);
                nullify_kids(&node);
                (*node).kids[0] = a.into_iter().map(Convert::convert).convert();
                node
            },
        }
    }
}

impl Convert for Block {
    fn convert(self) -> *mut AstNode_T {
        let this = unsafe {
            let this = ffi::new_astnode(ffi::AstNodeType_AstntBlock);
            nullify_kids(&this);
            this
        };

        // DO *NOT* try to inline this with `Convert::convert` on the iterator.
        // The iterator version wraps every element in `NodeList`, but because
        // `BLOCK_STMTS` is a special snowflake that doesn't wrap the final
        // expression, that breaks the parsing. So fix `BLOCK_STMTS` please dear god.
        let expr_node = self.value.map_or(std::ptr::null_mut(), |x| x.convert());
        let block =
            self.stats
                .into_iter()
                .map(Convert::convert)
                .rev()
                .fold(expr_node, |acc, i| unsafe {
                    let node = ffi::new_astnode(ffi::AstNodeType_AstntNodeList);
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
    fn convert(self) -> *mut AstNode_T {
        let node = unsafe { ffi::new_astnode(ffi::AstNodeType_AstntIdent) };
        let cstr = CString::new(self.0).expect("Ident should not contain internal 0 byte");
        unsafe { (*node).val.i = ffi::strdup(cstr.as_ptr()) };
        node
    }
}

impl<T> Convert for T
where
    T: Iterator<Item = *mut AstNode_T> + DoubleEndedIterator,
{
    fn convert(self) -> *mut AstNode_T {
        let new_node = || unsafe { ffi::new_astnode(ffi::AstNodeType_AstntNodeList) };
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

        self.rev().fold(std::ptr::null_mut(), |acc, i| unsafe {
            let node = new_node();
            (*node).kids[1] = acc;
            (*node).kids[0] = i;
            node
        })
    }
}
