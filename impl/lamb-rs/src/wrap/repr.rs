use std::{ffi::CStr, str::Utf8Error};

use crate::wrap::bindings as ffi;

use super::{
    ast::{self, BinaryOp, Block, Expr, Literal, Statement, UnaryOp},
    AstNode_T,
};

#[derive(Debug, Clone, PartialEq)]
pub struct Script {
    pub block: Block,
}

impl Script {
    /// # Safety
    /// `node` must point to a validly constructed AstNode_T with the following
    /// structure, which is dependent on its `type_`:
    ///
    /// AstNode_T {
    ///    kids: [*mut AstNode_T; 3],
    ///    val: union { n: i64, c: c_char, b: bool, i: CString, s: CString },
    ///    type_: AstntKind,
    /// }
    ///
    /// Literal:
    /// type_           |  Rest of structure
    /// ----------------+-------------------
    /// AstntNilLit     | empty
    /// AstntNumLit     | val.n contains i64    
    /// AstntCharLit    | val.c contains c_char
    /// AstntBoolLit    | val.b contains bool   
    /// AstntIdent      | val.i contains CString
    /// AstntStrLit     | val.s contains CString
    /// AstntUnaryXX    | kids[0] contains an expression
    /// AstntBinaryXX   | kids[0] and kids[1] contain expressions
    /// AstntIf         | kids[0] contains an expression, kids[1] contains a block, kids[2] *may* contain an AstntIf *OR* (else) AstntBlock
    /// AstntCase       | kids[0] contains an expression, kids[1] *may* contain a valid AstntCaseArm
    /// AstntCaseArm    | kids[0] contains an Astnt__Lit *OR* AstntId, kids[1] contains a valid expression or AstntBlock, kids[2] *may* contain an AstntCaseArm
    /// AstntFuncDef    | kids[0] may contain an AstntNodeList of AstntIdent, kids[1] contains an expression, kids[2] contains a AstntBoolLit
    /// AstntFuncCall   | kids[0] contains an expression, kids[1] may contain an AstntNodeList of expressions
    /// AstntArrayIndex | kids[0] contains an expression, kids[1] contains an expression
    /// AstntReturn     | kids[0] may contain an expression
    /// AstntExprStmt   | kids[0] contains an expression
    /// AstntAssignStmt | kids[0] contains an AstntIdent, kids[1] contains an expression
    /// AstntBlock      | kids[0] contains an AstntNodeList, which ends NULL *OR* an expression
    /// AstntNodeList   | kids[0] contains a node of type T, kids[1] contains an AstntNodeList of type
    /// AstntArray      | kids[0] contains an AstntNodeList
    pub unsafe fn from_ptr(node: *mut AstNode_T) -> Result<Self, NodeError> {
        Ok(Self {
            block: block_inner(node)?.expect_expr()?.expect_block()?,
        })
    }

    pub fn to_ptr(self) -> *mut AstNode_T {
        crate::convert::Convert::convert(self)
    }
}

#[derive(PartialEq, Eq)]
pub enum AstRepr {
    Expr(Expr),
    Statement(Statement),
}

impl std::fmt::Debug for AstRepr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AstRepr::Expr(e) => write!(f, "{e:#?}"),
            AstRepr::Statement(s) => write!(f, "{s:#?}"),
        }
    }
}

impl AstRepr {
    pub unsafe fn from_ptr(node: *mut AstNode_T) -> Result<Self, NodeError> {
        into_rust_repr(node)
    }

    fn expect_expr(self) -> Result<Expr, NodeError> {
        match self {
            Self::Expr(e) => Ok(e),
            Self::Statement(_) => Err(NodeError::malformed()),
        }
    }

    fn expect_stmt(self) -> Result<Statement, NodeError> {
        match self {
            Self::Statement(s) => Ok(s),
            Self::Expr(_) => Err(NodeError::malformed()),
        }
    }
}

impl<T> From<T> for AstRepr
where
    T: Into<Expr>,
{
    fn from(value: T) -> Self {
        Self::Expr(value.into())
    }
}

impl Expr {
    fn expect_block(self) -> Result<Block, NodeError> {
        match self {
            Expr::Block(b) => Ok(b),
            _ => Err(NodeError::malformed()),
        }
    }
}

#[derive(Debug)]
pub enum NodeError {
    UnknownNodeType(u32),
    InvalidUtf8(Utf8Error),
    MalformedNode,
}

impl NodeError {
    // Helper function for debugging mistakes between the C and Rust representations
    fn malformed() -> Self {
        let bt = std::backtrace::Backtrace::force_capture();
        println!("{bt}");

        Self::MalformedNode
    }
}

/// # Safety
///
/// See 'Safety' section for `Script::from_ptr`
unsafe fn into_rust_repr(node: *mut AstNode_T) -> Result<AstRepr, NodeError> {
    if node.is_null() {
        return Err(NodeError::malformed());
    }

    match (*node).type_ {
        ffi::AstNodeType_AstntNilLit => Ok(AstRepr::from(Literal::Nil)),
        ffi::AstNodeType_AstntNumLit => Ok(AstRepr::from(Literal::Num((*node).val.n))),
        ffi::AstNodeType_AstntCharLit => Ok(AstRepr::from(Literal::Char(char::from(
            (*node).val.c as u8,
        )))),
        ffi::AstNodeType_AstntBoolLit => Ok(AstRepr::from(Literal::Bool((*node).val.b))),
        ffi::AstNodeType_AstntIdent => CStr::from_ptr((*node).val.i)
            .to_str()
            .map(|s| AstRepr::from(ast::Ident::from(s)))
            .map_err(NodeError::InvalidUtf8),
        ffi::AstNodeType_AstntStrLit => CStr::from_ptr((*node).val.s)
            .to_str()
            .map(|s| AstRepr::from(Literal::Str(s.into())))
            .map_err(NodeError::InvalidUtf8),
        ffi::AstNodeType_AstntUnaryNeg => new_unary(node, UnaryOp::NumNeg),
        ffi::AstNodeType_AstntUnaryLogNot => new_unary(node, UnaryOp::LogNot),
        ffi::AstNodeType_AstntUnaryBitNot => new_unary(node, UnaryOp::BinNot),
        ffi::AstNodeType_AstntBinaryAdd => new_binary(node, BinaryOp::Add),
        ffi::AstNodeType_AstntBinarySub => new_binary(node, BinaryOp::Sub),
        ffi::AstNodeType_AstntBinaryMul => new_binary(node, BinaryOp::Mul),
        ffi::AstNodeType_AstntBinaryDiv => new_binary(node, BinaryOp::Div),
        ffi::AstNodeType_AstntBinaryMod => new_binary(node, BinaryOp::Mod),
        ffi::AstNodeType_AstntBinaryLCompose => new_binary(node, BinaryOp::LCompose),
        ffi::AstNodeType_AstntBinaryRCompose => new_binary(node, BinaryOp::RCompose),
        ffi::AstNodeType_AstntBinaryLApply => new_binary(node, BinaryOp::LApply),
        ffi::AstNodeType_AstntBinaryRApply => new_binary(node, BinaryOp::RApply),
        ffi::AstNodeType_AstntBinaryLogAnd => new_binary(node, BinaryOp::LogAnd),
        ffi::AstNodeType_AstntBinaryLogOr => new_binary(node, BinaryOp::LogOr),
        ffi::AstNodeType_AstntBinaryEq => new_binary(node, BinaryOp::Eq),
        ffi::AstNodeType_AstntBinaryNe => new_binary(node, BinaryOp::Ne),
        ffi::AstNodeType_AstntBinaryGt => new_binary(node, BinaryOp::Gt),
        ffi::AstNodeType_AstntBinaryGe => new_binary(node, BinaryOp::Ge),
        ffi::AstNodeType_AstntBinaryLt => new_binary(node, BinaryOp::Lt),
        ffi::AstNodeType_AstntBinaryLe => new_binary(node, BinaryOp::Le),
        ffi::AstNodeType_AstntBinaryOr => new_binary(node, BinaryOp::BinOr),
        ffi::AstNodeType_AstntBinaryXor => new_binary(node, BinaryOp::BinXor),
        ffi::AstNodeType_AstntBinaryAnd => new_binary(node, BinaryOp::BinAnd),
        ffi::AstNodeType_AstntBinaryRShift => new_binary(node, BinaryOp::RShift),
        ffi::AstNodeType_AstntBinaryLShift => new_binary(node, BinaryOp::LShift),
        ffi::AstNodeType_AstntIf => new_if(node),
        ffi::AstNodeType_AstntCase => new_case(node),
        ffi::AstNodeType_AstntArray => new_array(node),
        ffi::AstNodeType_AstntFuncDef => new_func_def(node),
        ffi::AstNodeType_AstntFuncCall => new_func_call(node),
        ffi::AstNodeType_AstntArrayIndex => new_index(node),
        ffi::AstNodeType_AstntReturn => new_return(node),
        ffi::AstNodeType_AstntExprStmt => new_expr_stmt(node),
        ffi::AstNodeType_AstntAssignStmt => new_binding(node),
        ffi::AstNodeType_AstntBlock => new_block(node),
        ffi::AstNodeType_AstntNodeList => {
            unimplemented!(
                "NodeList shouldn't be parseable outside of a EXPR_LIST or FUNC_ARGS_LIST"
            )
        }
        ffi::AstNodeType_AstntCaseArm => {
            unimplemented!("CaseArms shouldn't be parseable outside of a Case")
        }
        t => Err(NodeError::UnknownNodeType(t)),
    }
}

/// # Safety
/// The `node` must be of the type `AstntBlock` and have the following structure:
/// + kids[0] contains an AstntNodeList, which ends in NULL *OR* an expression
unsafe fn new_block(node: *mut AstNode_T) -> Result<AstRepr, NodeError> {
    block_inner((*node).kids[0])
}

/// # Safety
/// The `node` must be an `AstntNodeList` of statements which ends in either NULL
/// or an expression.
unsafe fn block_inner(node: *mut AstNode_T) -> Result<AstRepr, NodeError> {
    // Can't work with CListIter because the final expression node
    // is hung directly on the final node
    let (stats, node) = CListIter::new(
        |node| {
            if (*node).type_ == ffi::AstNodeType_AstntNodeList {
                if (*node).kids[0].is_null() {
                    None
                } else {
                    Some(into_rust_repr((*node).kids[0]).and_then(AstRepr::expect_stmt))
                }
            } else {
                None
            }
        },
        |node| node,
        |node| (*node).kids[1],
        node,
    )
    .collect_with_node()?;

    let value = if node.is_null() {
        None
    } else {
        Some(Box::new(into_rust_repr(node)?.expect_expr()?))
    };

    Ok(AstRepr::Expr(Expr::Block(Block { stats, value })))
}

/// # Safety
/// The `node` must be of the type `AstntAssignStmt` and have the following structure:
/// + kids[0] contains an AstntAssignStmt
/// + kids[1] contains an expression
unsafe fn new_binding(node: *mut AstNode_T) -> Result<AstRepr, NodeError> {
    let ident = match into_rust_repr((*node).kids[0])? {
        AstRepr::Expr(Expr::Atom(ast::Atom::Ident(i))) => i,
        _ => return Err(NodeError::malformed()),
    };

    let value = into_rust_repr((*node).kids[1])?.expect_expr()?;

    Ok(AstRepr::Statement(Statement::Assign(ast::Assign {
        assignee: ident,
        value,
    })))
}

/// # Safety
/// The `node` must be of the type `AstntExprStmt` and have the following structure:
/// + kids[0] contains an expression
unsafe fn new_expr_stmt(node: *mut AstNode_T) -> Result<AstRepr, NodeError> {
    let expr = into_rust_repr((*node).kids[0])?.expect_expr()?;
    Ok(AstRepr::Statement(Statement::Expr(expr)))
}

/// # Safety
/// The `node` must be of the type `AstntReturn` and have the following structure:
/// + kids[0] may contain an expression
unsafe fn new_return(node: *mut AstNode_T) -> Result<AstRepr, NodeError> {
    let value = if (*node).kids[0].is_null() {
        None
    } else {
        Some(into_rust_repr((*node).kids[0])?.expect_expr()?)
    };

    Ok(AstRepr::Statement(Statement::Return(value)))
}

/// # Safety
/// The `node` must be of the type `AstntArrayIndex` and have the following structure:
/// + kids[0] may contain an expression
/// + kids[1] may contain an expression
unsafe fn new_index(node: *mut AstNode_T) -> Result<AstRepr, NodeError> {
    let indexee = into_rust_repr((*node).kids[0])?.expect_expr()?;
    let index = into_rust_repr((*node).kids[1])?.expect_expr()?;

    Ok(AstRepr::Expr(Expr::Index(ast::Index {
        index: Box::new(index),
        indexee: Box::new(indexee),
    })))
}

/// # Safety
/// The `node` must be of the type `AstntFuncCall` and have the following structure:
/// + kids[0] contains an expression
/// + kids[1] may contain an AstntNodeList of expressions
unsafe fn new_func_call(node: *mut AstNode_T) -> Result<AstRepr, NodeError> {
    let callee = into_rust_repr((*node).kids[0])?.expect_expr()?;
    let args = expr_list((*node).kids[1])?;

    Ok(AstRepr::Expr(Expr::FuncCall(ast::FuncCall {
        callee: Box::new(callee),
        args,
    })))
}

/// # Safety
/// The `node` must be of the type `AstntFuncDef` and have the following structure:
/// + kids[0] may contain an AstntNodeList of AstntIdent
/// + kids[1] contains an expression
/// + kids[2] contains a AstntBoolLit
unsafe fn new_func_def(node: *mut AstNode_T) -> Result<AstRepr, NodeError> {
    let args = ident_list((*node).kids[0])?;
    let body = into_rust_repr((*node).kids[1])?.expect_expr()?;

    let is_recursive = match into_rust_repr((*node).kids[2])? {
        AstRepr::Expr(Expr::Atom(ast::Atom::Literal(Literal::Bool(b)))) => b,
        _ => return Err(NodeError::malformed()),
    };

    Ok(AstRepr::Expr(Expr::FuncDef(ast::FuncDef {
        args,
        body: Box::new(body),
        is_recursive,
    })))
}

/// # Safety
/// The `node` must be of the type `AstntUnaryXX` and have the following structure:
/// + kids[0] contains an expression
unsafe fn new_unary(node: *mut AstNode_T, op: UnaryOp) -> Result<AstRepr, NodeError> {
    Ok(AstRepr::Expr(Expr::Unary(ast::Unary::new(
        into_rust_repr((*node).kids[0])?.expect_expr()?,
        op,
    ))))
}

/// # Safety
/// The `node` must be of the type `AstntBinaryXX` and have the following structure:
/// + kids[0] contains an expression
/// + kids[1] contains an expression
unsafe fn new_binary(node: *mut AstNode_T, op: BinaryOp) -> Result<AstRepr, NodeError> {
    let lhs = into_rust_repr((*node).kids[0])?.expect_expr()?;
    let rhs = into_rust_repr((*node).kids[1])?.expect_expr()?;

    Ok(AstRepr::Expr(Expr::Binary(ast::Binary::new(lhs, rhs, op))))
}

/// # Safety
/// The `node` must be of the type `AstntIf` and have the following structure:
/// + kids[0] contains an expression
/// + kids[1] contains a block
/// + kids[2] *may* contain an AstntIf *OR* (else) AstntBlock
unsafe fn new_if(node: *mut AstNode_T) -> Result<AstRepr, NodeError> {
    let cond = into_rust_repr((*node).kids[0])?.expect_expr()?;
    let block = into_rust_repr((*node).kids[1])?
        .expect_expr()?
        .expect_block()?;

    let (elifs, node) = CListIter::new(
        |node| ((*node).type_ == ffi::AstNodeType_AstntIf).then(|| new_elif(node)),
        |node| node,
        |node| (*node).kids[2],
        (*node).kids[2],
    )
    .collect_with_node()?;

    let els = if node.is_null() {
        None
    } else {
        Some(new_else(node)?)
    };

    Ok(AstRepr::Expr(Expr::If(ast::If::new(
        cond, block, elifs, els,
    ))))
}

/// # Safety
/// The `node` must be of the type `AstntIf` and have the following structure:
/// + kids[0] contains an expression
/// + kids[1] contains a block
/// + kids[2] *may* contain an AstntIf *OR* (else) AstntBlock
unsafe fn new_elif(node: *mut AstNode_T) -> Result<ast::Elif, NodeError> {
    let cond = into_rust_repr((*node).kids[0])?.expect_expr()?;
    let block = into_rust_repr((*node).kids[1])?
        .expect_expr()?
        .expect_block()?;

    Ok(ast::Elif::new(cond, block))
}

/// # Safety
/// The `node` must be of the type `AstntBlock` and have the following structure:
/// + kids[0] contains an AstntNodeList, which ends in NULL *OR* an expression
unsafe fn new_else(node: *mut AstNode_T) -> Result<ast::Else, NodeError> {
    let block = into_rust_repr(node)?.expect_expr()?.expect_block()?;
    Ok(ast::Else::new(block))
}

/// # Safety
/// The `node` must be of the type `AstntCase` and have the following structure:
/// + kids[0] contains an expression
/// + kids[1] *may* contain a valid `AstntCaseArm`
unsafe fn new_case(node: *mut AstNode_T) -> Result<AstRepr, NodeError> {
    let value = Box::new(into_rust_repr((*node).kids[0])?.expect_expr()?);
    let arms = CListIter::new(
        |node| Some(new_case_arm(node)),
        |node| node,
        |node| (*node).kids[2],
        (*node).kids[1],
    )
    .collect::<Result<_, _>>()?;

    Ok(AstRepr::Expr(Expr::Case(ast::Case { value, arms })))
}

/// # Safety
/// The `node` must be of the type `AstntCase` and have the following structure:
/// + kids[0] contains an Astnt__Lit *OR* AstntId
/// + kids[1] contains a valid expression or AstntBlock
/// + kids[2] *may* contain an AstntCaseArm
unsafe fn new_case_arm(node: *mut AstNode_T) -> Result<ast::CaseArm, NodeError> {
    let pattern = match into_rust_repr((*node).kids[0])? {
        AstRepr::Expr(Expr::Atom(ast::Atom::Literal(l))) => ast::Either::Left(l),
        AstRepr::Expr(Expr::Atom(ast::Atom::Ident(i))) => ast::Either::Right(i),
        _ => return Err(NodeError::malformed()),
    };

    let on_match = match into_rust_repr((*node).kids[1])? {
        AstRepr::Expr(Expr::Block(b)) => ast::Either::Left(b),
        AstRepr::Expr(e) => ast::Either::Right(e),
        _ => return Err(NodeError::malformed()),
    };

    Ok(ast::CaseArm { pattern, on_match })
}

/// # Safety
/// The `node` must be of the type `AstntArray` and have the following structure:
/// + kids[0] contains an AstntNodeList of expressions
unsafe fn new_array(node: *mut AstNode_T) -> Result<AstRepr, NodeError> {
    Ok(AstRepr::Expr(Expr::Atom(ast::Atom::Array(expr_list(
        (*node).kids[0],
    )?))))
}

/// # Safety
/// The `node` must be of the type `AstntNodeList` and have the following structure:
/// + kids[0] contains an expressions
/// + kids[1] contains an `AstntNodeList` of expressions
unsafe fn expr_list(node: *mut AstNode_T) -> Result<Vec<Expr>, NodeError> {
    CListIter::new(
        |node| Some(into_rust_repr(node).and_then(AstRepr::expect_expr)),
        |node| (*node).kids[0],
        |node| (*node).kids[1],
        node,
    )
    .collect()
}

/// # Safety
/// The `node` must be of the type `AstntNodeList` and have the following structure:
/// + kids[0] contains an `AstntIdent`
/// + kids[1] contains an `AstntNodeList` of `AstntIdent`
unsafe fn ident_list(node: *mut AstNode_T) -> Result<Vec<ast::Ident>, NodeError> {
    CListIter::new(
        |node| match into_rust_repr(node) {
            Ok(AstRepr::Expr(Expr::Atom(ast::Atom::Ident(i)))) => Some(Ok(i)),
            _ => Some(Err(NodeError::malformed())),
        },
        |node| (*node).kids[0],
        |node| (*node).kids[1],
        node,
    )
    .collect()
}

struct CListIter<F, N, K> {
    f: F,
    kid: K,
    next: N,
    node: *mut AstNode_T,
}

impl<F, T, N, K> CListIter<F, N, K>
where
    F: FnMut(*mut AstNode_T) -> Option<Result<T, NodeError>>,
    K: FnMut(*mut AstNode_T) -> *mut AstNode_T,
    N: FnMut(*mut AstNode_T) -> *mut AstNode_T,
{
    fn new(f: F, kid: K, next: N, node: *mut AstNode_T) -> Self {
        Self { f, kid, next, node }
    }

    // Behaves like `Iter::collect` for `Result<Vec<T>>` but returns the last node held by `self`, as
    // well as the `Vec<T>` that is expected
    fn collect_with_node(mut self) -> Result<(Vec<T>, *mut AstNode_T), NodeError> {
        (&mut self)
            .collect::<Result<_, _>>()
            .map(|x| (x, self.node))
    }
}

impl<F, N, K, T> Iterator for CListIter<F, N, K>
where
    F: FnMut(*mut AstNode_T) -> Option<Result<T, NodeError>>,
    K: FnMut(*mut AstNode_T) -> *mut AstNode_T,
    N: FnMut(*mut AstNode_T) -> *mut AstNode_T,
{
    type Item = Result<T, NodeError>;

    fn next(&mut self) -> Option<Self::Item> {
        let Self { f, kid, next, node } = self;

        if node.is_null() {
            None
        } else {
            let item = f(kid(*node))?;
            *node = next(*node);
            Some(item)
        }
    }
}
