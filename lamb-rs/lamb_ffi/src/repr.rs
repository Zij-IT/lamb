use std::{ffi::CStr, str::Utf8Error};

use super::AstNode_T;
use crate::bindings;
use lamb_ast::Atom::{self, Ident};
use lamb_ast::{self, BinaryOp, Block, Expr, Literal, Statement, UnaryOp};

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

impl std::fmt::Display for NodeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            NodeError::UnknownNodeType(ty) => write!(f, "[NodeError] Unknown Node Type: {ty:#?}"),
            NodeError::InvalidUtf8(err) => write!(f, "[NodeError] InvalidUtf8: {err}"),
            NodeError::MalformedNode => write!(f, "[NodeError] Malformed Node"),
        }
    }
}

impl std::error::Error for NodeError {}

pub type Result<T> = std::result::Result<T, NodeError>;

/// # Safety
/// `node` must point to a validly constructed `AstNode_T` with the following
/// structure, which is dependent on its `type_`:
///
/// ```
/// AstNode_T {
///    kids: [*mut AstNode_T; 3],
///    val: union { n: i64, c: c_char, b: bool, i: CString, s: CString },
///    type_: AstntKind,
/// }
/// ```
///
/// Literal:
/// `(*node).type_`   |  Rest of structure
/// ----------------+-------------------
/// `AstntNilLit`     | empty
/// `AstntNumLit`     | `val.n` contains `i64`
/// `AstntCharLit`    | `val.c` contains `c_char`
/// `AstntBoolLit`    | `val.b` contains `bool`
/// `AstntIdent`      | `val.i` contains `CString`
/// `AstntStrLit`     | `val.s` contains `CString`
/// `AstntUnaryXX`    | `kids[0]` contains an expression
/// `AstntBinaryXX`   | `kids[0]` and `kids[1]` contain expressions
/// `AstntIf`         | `kids[0]` contains an expression, `kids[1]` contains a block, `kids[2]` *may* contain an `AstntIf` *OR* (else) `AstntBlock`
/// `AstntCase`       | `kids[0]` contains an expression, `kids[1]` *may* contain a valid `AstntCaseArm`
/// `AstntCaseArm`    | `kids[0]` contains an `Astnt__Lit` *OR* `AstntId`, `kids[1]` contains a valid expression or `AstntBlock`, `kids[2]` *may* contain an `AstntCaseArm`
/// `AstntFuncDef`    | `kids[0]` may contain an `AstntNodeList` of `AstntIdent`, `kids[1]` contains an expression, `kids[2]` contains a `AstntBoolLit`
/// `AstntFuncCall`   | `kids[0]` contains an expression, `kids[1]` may contain an `AstntNodeList` of expressions
/// `AstntArrayIndex` | `kids[0]` contains an expression, `kids[1]` contains an expression
/// `AstntReturn`     | `kids[0]` may contain an expression
/// `AstntExprStmt`   | `kids[0]` contains an expression
/// `AstntAssignStmt` | `kids[0]` contains an `AstntIdent`, `kids[1]` contains an expression
/// `AstntBlock`      | `kids[0]` contains an `AstntNodeList`, which ends `null` *OR* an expression
/// `AstntNodeList`   | `kids[0]` contains a node of type T, `kids[1]` contains an `AstntNodeList` of type
/// `AstntArray`      | `kids[0]` contains an `AstntNodeList`
pub unsafe fn script_from_ptr(node: *mut AstNode_T) -> Result<lamb_ast::Script> {
    Ok(lamb_ast::Script {
        block: block_inner(node)?.expect_block()?,
    })
}

#[doc(hidden)]
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
    #[cfg(test)]
    pub unsafe fn from_ptr(node: *mut AstNode_T) -> Result<Self> {
        into_rust_repr(node)
    }

    fn expect_expr(self) -> Result<Expr> {
        match self {
            Self::Expr(e) => Ok(e),
            Self::Statement(_) => Err(NodeError::malformed()),
        }
    }

    fn expect_block(self) -> Result<Block> {
        match self {
            Self::Expr(Expr::Block(b)) => Ok(b),
            Self::Expr(_) | Self::Statement(_) => Err(NodeError::malformed()),
        }
    }

    fn expect_stmt(self) -> Result<Statement> {
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

/// # Safety
///
/// See 'Safety' section for `Script::from_ptr`
unsafe fn into_rust_repr(node: *mut AstNode_T) -> Result<AstRepr> {
    if node.is_null() {
        return Err(NodeError::malformed());
    }

    match (*node).type_ {
        bindings::AstNodeType_AstntNilLit => Ok(AstRepr::from(Literal::Nil)),
        bindings::AstNodeType_AstntNumLit => Ok(AstRepr::from(Literal::Num((*node).val.n))),
        bindings::AstNodeType_AstntCharLit => Ok(AstRepr::from(Literal::Char(char::from(
            (*node).val.c as u8,
        )))),
        bindings::AstNodeType_AstntBoolLit => Ok(AstRepr::from(Literal::Bool((*node).val.b))),
        bindings::AstNodeType_AstntIdent => CStr::from_ptr((*node).val.i)
            .to_str()
            .map(|s| AstRepr::from(lamb_ast::Ident::from(s)))
            .map_err(NodeError::InvalidUtf8),
        bindings::AstNodeType_AstntStrLit => CStr::from_ptr((*node).val.s)
            .to_str()
            .map(|s| AstRepr::from(Literal::Str(s.into())))
            .map_err(NodeError::InvalidUtf8),
        bindings::AstNodeType_AstntUnaryNeg => new_unary(node, UnaryOp::NumNeg),
        bindings::AstNodeType_AstntUnaryLogNot => new_unary(node, UnaryOp::LogNot),
        bindings::AstNodeType_AstntUnaryBitNot => new_unary(node, UnaryOp::BinNot),
        bindings::AstNodeType_AstntBinaryAdd => new_binary(node, BinaryOp::Add),
        bindings::AstNodeType_AstntBinarySub => new_binary(node, BinaryOp::Sub),
        bindings::AstNodeType_AstntBinaryMul => new_binary(node, BinaryOp::Mul),
        bindings::AstNodeType_AstntBinaryDiv => new_binary(node, BinaryOp::Div),
        bindings::AstNodeType_AstntBinaryMod => new_binary(node, BinaryOp::Mod),
        bindings::AstNodeType_AstntBinaryLCompose => new_binary(node, BinaryOp::LCompose),
        bindings::AstNodeType_AstntBinaryRCompose => new_binary(node, BinaryOp::RCompose),
        bindings::AstNodeType_AstntBinaryLApply => new_binary(node, BinaryOp::LApply),
        bindings::AstNodeType_AstntBinaryRApply => new_binary(node, BinaryOp::RApply),
        bindings::AstNodeType_AstntBinaryLogAnd => new_binary(node, BinaryOp::LogAnd),
        bindings::AstNodeType_AstntBinaryLogOr => new_binary(node, BinaryOp::LogOr),
        bindings::AstNodeType_AstntBinaryEq => new_binary(node, BinaryOp::Eq),
        bindings::AstNodeType_AstntBinaryNe => new_binary(node, BinaryOp::Ne),
        bindings::AstNodeType_AstntBinaryGt => new_binary(node, BinaryOp::Gt),
        bindings::AstNodeType_AstntBinaryGe => new_binary(node, BinaryOp::Ge),
        bindings::AstNodeType_AstntBinaryLt => new_binary(node, BinaryOp::Lt),
        bindings::AstNodeType_AstntBinaryLe => new_binary(node, BinaryOp::Le),
        bindings::AstNodeType_AstntBinaryOr => new_binary(node, BinaryOp::BinOr),
        bindings::AstNodeType_AstntBinaryXor => new_binary(node, BinaryOp::BinXor),
        bindings::AstNodeType_AstntBinaryAnd => new_binary(node, BinaryOp::BinAnd),
        bindings::AstNodeType_AstntBinaryRShift => new_binary(node, BinaryOp::RShift),
        bindings::AstNodeType_AstntBinaryLShift => new_binary(node, BinaryOp::LShift),
        bindings::AstNodeType_AstntIf => new_if(node),
        bindings::AstNodeType_AstntCase => new_case(node),
        bindings::AstNodeType_AstntArray => new_array(node),
        bindings::AstNodeType_AstntFuncDef => new_func_def(node),
        bindings::AstNodeType_AstntFuncCall => new_func_call(node),
        bindings::AstNodeType_AstntArrayIndex => new_index(node),
        bindings::AstNodeType_AstntReturn => new_return(node),
        bindings::AstNodeType_AstntExprStmt => new_expr_stmt(node),
        bindings::AstNodeType_AstntAssignStmt => new_binding(node),
        bindings::AstNodeType_AstntBlock => new_block(node),
        bindings::AstNodeType_AstntNodeList => {
            unimplemented!(
                "NodeList shouldn't be parseable outside of a EXPR_LIST or FUNC_ARGS_LIST"
            )
        }
        bindings::AstNodeType_AstntCaseArm => {
            unimplemented!("CaseArms shouldn't be parseable outside of a Case")
        }
        t => Err(NodeError::UnknownNodeType(t)),
    }
}

/// # Safety
/// The `node` must be of the type `AstntBlock` and have the following structure:
/// + `kids[0]` contains an `AstntNodeList`, which ends in `null` *OR* an expression
unsafe fn new_block(node: *mut AstNode_T) -> Result<AstRepr> {
    block_inner((*node).kids[0])
}

/// # Safety
/// The `node` must be an `AstntNodeList` of statements which ends in either `null`
/// or an expression.
unsafe fn block_inner(node: *mut AstNode_T) -> Result<AstRepr> {
    let mut list = LinkedList::new(
        |node| {
            if (*node).type_ == bindings::AstNodeType_AstntNodeList && !(*node).kids[0].is_null() {
                Some(into_rust_repr((*node).kids[0]).and_then(AstRepr::expect_stmt))
            } else {
                None
            }
        },
        |node| node,
        |node| (*node).kids[1],
        node,
    );

    let stats = list.into_iter().collect::<Result<_>>()?;
    let node = list.node;

    let value = if node.is_null() {
        None
    } else {
        Some(Box::new(into_rust_repr(node)?.expect_expr()?))
    };

    Ok(AstRepr::Expr(Expr::Block(Block { stats, value })))
}

/// # Safety
/// The `node` must be of the type `AstntAssignStmt` and have the following structure:
/// + `kids[0]` contains an `AstntAssignStmt`
/// + `kids[1]` contains an expression
unsafe fn new_binding(node: *mut AstNode_T) -> Result<AstRepr> {
    let Expr::Atom(Ident(ident)) = into_rust_repr((*node).kids[0])?.expect_expr()? else {
        return Err(NodeError::malformed());
    };

    let value = into_rust_repr((*node).kids[1])?.expect_expr()?;

    Ok(AstRepr::Statement(Statement::Assign(lamb_ast::Assign {
        assignee: ident,
        value,
    })))
}

/// # Safety
/// The `node` must be of the type `AstntExprStmt` and have the following structure:
/// + `kids[0]` contains an expression
unsafe fn new_expr_stmt(node: *mut AstNode_T) -> Result<AstRepr> {
    let expr = into_rust_repr((*node).kids[0])?.expect_expr()?;
    Ok(AstRepr::Statement(Statement::Expr(expr)))
}

/// # Safety
/// The `node` must be of the type `AstntReturn` and have the following structure:
/// + `kids[0]` may contain an expression
unsafe fn new_return(node: *mut AstNode_T) -> Result<AstRepr> {
    let value = if (*node).kids[0].is_null() {
        None
    } else {
        Some(into_rust_repr((*node).kids[0])?.expect_expr()?)
    };

    Ok(AstRepr::Statement(Statement::Return(value)))
}

/// # Safety
/// The `node` must be of the type `AstntArrayIndex` and have the following structure:
/// + `kids[0]` may contain an expression
/// + `kids[1]` may contain an expression
unsafe fn new_index(node: *mut AstNode_T) -> Result<AstRepr> {
    let indexee = into_rust_repr((*node).kids[0])?.expect_expr()?;
    let index = into_rust_repr((*node).kids[1])?.expect_expr()?;

    Ok(AstRepr::Expr(Expr::Index(lamb_ast::Index {
        index: Box::new(index),
        indexee: Box::new(indexee),
    })))
}

/// # Safety
/// The `node` must be of the type `AstntFuncCall` and have the following structure:
/// + `kids[0]` contains an expression
/// + `kids[1]` may contain an `AstntNodeList` of expressions
unsafe fn new_func_call(node: *mut AstNode_T) -> Result<AstRepr> {
    let callee = into_rust_repr((*node).kids[0])?.expect_expr()?;
    let args = expr_list((*node).kids[1])?;

    Ok(AstRepr::Expr(Expr::FuncCall(lamb_ast::FuncCall {
        callee: Box::new(callee),
        args,
    })))
}

/// # Safety
/// The `node` must be of the type `AstntFuncDef` and have the following structure:
/// + `kids[0]` may contain an `AstntNodeList` of `AstntIdent`
/// + `kids[1]` contains an expression
/// + `kids[2]` contains a `AstntBoolLit`
unsafe fn new_func_def(node: *mut AstNode_T) -> Result<AstRepr> {
    let args = ident_list((*node).kids[0])?;
    let body = into_rust_repr((*node).kids[1])?.expect_expr()?;

    let Expr::Atom(Atom::Literal(Literal::Bool(is_recursive))) = into_rust_repr((*node).kids[2])?.expect_expr()? else {
        return Err(NodeError::malformed());
    };

    Ok(AstRepr::Expr(Expr::FuncDef(lamb_ast::FuncDef {
        args,
        body: Box::new(body),
        is_recursive,
    })))
}

/// # Safety
/// The `node` must be of the type `AstntUnaryXX` and have the following structure:
/// + `kids[0]` contains an expression
unsafe fn new_unary(node: *mut AstNode_T, op: UnaryOp) -> Result<AstRepr> {
    Ok(AstRepr::Expr(Expr::Unary(lamb_ast::Unary::new(
        into_rust_repr((*node).kids[0])?.expect_expr()?,
        op,
    ))))
}

/// # Safety
/// The `node` must be of the type `AstntBinaryXX` and have the following structure:
/// + `kids[0]` contains an expression
/// + `kids[1]` contains an expression
unsafe fn new_binary(node: *mut AstNode_T, op: BinaryOp) -> Result<AstRepr> {
    let lhs = into_rust_repr((*node).kids[0])?.expect_expr()?;
    let rhs = into_rust_repr((*node).kids[1])?.expect_expr()?;

    Ok(AstRepr::Expr(Expr::Binary(lamb_ast::Binary::new(
        lhs, rhs, op,
    ))))
}

/// # Safety
/// The `node` must be of the type `AstntIf` and have the following structure:
/// + `kids[0]` contains an expression
/// + `kids[1]` contains a block
/// + `kids[2]` *may* contain an `AstntIf` *OR* (else) `AstntBlock`
unsafe fn new_if(node: *mut AstNode_T) -> Result<AstRepr> {
    let cond = into_rust_repr((*node).kids[0])?.expect_expr()?;
    let block = into_rust_repr((*node).kids[1])?.expect_block()?;

    let mut list = LinkedList::new(
        |node| ((*node).type_ == bindings::AstNodeType_AstntIf).then(|| new_elif(node)),
        |node| node,
        |node| (*node).kids[2],
        (*node).kids[2],
    );

    let elifs = list.into_iter().collect::<Result<_>>()?;
    let node = list.node;

    let els = if node.is_null() {
        None
    } else {
        Some(new_else(node)?)
    };

    Ok(AstRepr::Expr(Expr::If(lamb_ast::If {
        cond: Box::new(cond),
        block: Box::new(block),
        elifs,
        els: els.map(Box::new),
    })))
}

/// # Safety
/// The `node` must be of the type `AstntIf` and have the following structure:
/// + `kids[0]` contains an expression
/// + `kids[1]` contains a block
/// + `kids[2]` *may* contain an `AstntIf` *OR* (else) `AstntBlock`
unsafe fn new_elif(node: *mut AstNode_T) -> Result<lamb_ast::Elif> {
    let cond = into_rust_repr((*node).kids[0])?.expect_expr()?;
    let block = into_rust_repr((*node).kids[1])?.expect_block()?;

    Ok(lamb_ast::Elif { cond, block })
}

/// # Safety
/// The `node` must be of the type `AstntBlock` and have the following structure:
/// + `kids[0]` contains an `AstntNodeList`, which ends in `null` *OR* an expression
unsafe fn new_else(node: *mut AstNode_T) -> Result<lamb_ast::Else> {
    let block = into_rust_repr(node)?.expect_block()?;
    Ok(lamb_ast::Else { block })
}

/// # Safety
/// The `node` must be of the type `AstntCase` and have the following structure:
/// + `kids[0]` contains an expression
/// + `kids[1]` *may* contain a valid `AstntCaseArm`
unsafe fn new_case(node: *mut AstNode_T) -> Result<AstRepr> {
    let value = Box::new(into_rust_repr((*node).kids[0])?.expect_expr()?);
    let arms = LinkedList::new(
        |node| Some(new_case_arm(node)),
        |node| node,
        |node| (*node).kids[2],
        (*node).kids[1],
    )
    .into_iter()
    .collect::<Result<_>>()?;

    Ok(AstRepr::Expr(Expr::Case(lamb_ast::Case { value, arms })))
}

/// # Safety
/// The `node` must be of the type `AstntCase` and have the following structure:
/// + `kids[0]` contains an `Astnt__Lit` *OR* `AstntId`
/// + `kids[1]` contains a valid expression or `AstntBlock`
/// + `kids[2]` *may* contain an `AstntCaseArm`
unsafe fn new_case_arm(node: *mut AstNode_T) -> Result<lamb_ast::CaseArm> {
    let pattern = match into_rust_repr((*node).kids[0])? {
        AstRepr::Expr(Expr::Atom(lamb_ast::Atom::Literal(l))) => lamb_ast::Either::Left(l),
        AstRepr::Expr(Expr::Atom(Ident(i))) => lamb_ast::Either::Right(i),
        _ => return Err(NodeError::malformed()),
    };

    let on_match = match into_rust_repr((*node).kids[1])? {
        AstRepr::Expr(Expr::Block(b)) => lamb_ast::Either::Left(b),
        AstRepr::Expr(e) => lamb_ast::Either::Right(e),
        AstRepr::Statement(_) => return Err(NodeError::malformed()),
    };

    Ok(lamb_ast::CaseArm { pattern, on_match })
}

/// # Safety
/// The `node` must be of the type `AstntArray` and have the following structure:
/// + `kids[0]` contains an `AstntNodeList` of expressions
unsafe fn new_array(node: *mut AstNode_T) -> Result<AstRepr> {
    Ok(AstRepr::Expr(Expr::Atom(lamb_ast::Atom::Array(expr_list(
        (*node).kids[0],
    )?))))
}

/// # Safety
/// The `node` must be of the type `AstntNodeList` and have the following structure:
/// + `kids[0]` contains an expressions
/// + `kids[1]` contains an `AstntNodeList` of expressions
unsafe fn expr_list(node: *mut AstNode_T) -> Result<Vec<Expr>> {
    LinkedList::new(
        |node| Some(into_rust_repr(node).and_then(AstRepr::expect_expr)),
        |node| (*node).kids[0],
        |node| (*node).kids[1],
        node,
    )
    .into_iter()
    .collect()
}

/// # Safety
/// The `node` must be of the type `AstntNodeList` and have the following structure:
/// + `kids[0]` contains an `AstntIdent`
/// + `kids[1]` contains an `AstntNodeList` of `AstntIdent`
unsafe fn ident_list(node: *mut AstNode_T) -> Result<Vec<lamb_ast::Ident>> {
    LinkedList::new(
        |node| match into_rust_repr(node) {
            Ok(AstRepr::Expr(Expr::Atom(Ident(i)))) => Some(Ok(i)),
            _ => Some(Err(NodeError::malformed())),
        },
        |node| (*node).kids[0],
        |node| (*node).kids[1],
        node,
    )
    .into_iter()
    .collect()
}

struct LinkedList<F, N, K> {
    f: F,
    kid: K,
    next: N,
    node: *mut AstNode_T,
}

impl<F, T, N, K> LinkedList<F, N, K>
where
    F: FnMut(*mut AstNode_T) -> Option<Result<T>>,
    K: FnMut(*mut AstNode_T) -> *mut AstNode_T,
    N: FnMut(*mut AstNode_T) -> *mut AstNode_T,
{
    fn new(f: F, kid: K, next: N, node: *mut AstNode_T) -> Self {
        Self { f, kid, next, node }
    }

    fn into_node(self) -> *mut AstNode_T {
        self.node
    }
}

impl<'a, F, T, N, K> IntoIterator for &'a mut LinkedList<F, N, K>
where
    F: FnMut(*mut AstNode_T) -> Option<Result<T>>,
    K: FnMut(*mut AstNode_T) -> *mut AstNode_T,
    N: FnMut(*mut AstNode_T) -> *mut AstNode_T,
{
    type Item = Result<T>;

    type IntoIter = Iter<'a, F, N, K>;

    fn into_iter(self) -> Self::IntoIter {
        Iter { list: self }
    }
}

struct Iter<'a, F, N, K> {
    list: &'a mut LinkedList<F, N, K>,
}

impl<'a, T, F, N, K> Iterator for Iter<'a, F, N, K>
where
    F: FnMut(*mut AstNode_T) -> Option<Result<T>>,
    K: FnMut(*mut AstNode_T) -> *mut AstNode_T,
    N: FnMut(*mut AstNode_T) -> *mut AstNode_T,
{
    type Item = Result<T>;

    fn next(&mut self) -> Option<Self::Item> {
        let LinkedList { f, kid, next, node } = self.list;

        if node.is_null() {
            None
        } else {
            let item = f(kid(*node))?;
            *node = next(*node);
            Some(item)
        }
    }
}
