use std::{ffi::CStr, str::Utf8Error};

use crate::wrap::bindings as ffi;

use super::{
    ast::{self, BinaryOp, Block, Expr, Literal, Statement, UnaryOp},
    AstNode_T,
};

pub enum AstRepr {
    Expr(Expr),
    Statement(Statement),
    StatementList(Vec<Statement>),
}

impl std::fmt::Debug for AstRepr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AstRepr::Expr(e) => write!(f, "{e:#?}"),
            AstRepr::Statement(s) => write!(f, "{s:#?}"),
            AstRepr::StatementList(l) => write!(f, "{l:#?}"),
        }
    }
}

impl AstRepr {
    pub unsafe fn from_c(node: *mut AstNode_T) -> Result<Self, NodeError> {
        if node.is_null() {
            return Err(NodeError::MalformedNode);
        }

        into_rust_repr(node)
    }

    fn expect_expr(self) -> Result<Expr, NodeError> {
        match self {
            Self::Expr(e) => Ok(e),
            Self::Statement(_) | Self::StatementList(_) => Err(NodeError::MalformedNode),
        }
    }

    fn expect_stmt(self) -> Result<Statement, NodeError> {
        match self {
            Self::Statement(s) => Ok(s),
            Self::Expr(_) | Self::StatementList(_) => Err(NodeError::MalformedNode),
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
    pub fn malformed() -> Self {
        let bt = std::backtrace::Backtrace::force_capture();
        println!("{bt}");

        Self::MalformedNode
    }
}

unsafe fn into_rust_repr(node: *mut AstNode_T) -> Result<AstRepr, NodeError> {
    if node.is_null() {
        return Err(NodeError::MalformedNode);
    }

    let type_ = unsafe { (*node).type_ };
    match type_ {
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
        ffi::AstNodeType_AstntStmts => new_stmt_list(node),
        ffi::AstNodeType_AstntNodeList => {
            unimplemented!(
                "NodeList shouldn't be parseable outside of a EXPR_LIST or FUNC_ARGS_LIST"
            )
        }
        ffi::AstNodeType_AstntCaseArm => {
            unimplemented!("CaseArms shouldn't be parseable outside of a Case")
        }
        _ => Err(NodeError::UnknownNodeType(type_)),
    }
}

unsafe fn new_stmt_list(mut node: *mut AstNode_T) -> Result<AstRepr, NodeError> {
    std::iter::from_fn(|| {
        if node.is_null() {
            None
        } else {
            let stmt = into_rust_repr((*node).kids[0]).and_then(AstRepr::expect_stmt);
            node = (*node).kids[1];
            Some(stmt)
        }
    })
    .collect::<Result<_, _>>()
    .map(AstRepr::StatementList)
}

unsafe fn new_block(node: *mut AstNode_T) -> Result<AstRepr, NodeError> {
    let mut block_stmts = (*node).kids[0];
    let mut stats = Vec::new();
    while !block_stmts.is_null() && (*block_stmts).type_ == ffi::AstNodeType_AstntStmts {
        let stat = into_rust_repr((*block_stmts).kids[0])?.expect_stmt()?;
        stats.push(stat);

        block_stmts = (*block_stmts).kids[1];
    }

    let value = if block_stmts.is_null() {
        None
    } else {
        Some(Box::new(into_rust_repr(block_stmts)?.expect_expr()?))
    };

    Ok(AstRepr::Expr(Expr::Block(Block { stats, value })))
}

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

unsafe fn new_expr_stmt(node: *mut AstNode_T) -> Result<AstRepr, NodeError> {
    let expr = into_rust_repr((*node).kids[0])?.expect_expr()?;
    Ok(AstRepr::Statement(Statement::Expr(expr)))
}

unsafe fn new_return(node: *mut AstNode_T) -> Result<AstRepr, NodeError> {
    let value = if node.is_null() {
        None
    } else {
        Some(into_rust_repr((*node).kids[0])?.expect_expr()?)
    };

    Ok(AstRepr::Statement(Statement::Return(value)))
}

unsafe fn new_index(node: *mut AstNode_T) -> Result<AstRepr, NodeError> {
    let indexee = into_rust_repr((*node).kids[0])?.expect_expr()?;
    let index = into_rust_repr((*node).kids[1])?.expect_expr()?;

    Ok(AstRepr::Expr(Expr::Index(ast::Index {
        index: Box::new(index),
        indexee: Box::new(indexee),
    })))
}

unsafe fn new_func_call(node: *mut AstNode_T) -> Result<AstRepr, NodeError> {
    let callee = into_rust_repr((*node).kids[0])?.expect_expr()?;
    let args = expr_list((*node).kids[1])?;

    Ok(AstRepr::Expr(Expr::FuncCall(ast::FuncCall {
        callee: Box::new(callee),
        args,
    })))
}

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

unsafe fn new_unary(node: *mut AstNode_T, op: UnaryOp) -> Result<AstRepr, NodeError> {
    Ok(AstRepr::Expr(Expr::Unary(ast::Unary::new(
        into_rust_repr((*node).kids[0])?.expect_expr()?,
        op,
    ))))
}

unsafe fn new_binary(node: *mut AstNode_T, op: BinaryOp) -> Result<AstRepr, NodeError> {
    let lhs = into_rust_repr((*node).kids[0])?.expect_expr()?;
    let rhs = into_rust_repr((*node).kids[1])?.expect_expr()?;

    Ok(AstRepr::Expr(Expr::Binary(ast::Binary::new(lhs, rhs, op))))
}

unsafe fn new_if(mut node: *mut AstNode_T) -> Result<AstRepr, NodeError> {
    let cond = into_rust_repr((*node).kids[0])?.expect_expr()?;
    let block = into_rust_repr((*node).kids[1])?
        .expect_expr()?
        .expect_block()?;

    let mut elifs = Vec::new();
    node = (*node).kids[2];

    while !node.is_null() && (*node).type_ == ffi::AstNodeType_AstntIf {
        elifs.push(new_elif(node)?);
        node = (*node).kids[2];
    }

    let els = if node.is_null() {
        None
    } else {
        Some(new_else(node)?)
    };

    Ok(AstRepr::Expr(Expr::If(ast::If::new(
        cond, block, elifs, els,
    ))))
}

unsafe fn new_elif(node: *mut AstNode_T) -> Result<ast::Elif, NodeError> {
    let cond = into_rust_repr((*node).kids[0])?.expect_expr()?;
    let block = into_rust_repr((*node).kids[1])?
        .expect_expr()?
        .expect_block()?;

    Ok(ast::Elif::new(cond, block))
}

unsafe fn new_else(node: *mut AstNode_T) -> Result<ast::Else, NodeError> {
    let block = into_rust_repr(node)?.expect_expr()?.expect_block()?;
    Ok(ast::Else::new(block))
}

unsafe fn new_case(node: *mut AstNode_T) -> Result<AstRepr, NodeError> {
    let expr = into_rust_repr((*node).kids[0])?.expect_expr()?;
    let mut case = ast::Case::new(expr);

    let mut arms = (*node).kids[1];
    while !arms.is_null() {
        case.add_arm(new_case_arm(arms)?);
        arms = (*arms).kids[2];
    }

    Ok(AstRepr::Expr(Expr::Case(case)))
}

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

unsafe fn new_array(node: *mut AstNode_T) -> Result<AstRepr, NodeError> {
    Ok(AstRepr::Expr(Expr::Atom(ast::Atom::Array(expr_list(
        (*node).kids[0],
    )?))))
}

unsafe fn expr_list(mut node: *mut AstNode_T) -> Result<Vec<Expr>, NodeError> {
    std::iter::from_fn(|| {
        if node.is_null() {
            None
        } else {
            let expr = into_rust_repr((*node).kids[0]).and_then(AstRepr::expect_expr);
            node = (*node).kids[1];
            Some(expr)
        }
    })
    .collect()
}

unsafe fn ident_list(mut node: *mut AstNode_T) -> Result<Vec<ast::Ident>, NodeError> {
    std::iter::from_fn(|| {
        if node.is_null() {
            None
        } else {
            let ident = match into_rust_repr((*node).kids[0]) {
                Ok(AstRepr::Expr(Expr::Atom(ast::Atom::Ident(i)))) => Ok(i),
                _ => return Some(Err(NodeError::malformed())),
            };

            node = (*node).kids[1];
            Some(ident)
        }
    })
    .collect()
}
