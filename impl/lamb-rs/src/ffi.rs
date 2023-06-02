#[allow(warnings, clippy::all)]
pub mod bindings;
mod repr;

pub use bindings::{free_ast, AstNode_T};
pub use repr::{AstRepr as Ast, NodeError};

use self::bindings::{
    AstNodeType, AstNodeType_AstntArray, AstNodeType_AstntArrayIndex, AstNodeType_AstntAssignStmt,
    AstNodeType_AstntBinaryAdd, AstNodeType_AstntBinaryAnd, AstNodeType_AstntBinaryDiv,
    AstNodeType_AstntBinaryEq, AstNodeType_AstntBinaryGe, AstNodeType_AstntBinaryGt,
    AstNodeType_AstntBinaryLApply, AstNodeType_AstntBinaryLCompose, AstNodeType_AstntBinaryLShift,
    AstNodeType_AstntBinaryLe, AstNodeType_AstntBinaryLogAnd, AstNodeType_AstntBinaryLogOr,
    AstNodeType_AstntBinaryLt, AstNodeType_AstntBinaryMod, AstNodeType_AstntBinaryMul,
    AstNodeType_AstntBinaryNe, AstNodeType_AstntBinaryOr, AstNodeType_AstntBinaryRApply,
    AstNodeType_AstntBinaryRCompose, AstNodeType_AstntBinaryRShift, AstNodeType_AstntBinarySub,
    AstNodeType_AstntBinaryXor, AstNodeType_AstntBlock, AstNodeType_AstntBoolLit,
    AstNodeType_AstntCase, AstNodeType_AstntCaseArm, AstNodeType_AstntCharLit,
    AstNodeType_AstntExprStmt, AstNodeType_AstntFuncCall, AstNodeType_AstntFuncDef,
    AstNodeType_AstntIdent, AstNodeType_AstntIf, AstNodeType_AstntNilLit,
    AstNodeType_AstntNodeList, AstNodeType_AstntNumLit, AstNodeType_AstntReturn,
    AstNodeType_AstntStrLit, AstNodeType_AstntUnaryBitNot, AstNodeType_AstntUnaryLogNot,
    AstNodeType_AstntUnaryNeg,
};

#[derive(Copy, Clone)]
pub enum NodeKind {
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
    pub fn as_u32(&self) -> u32 {
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

pub fn new_node(kind: NodeKind) -> *mut AstNode_T {
    let ptr = unsafe { bindings::new_astnode(kind.as_u32()) };
    if ptr.is_null() {
        panic!("Memory allocation failed. Rough day buddy.");
    }

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
