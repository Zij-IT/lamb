#ifndef AST_HEADER
#define AST_HEADER

#include "../types.hpp"

#define MAX_AST_KID_COUNT 3

enum AstNodeType {
    // Literals
    AstntStrLit,
    AstntNumLit,
    AstntCharLit,
    AstntBoolLit,
    AstntNilLit,
    AstntIdent,

    // Unary Ops
    AstntUnaryNeg,
    AstntUnaryLogNot,
    AstntUnaryBitNot,

    // Binary Ops
    AstntBinaryAdd,
    AstntBinarySub,
    AstntBinaryMul,
    AstntBinaryDiv,
    AstntBinaryMod,
    AstntBinaryLCompose,
    AstntBinaryRCompose,
    AstntBinaryLApply,
    AstntBinaryRApply,
    AstntBinaryLogAnd,
    AstntBinaryLogOr,
    AstntBinaryEq,
    AstntBinaryNe,
    AstntBinaryGt,
    AstntBinaryGe,
    AstntBinaryLt,
    AstntBinaryLe,
    AstntBinaryOr,
    AstntBinaryXor,
    AstntBinaryAnd,
    AstntBinaryRShift,
    AstntBinaryLShift,

    // ExpressionKinds
    AstntIf,
    AstntCase,
    AstntCaseArm,
    AstntArray,
    AstntFuncDef,
    AstntFuncCall,
    AstntArrayIndex,
    AstntReturn,

    // Statement Kinds
    AstntExprStmt,
    AstntAssignStmt,
    AstntBlock,

    // Pattern Nodes
    AstntPattern,
    AstntPatternTopLit,
    AstntPatternTopIdent,
    AstntPatternTopArray,
    AstntPatternArrayExt,

    // Meta
    AstntNodeList,
    AstntOptional,

};

struct AstNode {
    struct AstNode *kids[MAX_AST_KID_COUNT];
    union {
        string s;
        string i;
        char c;
        bool b;
        i64 n;
    } val;
    AstNodeType type;
};

AstNode *new_astnode(AstNodeType type);

AstNode *new_unary_astnode(AstNodeType type, AstNode *rhs);

AstNode *new_binary_astnode(AstNodeType type, AstNode *lhs, AstNode *rhs);

void free_ast(AstNode *root);

i64 eval_ast(AstNode *root);

#endif // AST_HEADER
