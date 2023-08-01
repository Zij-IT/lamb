#include <cstdlib>

#include "ast.hpp"

AstNode *new_astnode(AstNodeType type) {
    auto *node = (AstNode *)calloc(1, sizeof(AstNode));
    node->type = type;

    return node;
}

AstNode *new_unary_astnode(AstNodeType type, AstNode *rhs) {
    AstNode *node = new_astnode(type);
    node->kids[0] = rhs;

    return node;
}

AstNode *new_binary_astnode(AstNodeType type, AstNode *lhs, AstNode *rhs) {
    AstNode *node = new_astnode(type);
    node->kids[0] = lhs;
    node->kids[1] = rhs;

    return node;
}

// NOLINTNEXTLINE(misc-no-recursion)
void free_ast(AstNode *root) {
    if (root == nullptr) {
        return;
    }

    switch (root->type) {
        case AstntBoolLit:
        case AstntCharLit:
        case AstntNilLit:
        case AstntNumLit:
            break;
        case AstntStrLit:
            free(root->val.s);
            break;
        case AstntIdent:
            free(root->val.i);
            break;
        case AstntArray:
        case AstntBlock:
        case AstntExprStmt:
        case AstntReturn:
        case AstntUnaryBitNot:
        case AstntUnaryLogNot:
        case AstntUnaryNeg:
            free_ast(root->kids[0]);
            break;
        case AstntArrayIndex:
        case AstntAssignStmt:
        case AstntBinaryAdd:
        case AstntBinaryAnd:
        case AstntBinaryDiv:
        case AstntBinaryEq:
        case AstntBinaryGe:
        case AstntBinaryGt:
        case AstntBinaryLApply:
        case AstntBinaryLCompose:
        case AstntBinaryLShift:
        case AstntBinaryLe:
        case AstntBinaryLogAnd:
        case AstntBinaryLogOr:
        case AstntBinaryLt:
        case AstntBinaryMod:
        case AstntBinaryMul:
        case AstntBinaryNe:
        case AstntBinaryOr:
        case AstntBinaryRApply:
        case AstntBinaryRCompose:
        case AstntBinaryRShift:
        case AstntBinarySub:
        case AstntBinaryXor:
        case AstntCase:
        case AstntFuncCall:
        case AstntNodeList:
            free_ast(root->kids[0]);
            free_ast(root->kids[1]);
            break;
        case AstntCaseArm:
        case AstntFuncDef:
        case AstntIf:
            free_ast(root->kids[0]);
            free_ast(root->kids[1]);
            free_ast(root->kids[2]);
            break;
    }

    free(root);
}
