#include <stdlib.h>
#include <string.h>

#include "ast.hpp"

AstNode *new_astnode(AstNodeType type) {
    AstNode *node = (AstNode*)calloc(1, sizeof(AstNode));
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
void free_ast(AstNode *root) {
    if (root == NULL) {
        return;
    }

    switch (root->type) {
        case AstntStrLit:
            free(root->val.s);
            break;
        case AstntIdent:
            free(root->val.i);
            break;
        case AstntUnaryNeg:
        case AstntUnaryLogNot:
        case AstntUnaryBitNot:
            free_ast(root->kids[0]);
            break;
        case AstntBinaryAdd:
        case AstntBinarySub:
        case AstntBinaryMul:
        case AstntBinaryDiv:
        case AstntBinaryMod:
        case AstntBinaryLCompose:
        case AstntBinaryRCompose:
        case AstntBinaryLApply:
        case AstntBinaryRApply:
        case AstntBinaryLogAnd:
        case AstntBinaryLogOr:
        case AstntBinaryEq:
        case AstntBinaryNe:
        case AstntBinaryGt:
        case AstntBinaryGe:
        case AstntBinaryLt:
        case AstntBinaryLe:
        case AstntBinaryOr:
        case AstntBinaryXor:
        case AstntBinaryAnd:
        case AstntBinaryRShift:
        case AstntBinaryLShift:
            free_ast(root->kids[0]);
            free_ast(root->kids[1]);
            break;
        case AstntIf:
            free_ast(root->kids[0]);
            free_ast(root->kids[1]);
            free_ast(root->kids[2]);
            break;
        case AstntCase:
            free_ast(root->kids[0]);
            free_ast(root->kids[1]);
            break;
        case AstntCaseArm:
            free_ast(root->kids[0]);
            free_ast(root->kids[1]);
            free_ast(root->kids[2]);
            break;
        case AstntArray:
            free_ast(root->kids[0]);
            break;
        case AstntFuncDef:
            free_ast(root->kids[0]);
            free_ast(root->kids[1]);
            free_ast(root->kids[2]);
            break;
        case AstntFuncCall:
            free_ast(root->kids[0]);
            free_ast(root->kids[1]);
            break;
        case AstntArrayIndex:
            free_ast(root->kids[0]);
            free_ast(root->kids[1]);
            break;
        case AstntReturn:
            free_ast(root->kids[0]);
            break;
        case AstntExprStmt:
            free_ast(root->kids[0]);
            break;
        case AstntAssignStmt:
            free_ast(root->kids[0]);
            free_ast(root->kids[1]);
            break;
        case AstntNodeList:
            free_ast(root->kids[0]);
            free_ast(root->kids[1]);
            break;
        case AstntBlock:
            free_ast(root->kids[0]);
            break;
        case AstntNumLit:
        case AstntCharLit:
        case AstntBoolLit:
        case AstntNilLit:
            break;
    }

    free(root);
}
