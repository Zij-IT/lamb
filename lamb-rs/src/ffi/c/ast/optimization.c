#include "optimization.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define CONSTANT_NUM_BIN_FOLD(root, op)                                                            \
    do {                                                                                           \
        AstNode *lhs = root->kids[0];                                                              \
        AstNode *rhs = root->kids[1];                                                              \
                                                                                                   \
        optimize_ast(lhs);                                                                         \
        optimize_ast(rhs);                                                                         \
                                                                                                   \
        if (lhs->type == AstntNumLit && rhs->type == AstntNumLit) {                                \
            root->type = AstntNumLit;                                                              \
            root->val.n = lhs->val.n op rhs->val.n;                                                \
            root->kids[0] = NULL;                                                                  \
            root->kids[1] = NULL;                                                                  \
            free_ast(lhs);                                                                         \
            free_ast(rhs);                                                                         \
        }                                                                                          \
    } while (false)

#define CONSTANT_BOOL_BIN_FOLD(root, op)                                                           \
    do {                                                                                           \
        AstNode *lhs = root->kids[0];                                                              \
        AstNode *rhs = root->kids[1];                                                              \
                                                                                                   \
        optimize_ast(lhs);                                                                         \
        optimize_ast(rhs);                                                                         \
                                                                                                   \
        if (lhs->type == AstntBoolLit && rhs->type == AstntBoolLit) {                              \
            root->type = AstntBoolLit;                                                             \
            root->val.b = lhs->val.b op rhs->val.b;                                                \
            root->kids[0] = NULL;                                                                  \
            root->kids[1] = NULL;                                                                  \
            free_ast(lhs);                                                                         \
            free_ast(rhs);                                                                         \
        }                                                                                          \
    } while (false)

#define CONSTANT_RELATIVE_BIN_FOLD(root, op)                                                       \
    do {                                                                                           \
        AstNode *lhs = root->kids[0];                                                              \
        AstNode *rhs = root->kids[1];                                                              \
                                                                                                   \
        optimize_ast(lhs);                                                                         \
        optimize_ast(rhs);                                                                         \
                                                                                                   \
        if (lhs->type == rhs->type) {                                                              \
            switch (lhs->type) {                                                                   \
                case AstntStrLit:                                                                  \
                    root->val.b = strcmp(lhs->val.s, rhs->val.s) op 0;                             \
                    break;                                                                         \
                case AstntNumLit:                                                                  \
                    root->val.b = lhs->val.n op rhs->val.n;                                        \
                    break;                                                                         \
                case AstntCharLit:                                                                 \
                    root->val.b = lhs->val.c op rhs->val.c;                                        \
                    break;                                                                         \
                case AstntBoolLit:                                                                 \
                    root->val.b = lhs->val.b op rhs->val.b;                                        \
                    break;                                                                         \
                default:                                                                           \
                    return;                                                                        \
            }                                                                                      \
            root->type = AstntBoolLit;                                                             \
            root->kids[0] = NULL;                                                                  \
            root->kids[1] = NULL;                                                                  \
            free_ast(lhs);                                                                         \
            free_ast(rhs);                                                                         \
        }                                                                                          \
    } while (false)

static void constant_fold(AstNode *root) {
    switch (root->type) {
        case AstntUnaryNeg: {
            AstNode *rhs = root->kids[0];
            optimize_ast(rhs);
            if (rhs->type == AstntNumLit) {
                root->type = AstntNumLit;
                root->val.n = -rhs->val.n;
                root->kids[0] = NULL;
                free_ast(rhs);
            }
            break;
        }
        case AstntUnaryLogNot: {
            AstNode *rhs = root->kids[0];
            optimize_ast(rhs);
            if (rhs->type == AstntBoolLit) {
                root->type = AstntBoolLit;
                root->val.b = !rhs->val.b;
                root->kids[0] = NULL;
                free_ast(rhs);
            }
            break;
        }
        case AstntUnaryBitNot: {
            AstNode *rhs = root->kids[0];
            optimize_ast(rhs);
            if (rhs->type == AstntNumLit) {
                root->type = AstntNumLit;
                root->val.n = ~rhs->val.n;
                root->kids[0] = NULL;
                free_ast(rhs);
            }
            break;
        }
        case AstntBinaryAdd:
            CONSTANT_NUM_BIN_FOLD(root, +);
            break;
        case AstntBinarySub:
            CONSTANT_NUM_BIN_FOLD(root, -);
            break;
        case AstntBinaryMul:
            CONSTANT_NUM_BIN_FOLD(root, *);
            break;
        case AstntBinaryDiv:
            CONSTANT_NUM_BIN_FOLD(root, /);
            break;
        case AstntBinaryMod:
            CONSTANT_NUM_BIN_FOLD(root, %);
            break;
        case AstntBinaryOr:
            CONSTANT_NUM_BIN_FOLD(root, |);
            break;
        case AstntBinaryXor:
            CONSTANT_NUM_BIN_FOLD(root, ^);
            break;
        case AstntBinaryAnd:
            CONSTANT_NUM_BIN_FOLD(root, &);
            break;
        case AstntBinaryRShift:
            CONSTANT_NUM_BIN_FOLD(root, >>);
            break;
        case AstntBinaryLShift:
            CONSTANT_NUM_BIN_FOLD(root, <<);
            break;
        case AstntBinaryLogAnd:
            CONSTANT_BOOL_BIN_FOLD(root, &&);
            break;
        case AstntBinaryLogOr:
            CONSTANT_BOOL_BIN_FOLD(root, ||);
            break;
        case AstntBinaryEq:
            CONSTANT_RELATIVE_BIN_FOLD(root, ==);
            break;
        case AstntBinaryNe:
            CONSTANT_RELATIVE_BIN_FOLD(root, !=);
            break;
        case AstntBinaryGt:
            CONSTANT_RELATIVE_BIN_FOLD(root, >);
            break;
        case AstntBinaryLt:
            CONSTANT_RELATIVE_BIN_FOLD(root, <);
            break;
        case AstntBinaryGe:
            CONSTANT_RELATIVE_BIN_FOLD(root, >=);
            break;
        case AstntBinaryLe:
            CONSTANT_RELATIVE_BIN_FOLD(root, <=);
            break;
        default:
            break;
    }
}

// TODO: Add dead code elimination
void optimize_ast(AstNode *root) {
    if (root == NULL) {
        return;
    }

    switch (root->type) {
        case AstntUnaryNeg:
            constant_fold(root);
            break;
        case AstntUnaryLogNot:
            constant_fold(root);
            break;
        case AstntUnaryBitNot:
            constant_fold(root);
            break;
        case AstntBinaryAdd:
            constant_fold(root);
            break;
        case AstntBinarySub:
            constant_fold(root);
            break;
        case AstntBinaryMul:
            constant_fold(root);
            break;
        case AstntBinaryDiv:
            constant_fold(root);
            break;
        case AstntBinaryMod:
            constant_fold(root);
            break;
        case AstntBinaryLogAnd:
            constant_fold(root);
            break;
        case AstntBinaryLogOr:
            constant_fold(root);
            break;
        case AstntBinaryEq:
            constant_fold(root);
            break;
        case AstntBinaryNe:
            constant_fold(root);
            break;
        case AstntBinaryGt:
            constant_fold(root);
            break;
        case AstntBinaryGe:
            constant_fold(root);
            break;
        case AstntBinaryLt:
            constant_fold(root);
            break;
        case AstntBinaryLe:
            constant_fold(root);
            break;
        case AstntBinaryOr:
            constant_fold(root);
            break;
        case AstntBinaryXor:
            constant_fold(root);
            break;
        case AstntBinaryAnd:
            constant_fold(root);
            break;
        case AstntBinaryRShift:
            constant_fold(root);
            break;
        case AstntBinaryLShift:
            constant_fold(root);
            break;
        case AstntExprStmt:
            optimize_ast(root->kids[0]);
            break;
        case AstntAssignStmt:
            optimize_ast(root->kids[1]);
            break;
        case AstntBlock:
            optimize_ast(root->kids[0]);
            break;
        // TODO: Figure out what to do with closure binary operators -- {
        case AstntBinaryLCompose:
            optimize_ast(root->kids[0]);
            optimize_ast(root->kids[1]);
            break;
        case AstntBinaryRCompose:
            optimize_ast(root->kids[0]);
            optimize_ast(root->kids[1]);
            break;
        case AstntBinaryLApply:
            optimize_ast(root->kids[0]);
            optimize_ast(root->kids[1]);
            break;
        case AstntBinaryRApply:
            optimize_ast(root->kids[0]);
            optimize_ast(root->kids[1]);
            break;
        // -- }
        case AstntIf:
            optimize_ast(root->kids[0]);
            optimize_ast(root->kids[1]);
            optimize_ast(root->kids[2]);
            // TODO: Valgrind revealed an error in dead_code_if_elim which causes a
            // seg-fault as elements
            //			 of the if-elif-else chain aren't properly handled.
            // dead_code_if_elim(root);
            break;
        case AstntCase:
            optimize_ast(root->kids[0]);
            optimize_ast(root->kids[1]);
            break;
        case AstntCaseArm:
            optimize_ast(root->kids[0]);
            optimize_ast(root->kids[1]);
            optimize_ast(root->kids[2]);
            break;
        case AstntArray:
            optimize_ast(root->kids[0]);
            break;
        case AstntFuncDef:
            optimize_ast(root->kids[0]);
            optimize_ast(root->kids[1]);
            break;
        case AstntFuncCall:
            optimize_ast(root->kids[0]);
            optimize_ast(root->kids[1]);
            break;
        case AstntArrayIndex:
            optimize_ast(root->kids[0]);
            optimize_ast(root->kids[1]);
            break;
        case AstntReturn:
            optimize_ast(root->kids[0]);
            break;
        case AstntNodeList:
            optimize_ast(root->kids[0]);
            optimize_ast(root->kids[1]);
            break;
        // NOTE: No Optimizations for these cases -- {
        case AstntStrLit:
        case AstntNumLit:
        case AstntCharLit:
        case AstntBoolLit:
        case AstntNilLit:
        case AstntIdent:
            break;
            //  -- }
            // default:
            break;
    }
}

#undef CONSTANT_NUM_BIN_FOLD
#undef CONSTANT_BOOL_BIN_FOLD
#undef CONSTANT_RELATIVE_BIN_FOLD