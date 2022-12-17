#ifndef AST_HEADER
#define AST_HEADER

#include "../types.h"

#define MAX_AST_KID_COUNT 4

typedef enum {
  // Literals
  AstntStrLit,
  AstntNumLit,
  AstntCharLit,
  AstntBoolLit,
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
  AstntElif,
  AstntElse,
  AstntCase,
  AstntCaseArm,
  AstntArray,
  AstntFuncDef,
  AstntFuncCall,
  AstntArrayIndex,
    
  // Statement Kinds
  AstntExprStmt,
  AstntAssignStmt,
  AstntStmts,
  
  // Meta
  AstntNodeList,
  
} AstNodeType;

typedef struct AstNode_T {
  struct AstNode_T* kids[MAX_AST_KID_COUNT];
  union {
    string s;
    string i;
    char c;
    bool b;
    i64  n;
  } val;
  AstNodeType type;
} AstNode;

AstNode* new_astnode(AstNodeType type);

AstNode* new_unary_astnode(AstNodeType type, AstNode* rhs);

AstNode* new_binary_astnode(AstNodeType type, AstNode* lhs, AstNode* rhs);

void print_ast(AstNode* root, u16 depth);

void free_ast(AstNode* root);

i64 eval_ast(AstNode* root);

#endif//AST_HEADER
