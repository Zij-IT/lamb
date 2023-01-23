#ifndef COMPILING_AST_HEADER
#define COMPILING_AST_HEADER

#include "../ast/ast.h"
#include "vm.h"

typedef enum {
  CarOk,
  CarUnsupportedAst,  
} CompileAstResult;

CompileAstResult compile_ast(Vm* vm, AstNode* node);

#endif//COMPILING_AST_HEADER
