#ifndef COMPILING_AST_HEADER
#define COMPILING_AST_HEADER

#include "../ast/ast.h"
#include "vm.h"

typedef enum {
  CarOk,
  CarUnsupportedAst,  
} CompileAstResult;

CompileAstResult compile_to_chunk(Vm* vm, Compiler* compiler, AstNode* node);

#endif//COMPILING_AST_HEADER
