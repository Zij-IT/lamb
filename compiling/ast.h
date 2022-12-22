#ifndef COMPILING_AST_HEADER
#define COMPILING_AST_HEADER

#include "../ast/ast.h"
#include "chunk.h"

void compile_ast(Chunk* chunk, AstNode* node);

#endif//COMPILING_AST_HEADER
