#ifndef COMPILING_AST_HEADER
#define COMPILING_AST_HEADER

#include "compiler.hpp"
#include "../ast/ast.hpp"
#include "../vm/vm.hpp"

enum CompileAstResult {
    CarOk,
    CarUnsupportedAst,
};

CompileAstResult compile(Vm& vm, Compiler *compiler, AstNode *node);

#endif // COMPILING_AST_HEADER
