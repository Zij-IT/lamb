#include <stdio.h>
#include <string.h>

#include "ast.h"
#include "object.h"

void compile_ast(Vm* vm, AstNode* node) {
  switch (node->type) {
    case AstntStrLit: {
      LambString* st = (LambString*)alloc_obj(vm, OtString);
      st->chars = strdup(node->val.s);
      st->len = strlen(node->val.s);
      
      chunk_write_constant(vm->chunk, new_object((Object*)st));
      break;
    }
    case AstntNumLit: {
      chunk_write_constant(vm->chunk, new_int(node->val.n));
      break;
    }
    case AstntCharLit: {
      chunk_write_constant(vm->chunk, new_char(node->val.c));
      break;
    }
    case AstntBoolLit: {
      chunk_write_constant(vm->chunk, new_boolean(node->val.b));
      break;
    }
    case AstntIdent: {
      fprintf(stderr, "Unable to compile AstNode of kind: (%d)", node->type);
      break;
    }
    case AstntUnaryNeg: {
      compile_ast(vm, node->kids[0]);
      chunk_write(vm->chunk, OpNumNeg);
      break;
    }
    case AstntUnaryLogNot: {
      compile_ast(vm, node->kids[0]);
      chunk_write(vm->chunk, OpLogNeg);
      break;
    }
    case AstntUnaryBitNot: {
      compile_ast(vm, node->kids[0]);
      chunk_write(vm->chunk, OpBinNeg);
      break;
    }
    case AstntBinaryAdd: {
      compile_ast(vm, node->kids[0]);
      compile_ast(vm, node->kids[1]);
      chunk_write(vm->chunk, OpAdd);
      break;
    }
    case AstntBinarySub: {
      compile_ast(vm, node->kids[0]);
      compile_ast(vm, node->kids[1]);
      chunk_write(vm->chunk, OpSub);
      break;
    }
    case AstntBinaryMul: {
      compile_ast(vm, node->kids[0]);
      compile_ast(vm, node->kids[1]);
      chunk_write(vm->chunk, OpMul);
      break;
    }
    case AstntBinaryDiv: {
      compile_ast(vm, node->kids[0]);
      compile_ast(vm, node->kids[1]);
      chunk_write(vm->chunk, OpDiv);
      break;
    }
    case AstntBinaryMod: {
      compile_ast(vm, node->kids[0]);
      compile_ast(vm, node->kids[1]);
      chunk_write(vm->chunk, OpMod);
      break;
    }
    case AstntBinaryLCompose: {
      compile_ast(vm, node->kids[0]);
      compile_ast(vm, node->kids[1]);
      chunk_write(vm->chunk, OpLCompose);
      break;
      break;
    }
    case AstntBinaryRCompose: {
      compile_ast(vm, node->kids[0]);
      compile_ast(vm, node->kids[1]);
      chunk_write(vm->chunk, OpRCompose);
      break;
    }
    case AstntBinaryLApply: {
      compile_ast(vm, node->kids[0]);
      compile_ast(vm, node->kids[1]);
      chunk_write(vm->chunk, OpLApply);
      break;
    }
    case AstntBinaryRApply: {
      compile_ast(vm, node->kids[0]);
      compile_ast(vm, node->kids[1]);
      chunk_write(vm->chunk, OpRApply);
      break;
    }
    case AstntBinaryLogAnd: {
      compile_ast(vm, node->kids[0]);
      compile_ast(vm, node->kids[1]);
      chunk_write(vm->chunk, OpLogAnd);
      break;
    }
    case AstntBinaryLogOr: {
      compile_ast(vm, node->kids[0]);
      compile_ast(vm, node->kids[1]);
      chunk_write(vm->chunk, OpLogOr);
      break;
    }
    case AstntBinaryEq: {
      compile_ast(vm, node->kids[0]);
      compile_ast(vm, node->kids[1]);
      chunk_write(vm->chunk, OpEq);
      break;
    }
    case AstntBinaryNe: {
      compile_ast(vm, node->kids[0]);
      compile_ast(vm, node->kids[1]);
      chunk_write(vm->chunk, OpNe);
      break;
    }
    case AstntBinaryGt: {
      compile_ast(vm, node->kids[0]);
      compile_ast(vm, node->kids[1]);
      chunk_write(vm->chunk, OpGt);
      break;
    }
    case AstntBinaryGe: {
      compile_ast(vm, node->kids[0]);
      compile_ast(vm, node->kids[1]);
      chunk_write(vm->chunk, OpGe);
      break;
    }
    case AstntBinaryLt: {
      compile_ast(vm, node->kids[0]);
      compile_ast(vm, node->kids[1]);
      chunk_write(vm->chunk, OpLt);
      break;
    }
    case AstntBinaryLe: {
      compile_ast(vm, node->kids[0]);
      compile_ast(vm, node->kids[1]);
      chunk_write(vm->chunk, OpLe);
      break;
    }
    case AstntBinaryOr: {
      compile_ast(vm, node->kids[0]);
      compile_ast(vm, node->kids[1]);
      chunk_write(vm->chunk, OpBinOr);
      break;
    }
    case AstntBinaryXor: {
      compile_ast(vm, node->kids[0]);
      compile_ast(vm, node->kids[1]);
      chunk_write(vm->chunk, OpBinXor);
      break;
    }
    case AstntBinaryAnd: {
      compile_ast(vm, node->kids[0]);
      compile_ast(vm, node->kids[1]);
      chunk_write(vm->chunk, OpBinAnd);
      break;
    }
    case AstntBinaryRShift: {
      compile_ast(vm, node->kids[0]);
      compile_ast(vm, node->kids[1]);
      chunk_write(vm->chunk, OpRShift);
      break;
    }
    case AstntBinaryLShift: {
      compile_ast(vm, node->kids[0]);
      compile_ast(vm, node->kids[1]);
      chunk_write(vm->chunk, OpLShift);
      break;
    }
    case AstntIf: {
      fprintf(stderr, "Unable to compile AstNode of kind: (%d)", node->type);
      break;
    }
    case AstntElif: {
      fprintf(stderr, "Unable to compile AstNode of kind: (%d)", node->type);
      break;
    }
    case AstntElse: {
      fprintf(stderr, "Unable to compile AstNode of kind: (%d)", node->type);
      break;
    }
    case AstntCase: {
      fprintf(stderr, "Unable to compile AstNode of kind: (%d)", node->type);
      break;
    }
    case AstntCaseArm: {
      fprintf(stderr, "Unable to compile AstNode of kind: (%d)", node->type);
      break;
    }
    case AstntArray: {
      fprintf(stderr, "Unable to compile AstNode of kind: (%d)", node->type);
      break;
    }
    case AstntFuncDef: {
      fprintf(stderr, "Unable to compile AstNode of kind: (%d)", node->type);
      break;
    }
    case AstntFuncCall: {
      fprintf(stderr, "Unable to compile AstNode of kind: (%d)", node->type);
      break;
    }
    case AstntArrayIndex: {
      fprintf(stderr, "Unable to compile AstNode of kind: (%d)", node->type);
      break;
    }
    case AstntExprStmt: {
      compile_ast(vm, node->kids[0]);
      chunk_write(vm->chunk, OpPop);
      break;
    }
    case AstntAssignStmt: {
      fprintf(stderr, "Unable to compile AstNode of kind: (%d)", node->type);
      break;
    }
    case AstntStmts: {
      for(AstNode* stmt = node; stmt != NULL; stmt = stmt->kids[1]) {
        compile_ast(vm, stmt->kids[0]);        
      }
      break;
    }
    case AstntNodeList: {
      fprintf(stderr, "Unable to compile AstNode of kind: (%d)", node->type);
      break;
    }
    default:
      fprintf(stderr, "Unable to compile AstNode of kind: (%d)", node->type);
      fprintf(stderr, "Default branch reached while matching on node->type in %s on line %d", __FILE__, __LINE__);
      break;
  }
}
