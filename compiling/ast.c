#include "ast.h"
#include <stdio.h>

void compile_ast(Chunk* chunk, AstNode* node) {
  switch (node->type) {
    case AstntStrLit: {
      chunk_write_constant(chunk, new_string(node->val.s));
      break;
    }
    case AstntNumLit: {
      chunk_write_constant(chunk, new_int(node->val.n));
      break;
    }
    case AstntCharLit: {
      chunk_write_constant(chunk, new_char(node->val.c));
      break;
    }
    case AstntBoolLit: {
      chunk_write_constant(chunk, new_boolean(node->val.b));
      break;
    }
    case AstntIdent: {
      fprintf(stderr, "Unable to compile AstNode of kind: (%d)", node->type);
      break;
    }
    case AstntUnaryNeg: {
      compile_ast(chunk, node->kids[0]);
      chunk_write(chunk, OpNumNeg);
      break;
    }
    case AstntUnaryLogNot: {
      compile_ast(chunk, node->kids[0]);
      chunk_write(chunk, OpLogNeg);
      break;
    }
    case AstntUnaryBitNot: {
      compile_ast(chunk, node->kids[0]);
      chunk_write(chunk, OpBinNeg);
      break;
    }
    case AstntBinaryAdd: {
      compile_ast(chunk, node->kids[0]);
      compile_ast(chunk, node->kids[1]);
      chunk_write(chunk, OpAdd);
      break;
    }
    case AstntBinarySub: {
      compile_ast(chunk, node->kids[0]);
      compile_ast(chunk, node->kids[1]);
      chunk_write(chunk, OpSub);
      break;
    }
    case AstntBinaryMul: {
      compile_ast(chunk, node->kids[0]);
      compile_ast(chunk, node->kids[1]);
      chunk_write(chunk, OpMul);
      break;
    }
    case AstntBinaryDiv: {
      compile_ast(chunk, node->kids[0]);
      compile_ast(chunk, node->kids[1]);
      chunk_write(chunk, OpDiv);
      break;
    }
    case AstntBinaryMod: {
      compile_ast(chunk, node->kids[0]);
      compile_ast(chunk, node->kids[1]);
      chunk_write(chunk, OpMod);
      break;
    }
    case AstntBinaryLCompose: {
      compile_ast(chunk, node->kids[0]);
      compile_ast(chunk, node->kids[1]);
      chunk_write(chunk, OpLCompose);
      break;
      break;
    }
    case AstntBinaryRCompose: {
      compile_ast(chunk, node->kids[0]);
      compile_ast(chunk, node->kids[1]);
      chunk_write(chunk, OpRCompose);
      break;
    }
    case AstntBinaryLApply: {
      compile_ast(chunk, node->kids[0]);
      compile_ast(chunk, node->kids[1]);
      chunk_write(chunk, OpLApply);
      break;
    }
    case AstntBinaryRApply: {
      compile_ast(chunk, node->kids[0]);
      compile_ast(chunk, node->kids[1]);
      chunk_write(chunk, OpRApply);
      break;
    }
    case AstntBinaryLogAnd: {
      compile_ast(chunk, node->kids[0]);
      compile_ast(chunk, node->kids[1]);
      chunk_write(chunk, OpLogAnd);
      break;
    }
    case AstntBinaryLogOr: {
      compile_ast(chunk, node->kids[0]);
      compile_ast(chunk, node->kids[1]);
      chunk_write(chunk, OpLogOr);
      break;
    }
    case AstntBinaryEq: {
      compile_ast(chunk, node->kids[0]);
      compile_ast(chunk, node->kids[1]);
      chunk_write(chunk, OpEq);
      break;
    }
    case AstntBinaryNe: {
      compile_ast(chunk, node->kids[0]);
      compile_ast(chunk, node->kids[1]);
      chunk_write(chunk, OpNe);
      break;
    }
    case AstntBinaryGt: {
      compile_ast(chunk, node->kids[0]);
      compile_ast(chunk, node->kids[1]);
      chunk_write(chunk, OpGt);
      break;
    }
    case AstntBinaryGe: {
      compile_ast(chunk, node->kids[0]);
      compile_ast(chunk, node->kids[1]);
      chunk_write(chunk, OpGe);
      break;
    }
    case AstntBinaryLt: {
      compile_ast(chunk, node->kids[0]);
      compile_ast(chunk, node->kids[1]);
      chunk_write(chunk, OpLt);
      break;
    }
    case AstntBinaryLe: {
      compile_ast(chunk, node->kids[0]);
      compile_ast(chunk, node->kids[1]);
      chunk_write(chunk, OpLe);
      break;
    }
    case AstntBinaryOr: {
      compile_ast(chunk, node->kids[0]);
      compile_ast(chunk, node->kids[1]);
      chunk_write(chunk, OpBinOr);
      break;
    }
    case AstntBinaryXor: {
      compile_ast(chunk, node->kids[0]);
      compile_ast(chunk, node->kids[1]);
      chunk_write(chunk, OpBinXor);
      break;
    }
    case AstntBinaryAnd: {
      compile_ast(chunk, node->kids[0]);
      compile_ast(chunk, node->kids[1]);
      chunk_write(chunk, OpBinAnd);
      break;
    }
    case AstntBinaryRShift: {
      compile_ast(chunk, node->kids[0]);
      compile_ast(chunk, node->kids[1]);
      chunk_write(chunk, OpRShift);
      break;
    }
    case AstntBinaryLShift: {
      compile_ast(chunk, node->kids[0]);
      compile_ast(chunk, node->kids[1]);
      chunk_write(chunk, OpLShift);
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
      compile_ast(chunk, node->kids[0]);
      chunk_write(chunk, OpPop);
      break;
    }
    case AstntAssignStmt: {
      fprintf(stderr, "Unable to compile AstNode of kind: (%d)", node->type);
      break;
    }
    case AstntStmts: {
      for(AstNode* stmt = node; stmt != NULL; stmt = stmt->kids[1]) {
        compile_ast(chunk, stmt->kids[0]);        
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
