#include <stdio.h>
#include <string.h>

#include "ast.h"
#include "misc.h"
#include "object.h"

static void compile_ast_helper(Vm* vm, AstNode* node, i32 scope_depth) {
  switch (node->type) {
    case AstntStrLit: {
      // Check if already interned and if not intern it
      u64 len = strlen(node->val.s);
      u32 hash = hash_string(node->val.s);
      LambString* interned = table_find_string(&vm->strings, node->val.s, len, hash);
      if (interned != NULL) {
        // If the string is interned, write it as a constant and return. No need to make own string
        chunk_write_constant(vm->chunk, new_object((Object*)interned));
        return;
      }
      
      LambString* st = (LambString*)alloc_obj(vm, OtString);
      st->chars = strdup(node->val.s);
      st->hash = hash;
      st->len = len;

      table_insert(&vm->strings, st, new_boolean(false));
      chunk_write_constant(vm->chunk, new_object((Object*)st));
      break;
    }
    case AstntIdent: {
      // AstntIdent's are treatead as strings for the purposes of compilation. This does mean that
      // they show up in the 'strings' table of the VM, but that's not of consequence.

      u64 len = strlen(node->val.i);
      u32 hash = hash_string(node->val.i);
      LambString* interned = table_find_string(&vm->strings, node->val.i, len, hash);
      if (interned != NULL) {
        // If the string is interned, write it as a constant and return. No need to make own string
        chunk_write_constant(vm->chunk, new_object((Object*)interned));
      } else {
        LambString* st = (LambString*)alloc_obj(vm, OtString);
        st->chars = strdup(node->val.i);
        st->hash = hash;
        st->len = len;

        table_insert(&vm->strings, st, new_boolean(false));
        chunk_write_constant(vm->chunk, new_object((Object*)st));       
      }

      if (scope_depth == 0) {
        chunk_write(vm->chunk, OpGetGlobal);
      } else {
        printf("Reminder: GetLocal not yet implemented. Substituting with GetGlobal");
        chunk_write(vm->chunk, OpGetGlobal);
      }

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
    case AstntUnaryNeg: {
      compile_ast_helper(vm, node->kids[0], scope_depth);
      chunk_write(vm->chunk, OpNumNeg);
      break;
    }
    case AstntUnaryLogNot: {
      compile_ast_helper(vm, node->kids[0], scope_depth);
      chunk_write(vm->chunk, OpLogNeg);
      break;
    }
    case AstntUnaryBitNot: {
      compile_ast_helper(vm, node->kids[0], scope_depth);
      chunk_write(vm->chunk, OpBinNeg);
      break;
    }
    case AstntBinaryAdd: {
      compile_ast_helper(vm, node->kids[0], scope_depth);
      compile_ast_helper(vm, node->kids[1], scope_depth);
      chunk_write(vm->chunk, OpAdd);
      break;
    }
    case AstntBinarySub: {
      compile_ast_helper(vm, node->kids[0], scope_depth);
      compile_ast_helper(vm, node->kids[1], scope_depth);
      chunk_write(vm->chunk, OpSub);
      break;
    }
    case AstntBinaryMul: {
      compile_ast_helper(vm, node->kids[0], scope_depth);
      compile_ast_helper(vm, node->kids[1], scope_depth);
      chunk_write(vm->chunk, OpMul);
      break;
    }
    case AstntBinaryDiv: {
      compile_ast_helper(vm, node->kids[0], scope_depth);
      compile_ast_helper(vm, node->kids[1], scope_depth);
      chunk_write(vm->chunk, OpDiv);
      break;
    }
    case AstntBinaryMod: {
      compile_ast_helper(vm, node->kids[0], scope_depth);
      compile_ast_helper(vm, node->kids[1], scope_depth);
      chunk_write(vm->chunk, OpMod);
      break;
    }
    case AstntBinaryLCompose: {
      compile_ast_helper(vm, node->kids[0], scope_depth);
      compile_ast_helper(vm, node->kids[1], scope_depth);
      chunk_write(vm->chunk, OpLCompose);
      break;
      break;
    }
    case AstntBinaryRCompose: {
      compile_ast_helper(vm, node->kids[0], scope_depth);
      compile_ast_helper(vm, node->kids[1], scope_depth);
      chunk_write(vm->chunk, OpRCompose);
      break;
    }
    case AstntBinaryLApply: {
      compile_ast_helper(vm, node->kids[0], scope_depth);
      compile_ast_helper(vm, node->kids[1], scope_depth);
      chunk_write(vm->chunk, OpLApply);
      break;
    }
    case AstntBinaryRApply: {
      compile_ast_helper(vm, node->kids[0], scope_depth);
      compile_ast_helper(vm, node->kids[1], scope_depth);
      chunk_write(vm->chunk, OpRApply);
      break;
    }
    case AstntBinaryLogAnd: {
      compile_ast_helper(vm, node->kids[0], scope_depth);
      compile_ast_helper(vm, node->kids[1], scope_depth);
      chunk_write(vm->chunk, OpLogAnd);
      break;
    }
    case AstntBinaryLogOr: {
      compile_ast_helper(vm, node->kids[0], scope_depth);
      compile_ast_helper(vm, node->kids[1], scope_depth);
      chunk_write(vm->chunk, OpLogOr);
      break;
    }
    case AstntBinaryEq: {
      compile_ast_helper(vm, node->kids[0], scope_depth);
      compile_ast_helper(vm, node->kids[1], scope_depth);
      chunk_write(vm->chunk, OpEq);
      break;
    }
    case AstntBinaryNe: {
      compile_ast_helper(vm, node->kids[0], scope_depth);
      compile_ast_helper(vm, node->kids[1], scope_depth);
      chunk_write(vm->chunk, OpNe);
      break;
    }
    case AstntBinaryGt: {
      compile_ast_helper(vm, node->kids[0], scope_depth);
      compile_ast_helper(vm, node->kids[1], scope_depth);
      chunk_write(vm->chunk, OpGt);
      break;
    }
    case AstntBinaryGe: {
      compile_ast_helper(vm, node->kids[0], scope_depth);
      compile_ast_helper(vm, node->kids[1], scope_depth);
      chunk_write(vm->chunk, OpGe);
      break;
    }
    case AstntBinaryLt: {
      compile_ast_helper(vm, node->kids[0], scope_depth);
      compile_ast_helper(vm, node->kids[1], scope_depth);
      chunk_write(vm->chunk, OpLt);
      break;
    }
    case AstntBinaryLe: {
      compile_ast_helper(vm, node->kids[0], scope_depth);
      compile_ast_helper(vm, node->kids[1], scope_depth);
      chunk_write(vm->chunk, OpLe);
      break;
    }
    case AstntBinaryOr: {
      compile_ast_helper(vm, node->kids[0], scope_depth);
      compile_ast_helper(vm, node->kids[1], scope_depth);
      chunk_write(vm->chunk, OpBinOr);
      break;
    }
    case AstntBinaryXor: {
      compile_ast_helper(vm, node->kids[0], scope_depth);
      compile_ast_helper(vm, node->kids[1], scope_depth);
      chunk_write(vm->chunk, OpBinXor);
      break;
    }
    case AstntBinaryAnd: {
      compile_ast_helper(vm, node->kids[0], scope_depth);
      compile_ast_helper(vm, node->kids[1], scope_depth);
      chunk_write(vm->chunk, OpBinAnd);
      break;
    }
    case AstntBinaryRShift: {
      compile_ast_helper(vm, node->kids[0], scope_depth);
      compile_ast_helper(vm, node->kids[1], scope_depth);
      chunk_write(vm->chunk, OpRShift);
      break;
    }
    case AstntBinaryLShift: {
      compile_ast_helper(vm, node->kids[0], scope_depth);
      compile_ast_helper(vm, node->kids[1], scope_depth);
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
      compile_ast_helper(vm, node->kids[0], scope_depth);
      chunk_write(vm->chunk, OpPop);
      break;
    }
    case AstntAssignStmt: {
      AstNode* ident_node = node->kids[0];
      AstNode* value_node = node->kids[1];

      // Note: These values are swapped, as it makes dealing with garbage collection
      //       easier. See OpDefineGlobal or OpDefineLocal for more details. 
      compile_ast_helper(vm, value_node, scope_depth);

      // This next bit is basically what happens with a string node, but this node is an ident
      u64 len = strlen(ident_node->val.i);
      u32 hash = hash_string(ident_node->val.i);
      LambString* interned = table_find_string(&vm->strings, ident_node->val.i, len, hash);
      if (interned != NULL) {
        // If the string is interned, write it as a constant and return. No need to make own string
        chunk_write_constant(vm->chunk, new_object((Object*)interned));
      } else {
        LambString* st = (LambString*)alloc_obj(vm, OtString);
        st->chars = strdup(ident_node->val.i);
        st->hash = hash;
        st->len = len;

        table_insert(&vm->strings, st, new_boolean(false));
        chunk_write_constant(vm->chunk, new_object((Object*)st));       
      }
    

      if (scope_depth == 0) {
        // Define global
        chunk_write(vm->chunk, OpDefineGlobal);
      } else {
        // Define local
        // Yes, this is wrong, but purposefully so. For now it just means that there is no such
        // thing as local variables, and everything is global :D
        chunk_write(vm->chunk, OpDefineGlobal);
        fprintf(stderr, "Unable to compile AstNode of kind: (%d)", node->type);
      }
      
      break;
    }
    case AstntStmts: {
      for(AstNode* stmt = node; stmt != NULL; stmt = stmt->kids[1]) {
        compile_ast_helper(vm, stmt->kids[0], scope_depth);
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

void compile_ast(Vm* vm, AstNode* node) {
  compile_ast_helper(vm, node, 0);
}

