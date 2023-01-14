#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "ast.h"
#include "misc.h"
#include "object.h"

#define LOCAL_NOT_FOUND -1

static i32 resolve_local(Block* block, LambString* name) {
  for (i32 i = block->local_count - 1; i >= 0; i--) {
    Local* local = &block->locals[i];
    if (local->name == name) {
      return i;
    }
  }
  
  return LOCAL_NOT_FOUND;
}

void compile_ast(Vm* vm, AstNode* node) {
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
      if (interned == NULL) {
        LambString* st = (LambString*)alloc_obj(vm, OtString);
        st->chars = strdup(node->val.i);
        st->hash = hash;
        st->len = len;

        table_insert(&vm->strings, st, new_boolean(false));
        interned = st;
      }
      
      i32 local_slot = resolve_local(&vm->curr_block, interned); 
      if (local_slot == LOCAL_NOT_FOUND) {
        chunk_write_constant(vm->chunk, new_object((Object*)interned));
        chunk_write(vm->chunk, OpGetGlobal);
      } else {
        chunk_write_constant(vm->chunk, new_int(local_slot));
        chunk_write(vm->chunk, OpGetLocal);
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
      AstNode* ident_node = node->kids[0];
      AstNode* value_node = node->kids[1];
      
      // Note: These values are swapped, as it makes dealing with garbage collection
      //       easier. See OpDefineGlobal or OpDefineLocal for more details. 
      compile_ast(vm, value_node);

      // This next bit is basically what happens with a string node, but this node is an ident
      u64 len = strlen(ident_node->val.i);
      u32 hash = hash_string(ident_node->val.i);
      LambString* interned = table_find_string(&vm->strings, ident_node->val.i, len, hash);
      if (interned == NULL) {
        LambString* st = (LambString*)alloc_obj(vm, OtString);
        st->chars = strdup(ident_node->val.i);
        st->hash = hash;
        st->len = len;

        table_insert(&vm->strings, st, new_boolean(false));
        interned = st;
      }
      
      if(vm->curr_block.scope_depth == 0) {
        chunk_write_constant(vm->chunk, new_object((Object*)interned));
        chunk_write(vm->chunk, OpDefineGlobal);
      } else {
        if (vm->curr_block.local_count == MAX_LOCAL_COUNT) {
          // TODO: Implement actual error handling so that this explodes cleanly
          fprintf(stderr, "COMPILE_ERR: Too many local variables defined... Exiting.");
          exit(1);
        }

        Local* local = &vm->curr_block.locals[vm->curr_block.local_count++];
        local->name = interned;
        local->depth = vm->curr_block.scope_depth;
        
        chunk_write_constant(vm->chunk, new_int(vm->curr_block.local_count - 1));
        chunk_write(vm->chunk, OpDefineLocal);
      }

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

