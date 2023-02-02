#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "ast.h"
#include "misc.h"
#include "object.h"

#define LOCAL_NOT_FOUND -1

static i32 resolve_local(Compiler* compiler, LambString* name) {
  for (i32 i = compiler->locals.len - 1; i >= 0; i--) {
    Local* local = &compiler->locals.values[i];
    if (local->name == name) {
      return i;
    }
  }
  
  return LOCAL_NOT_FOUND;
}

#define BUBBLE(x) \
    if ((x) == CarUnsupportedAst) { \
      return CarUnsupportedAst;     \
    }

CompileAstResult compile_to_chunk(Vm* vm, Chunk* chunk, AstNode* node) {
   switch (node->type) {
    case AstntStrLit: {
      // Check if already interned and if not intern it
      u64 len = strlen(node->val.s);
      u32 hash = hash_string(node->val.s);
      LambString* interned = table_find_string(&vm->strings, node->val.s, len, hash);
      if (interned != NULL) {
        // If the string is interned, write it as a constant and return. No need to make own string
        chunk_write_constant(chunk, new_object((Object*)interned));
        return CarOk;
      }
      
      LambString* st = (LambString*)alloc_obj(vm, OtString);
      st->chars = strdup(node->val.s);
      st->hash = hash;
      st->len = len;

      table_insert(&vm->strings, st, new_boolean(false));
      chunk_write_constant(chunk, new_object((Object*)st));
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
      
      i32 local_slot = resolve_local(vm->curr_compiler, interned); 
      if (local_slot == LOCAL_NOT_FOUND) {
        chunk_write_constant(chunk, new_object((Object*)interned));
        chunk_write(chunk, OpGetGlobal);
      } else {
        chunk_write_constant(chunk, new_int(local_slot));
        chunk_write(chunk, OpGetLocal);
      }
      
      break;
    }
    case AstntNilLit: {
      chunk_write_constant(chunk, new_nil());
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
    case AstntUnaryNeg: {
      BUBBLE(compile_to_chunk(vm, chunk, node->kids[0]));
      chunk_write(chunk, OpNumNeg);
      break;
    }
    case AstntUnaryLogNot: {
      BUBBLE(compile_to_chunk(vm, chunk, node->kids[0]));
      chunk_write(chunk, OpLogNeg);
      break;
    }
    case AstntUnaryBitNot: {
      BUBBLE(compile_to_chunk(vm, chunk, node->kids[0]));
      chunk_write(chunk, OpBinNeg);
      break;
    }
    case AstntBinaryAdd: {
      BUBBLE(compile_to_chunk(vm, chunk, node->kids[0]));
      BUBBLE(compile_to_chunk(vm, chunk, node->kids[1]));
      chunk_write(chunk, OpAdd);
      break;
    }
    case AstntBinarySub: {
      BUBBLE(compile_to_chunk(vm, chunk, node->kids[0]));
      BUBBLE(compile_to_chunk(vm, chunk, node->kids[1]));
      chunk_write(chunk, OpSub);
      break;
    }
    case AstntBinaryMul: {
      BUBBLE(compile_to_chunk(vm, chunk, node->kids[0]));
      BUBBLE(compile_to_chunk(vm, chunk, node->kids[1]));
      chunk_write(chunk, OpMul);
      break;
    }
    case AstntBinaryDiv: {
      BUBBLE(compile_to_chunk(vm, chunk, node->kids[0]));
      BUBBLE(compile_to_chunk(vm, chunk, node->kids[1]));
      chunk_write(chunk, OpDiv);
      break;
    }
    case AstntBinaryMod: {
      BUBBLE(compile_to_chunk(vm, chunk, node->kids[0]));
      BUBBLE(compile_to_chunk(vm, chunk, node->kids[1]));
      chunk_write(chunk, OpMod);
      break;
    }
    case AstntBinaryLogAnd: {
      BUBBLE(compile_to_chunk(vm, chunk, node->kids[0]));
      i32 if_false = chunk_write_jump(chunk, OpJumpIfFalse);
      chunk_write(chunk, OpPop);
      BUBBLE(compile_to_chunk(vm, chunk, node->kids[1]));
      chunk_patch_jump(chunk, if_false);
      break;
    }
    case AstntBinaryLogOr: {
      BUBBLE(compile_to_chunk(vm, chunk, node->kids[0]));
      i32 if_false = chunk_write_jump(chunk, OpJumpIfFalse);
      i32 skip_right = chunk_write_jump(chunk, OpJump);
      chunk_patch_jump(chunk, if_false);
      chunk_write(chunk, OpPop);
      BUBBLE(compile_to_chunk(vm, chunk, node->kids[1]));
      chunk_patch_jump(chunk, skip_right);
      break;
    }    case AstntBinaryEq: {
      BUBBLE(compile_to_chunk(vm, chunk, node->kids[0]));
      BUBBLE(compile_to_chunk(vm, chunk, node->kids[1]));
      chunk_write(chunk, OpEq);
      break;
    }
    case AstntBinaryNe: {
      BUBBLE(compile_to_chunk(vm, chunk, node->kids[0]));
      BUBBLE(compile_to_chunk(vm, chunk, node->kids[1]));
      chunk_write(chunk, OpNe);
      break;
    }
    case AstntBinaryGt: {
      BUBBLE(compile_to_chunk(vm, chunk, node->kids[0]));
      BUBBLE(compile_to_chunk(vm, chunk, node->kids[1]));
      chunk_write(chunk, OpGt);
      break;
    }
    case AstntBinaryGe: {
      BUBBLE(compile_to_chunk(vm, chunk, node->kids[0]));
      BUBBLE(compile_to_chunk(vm, chunk, node->kids[1]));
      chunk_write(chunk, OpGe);
      break;
    }
    case AstntBinaryLt: {
      BUBBLE(compile_to_chunk(vm, chunk, node->kids[0]));
      BUBBLE(compile_to_chunk(vm, chunk, node->kids[1]));
      chunk_write(chunk, OpLt);
      break;
    }
    case AstntBinaryLe: {
      BUBBLE(compile_to_chunk(vm, chunk, node->kids[0]));
      BUBBLE(compile_to_chunk(vm, chunk, node->kids[1]));
      chunk_write(chunk, OpLe);
      break;
    }
    case AstntBinaryOr: {
      BUBBLE(compile_to_chunk(vm, chunk, node->kids[0]));
      BUBBLE(compile_to_chunk(vm, chunk, node->kids[1]));
      chunk_write(chunk, OpBinOr);
      break;
    }
    case AstntBinaryXor: {
      BUBBLE(compile_to_chunk(vm, chunk, node->kids[0]));
      BUBBLE(compile_to_chunk(vm, chunk, node->kids[1]));
      chunk_write(chunk, OpBinXor);
      break;
    }
    case AstntBinaryAnd: {
      BUBBLE(compile_to_chunk(vm, chunk, node->kids[0]));
      BUBBLE(compile_to_chunk(vm, chunk, node->kids[1]));
      chunk_write(chunk, OpBinAnd);
      break;
    }
    case AstntBinaryRShift: {
      BUBBLE(compile_to_chunk(vm, chunk, node->kids[0]));
      BUBBLE(compile_to_chunk(vm, chunk, node->kids[1]));
      chunk_write(chunk, OpRShift);
      break;
    }
    case AstntBinaryLShift: {
      BUBBLE(compile_to_chunk(vm, chunk, node->kids[0]));
      BUBBLE(compile_to_chunk(vm, chunk, node->kids[1]));
      chunk_write(chunk, OpLShift);
      break;
    }
    case AstntIf: {
      i32* elif_jumps = malloc(sizeof(i32));
      i32  jump_lengths = 1;
      
      BUBBLE(compile_to_chunk(vm, chunk, node->kids[0]));
      i32 if_false_jump = chunk_write_jump(chunk, OpJumpIfFalse);

      chunk_write(chunk, OpPop);
      BUBBLE(compile_to_chunk(vm, chunk, node->kids[1]));
      i32 past_else = chunk_write_jump(chunk, OpJump);
      elif_jumps[0] = past_else;
      
      chunk_patch_jump(chunk, if_false_jump);
      chunk_write(chunk, OpPop);
      
      for (AstNode* elif = node->kids[2]; elif != NULL; elif = elif->kids[2]) {
        BUBBLE(compile_to_chunk(vm, chunk, elif->kids[0]));
        i32 if_false_jump = chunk_write_jump(chunk, OpJumpIfFalse);

        chunk_write(chunk, OpPop);
        BUBBLE(compile_to_chunk(vm, chunk, elif->kids[1]));
        i32 past_else = chunk_write_jump(chunk, OpJump);
        
        elif_jumps = realloc(elif_jumps, sizeof(i32) * (++jump_lengths));
        elif_jumps[jump_lengths - 1] = past_else;

        chunk_patch_jump(chunk, if_false_jump);
        chunk_write(chunk, OpPop);
     }

      // ELSE
      if (node->kids[3] != NULL) {
        BUBBLE(compile_to_chunk(vm, chunk, node->kids[3]));
      }

      for (i32 i = 0; i < jump_lengths; i++) {
        chunk_patch_jump(chunk, elif_jumps[i]);
      }
      free(elif_jumps);
      
      // TODO: An 'if' expression should keep result of the run branch on the stack, which isn't exactly possible
      //       right now, so we put a dummy value on the stack.
      chunk_write_constant(chunk, new_nil());
      break;
    }
    case AstntCase: {
      BUBBLE(compile_to_chunk(vm, chunk, node->kids[0]));

      i32* out_of_case = malloc(0);
      i32  jump_lengths = 0;
      
      for (AstNode* arm = node->kids[1]; arm != NULL; arm = arm->kids[2]) {
        chunk_write(chunk, OpDup);
        BUBBLE(compile_to_chunk(vm, chunk, arm->kids[0]));
        chunk_write(chunk, OpEq);
        i32 if_neq = chunk_write_jump(chunk, OpJumpIfFalse);

        chunk_write(chunk, OpPop);
        BUBBLE(compile_to_chunk(vm, chunk, arm->kids[1]));
        i32 past_else = chunk_write_jump(chunk, OpJump);
        
        out_of_case = realloc(out_of_case, sizeof(i32) * (++jump_lengths));
        out_of_case[jump_lengths - 1] = past_else;

        chunk_patch_jump(chunk, if_neq);
        chunk_write(chunk, OpPop);
      }
      
      for(i32 i = 0; i < jump_lengths; i++) {
        chunk_patch_jump(chunk, out_of_case[i]);
      }
      free(out_of_case);
      
      // TODO: Fix the grammar so this can be done with... This is a sad state of affairs.
      // Pop the test value off of the stack and because the case expression can't have a final expression,
      // write a dummy value similar to 'if'
      chunk_write(chunk, OpPop);
      chunk_write_constant(chunk, new_nil());
      break;
    }
    case AstntExprStmt: {
      BUBBLE(compile_to_chunk(vm, chunk, node->kids[0]));
      chunk_write(chunk, OpPop);
      break;
    }
    case AstntAssignStmt: {
      AstNode* ident_node = node->kids[0];
      AstNode* value_node = node->kids[1];
      
      BUBBLE(compile_to_chunk(vm, chunk, value_node));

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
      
      if(vm->curr_compiler->scope_depth == 0) {
        chunk_write_constant(chunk, new_object((Object*)interned));
        chunk_write(chunk, OpDefineGlobal);
      } else {
        Local loc = { .depth = vm->curr_compiler->scope_depth, .name = interned };
        local_arr_write(&vm->curr_compiler->locals, loc);
        
        chunk_write_constant(chunk, new_int(vm->curr_compiler->locals.len- 1));
        chunk_write(chunk, OpDefineLocal);
      }

      break;
    }
    case AstntBlockStmt: {
      if (node->kids[0] != NULL) {
        compiler_new_scope(vm->curr_compiler);
        BUBBLE(compile_to_chunk(vm, chunk, node->kids[0]));
        compiler_end_scope(chunk, vm->curr_compiler);
      }
      break;
    }
    case AstntStmts: {
      for(AstNode* stmt = node; stmt != NULL; stmt = stmt->kids[1]) {
        BUBBLE(compile_to_chunk(vm, chunk, stmt->kids[0]));
      }
      break;
    }
    case AstntElif: {
      fprintf(stderr, "Attempting to compile a lone 'Elif' node. You done messed up.");
      break;
    }
    case AstntElse: {
      fprintf(stderr, "Attempting to compile a lone 'Else' node. You done messed up.");
      break;
    }
    case AstntCaseArm: {
      fprintf(stderr, "Attempting to compile a lone 'CaseArm' node. You done messed up.");
      break;
    }
    case AstntArray: {
      // Reverse the linked list
      AstNode* prev = NULL;
      AstNode* curr = node->kids[0];
      if(curr == NULL) {
        chunk_write_constant(chunk, new_int(0));
        chunk_write(chunk, OpMakeArray);
        return CarOk;
      }
      
      AstNode* next = curr->kids[1];
      while (curr != NULL) {
        curr->kids[1] = prev;
        prev = curr;
        curr = next;
        
        if (next != NULL) {
          next = next->kids[1];
        }
      }
      
      node->kids[0] = prev;

      // The list is guarunteed to be reversed at this time => compilation order is the same 
      // as the order of the elements.
      i32 len = 0;
      for(AstNode* expr_list = node->kids[0]; expr_list != NULL; expr_list = expr_list->kids[1]) {
        BUBBLE(compile_to_chunk(vm, chunk, expr_list->kids[0]));
        len += 1;
      }
      chunk_write_constant(chunk, new_int(len));
      chunk_write(chunk, OpMakeArray);
      break;
    }
    case AstntArrayIndex: {
      BUBBLE(compile_to_chunk(vm, chunk, node->kids[0]));
      BUBBLE(compile_to_chunk(vm, chunk, node->kids[1]));      
      chunk_write(chunk, OpIndexArray);
      break;
    }
    // case AstntBinaryLCompose: {
    //   fprintf(stderr, "Unable to compile AstNode of kind: (%d)", node->type);
    //   break;
    // }
    // case AstntBinaryRCompose: {
    //   fprintf(stderr, "Unable to compile AstNode of kind: (%d)", node->type);
    //   break;
    // }
    // case AstntBinaryLApply: {
    //   fprintf(stderr, "Unable to compile AstNode of kind: (%d)", node->type);
    //   break;
    // }
    // case AstntBinaryRApply: {
    //   fprintf(stderr, "Unable to compile AstNode of kind: (%d)", node->type);
    //   break;
    // }
    // case AstntFuncDef: {
    //   fprintf(stderr, "Unable to compile AstNode of kind: (%d)", node->type);
    //   break;
    // }
    // case AstntNodeList: {
    //   fprintf(stderr, "Unable to compile AstNode of kind: (%d)", node->type);
    //   break;
    // }
    // case AstntFuncCall: {
    //   fprintf(stderr, "Unable to compile AstNode of kind: (%d)", node->type);
    //   break;
    // }
    // case AstntReturn {
    //   fprintf(stderr, "Unable to compile AstNode of kind: (%d)", node->type);
    //   break;
    // }
    default:
      // fprintf(stderr, "Unable to compile AstNode of kind: (%d)", node->type);
      // fprintf(stderr, "Default branch reached while matching on node->type in %s on line %d", __FILE__, __LINE__);
      return CarUnsupportedAst;
  }
  
  return CarOk; 
}
