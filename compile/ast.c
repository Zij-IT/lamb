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
      
      i32 local_slot = resolve_local(&vm->curr_compiler, interned); 
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
      // LHS
      // OpJumpIfFalse -- Jump over RHS
      // OpPop         -- Pop off the LHS because && evaluates to RHS if LHS is true
      // RHS
      compile_ast(vm, node->kids[0]);
      i32 if_false = chunk_write_jump(vm->chunk, OpJumpIfFalse);
      chunk_write(vm->chunk, OpPop);
      compile_ast(vm, node->kids[1]);
      chunk_write(vm->chunk, OpLogAnd);
      chunk_patch_jump(vm->chunk, if_false);
      break;
    }
    case AstntBinaryLogOr: {
      // LHS
      // OpJumpIfFalse -- Jump over the next instruction
      // OpJump        -- Jump over the RHS
      // OpPop         -- Pop off the LHS
      // RHS
      compile_ast(vm, node->kids[0]);
      i32 if_false = chunk_write_jump(vm->chunk, OpJumpIfFalse);
      i32 skip_right = chunk_write_jump(vm->chunk, OpJump);
      chunk_patch_jump(vm->chunk, if_false);
      chunk_write(vm->chunk, OpPop);
      compile_ast(vm, node->kids[1]);
      chunk_write(vm->chunk, OpLogOr);
      chunk_patch_jump(vm->chunk, skip_right);
      break;
    }    case AstntBinaryEq: {
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
      // EXPR BLOCK ELIFS ELSE
      compile_ast(vm, node->kids[0]);

      i32 then_jump = chunk_write_jump(vm->chunk, OpJumpIfFalse);
      chunk_write(vm->chunk, OpPop);
      compile_ast(vm, node->kids[1]);
      
      i32 else_jump = chunk_write_jump(vm->chunk, OpJump);
      chunk_patch_jump(vm->chunk, then_jump);
      chunk_write(vm->chunk, OpPop);
      
      // ELIFS
      
      // ELSE
      if (node->kids[3] != NULL) {
        compile_ast(vm, node->kids[3]);
      }

      chunk_patch_jump(vm->chunk, else_jump);
      
      // TODO: An 'if' expression should keep result of the run branch on the stack, which isn't exactly possible
      //       right now, so we put a dummy value on the stack.
      chunk_write_constant(vm->chunk, new_boolean(false));
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
      
      if(vm->curr_compiler.scope_depth == 0) {
        // TODO: Implement no shadowing of items in the global scope...
        chunk_write_constant(vm->chunk, new_object((Object*)interned));
        chunk_write(vm->chunk, OpDefineGlobal);
      } else {
        Local loc = { .depth = vm->curr_compiler.scope_depth, .name = interned };
        local_arr_write(&vm->curr_compiler.locals, loc);
        
        chunk_write_constant(vm->chunk, new_int(vm->curr_compiler.locals.len- 1));
        chunk_write(vm->chunk, OpDefineLocal);
      }

      break;
    }
    case AstntBlockStmt: {
      if (node->kids[0] != NULL) {
        compiler_new_scope(&vm->curr_compiler);
        compile_ast(vm, node->kids[0]);
        compiler_end_scope(vm->chunk, &vm->curr_compiler);
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

