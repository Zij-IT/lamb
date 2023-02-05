#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "ast.h"
#include "debug.h"
#include "misc.h"
#include "object.h"

#define LOCAL_NOT_FOUND -1
#define UPVALUE_NOT_FOUND -1

static i32 add_upvalue(Compiler* compiler, i32 index, bool is_local) {
  int count = compiler->function->upvalue_count;
  
  if (index > 255) {
    // TODO: Figure out how to get a dynamic amount of upvalues
    printf("COMPILER_ERR: Too many upvalues. Max 255...");
    exit(1);
  }
  
  for (i32 i = 0; i < count; i++) {
    Upvalue* upvalue = &compiler->upvalues[i];
    if (upvalue->index == index && upvalue->is_local == is_local) {
      return i;
    }
  }
  
  compiler->upvalues[count].is_local = is_local;
  compiler->upvalues[count].index = index;
  
  return compiler->function->upvalue_count++;
}

static i32 resolve_local(Compiler* compiler, LambString* name) {
  for (i32 i = compiler->locals.len - 1; i >= 0; i--) {
    Local* local = &compiler->locals.values[i];
    if (local->name == name->chars) {
      return i;
    }
  }
  
  return LOCAL_NOT_FOUND;
}

static i32 resolve_upvalue(Compiler* compiler, LambString* name) {
  if (compiler->enclosing == NULL) {
    return UPVALUE_NOT_FOUND;
  }
  
  i32 local = resolve_local(compiler->enclosing, name);
  if (local != LOCAL_NOT_FOUND) {
    compiler->enclosing->locals.values[local].is_captured = true;
    return add_upvalue(compiler, local, true);
  }
  
  i32 upvalue = resolve_upvalue(compiler->enclosing, name);
  if (upvalue != UPVALUE_NOT_FOUND) {
    return add_upvalue(compiler, upvalue, false);
  }
  
  return UPVALUE_NOT_FOUND;
}

#define BUBBLE(x) \
    if ((x) == CarUnsupportedAst) { \
      return CarUnsupportedAst;     \
    }

static Chunk* compiler_chunk(Compiler* compiler) {
  return &compiler->function->chunk;
}

static CompileAstResult compile_rec_func_def(Vm* vm, Compiler* compiler, AstNode* node) {
  AstNode* ident = node->kids[0];
  AstNode* func_def = node->kids[1];
  AstNode* params = func_def->kids[0];
  AstNode* body = func_def->kids[1];
  
  LambString* interned = cstr_to_lambstring(vm, ident->val.i);
  
  {
      Compiler func_comp;
      compiler_init(&func_comp, FtNormal);
      compiler_new_scope(&func_comp);
      func_comp.function = (LambFunc*)alloc_obj(vm, OtFunc);
      func_comp.function->name = interned->chars;
      func_comp.enclosing = compiler;
   
      // Normally this slot is left blank so that the user cannot refer to it
      // However this spot points back to the identifier of the function, and
      // thus the item looking to be called in a recursive context.
      func_comp.locals.values[0].name = interned->chars;

      // Add parameters to locals
      for (AstNode* child = params; child != NULL; child = child->kids[1]) {
        AstNode* ident_node = child->kids[0];

        LambString* interned = cstr_to_lambstring(vm, ident_node->val.i);
        if (resolve_local(&func_comp, interned) != LOCAL_NOT_FOUND) {
          // Parameters have the same name. Likely a mistake on the programmers part.
          // Somehow output a compiler error to let them know.
        }

        Local loc = { .depth = func_comp.scope_depth, .name = interned->chars, .is_captured = false };
        local_arr_write(&func_comp.locals, loc);

        func_comp.function->arity++;
      }
      
      // Compile the body
      if (body->type == AstntBlockStmt) {
        compile(vm, &func_comp, body);
        chunk_write_constant(compiler_chunk(&func_comp), new_nil());        
        chunk_write(compiler_chunk(&func_comp), OpReturn);
      } else {
        compile(vm, &func_comp, body);
        chunk_write(compiler_chunk(&func_comp), OpReturn);
      }
      
      chunk_debug(&func_comp.function->chunk, "Rec Function Chunk");
      
      // TODO: Figure out how to have function and closure objects so that this wrap isn't necessary
      chunk_write_constant(compiler_chunk(compiler), new_object((Object*)func_comp.function));
      chunk_write(compiler_chunk(compiler), OpClosure);
      for(i32 i = 0; i < func_comp.function->upvalue_count; i++) {
        chunk_write(compiler_chunk(compiler), func_comp.upvalues[i].is_local ? 1 : 0);
        chunk_write(compiler_chunk(compiler), func_comp.upvalues[i].index);
      }
      compiler_free(&func_comp);       
  }
  
  if(compiler->scope_depth == 0) {
    chunk_write_constant(compiler_chunk(compiler), new_object((Object*)interned));
    chunk_write(compiler_chunk(compiler), OpDefineGlobal);
  } else {
    Local loc = { .depth = compiler->scope_depth, .name = interned->chars, .is_captured = false };
    local_arr_write(&compiler->locals, loc);
    
    chunk_write_constant(compiler_chunk(compiler), new_int(compiler->locals.len - 1));
    chunk_write(compiler_chunk(compiler), OpDefineLocal);
  }
  
  return CarOk;
}

CompileAstResult compile(Vm* vm, Compiler* compiler, AstNode* node) {
   switch (node->type) {
    case AstntStrLit: {
      LambString* lit = cstr_to_lambstring(vm, node->val.s);
      chunk_write_constant(compiler_chunk(compiler), new_object((Object*)lit));
      break;
    }
    case AstntIdent: {
      LambString* interned = cstr_to_lambstring(vm, node->val.i);
      
      i32 local_slot = resolve_local(compiler, interned); 
      if (local_slot != LOCAL_NOT_FOUND) {
        chunk_write_constant(compiler_chunk(compiler), new_int(local_slot));
        chunk_write(compiler_chunk(compiler), OpGetLocal);
      }  else {
        
        i32 upvalue_slot = resolve_upvalue(compiler, interned);
        if (upvalue_slot == UPVALUE_NOT_FOUND) {
          chunk_write_constant(compiler_chunk(compiler), new_object((Object*)interned));
          chunk_write(compiler_chunk(compiler), OpGetGlobal);
        } else {
          chunk_write_constant(compiler_chunk(compiler), new_int(upvalue_slot));
          chunk_write(compiler_chunk(compiler), OpGetUpvalue);
        }
      }
      
      break;
    }
    case AstntNilLit: {
      chunk_write_constant(compiler_chunk(compiler), new_nil());
      break;
    }
    case AstntNumLit: {
      chunk_write_constant(compiler_chunk(compiler), new_int(node->val.n));
      break;
    }
    case AstntCharLit: {
      chunk_write_constant(compiler_chunk(compiler), new_char(node->val.c));
      break;
    }
    case AstntBoolLit: {
      chunk_write_constant(compiler_chunk(compiler), new_boolean(node->val.b));
      break;
    }
    case AstntUnaryNeg: {
      BUBBLE(compile(vm, compiler, node->kids[0]));
      chunk_write(compiler_chunk(compiler), OpNumNeg);
      break;
    }
    case AstntUnaryLogNot: {
      BUBBLE(compile(vm, compiler, node->kids[0]));
      chunk_write(compiler_chunk(compiler), OpLogNeg);
      break;
    }
    case AstntUnaryBitNot: {
      BUBBLE(compile(vm, compiler, node->kids[0]));
      chunk_write(compiler_chunk(compiler), OpBinNeg);
      break;
    }
    case AstntBinaryAdd: {
      BUBBLE(compile(vm, compiler, node->kids[0]));
      BUBBLE(compile(vm, compiler, node->kids[1]));
      chunk_write(compiler_chunk(compiler), OpAdd);
      break;
    }
    case AstntBinarySub: {
      BUBBLE(compile(vm, compiler, node->kids[0]));
      BUBBLE(compile(vm, compiler, node->kids[1]));
      chunk_write(compiler_chunk(compiler), OpSub);
      break;
    }
    case AstntBinaryMul: {
      BUBBLE(compile(vm, compiler, node->kids[0]));
      BUBBLE(compile(vm, compiler, node->kids[1]));
      chunk_write(compiler_chunk(compiler), OpMul);
      break;
    }
    case AstntBinaryDiv: {
      BUBBLE(compile(vm, compiler, node->kids[0]));
      BUBBLE(compile(vm, compiler, node->kids[1]));
      chunk_write(compiler_chunk(compiler), OpDiv);
      break;
    }
    case AstntBinaryMod: {
      BUBBLE(compile(vm, compiler, node->kids[0]));
      BUBBLE(compile(vm, compiler, node->kids[1]));
      chunk_write(compiler_chunk(compiler), OpMod);
      break;
    }
    case AstntBinaryLogAnd: {
      BUBBLE(compile(vm, compiler, node->kids[0]));
      i32 if_false = chunk_write_jump(compiler_chunk(compiler), OpJumpIfFalse);
      chunk_write(compiler_chunk(compiler), OpPop);
      BUBBLE(compile(vm, compiler, node->kids[1]));
      chunk_patch_jump(compiler_chunk(compiler), if_false);
      break;
    }
    case AstntBinaryLogOr: {
      BUBBLE(compile(vm, compiler, node->kids[0]));
      i32 if_false = chunk_write_jump(compiler_chunk(compiler), OpJumpIfFalse);
      i32 skip_right = chunk_write_jump(compiler_chunk(compiler), OpJump);
      chunk_patch_jump(compiler_chunk(compiler), if_false);
      chunk_write(compiler_chunk(compiler), OpPop);
      BUBBLE(compile(vm, compiler, node->kids[1]));
      chunk_patch_jump(compiler_chunk(compiler), skip_right);
      break;
    }    case AstntBinaryEq: {
      BUBBLE(compile(vm, compiler, node->kids[0]));
      BUBBLE(compile(vm, compiler, node->kids[1]));
      chunk_write(compiler_chunk(compiler), OpEq);
      break;
    }
    case AstntBinaryNe: {
      BUBBLE(compile(vm, compiler, node->kids[0]));
      BUBBLE(compile(vm, compiler, node->kids[1]));
      chunk_write(compiler_chunk(compiler), OpNe);
      break;
    }
    case AstntBinaryGt: {
      BUBBLE(compile(vm, compiler, node->kids[0]));
      BUBBLE(compile(vm, compiler, node->kids[1]));
      chunk_write(compiler_chunk(compiler), OpGt);
      break;
    }
    case AstntBinaryGe: {
      BUBBLE(compile(vm, compiler, node->kids[0]));
      BUBBLE(compile(vm, compiler, node->kids[1]));
      chunk_write(compiler_chunk(compiler), OpGe);
      break;
    }
    case AstntBinaryLt: {
      BUBBLE(compile(vm, compiler, node->kids[0]));
      BUBBLE(compile(vm, compiler, node->kids[1]));
      chunk_write(compiler_chunk(compiler), OpLt);
      break;
    }
    case AstntBinaryLe: {
      BUBBLE(compile(vm, compiler, node->kids[0]));
      BUBBLE(compile(vm, compiler, node->kids[1]));
      chunk_write(compiler_chunk(compiler), OpLe);
      break;
    }
    case AstntBinaryOr: {
      BUBBLE(compile(vm, compiler, node->kids[0]));
      BUBBLE(compile(vm, compiler, node->kids[1]));
      chunk_write(compiler_chunk(compiler), OpBinOr);
      break;
    }
    case AstntBinaryXor: {
      BUBBLE(compile(vm, compiler, node->kids[0]));
      BUBBLE(compile(vm, compiler, node->kids[1]));
      chunk_write(compiler_chunk(compiler), OpBinXor);
      break;
    }
    case AstntBinaryAnd: {
      BUBBLE(compile(vm, compiler, node->kids[0]));
      BUBBLE(compile(vm, compiler, node->kids[1]));
      chunk_write(compiler_chunk(compiler), OpBinAnd);
      break;
    }
    case AstntBinaryRShift: {
      BUBBLE(compile(vm, compiler, node->kids[0]));
      BUBBLE(compile(vm, compiler, node->kids[1]));
      chunk_write(compiler_chunk(compiler), OpRShift);
      break;
    }
    case AstntBinaryLShift: {
      BUBBLE(compile(vm, compiler, node->kids[0]));
      BUBBLE(compile(vm, compiler, node->kids[1]));
      chunk_write(compiler_chunk(compiler), OpLShift);
      break;
    }
    case AstntIf: {
      i32* elif_jumps = malloc(sizeof(i32));
      i32  jump_lengths = 1;
      
      BUBBLE(compile(vm, compiler, node->kids[0]));
      i32 if_false_jump = chunk_write_jump(compiler_chunk(compiler), OpJumpIfFalse);

      chunk_write(compiler_chunk(compiler), OpPop);
      BUBBLE(compile(vm, compiler, node->kids[1]));
      i32 past_else = chunk_write_jump(compiler_chunk(compiler), OpJump);
      elif_jumps[0] = past_else;
      
      chunk_patch_jump(compiler_chunk(compiler), if_false_jump);
      chunk_write(compiler_chunk(compiler), OpPop);
      
      for (AstNode* elif = node->kids[2]; elif != NULL; elif = elif->kids[2]) {
        BUBBLE(compile(vm, compiler, elif->kids[0]));
        i32 if_false_jump = chunk_write_jump(compiler_chunk(compiler), OpJumpIfFalse);

        chunk_write(compiler_chunk(compiler), OpPop);
        BUBBLE(compile(vm, compiler, elif->kids[1]));
        i32 past_else = chunk_write_jump(compiler_chunk(compiler), OpJump);
        
        elif_jumps = realloc(elif_jumps, sizeof(i32) * (++jump_lengths));
        elif_jumps[jump_lengths - 1] = past_else;

        chunk_patch_jump(compiler_chunk(compiler), if_false_jump);
        chunk_write(compiler_chunk(compiler), OpPop);
     }

      // ELSE
      if (node->kids[3] != NULL) {
        BUBBLE(compile(vm, compiler, node->kids[3]->kids[0]));
      }

      for (i32 i = 0; i < jump_lengths; i++) {
        chunk_patch_jump(compiler_chunk(compiler), elif_jumps[i]);
      }
      free(elif_jumps);
      
      // TODO: An 'if' expression should keep result of the run branch on the stack, which isn't exactly possible
      //       right now, so we put a dummy value on the stack.
      chunk_write_constant(compiler_chunk(compiler), new_nil());
      break;
    }
    case AstntCase: {
      BUBBLE(compile(vm, compiler, node->kids[0]));

      i32* out_of_case = malloc(0);
      i32  jump_lengths = 0;
      
      for (AstNode* arm = node->kids[1]; arm != NULL; arm = arm->kids[2]) {
        chunk_write(compiler_chunk(compiler), OpDup);
        BUBBLE(compile(vm, compiler, arm->kids[0]));
        chunk_write(compiler_chunk(compiler), OpEq);
        i32 if_neq = chunk_write_jump(compiler_chunk(compiler), OpJumpIfFalse);

        chunk_write(compiler_chunk(compiler), OpPop);
        BUBBLE(compile(vm, compiler, arm->kids[1]));
        i32 past_else = chunk_write_jump(compiler_chunk(compiler), OpJump);
        
        out_of_case = realloc(out_of_case, sizeof(i32) * (++jump_lengths));
        out_of_case[jump_lengths - 1] = past_else;

        chunk_patch_jump(compiler_chunk(compiler), if_neq);
        chunk_write(compiler_chunk(compiler), OpPop);
      }
      
      for(i32 i = 0; i < jump_lengths; i++) {
        chunk_patch_jump(compiler_chunk(compiler), out_of_case[i]);
      }
      free(out_of_case);
      
      // TODO: Fix the grammar so this can be done with... This is a sad state of affairs.
      // Pop the test value off of the stack and because the case expression can't have a final expression,
      // write a dummy value similar to 'if'
      chunk_write(compiler_chunk(compiler), OpPop);
      chunk_write_constant(compiler_chunk(compiler), new_nil());
      break;
    }
    case AstntExprStmt: {
      BUBBLE(compile(vm, compiler, node->kids[0]));
      chunk_write(compiler_chunk(compiler), OpPop);
      break;
    }
    case AstntAssignStmt: {
      AstNode* ident_node = node->kids[0];
      AstNode* value_node = node->kids[1];
      
      if (value_node->type == AstntFuncDef && value_node->kids[2]->val.b){
        BUBBLE(compile_rec_func_def(vm, compiler, node));
        break;
      }
      
      BUBBLE(compile(vm, compiler, value_node));

      LambString* interned = cstr_to_lambstring(vm, ident_node->val.i);

      if(compiler->scope_depth == 0) {
        chunk_write_constant(compiler_chunk(compiler), new_object((Object*)interned));
        chunk_write(compiler_chunk(compiler), OpDefineGlobal);
      } else {
        Local loc = { .depth = compiler->scope_depth, .name = interned->chars, .is_captured = false };
        local_arr_write(&compiler->locals, loc);
        
        chunk_write_constant(compiler_chunk(compiler), new_int(compiler->locals.len - 1));
        chunk_write(compiler_chunk(compiler), OpDefineLocal);
      }

      break;
    }
    case AstntBlockStmt: {
      if (node->kids[0] != NULL) {
        compiler_new_scope(compiler);
        BUBBLE(compile(vm, compiler, node->kids[0]));
        compiler_end_scope(compiler);
      }
      break;
    }
    case AstntStmts: {
      for(AstNode* stmt = node; stmt != NULL; stmt = stmt->kids[1]) {
        BUBBLE(compile(vm, compiler, stmt->kids[0]));
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
        chunk_write_constant(compiler_chunk(compiler), new_int(0));
        chunk_write(compiler_chunk(compiler), OpMakeArray);
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
        BUBBLE(compile(vm, compiler, expr_list->kids[0]));
        len += 1;
      }
      chunk_write_constant(compiler_chunk(compiler), new_int(len));
      chunk_write(compiler_chunk(compiler), OpMakeArray);
      break;
    }
    case AstntArrayIndex: {
      BUBBLE(compile(vm, compiler, node->kids[0]));
      BUBBLE(compile(vm, compiler, node->kids[1]));      
      chunk_write(compiler_chunk(compiler), OpIndexArray);
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
    case AstntFuncDef: {
      // FuncDef:
      // 0: AstNodeList<AstntIdent>
      // 1: AstntBlock OR AstntExpr
      // 2: AstntBool: true => recursive, false => non-recursive

      Compiler func_comp;
      compiler_init(&func_comp, FtNormal);
      compiler_new_scope(&func_comp);
      func_comp.function = (LambFunc*)alloc_obj(vm, OtFunc);
      func_comp.function->name = "anonymous";
      func_comp.enclosing = compiler;
      
      // Add parameters to locals
      for (AstNode* child = node->kids[0]; child != NULL; child = child->kids[1]) {
        AstNode* ident_node = child->kids[0];

        LambString* interned = cstr_to_lambstring(vm, ident_node->val.i);

        if (resolve_local(&func_comp, interned) != LOCAL_NOT_FOUND) {
          // Parameters have the same name. Likely a mistake on the programmers part.
          // Somehow output a compiler error to let them know.
        }

        Local loc = { .depth = func_comp.scope_depth, .name = interned->chars, .is_captured = false };
        local_arr_write(&func_comp.locals, loc);

        func_comp.function->arity++;
      }
      
      // Compile the body
      if (node->kids[1]->type == AstntBlockStmt) {
        compile(vm, &func_comp, node->kids[1]);
        chunk_write_constant(compiler_chunk(&func_comp), new_nil());        
        chunk_write(compiler_chunk(&func_comp), OpReturn);
      } else {
        compile(vm, &func_comp, node->kids[1]);
        chunk_write(compiler_chunk(&func_comp), OpReturn);
      }
      
      chunk_debug(&func_comp.function->chunk, "Function Chunk");

      // TODO: Figure out how to have function and closure objects so that this wrap isn't necessary
      // NOTE: The order of this is opposite the rest of the compiler. Typically the items are put on
      //       and popped off the VM's stack, then they are popped off within the Opcode.
      chunk_write(compiler_chunk(compiler), OpClosure);
      chunk_write_constant(compiler_chunk(compiler), new_object((Object*)func_comp.function));
      for(i32 i = 0; i < func_comp.function->upvalue_count; i++) {
        chunk_write(compiler_chunk(compiler), func_comp.upvalues[i].is_local ? 1 : 0);
        chunk_write(compiler_chunk(compiler), func_comp.upvalues[i].index);
      }
      compiler_free(&func_comp);
      
      // Although it logically makes sense to call this, it isn't really necessary.
      // The compiler is dropped at the end of the scope anyhow. Calling it is wasted
      // CPU effort :D 
      // compiler_end_scope(&func_comp);

      break;
      // return CarUnsupportedAst;
    }
    // case AstntNodeList: {
    //   fprintf(stderr, "Unable to compile AstNode of kind: (%d)", node->type);
    //   break;
    // }
    case AstntFuncCall: {
      AstNode* callee = node->kids[0];
      compile(vm, compiler, callee);

      u32 arg_count = 0;
      for(AstNode* arg_list = node->kids[1]; arg_list != NULL; arg_list = arg_list->kids[1]) {
        BUBBLE(compile(vm, compiler, arg_list->kids[0]));
        arg_count += 1;
      }
            
      chunk_write_constant(compiler_chunk(compiler), new_int((i64)arg_count));
      chunk_write(compiler_chunk(compiler), OpCall);
      break;
    }
    case AstntReturn: {
      AstNode* val = node->kids[0];
      if (val == NULL) {
        chunk_write_constant(compiler_chunk(compiler), new_nil());
      } else {
        compile(vm, compiler, node->kids[0]);
      }
      chunk_write(compiler_chunk(compiler), OpReturn);
      break;
    }
    default:
      // fprintf(stderr, "Unable to compile AstNode of kind: (%d)", node->type);
      // fprintf(stderr, "Default branch reached while matching on node->type in %s on line %d", __FILE__, __LINE__);
      return CarUnsupportedAst;
  }
  
  return CarOk; 
}
