#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "../debug/debug.h"
#include "ast.h"
#include "misc.h"
#include "object.h"

#define LOCAL_NOT_FOUND -1
#define UPVALUE_NOT_FOUND -1
#define ANON_FUNC_NAME "anonymous"

static i32 add_upvalue(Compiler *compiler, i32 index, bool is_local) {
  int count = compiler->function->upvalue_count;

  if (index > 255) {
    // TODO: Figure out how to get a dynamic amount of upvalues
    printf("COMPILER_ERR: Too many upvalues. Max 255...");
    exit(1);
  }

  for (i32 i = 0; i < count; i++) {
    Upvalue *upvalue = &compiler->upvalues[i];
    if (upvalue->index == index && upvalue->is_local == is_local) {
      return i;
    }
  }

  compiler->upvalues[count].is_local = is_local;
  compiler->upvalues[count].index = index;

  return compiler->function->upvalue_count++;
}

static i32 resolve_local(Compiler *compiler, LambString *name) {
  for (i32 i = compiler->locals.len - 1; i >= 0; i--) {
    Local *local = &compiler->locals.values[i];
    if (local->name == name->chars) {
      return i;
    }
  }

  return LOCAL_NOT_FOUND;
}

static i32 resolve_upvalue(Compiler *compiler, LambString *name) {
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

#define BUBBLE(x)                                                              \
  if ((x) == CarUnsupportedAst) {                                              \
    return CarUnsupportedAst;                                                  \
  }

static Chunk *compiler_chunk(Compiler *compiler) {
  return &compiler->function->chunk;
}

static CompileAstResult compile_function(Vm *vm, Compiler *compiler,
                                         AstNode *node, str name) {
  Compiler func_comp;
  compiler_init(vm, &func_comp, FtNormal);
  compiler_new_scope(&func_comp);
  func_comp.function = (LambFunc *)alloc_obj(vm, OtFunc);
  func_comp.function->name = name;
  func_comp.enclosing = compiler;
  vm->curr_compiler = &func_comp;

  if (node->kids[2]->val.b) {
    func_comp.locals.values[0].name = name;
  }

  for (AstNode *child = node->kids[0]; child != NULL; child = child->kids[1]) {
    AstNode *ident_node = child->kids[0];
    LambString *ident = cstr_to_lambstring(vm, ident_node->val.i);
    chunk_add_constant(vm, compiler_chunk(compiler),
                       new_object((Object *)ident));
    Local loc = {.depth = func_comp.scope_depth,
                 .name = ident->chars,
                 .is_captured = false};
    local_arr_write(vm, &func_comp.locals, loc);
    func_comp.function->arity++;
  }

  BUBBLE(compile(vm, &func_comp, node->kids[1]));
  chunk_write(vm, compiler_chunk(&func_comp), OpReturn);

  if (vm->options.print_fn_chunks) {
    chunk_debug(&func_comp.function->chunk, "Function Chunk");
  }

  // TODO: Figure out how to have function and closure objects so that this
  // wrap isn't necessary
  chunk_write(vm, compiler_chunk(compiler), OpClosure);
  chunk_write_constant(vm, compiler_chunk(compiler),
                       new_object((Object *)func_comp.function));
  for (i32 i = 0; i < func_comp.function->upvalue_count; i++) {
    chunk_write(vm, compiler_chunk(compiler),
                func_comp.upvalues[i].is_local ? 1 : 0);
    chunk_write(vm, compiler_chunk(compiler), func_comp.upvalues[i].index);
  }
  compiler_end_scope(vm, &func_comp);
  compiler_free(vm, &func_comp);
  vm->curr_compiler = func_comp.enclosing;

  return CarOk;
}

static CompileAstResult compile_rec_func_def(Vm *vm, Compiler *compiler,
                                             AstNode *node) {
  AstNode *ident = node->kids[0];
  AstNode *func_def = node->kids[1];

  LambString *rec_func_ident = cstr_to_lambstring(vm, ident->val.i);
  chunk_add_constant(vm, compiler_chunk(compiler),
                     new_object((Object *)rec_func_ident));

  BUBBLE(compile_function(vm, compiler, func_def, rec_func_ident->chars));

  if (compiler->scope_depth == 0) {
    chunk_write(vm, compiler_chunk(compiler), OpDefineGlobal);
    chunk_write_constant(vm, compiler_chunk(compiler),
                         new_object((Object *)rec_func_ident));
  } else {
    Local loc = {.depth = compiler->scope_depth,
                 .name = rec_func_ident->chars,
                 .is_captured = false};
    local_arr_write(vm, &compiler->locals, loc);

    chunk_write(vm, compiler_chunk(compiler), OpDefineLocal);
    chunk_write_constant(vm, compiler_chunk(compiler),
                         new_int(compiler->locals.len - 1));
  }

  return CarOk;
}

static CompileAstResult compile_compose(Vm *vm, Compiler *compiler,
                                        AstNode *first, AstNode *second,
                                        str name) {
  Compiler func_comp;
  compiler_init(vm, &func_comp, FtNormal);
  compiler_new_scope(&func_comp);
  func_comp.function = (LambFunc *)alloc_obj(vm, OtFunc);
  func_comp.function->name = ANON_FUNC_NAME;
  func_comp.function->arity = 1;
  func_comp.enclosing = compiler;
  vm->curr_compiler = &func_comp;

  BUBBLE(compile(vm, &func_comp, first));
  BUBBLE(compile(vm, &func_comp, second));

  chunk_write(vm, compiler_chunk(&func_comp), OpGetLocal);
  chunk_write_constant(vm, compiler_chunk(&func_comp), new_int(1));

  chunk_write(vm, compiler_chunk(&func_comp), OpCall);
  chunk_write_constant(vm, compiler_chunk(&func_comp), new_int(1));

  chunk_write(vm, compiler_chunk(&func_comp), OpCall);
  chunk_write_constant(vm, compiler_chunk(&func_comp), new_int(1));

  chunk_write(vm, compiler_chunk(&func_comp), OpReturn);

  chunk_write(vm, compiler_chunk(compiler), OpClosure);
  chunk_write_constant(vm, compiler_chunk(compiler),
                       new_object((Object *)func_comp.function));

  if (vm->options.print_fn_chunks) {
    chunk_debug(compiler_chunk(&func_comp), name);
  }

  for (i32 i = 0; i < func_comp.function->upvalue_count; i++) {
    chunk_write(vm, compiler_chunk(compiler),
                func_comp.upvalues[i].is_local ? 1 : 0);
    chunk_write(vm, compiler_chunk(compiler), func_comp.upvalues[i].index);
  }

  compiler_end_scope(vm, &func_comp);
  compiler_free(vm, &func_comp);
  vm->curr_compiler = func_comp.enclosing;

  return CarOk;
}

CompileAstResult compile(Vm *vm, Compiler *compiler, AstNode *node) {
  switch (node->type) {
  case AstntStrLit: {
    LambString *lit = cstr_to_lambstring(vm, node->val.s);
    chunk_write_constant(vm, compiler_chunk(compiler),
                         new_object((Object *)lit));
    break;
  }
  case AstntIdent: {
    LambString *ident = cstr_to_lambstring(vm, node->val.i);

    i32 local_slot = resolve_local(compiler, ident);
    if (local_slot != LOCAL_NOT_FOUND) {
      chunk_write(vm, compiler_chunk(compiler), OpGetLocal);
      chunk_write_constant(vm, compiler_chunk(compiler), new_int(local_slot));
    } else {
      i32 upvalue_slot = resolve_upvalue(compiler, ident);
      if (upvalue_slot == UPVALUE_NOT_FOUND) {
        chunk_write(vm, compiler_chunk(compiler), OpGetGlobal);
        chunk_write_constant(vm, compiler_chunk(compiler),
                             new_object((Object *)ident));
      } else {
        chunk_write(vm, compiler_chunk(compiler), OpGetUpvalue);
        chunk_write_constant(vm, compiler_chunk(compiler),
                             new_int(upvalue_slot));
      }
    }

    break;
  }
  case AstntNilLit: {
    chunk_write_constant(vm, compiler_chunk(compiler), new_nil());
    break;
  }
  case AstntNumLit: {
    chunk_write_constant(vm, compiler_chunk(compiler), new_int(node->val.n));
    break;
  }
  case AstntCharLit: {
    chunk_write_constant(vm, compiler_chunk(compiler), new_char(node->val.c));
    break;
  }
  case AstntBoolLit: {
    chunk_write_constant(vm, compiler_chunk(compiler),
                         new_boolean(node->val.b));
    break;
  }
  case AstntUnaryNeg: {
    BUBBLE(compile(vm, compiler, node->kids[0]));
    chunk_write(vm, compiler_chunk(compiler), OpNumNeg);
    break;
  }
  case AstntUnaryLogNot: {
    BUBBLE(compile(vm, compiler, node->kids[0]));
    chunk_write(vm, compiler_chunk(compiler), OpLogNeg);
    break;
  }
  case AstntUnaryBitNot: {
    BUBBLE(compile(vm, compiler, node->kids[0]));
    chunk_write(vm, compiler_chunk(compiler), OpBinNeg);
    break;
  }
  case AstntBinaryAdd: {
    BUBBLE(compile(vm, compiler, node->kids[0]));
    BUBBLE(compile(vm, compiler, node->kids[1]));
    chunk_write(vm, compiler_chunk(compiler), OpAdd);
    break;
  }
  case AstntBinarySub: {
    BUBBLE(compile(vm, compiler, node->kids[0]));
    BUBBLE(compile(vm, compiler, node->kids[1]));
    chunk_write(vm, compiler_chunk(compiler), OpSub);
    break;
  }
  case AstntBinaryMul: {
    BUBBLE(compile(vm, compiler, node->kids[0]));
    BUBBLE(compile(vm, compiler, node->kids[1]));
    chunk_write(vm, compiler_chunk(compiler), OpMul);
    break;
  }
  case AstntBinaryDiv: {
    BUBBLE(compile(vm, compiler, node->kids[0]));
    BUBBLE(compile(vm, compiler, node->kids[1]));
    chunk_write(vm, compiler_chunk(compiler), OpDiv);
    break;
  }
  case AstntBinaryMod: {
    BUBBLE(compile(vm, compiler, node->kids[0]));
    BUBBLE(compile(vm, compiler, node->kids[1]));
    chunk_write(vm, compiler_chunk(compiler), OpMod);
    break;
  }
  case AstntBinaryEq: {
    BUBBLE(compile(vm, compiler, node->kids[0]));
    BUBBLE(compile(vm, compiler, node->kids[1]));
    chunk_write(vm, compiler_chunk(compiler), OpEq);
    break;
  }
  case AstntBinaryNe: {
    BUBBLE(compile(vm, compiler, node->kids[0]));
    BUBBLE(compile(vm, compiler, node->kids[1]));
    chunk_write(vm, compiler_chunk(compiler), OpNe);
    break;
  }
  case AstntBinaryGt: {
    BUBBLE(compile(vm, compiler, node->kids[0]));
    BUBBLE(compile(vm, compiler, node->kids[1]));
    chunk_write(vm, compiler_chunk(compiler), OpGt);
    break;
  }
  case AstntBinaryGe: {
    BUBBLE(compile(vm, compiler, node->kids[0]));
    BUBBLE(compile(vm, compiler, node->kids[1]));
    chunk_write(vm, compiler_chunk(compiler), OpGe);
    break;
  }
  case AstntBinaryLt: {
    BUBBLE(compile(vm, compiler, node->kids[0]));
    BUBBLE(compile(vm, compiler, node->kids[1]));
    chunk_write(vm, compiler_chunk(compiler), OpLt);
    break;
  }
  case AstntBinaryLe: {
    BUBBLE(compile(vm, compiler, node->kids[0]));
    BUBBLE(compile(vm, compiler, node->kids[1]));
    chunk_write(vm, compiler_chunk(compiler), OpLe);
    break;
  }
  case AstntBinaryOr: {
    BUBBLE(compile(vm, compiler, node->kids[0]));
    BUBBLE(compile(vm, compiler, node->kids[1]));
    chunk_write(vm, compiler_chunk(compiler), OpBinOr);
    break;
  }
  case AstntBinaryXor: {
    BUBBLE(compile(vm, compiler, node->kids[0]));
    BUBBLE(compile(vm, compiler, node->kids[1]));
    chunk_write(vm, compiler_chunk(compiler), OpBinXor);
    break;
  }
  case AstntBinaryAnd: {
    BUBBLE(compile(vm, compiler, node->kids[0]));
    BUBBLE(compile(vm, compiler, node->kids[1]));
    chunk_write(vm, compiler_chunk(compiler), OpBinAnd);
    break;
  }
  case AstntBinaryRShift: {
    BUBBLE(compile(vm, compiler, node->kids[0]));
    BUBBLE(compile(vm, compiler, node->kids[1]));
    chunk_write(vm, compiler_chunk(compiler), OpRShift);
    break;
  }
  case AstntBinaryLShift: {
    BUBBLE(compile(vm, compiler, node->kids[0]));
    BUBBLE(compile(vm, compiler, node->kids[1]));
    chunk_write(vm, compiler_chunk(compiler), OpLShift);
    break;
  }
  case AstntBinaryLogAnd: {
    BUBBLE(compile(vm, compiler, node->kids[0]));
    i32 if_false =
        chunk_write_jump(vm, compiler_chunk(compiler), OpJumpIfFalse);
    chunk_write(vm, compiler_chunk(compiler), OpPop);
    BUBBLE(compile(vm, compiler, node->kids[1]));
    chunk_patch_jump(compiler_chunk(compiler), if_false);
    break;
  }
  case AstntBinaryLogOr: {
    BUBBLE(compile(vm, compiler, node->kids[0]));
    i32 if_false =
        chunk_write_jump(vm, compiler_chunk(compiler), OpJumpIfFalse);
    i32 skip_right = chunk_write_jump(vm, compiler_chunk(compiler), OpJump);
    chunk_patch_jump(compiler_chunk(compiler), if_false);
    chunk_write(vm, compiler_chunk(compiler), OpPop);
    BUBBLE(compile(vm, compiler, node->kids[1]));
    chunk_patch_jump(compiler_chunk(compiler), skip_right);
    break;
  }
  case AstntBinaryLCompose: {
    AstNode *lhs = node->kids[0];
    AstNode *rhs = node->kids[1];

    compile_compose(vm, compiler, lhs, rhs, "BinaryLCompose");
    break;
  }
  case AstntBinaryRCompose: {
    AstNode *lhs = node->kids[0];
    AstNode *rhs = node->kids[1];
    compile_compose(vm, compiler, rhs, lhs, "BinaryRCompose");
    break;
  }
  case AstntBinaryLApply: {
    AstNode *lhs = node->kids[0];
    AstNode *rhs = node->kids[1];

    BUBBLE(compile(vm, compiler, lhs));
    BUBBLE(compile(vm, compiler, rhs));

    chunk_write(vm, compiler_chunk(compiler), OpCall);
    chunk_write_constant(vm, compiler_chunk(compiler), new_int(1));
    break;
  }
  case AstntBinaryRApply: {
    AstNode *lhs = node->kids[0];
    AstNode *rhs = node->kids[1];

    BUBBLE(compile(vm, compiler, rhs));
    BUBBLE(compile(vm, compiler, lhs));

    chunk_write(vm, compiler_chunk(compiler), OpCall);
    chunk_write_constant(vm, compiler_chunk(compiler), new_int(1));
    break;
  }
  case AstntFuncDef: {
    BUBBLE(compile_function(vm, compiler, node, ANON_FUNC_NAME));
    break;
  }
  case AstntFuncCall: {
    AstNode *callee = node->kids[0];
    BUBBLE(compile(vm, compiler, callee));

    u32 arg_count = 0;
    for (AstNode *arg_list = node->kids[1]; arg_list != NULL;
         arg_list = arg_list->kids[1]) {
      BUBBLE(compile(vm, compiler, arg_list->kids[0]));
      arg_count += 1;
    }

    chunk_write(vm, compiler_chunk(compiler), OpCall);
    chunk_write_constant(vm, compiler_chunk(compiler), new_int((i64)arg_count));
    break;
  }
  case AstntReturn: {
    AstNode *val = node->kids[0];
    if (val == NULL) {
      chunk_write_constant(vm, compiler_chunk(compiler), new_nil());
    } else {
      BUBBLE(compile(vm, compiler, val));
    }
    chunk_write(vm, compiler_chunk(compiler), OpReturn);
    break;
  }
  case AstntArray: {
    // Reverse the linked list
    AstNode *prev = NULL;
    AstNode *curr = node->kids[0];
    if (curr == NULL) {
      chunk_write_constant(vm, compiler_chunk(compiler), new_int(0));
      chunk_write(vm, compiler_chunk(compiler), OpMakeArray);
      return CarOk;
    }

    AstNode *next = curr->kids[1];
    while (curr != NULL) {
      curr->kids[1] = prev;
      prev = curr;
      curr = next;

      if (next != NULL) {
        next = next->kids[1];
      }
    }

    node->kids[0] = prev;

    i32 len = 0;
    for (AstNode *expr_list = node->kids[0]; expr_list != NULL;
         expr_list = expr_list->kids[1]) {
      BUBBLE(compile(vm, compiler, expr_list->kids[0]));
      len += 1;
    }
    chunk_write_constant(vm, compiler_chunk(compiler), new_int(len));
    chunk_write(vm, compiler_chunk(compiler), OpMakeArray);
    break;
  }
  case AstntArrayIndex: {
    BUBBLE(compile(vm, compiler, node->kids[0]));
    BUBBLE(compile(vm, compiler, node->kids[1]));
    chunk_write(vm, compiler_chunk(compiler), OpIndexArray);
    break;
  }
  case AstntIf: {
    // During parsing the following transformation happens:
    //
    // if x {} elif y {} elif z {} else {}
    //
    // if x {} else { if y { } else { if z { } else { }}}
    //
    // All nodes in this chain know where the else ends, and can thus jump past
    // it
    BUBBLE(compile(vm, compiler, node->kids[0]));
    i32 if_false_jump =
        chunk_write_jump(vm, compiler_chunk(compiler), OpJumpIfFalse);

    chunk_write(vm, compiler_chunk(compiler), OpPop);
    BUBBLE(compile(vm, compiler, node->kids[1]));
    i32 past_else = chunk_write_jump(vm, compiler_chunk(compiler), OpJump);

    chunk_patch_jump(compiler_chunk(compiler), if_false_jump);
    chunk_write(vm, compiler_chunk(compiler), OpPop);

    if (node->kids[2] != NULL) {
      BUBBLE(compile(vm, compiler, node->kids[2]));
    } else {
      chunk_write_constant(vm, compiler_chunk(compiler), new_nil());
    }

    chunk_patch_jump(compiler_chunk(compiler), past_else);

    break;
  }
  case AstntCase: {
    BUBBLE(compile(vm, compiler, node->kids[0]));
    BUBBLE(compile(vm, compiler, node->kids[1]));
    break;
  }
  case AstntCaseArm: {
    AstNode *value = node->kids[0];
    AstNode *branch = node->kids[1];
    AstNode *next_arm = node->kids[2];

    chunk_write(vm, compiler_chunk(compiler), OpDup);
    BUBBLE(compile(vm, compiler, value));
    chunk_write(vm, compiler_chunk(compiler), OpEq);

    i32 if_neq = chunk_write_jump(vm, compiler_chunk(compiler), OpJumpIfFalse);

    chunk_write(vm, compiler_chunk(compiler), OpPop);
    chunk_write(vm, compiler_chunk(compiler), OpPop);
    BUBBLE(compile(vm, compiler, branch));
    i32 past_else = chunk_write_jump(vm, compiler_chunk(compiler), OpJump);

    chunk_patch_jump(compiler_chunk(compiler), if_neq);
    chunk_write(vm, compiler_chunk(compiler), OpPop);

    if (next_arm != NULL) {
      BUBBLE(compile(vm, compiler, next_arm));
    } else {
      chunk_write(vm, compiler_chunk(compiler), OpPop);
      chunk_write_constant(vm, compiler_chunk(compiler), new_nil());
    }

    chunk_patch_jump(compiler_chunk(compiler), past_else);
    break;
  }
  case AstntBlock: {
    compiler_new_scope(compiler);
    AstNode *stmt = node->kids[0];
    for (; stmt != NULL && stmt->type == AstntStmts; stmt = stmt->kids[1]) {
      BUBBLE(compile(vm, compiler, stmt->kids[0]));
    }

    if (stmt != NULL) {
      BUBBLE(compile(vm, compiler, stmt));
    } else {
      chunk_write_constant(vm, compiler_chunk(compiler), new_nil());
    }
    compiler_end_scope(vm, compiler);
    break;
  }
  case AstntExprStmt: {
    BUBBLE(compile(vm, compiler, node->kids[0]));
    chunk_write(vm, compiler_chunk(compiler), OpPop);
    break;
  }
  case AstntAssignStmt: {
    AstNode *ident_node = node->kids[0];
    AstNode *value_node = node->kids[1];

    if (value_node->type == AstntFuncDef && value_node->kids[2]->val.b) {
      BUBBLE(compile_rec_func_def(vm, compiler, node));
      break;
    }

    BUBBLE(compile(vm, compiler, value_node));

    LambString *ident = cstr_to_lambstring(vm, ident_node->val.i);
    i32 idx = chunk_add_constant(vm, compiler_chunk(compiler),
                                 new_object((Object *)ident));

    if (compiler->scope_depth == 0) {
      chunk_write(vm, compiler_chunk(compiler), OpDefineGlobal);
      if (idx >= 256) {
        u8 hi = (idx >> 16) & 0xFF;
        u8 mi = (idx >> 8) & 0xFF;
        u8 lo = (idx >> 0) & 0xFF;

        chunk_write(vm, compiler_chunk(compiler), OpLongConstant);
        chunk_write(vm, compiler_chunk(compiler), hi);
        chunk_write(vm, compiler_chunk(compiler), mi);
        chunk_write(vm, compiler_chunk(compiler), lo);
      } else {
        chunk_write(vm, compiler_chunk(compiler), OpConstant);
        chunk_write(vm, compiler_chunk(compiler), (u8)idx);
      }
    } else {
      Local loc = {.depth = compiler->scope_depth,
                   .name = ident->chars,
                   .is_captured = false};
      local_arr_write(vm, &compiler->locals, loc);

      chunk_write(vm, compiler_chunk(compiler), OpDefineLocal);
      chunk_write_constant(vm, compiler_chunk(compiler),
                           new_int(compiler->locals.len));
    }

    break;
  }
  case AstntStmts: {
    for (AstNode *stmt = node; stmt != NULL; stmt = stmt->kids[1]) {
      BUBBLE(compile(vm, compiler, stmt->kids[0]));
    }
    break;
  }
  case AstntNodeList: {
    fprintf(stderr, "Unable to compile AstNode of kind: AstntNodeList");
    return CarUnsupportedAst;
  }
  default:
    fprintf(stderr, "Unable to compile AstNode of kind: (%d)", node->type);
    fprintf(
        stderr,
        "Default branch reached while matching on node->type in %s on line %d",
        __FILE__, __LINE__);
    return CarUnsupportedAst;
  }

  return CarOk;
}

#undef BUBBLE
