#include <stdlib.h>
#include "../memory.h"
#include "compiler.h"
#include "chunk.h"
#include "table.h"

void compiler_init(Compiler* compiler, FuncType type) {
  compiler->enclosing = NULL;
  compiler->function = NULL;
  compiler->scope_depth = 0;
  compiler->type = type;
  local_arr_init(&compiler->locals);
  
  Local loc = { .name = "", .depth = 0 };
  local_arr_write(&compiler->locals, loc);
}

void compiler_free(Compiler* compiler) {
  local_arr_free(&compiler->locals);
  local_arr_init(&compiler->locals);
}

void compiler_new_scope(Compiler* compiler) {
  compiler->scope_depth++;
}

void compiler_end_scope(Compiler* compiler) {
  compiler->scope_depth--;
  
  while(compiler->locals.len > 0 && compiler->locals.values[compiler->locals.len - 1].depth > compiler->scope_depth) {
    chunk_write(&compiler->function->chunk, OpPop);
    compiler->locals.len--;
  }
}

void local_arr_init(LocalArray* arr) {
  arr->len = 0;
  arr->capacity = 0;
  arr->values = NULL;
}

void local_arr_write(LocalArray* arr, Local val) {
  if(arr->capacity < arr->len + 1) {
    i32 old_cap = arr->capacity;
    arr->capacity = GROW_CAPACITY(old_cap);
    arr->values = GROW_ARRAY(Local, arr->values, old_cap, arr->capacity);
  }  

  arr->values[arr->len] = val;
  arr->len += 1;
}

void local_arr_free(LocalArray* arr) {
  FREE_ARRAY(Value, arr->values, arr->capacity);
  local_arr_init(arr);
}
