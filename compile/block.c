#include <stdlib.h>
#include "../memory.h"
#include "block.h"
#include "chunk.h"
#include "table.h"

void block_init(Block* block) {
  block->scope_depth = 0;
  local_arr_init(&block->locals);
}

void block_free(Block* block) {
  local_arr_free(&block->locals);
  local_arr_init(&block->locals);
}

void block_new_scope(Block* block) {
  block->scope_depth++;
}

void block_end_scope(Chunk* chunk, Block* block) {
  block->scope_depth--;
  
  while(block->locals.len > 0 && block->locals.values[block->locals.len - 1].depth > block->scope_depth) {
    chunk_write(chunk, OpPop);
    block->locals.len--;
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
