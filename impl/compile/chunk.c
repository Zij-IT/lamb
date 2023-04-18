#include <stdio.h>
#include <stdlib.h>

#include "../vm/vm.h"
#include "./chunk.h"
#include "./value.h"
#include "memory.h"

void chunk_init(Chunk *chunk) {
  chunk->len = 0;
  chunk->capacity = 0;
  chunk->bytes = NULL;
  value_arr_init(&chunk->constants);
}

void chunk_write(Vm *vm, Chunk *chunk, u8 byte) {
  if (chunk->capacity < chunk->len + 1) {
    i32 old_cap = chunk->capacity;
    chunk->capacity = GROW_CAPACITY(old_cap);
    chunk->bytes = GROW_ARRAY(vm, u8, chunk->bytes, old_cap, chunk->capacity);
  }

  chunk->bytes[chunk->len] = byte;
  chunk->len += 1;
}

i32 chunk_add_constant(Vm *vm, Chunk *chunk, Value val) {
  vm_push_stack(vm, val);
  value_arr_write(vm, &chunk->constants, val);
  vm_pop_stack(vm);
  return chunk->constants.len - 1;
}

void chunk_write_constant(Vm *vm, Chunk *chunk, Value val) {
  i32 idx = chunk_add_constant(vm, chunk, val);

  if (idx >= 256) {
    u8 hi = (idx >> 16) & 0xFF;
    u8 mi = (idx >> 8) & 0xFF;
    u8 lo = (idx >> 0) & 0xFF;

    chunk_write(vm, chunk, OpLongConstant);
    chunk_write(vm, chunk, hi);
    chunk_write(vm, chunk, mi);
    chunk_write(vm, chunk, lo);
  } else {
    chunk_write(vm, chunk, OpConstant);
    chunk_write(vm, chunk, (u8)idx);
  }
}

i32 chunk_write_jump(Vm *vm, Chunk *chunk, u8 op) {
  chunk_write(vm, chunk, op);
  chunk_write(vm, chunk, 0xff);
  chunk_write(vm, chunk, 0xff);

  return chunk->len - 2;
}

void chunk_patch_jump(Chunk *chunk, i32 offset) {
  i32 jump = chunk->len - offset - 2;
  if (jump > UINT16_MAX) {
    fprintf(stderr, "COMPILE_ERR: jump exceeds maximal bytes of %d",
            UINT16_MAX);
    exit(1);
  }

  chunk->bytes[offset] = (jump >> 8) & 0xff;
  chunk->bytes[offset + 1] = jump & 0xff;
}

void chunk_free(Vm *vm, Chunk *chunk) {
  FREE_ARRAY(vm, u8, chunk->bytes, chunk->capacity);
  value_arr_free(vm, &chunk->constants);
  chunk_init(chunk);
}
