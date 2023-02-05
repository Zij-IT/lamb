#include <stdio.h>
#include <stdlib.h>

#include "../memory.h"
#include "./chunk.h"
#include "./value.h"

void chunk_init(Chunk *chunk) {
  chunk->len = 0;
  chunk->capacity = 0;
  chunk->bytes = NULL;
  value_arr_init(&chunk->constants);
}

void chunk_write(Chunk *chunk, u8 byte) {
  if (chunk->capacity < chunk->len + 1) {
    i32 old_cap = chunk->capacity;
    chunk->capacity = GROW_CAPACITY(old_cap);
    chunk->bytes = GROW_ARRAY(u8, chunk->bytes, old_cap, chunk->capacity);
  }

  chunk->bytes[chunk->len] = byte;
  chunk->len += 1;
}

i32 chunk_add_constant(Chunk *chunk, Value val) {
  value_arr_write(&chunk->constants, val);
  return chunk->constants.len - 1;
}

void chunk_write_constant(Chunk *chunk, Value val) {
  value_arr_write(&chunk->constants, val);
  i32 idx = chunk->constants.len - 1;

  if (idx >= 256) {
    u8 hi = (idx >> 16) & 0xFF;
    u8 mi = (idx >> 8) & 0xFF;
    u8 lo = (idx >> 0) & 0xFF;

    chunk_write(chunk, OpLongConstant);
    chunk_write(chunk, hi);
    chunk_write(chunk, mi);
    chunk_write(chunk, lo);
  } else {
    chunk_write(chunk, OpConstant);
    chunk_write(chunk, (u8)idx);
  }
}

i32 chunk_write_jump(Chunk *chunk, u8 op) {
  chunk_write(chunk, op);
  chunk_write(chunk, 0xff);
  chunk_write(chunk, 0xff);

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

void chunk_free(Chunk *chunk) {
  FREE_ARRAY(u8, chunk->bytes, chunk->capacity);
  value_arr_free(&chunk->constants);
  chunk_init(chunk);
}
