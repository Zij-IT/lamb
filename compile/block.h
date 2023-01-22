#ifndef BLOCK_HEADER
#define BLOCK_HEADER

#include "../types.h"

typedef struct LambString LambString; 
typedef struct Chunk Chunk;

typedef struct {
  LambString* name;
  i32 depth;
} Local;

typedef struct {
  i32 capacity;
  i32 len;
  Local* values;
} LocalArray;

typedef struct {
  LocalArray locals;
  i32 scope_depth;
} Block;

void block_init(Block* block);

void block_free(Block* block);

void block_new_scope(Block* block);

void block_end_scope(Chunk* chunk, Block* block);

void block_declare_var(Block* block, str name);

void local_arr_init(LocalArray* arr);

void local_arr_write(LocalArray* arr, Local val);

void local_arr_free(LocalArray* arr);

#endif//BLOCK_HEADER
