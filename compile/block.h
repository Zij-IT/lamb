#ifndef BLOCK_HEADER
#define BLOCK_HEADER

#include "../types.h"

// This is a limitation imposed by Crafting Intepreters that actually
// isn't necessary due to the current implementation of LONG_CONSTANT
// I'm lazy though, so here it goes.
#define MAX_LOCAL_COUNT 256

typedef struct {
  // TODO: Check later how Locals are created. They may need to be string and not str
  str name;
  i32 depth;
} Local;

typedef struct {
  Local locals[MAX_LOCAL_COUNT];
  i32 local_count;
  i32 scope_depth;
} Block;

void block_init(Block* block);

void block_new_scope(Block* block);

void block_end_scope(Block* block);

void block_declare_var(Block* block, str name);

#endif//BLOCK_HEADER
