#include "block.h"
#include "chunk.h"

void block_init(Block* block) {
  block->scope_depth = 0;
  block->local_count = 0;
}

void block_new_scope(Block* block) {
  block->scope_depth++;
}

void block_end_scope(Chunk* chunk, Block* block) {
  block->scope_depth--;
  
  while(block->local_count > 0 && block->locals[block->local_count - 1].depth > block->scope_depth) {
    chunk_write(chunk, OpPop);
    block->local_count--;
  }
}
