#include "block.h"

void block_init(Block* block) {
  block->scope_depth = 0;
  block->local_count = 0;
}

void block_new_scope(Block* block) {
  block->scope_depth++;
}

void block_end_scope(Block* block) {
  block->scope_depth--;
}
