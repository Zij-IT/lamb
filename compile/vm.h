#ifndef VM_HEADER
#define VM_HEADER

#include "chunk.h"
#include "block.h"
#include "table.h"

#define STACK_MAX 256

// Forward declaration from 'object.h'
typedef struct Object Object;

typedef struct Vm {
  Chunk* chunk;
  u8* ip;
  Value* stack_top;
  Value stack[STACK_MAX];
  Object* poor_mans_gc;
  Table strings;
  Table globals;
  Block curr_block;
} Vm;

void vm_init(Vm* vm);

void vm_set_chunk(Vm* vm, Chunk* chunk);

void vm_reset_ip(Vm* vm);

void vm_free(Vm* vm);

void vm_push_stack(Vm* vm, Value val);

Value vm_pop_stack(Vm* vm);

void vm_run(Vm* vm);

#endif//VM_HEADER
