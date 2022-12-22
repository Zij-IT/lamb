#ifndef VM_HEADER
#define VM_HEADER

#include "chunk.h"

#define STACK_MAX 256

typedef struct {
  Chunk* chunk;
  u8* ip;
  Value* stack_top;
  Value stack[STACK_MAX];
} Vm;

void vm_init(Vm* vm);

void vm_init_with_chunk(Vm* vm, Chunk* chunk);

void vm_free(Vm* vm);

void vm_push_value(Vm* vm, Value val);

Value vm_pop_value(Vm* vm);

void vm_run(Vm* vm);

#endif//VM_HEADER
