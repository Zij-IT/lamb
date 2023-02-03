#ifndef VM_HEADER
#define VM_HEADER

#include "chunk.h"
#include "compiler.h"
#include "table.h"

#define STACK_MAX 256

typedef enum {
  InterpretOk,
  InterpretRuntimeError,
} InterpretResult;

// Forward declaration from 'object.h'
typedef struct Object Object;

typedef struct Vm {
  u8* ip;
  Value* stack_top;
  Value stack[STACK_MAX];
  Object* poor_mans_gc;
  Table strings;
  Table globals;
  Compiler* curr_compiler;
} Vm;

void vm_init(Vm* vm);

void vm_reset_ip(Vm* vm);

void vm_free(Vm* vm);

void vm_push_stack(Vm* vm, Value val);

Value vm_pop_stack(Vm* vm);

InterpretResult vm_run(Vm* vm);

Chunk* vm_chunk(Vm* vm);

#endif//VM_HEADER
