#ifndef VM_HEADER
#define VM_HEADER

#include "chunk.h"
#include "compiler.h"
#include "table.h"

#define MAX_FRAMES 1024
#define MAX_VALUES (MAX_FRAMES * UINT8_MAX)

typedef enum {
  InterpretOk,
  InterpretRuntimeError,
} InterpretResult;

typedef struct Callframe {
  LambClosure* closure;
  Value* slots;
  u8* ip;
} Callframe;

typedef struct Vm {
  Table strings;
  Table globals;

  Value* stack_top;
  Value stack[MAX_VALUES];
  
  Callframe frames[MAX_FRAMES];
  u16 frame_count;

  Object* poor_mans_gc;
} Vm;

void vm_init(Vm* vm);

void vm_free(Vm* vm);

void vm_push_stack(Vm* vm, Value val);

Value vm_pop_stack(Vm* vm);

InterpretResult vm_run(Vm* vm);

Chunk* vm_chunk(Vm* vm);

Callframe* vm_frame(Vm* vm);

#endif//VM_HEADER
