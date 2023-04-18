#ifndef VM_HEADER
#define VM_HEADER

#include "../compile/chunk.h"
#include "../compile/compiler.h"
#include "../compile/table.h"

#define MAX_FRAMES 1024
#define MAX_VALUES (MAX_FRAMES * UINT8_MAX)

typedef enum {
  InterpretOk,
  InterpretRuntimeError,
} InterpretResult;

typedef struct VmOptions {
  bool print_main_chunk;
  bool print_fn_chunks;
  bool print_ast;
  bool optimized;
} VmOptions;

typedef struct Callframe {
  LambClosure *closure;
  Value *slots;
  u8 *ip;
} Callframe;

typedef struct Vm {
  Table strings;
  Table globals;
  
  ObjectPtrArray gray_stack;
  Compiler* curr_compiler;
  u64 bytes_allocated;
  u64 next_collection;

  Value *stack_top;
  Value stack[MAX_VALUES];
  Value saved_value;
  
  Callframe frames[MAX_FRAMES];
  u16 frame_count;

  LambUpvalue *open_upvalues;
  Object *poor_mans_gc;

  VmOptions options;
} Vm;

void vm_init(Vm *vm, VmOptions options);

void vm_free(Vm *vm);

void vm_push_stack(Vm *vm, Value val);

Value vm_pop_stack(Vm *vm);

InterpretResult vm_run(Vm *vm);

#endif // VM_HEADER
