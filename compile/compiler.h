#ifndef BLOCK_HEADER
#define BLOCK_HEADER

#include "object.h"

typedef struct {
  str name;
  i32 depth;
  bool is_captured;
} Local;

typedef struct {
  i32 capacity;
  i32 len;
  Local *values;
} LocalArray;

typedef struct {
  u8 index;
  bool is_local;
} Upvalue;

typedef struct Compiler {
  LocalArray locals;

  LambFunc *function;
  Upvalue upvalues[UINT8_MAX];

  struct Compiler *enclosing;

  i32 scope_depth;
  FuncType type;
} Compiler;

void compiler_init(Vm* vm, Compiler *compiler, FuncType type);

void compiler_free(Compiler *compiler);

void compiler_new_scope(Compiler *compiler);

void compiler_end_scope(Vm* vm, Compiler *compiler);

void compiler_declare_var(Compiler *compiler, str name);

void local_arr_init(LocalArray *arr);

void local_arr_write(Vm* vm, LocalArray *arr, Local val);

void local_arr_free(LocalArray *arr);

#endif // BLOCK_HEADER
