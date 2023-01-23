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
} Compiler;

void compiler_init(Compiler* compiler);

void compiler_free(Compiler* compiler);

void compiler_new_scope(Compiler* compiler);

void compiler_end_scope(Chunk* chunk, Compiler* compiler);

void compiler_declare_var(Compiler* compiler, str name);

void local_arr_init(LocalArray* arr);

void local_arr_write(LocalArray* arr, Local val);

void local_arr_free(LocalArray* arr);

#endif//BLOCK_HEADER