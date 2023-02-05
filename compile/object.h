#ifndef OBJECT_HEADER
#define OBJECT_HEADER

#include "../types.h"
#include "chunk.h"

// Forward declare vm in vm.h
typedef struct Vm Vm;

typedef Value (*CFunc)(i32 args_passed, Value* args);

typedef enum {
  OtString,
  OtArray,
  OtFunc,
  OtNative,
  OtClosure,
  OtUpvalue,
} ObjectType;

typedef struct Object {
  ObjectType type;
  struct Object* next;
} Object;

typedef struct LambString {
  Object obj;
  i32 len;
  string chars;
  u32 hash;
} LambString;

typedef struct LambArray {
  Object obj;
  ValueArray items;  
} LambArray;

typedef struct LambFunc {
  Object obj;
  Chunk chunk;
  str name;
  i32 upvalue_count;
  u8 arity;
} LambFunc;

typedef struct NativeFunc {
  Object obj;
  CFunc func;
} NativeFunc;

typedef struct LambUpvalue {
  Object obj;
  Value* location;
  Value closed;
  struct LambUpvalue* next;
} LambUpvalue;

typedef struct LambClosure {
  Object obj;
  LambFunc* function;
  LambUpvalue** upvalues;
  i32 upvalue_count;
} LambClosure;

typedef enum FuncType {
  FtScript,
  FtNormal,
} FuncType;

Object* alloc_obj(Vm* vm, ObjectType type);

bool is_of_type(Object* obj, ObjectType type);

void object_free(Object* obj);

LambString* cstr_to_lambstring(Vm* vm, str cstr);

LambString* concat(Vm* vm, LambString* lhs, LambString* rhs);

LambClosure* to_closure(Vm* vm, LambFunc* func);

LambUpvalue* to_upvalue(Vm* vm, Value* slot);

#endif//OBJECT_HEADER
