#ifndef OBJECT_HEADER
#define OBJECT_HEADER

#include "../types.h"

// Forward declare vm in vm.h
typedef struct Vm Vm;

typedef enum {
  OtString,
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

Object* alloc_obj(Vm* vm, ObjectType type);

bool is_of_type(Object* obj, ObjectType type);

void object_free(Object* obj);

LambString* concat(Vm* vm, LambString* lhs, LambString* rhs);

#endif//OBJECT_HEADER
