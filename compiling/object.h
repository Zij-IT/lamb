#ifndef OBJECT_HEADER
#define OBJECT_HEADER

#include "../types.h"

typedef enum {
  OtString,
} ObjectType;

typedef struct {
  ObjectType type;
} Obj;

typedef struct {
  Obj obj;
  i32 len;
  string chars;
} ObjString;

#endif//OBJECT_HEADER
