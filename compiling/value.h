#ifndef VALUE_HEADER
#define VALUE_HEADER

#include "../types.h"
#include "object.h"

typedef enum {
  VkBool,
  VkInt,
  VkDouble,
  VkChar,
  VkObject,
} ValueKind;

typedef struct {
  ValueKind kind;  
  union {
    bool boolean;
    i64 intn;
    f64 doubn;
    char ch;
    Obj* obj;
  } as;
} Value;

typedef struct {
  i32 capacity;
  i32 len;
  Value* values;
} ValueArray;

bool is_bool(Value val);

bool is_double(Value val);

bool is_integer(Value val);

bool is_char(Value val);

bool is_obj(Value val);

bool is_obj_of_type(Value val, ObjectType type);

Value new_boolean(bool b);

Value new_int(i64 num);

Value new_double(double num);

Value new_string(string st);

Value new_char(char c);

Value new_obj(Obj* obj);

ObjString* as_string(Value val);

str as_cstring(Value val);

void arr_init(ValueArray* arr);

void arr_write(ValueArray* arr, Value val);

void arr_free(ValueArray* arr);

#endif//VALUE_HEADER
