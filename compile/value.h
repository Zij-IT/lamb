#ifndef VALUE_HEADER
#define VALUE_HEADER

#include "../types.h"

// Forward declare from object.h
typedef struct Object Object;
typedef struct LambString LambString;

typedef enum {
  VkBool,
  VkInt,
  VkDouble,
  VkChar,
  VkObj,
  VkNil,
} ValueKind;

typedef struct {
  ValueKind kind;  
  union {
    bool boolean;
    i64 intn;
    f64 doubn;
    char ch;
    Object* obj;
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

bool is_object(Value val);

bool is_nil(Value val);

Value new_nil();

Value new_boolean(bool b);

Value new_int(i64 num);

Value new_double(double num);

Value new_char(char c);

Value new_object(Object* obj);

void print_kind(Value val);

void arr_init(ValueArray* arr);

void arr_write(ValueArray* arr, Value val);

void arr_free(ValueArray* arr);

#endif//VALUE_HEADER
