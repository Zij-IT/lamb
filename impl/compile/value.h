#ifndef VALUE_HEADER
#define VALUE_HEADER

#include "../types.h"

// Forward declare from object.h
typedef struct Object Object;
typedef struct LambString LambString;
typedef struct Vm Vm;

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
    Object *obj;
  } as;
} Value;

typedef struct {
  i32 capacity;
  i32 len;
  Value *values;
} ValueArray;

typedef enum Order {
  OrderLess = -1,
  OrderEqual = 0,
  OrderGreater = 1,
} Order;

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

Value new_object(Object *obj);

Order value_compare(Value *lhs, Value *rhs);

str kind_as_cstr(Value val);

void value_arr_init(ValueArray *arr);

void value_arr_write(Vm* vm, ValueArray *arr, Value val);

void value_arr_free(Vm* vm, ValueArray *arr);

#endif // VALUE_HEADER
