#ifndef VALUE_HEADER
#define VALUE_HEADER

#include "../types.hpp"

// Forward declare from object.h
struct Object;
struct LambString;
struct Vm;

enum ValueKind {
    VkBool,
    VkInt,
    VkDouble,
    VkChar,
    VkObj,
    VkNil,
};

struct Value {
    ValueKind kind;
    union {
        bool boolean;
        i64 intn;
        f64 doubn;
        char ch;
        Object *obj;
    } as;

    
    bool is_bool();

    bool is_double();

    bool is_integer();

    bool is_char();

    bool is_object();

    bool is_nil();
};

struct ValueArray {
    i32 capacity;
    i32 len;
    Value *values;
};

enum Order {
    OrderLess = -1,
    OrderEqual = 0,
    OrderGreater = 1,
};

Value new_nil();

Value new_boolean(bool b);

Value new_int(i64 num);

Value new_double(double num);

Value new_char(char c);

Value new_object(Object *obj);

Order value_compare(Value *lhs, Value *rhs);

char const* kind_as_cstr(Value val);

void value_arr_init(ValueArray *arr);

void value_arr_write(Vm *vm, ValueArray *arr, Value val);

void value_arr_free(Vm *vm, ValueArray *arr);

#endif // VALUE_HEADER
