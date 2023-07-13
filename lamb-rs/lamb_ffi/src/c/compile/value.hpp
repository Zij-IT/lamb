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

    static Value from_bool(bool b);

    static Value from_i64(i64 i);

    static Value from_f64(f64 i);

    static Value from_char(char c);

    static Value from_obj(Object* o);

    static Value nil();
    
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

Order value_compare(Value *lhs, Value *rhs);

char const* kind_as_cstr(Value val);

void value_arr_init(ValueArray *arr);

void value_arr_write(Vm *vm, ValueArray *arr, Value val);

void value_arr_free(Vm *vm, ValueArray *arr);

#endif // VALUE_HEADER
