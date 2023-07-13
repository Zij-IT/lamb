#ifndef OBJECT_HEADER
#define OBJECT_HEADER

#include "../types.hpp"
#include "chunk.hpp"

// Forward declare vm in vm.h
struct Vm;

typedef Value (*CFunc)(i32 args_passed, Value *args);

enum ObjectType {
    OtString,
    OtArray,
    OtFunc,
    OtNative,
    OtClosure,
    OtUpvalue,
};

struct Object {
    ObjectType type;
    bool is_marked;
    Object *next;
};

struct LambString {
    Object obj;
    i32 len;
    string chars;
    u32 hash;
};

struct LambArray {
    Object obj;
    ValueArray items;
};

struct LambFunc {
    Object obj;
    Chunk chunk;
    char const* name;
    i32 upvalue_count;
    u8 arity;
};

struct NativeFunc {
    Object obj;
    CFunc func;
};

struct LambUpvalue {
    Object obj;
    Value *location;
    Value closed;
    LambUpvalue *next;
};

struct LambClosure {
    Object obj;
    LambFunc *function;
    LambUpvalue **upvalues;
    i32 upvalue_count;
};

enum FuncType {
    FtScript,
    FtNormal,
};

struct ObjectPtrArray {
    Object **values;
    i32 capacity;
    i32 len;
};

Object *alloc_obj(Vm *vm, ObjectType type);

bool is_of_type(Object *obj, ObjectType type);

void object_free(Vm *vm, Object *obj);

LambString *cstr_to_lambstring(Vm *vm, char const* cstr);

LambString *concat(Vm *vm, LambString *lhs, LambString *rhs);

LambClosure *to_closure(Vm *vm, LambFunc *func);

LambUpvalue *to_upvalue(Vm *vm, Value *slot);

void objectptr_array_init(ObjectPtrArray *arr);

void objectptr_array_write(Vm *vm, ObjectPtrArray *arr, Object *val);

void objectptr_array_free(Vm *vm, ObjectPtrArray *arr);

#endif // OBJECT_HEADER
