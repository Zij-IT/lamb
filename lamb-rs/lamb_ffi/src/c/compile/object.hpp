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
    Object *next;
    ObjectType type;
    bool is_marked;
};

struct LambString {
    Object obj;
    char const* chars;
    i32 len;
    u32 hash;

    static LambString* alloc(Vm& vm, char const* chars, i32 len, u32 hash);
};

struct LambArray {
    Object obj;
    GcVec<Value> items;
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
    Value closed;
    Value *location;
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

Object *alloc_obj(Vm& vm, ObjectType type);

bool is_of_type(Object *obj, ObjectType type);

void object_free(Vm& vm, Object *obj);

LambString *cstr_to_lambstring(Vm& vm, char const* cstr);

LambString *concat(Vm& vm, LambString *lhs, LambString *rhs);

LambClosure *to_closure(Vm& vm, LambFunc *func);

LambUpvalue *to_upvalue(Vm& vm, Value *slot);

#endif // OBJECT_HEADER
