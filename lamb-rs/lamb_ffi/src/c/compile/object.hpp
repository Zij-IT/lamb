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

    constexpr bool is(ObjectType type) const { return this->type == type; }

    std::string to_string() const;
};

struct LambString {
    Object obj;
    char const* chars;
    i32 len;
    u32 hash;

    static LambString* alloc(Vm& vm, char const* chars, i32 len, u32 hash);

    static LambString* from_cstr(Vm&vm, char const* chars);

    LambString* concat(Vm& vm, LambString* rhs);
};

struct LambArray {
    Object obj;
    GcVec<Value> items;

    static LambArray* alloc(Vm& vm, GcVec<Value> items);
};

struct LambFunc {
    Object obj;
    Chunk chunk;
    char const* name;
    i32 upvalue_count;
    u8 arity;

    static LambFunc* alloc(Vm& vm, char const* name, u8 arity);
};

struct NativeFunc {
    Object obj;
    CFunc func;

    static NativeFunc* alloc(Vm& vm, CFunc func);
};

struct LambUpvalue {
    Object obj;
    Value closed;
    Value *location;
    LambUpvalue *next;

    static LambUpvalue* alloc(Vm& vm, Value* slot, LambUpvalue* next);
};

struct LambClosure {
    Object obj;
    LambFunc *function;
    LambUpvalue **upvalues;
    i32 upvalue_count;

    static LambClosure* alloc(Vm& vm, LambFunc* func);
};

enum FuncType {
    FtScript,
    FtNormal,
};

void object_free(Vm& vm, Object *obj);

#endif // OBJECT_HEADER
