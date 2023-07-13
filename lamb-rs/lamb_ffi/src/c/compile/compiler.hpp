#ifndef BLOCK_HEADER
#define BLOCK_HEADER

#include "object.hpp"

struct Local {
    char const* name;
    i32 depth;
    bool is_captured;
};

struct LocalArray {
    i32 capacity;
    i32 len;
    Local *values;
};

struct Upvalue {
    u8 index;
    bool is_local;
};

struct Block {
    i32 base;
    i32 offset;
    i32 depth;
    Block *prev;
};

struct Compiler {
    LocalArray locals;

    LambFunc *function;
    Upvalue upvalues[UINT8_MAX];

    Compiler *enclosing;
    Block *block;

    FuncType type;

    Compiler(Vm* vm, Compiler* enclosing, Block* block, FuncType type, char const* name, i32 arity);

    void add_local(Vm* vm, char const* name);

    void new_scope();

    void end_scope(Vm* vm);

    Chunk* chunk();

    void destroy(Vm* vm);
};

#endif // BLOCK_HEADER
