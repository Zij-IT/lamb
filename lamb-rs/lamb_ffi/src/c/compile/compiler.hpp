#ifndef BLOCK_HEADER
#define BLOCK_HEADER

#include "object.hpp"

typedef struct {
    char const* name;
    i32 depth;
    bool is_captured;
} Local;

typedef struct {
    i32 capacity;
    i32 len;
    Local *values;
} LocalArray;

typedef struct {
    u8 index;
    bool is_local;
} Upvalue;

typedef struct Block {
    i32 base;
    i32 offset;
    i32 depth;
    struct Block *prev;
} Block;

struct Compiler {
    LocalArray locals;

    LambFunc *function;
    Upvalue upvalues[UINT8_MAX];

    struct Compiler *enclosing;
    Block *block;

    FuncType type;

    Compiler(Vm* vm, Compiler* enclosing, Block* block, FuncType type, char const* name, i32 arity);

    void add_local(Vm* vm, Local loc);

    void new_scope();

    void end_scope(Vm* vm);

    Chunk* chunk();

    void destroy(Vm* vm);
};

#endif // BLOCK_HEADER
