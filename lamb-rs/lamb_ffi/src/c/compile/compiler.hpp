#ifndef BLOCK_HEADER
#define BLOCK_HEADER

#include <limits>
#include <optional>

#include "../types.hpp"
#include "chunk.hpp"
#include "gcvec.hpp"
#include "object.hpp"

struct Local {
    char const *name;
    i32 depth;
    bool is_captured;
};

struct LocalArray {
    Local *values;
    i32 capacity;
    i32 len;
};

struct Upvalue {
    u8 index;
    bool is_local;
};

struct Block {
    Block *prev;
    i32 base;
    i32 offset;
    i32 depth;
};

struct Compiler {
    Upvalue upvalues[std::numeric_limits<u8>::max()];
    GcVec<Local> locals;

    LambFunc *function;
    Compiler *enclosing;
    Block *block;

    FuncType type;

    Compiler(Vm &vm, Compiler *enclosing, Block *block, FuncType type, char const *name, i32 arity);

    void add_local(Vm &vm, char const *name);

    std::optional<i32> local_slot(LambString *name);

    std::optional<i32> local_idx(LambString *name);

    std::optional<i32> upvalue_idx(LambString *name);

    constexpr void new_scope() const { this->block->depth++; }

    void end_scope(Vm &vm);

    [[nodiscard]] constexpr Chunk &chunk() const { return this->function->chunk; }

    void destroy(Vm &vm);

    void write_op(Vm &vm, OpCode op) const;

    void write_op_arg(Vm &vm, Value val) const;

    void write_vop(Vm &vm, OpCode op, i32 args) const;

    void write_byte(Vm &vm, u8 byte);

    void write_const(Vm &vm, Value val) const;

  private:
    std::optional<i32> add_upvalue(i32 idx, bool is_local);
};

#endif // BLOCK_HEADER
