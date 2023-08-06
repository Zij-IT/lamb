#ifndef CHUNK_HEADER
#define CHUNK_HEADER

#include <optional>
#include <string>
#include <tuple>

#include "../types.hpp"
#include "gcvec.hpp"
#include "value.hpp"

using InstrFormat = std::tuple<std::string, std::optional<std::string>, u32>;

enum OpCode {
    OpConstant,
    OpDefineGlobal,
    OpGetGlobal,
    OpGetLocal,
    OpGetUpvalue,
    OpNumNeg,
    OpBinNeg,
    OpLogNeg,
    OpAdd,
    OpSub,
    OpMul,
    OpMod,
    OpDiv,
    OpBinAnd,
    OpBinOr,
    OpBinXor,
    OpEq,
    OpNe,
    OpGt,
    OpGe,
    OpLt,
    OpLe,
    OpRShift,
    OpLShift,
    OpReturn,
    OpJump,
    OpJumpIfTrue,
    OpJumpIfFalse,
    OpMakeArray,
    OpLen,
    OpIndex,
    OpIndexRev,
    OpPop,
    OpDup,
    OpCall,
    OpClosure,
    OpCloseValue,
    OpSaveValue,
    OpUnsaveValue,
    OpSetSlot,
};

struct Chunk {
    GcVec<u8> bytes;
    GcVec<Value> constants;

    Chunk();

    void destroy(Vm &vm);

    void write(Vm &vm, u8 byte);

    i32 write_jump(Vm &vm, u8 op);
    void patch_jump(i32 offset) const;

    void write_const(Vm &vm, Value val);
    i32 add_const(Vm &vm, Value val);

    std::string to_string();

  private:
    [[nodiscard]] std::tuple<std::string, std::optional<std::string>, u32>
    format_instruction(u32 offset) const;
};

#endif // CHUNK_HEADER
