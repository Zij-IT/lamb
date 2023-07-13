#ifndef CHUNK_HEADER
#define CHUNK_HEADER

#include "./value.hpp"
#include "gcvec.hpp"

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
    OpJumpIfFalse,
    OpMakeArray,
    OpIndexArray,
    OpPop,
    OpDup,
    OpCall,
    OpClosure,
    OpCloseValue,
    OpSaveValue,
    OpUnsaveValue,
};

struct Chunk {
    GcVec<u8> bytes;

    GcVec<Value> constants;

    Chunk();
   
    void destroy(Vm* vm);

    void write(Vm* vm, u8 byte);

    i32 write_jump(Vm* vm, u8 op);
    void patch_jump(i32 offset);

    void write_const(Vm* vm, Value val);
    i32 add_const(Vm* vm, Value val);
};

#endif // CHUNK_HEADER
