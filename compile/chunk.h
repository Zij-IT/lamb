#ifndef CHUNK_HEADER
#define CHUNK_HEADER

#include "../types.h"
#include "./value.h"

typedef enum {
  OpConstant,
  OpLongConstant,
  OpDefineGlobal,
  OpDefineLocal,
  OpGetGlobal,
  OpGetLocal,
  OpNumNeg,
  OpBinNeg,
  OpLogNeg,
  OpAdd,
  OpSub,
  OpMul,
  OpMod,
  OpDiv,
  OpLApply,
  OpRApply,
  OpLCompose,
  OpRCompose,
  OpLogAnd,
  OpLogOr,
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
  OpPop,
  OpDup,
  OpHalt,
} OpCode;

typedef struct Chunk {
  i32 capacity;
  i32 len;
  u8* bytes;
  ValueArray constants;
} Chunk;

void chunk_init(Chunk* chunk);

void chunk_write(Chunk* chunk, u8 byte);

void chunk_free(Chunk* chunk);

void chunk_write_constant(Chunk* chunk, Value val); 

i32 chunk_write_jump(Chunk* chunk, u8 op);

void chunk_patch_jump(Chunk* chunk, i32 offset);
#endif//CHUNK_HEADER
