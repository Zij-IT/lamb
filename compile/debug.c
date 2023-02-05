#include <stdio.h>

#include "debug.h"

static i32 print_simple_op(str name) {
  printf("%s\n", name);
  return 1;
}

static i32 print_jump(Chunk* chunk, str name, i32 offset, i32 sign) {
  u16 jump = (u16)(chunk->bytes[offset + 1] << 8);
  jump |= chunk->bytes[offset + 2];

  printf("%-16s %4d -> %d\n", name, offset, offset + 3 + sign * jump);
  return 3;
}

static i32 print_constant(Chunk* chunk, str name, i32 offset, bool is_long) {
  if (is_long) {
    u8 lo = chunk->bytes[offset + 3];
    u8 mi = chunk->bytes[offset + 2];
    u8 hi = chunk->bytes[offset + 1];
    
    i32 idx = ((i32)hi) << 16 | ((i32)mi) << 8 | (i32)lo;
    Value val = chunk->constants.values[idx];
    printf("%-16s %4d '", name, idx);
    print_value(val);
    printf("'\n");
    return 4;
  } else {
    u8 idx = chunk->bytes[offset + 1];
    Value val = chunk->constants.values[idx];

    printf("%-16s %4d '", name, idx);
    print_value(val);
    printf("'\n");
    return 2;
  }
}

static i32 print_op(Chunk* chunk, i32 offset) {
  switch(chunk->bytes[offset]) {
    case OpConstant: return print_constant(chunk, "OpConstant", offset, false);
    case OpLongConstant: return print_constant(chunk, "OpLongConstant", offset, true);
    case OpDefineGlobal: return print_simple_op("OpDefineGlobal");
    case OpGetGlobal: return print_simple_op("OpGetGlobal");
    case OpDefineLocal: return print_simple_op("OpDefineLocal");
    case OpGetLocal: return print_simple_op("OpGetLocal");
    case OpJump: return print_jump(chunk, "OpJump", offset, 1);
    case OpJumpIfFalse: return print_jump(chunk, "OpJumpIfFalse", offset, 1);
    case OpNumNeg: return print_simple_op("OpNumNeg");
    case OpBinNeg: return print_simple_op("OpBinNeg");
    case OpLogNeg: return print_simple_op("OpLogNeg");
    case OpAdd: return print_simple_op("OpAdd");
    case OpSub: return print_simple_op("OpSub");
    case OpMul: return print_simple_op("OpMul");
    case OpMod: return print_simple_op("OpMod");
    case OpDiv: return print_simple_op("OpDiv");
    case OpBinAnd: return print_simple_op("OpBinAnd");
    case OpBinOr: return print_simple_op("OpBinOr");
    case OpBinXor: return print_simple_op("OpBinXor");
    case OpEq: return print_simple_op("OpEq");
    case OpNe: return print_simple_op("OpNe");
    case OpGt: return print_simple_op("OpGt");
    case OpGe: return print_simple_op("OpGe");
    case OpLt: return print_simple_op("OpLt");
    case OpLe: return print_simple_op("OpLe");
    case OpRShift: return print_simple_op("OpRShift");
    case OpLShift: return print_simple_op("OpLShift");
    case OpReturn: return print_simple_op("OpReturn");
    case OpMakeArray: return print_simple_op("OpMakeArray");
    case OpIndexArray: return print_simple_op("OpIndexArray");
    case OpPop: return print_simple_op("OpPop");
    case OpDup: return print_simple_op("OpDup");
    case OpCall: return print_simple_op("OpCall");
    case OpCloseValue: return print_simple_op("OpCloseValue");
    case OpGetUpvalue: return print_simple_op("OpGetUpvalue");
    case OpClosure: {
      printf("OpClosure\n");
      i32 x = 0;
      i32 idx = 0;
      if (chunk->bytes[offset + 1] == OpConstant) {
        x = 2;
        idx = chunk->bytes[offset + x];
      } else {
        x = 4;
        u8 lo = chunk->bytes[offset + 4];
        u8 mi = chunk->bytes[offset + 3];
        u8 hi = chunk->bytes[offset + 2];

        idx = ((i32)hi) << 16 | ((i32)mi) << 8 | (i32)lo;
      }
      
      LambFunc* func = (LambFunc*)chunk->constants.values[idx].as.obj;
      
      // NOTE: x bytes for constant + 2 bytes per upvalue + 1 to jump over OpClosure 
      return x + 2 * func->upvalue_count + 1;
    }
    default:
      fprintf(stderr, "Unknown OpCode (%d) in switch in %s at %d", chunk->bytes[offset], __FILE__, __LINE__);
      return 1;
  }
}

void chunk_debug(Chunk* chunk, str name) {
  printf("====== %s ======\n", name);
  i32 offset = 0;
  while (offset < chunk->len) {
    offset += print_op(chunk, offset);    
  }
  printf("====== %s ======\n", name);
}


void print_value(Value v)  {
  switch(v.kind) {
    case VkBool:
      printf("%s", v.as.boolean ? "true" : "false");
      return;
    case VkInt:
      printf("%ld", v.as.intn);
      return;
    case VkDouble:
      printf("%g", v.as.doubn);
      return;
    case VkChar:
      printf("%c", v.as.ch);
      return;
    case VkNil:
      printf("nil");
      return;
    case VkObj:
      print_object(v.as.obj);
      break;
    }
}

void print_object(Object* obj) {
  switch(obj->type) {
    case OtString: {
      printf("%s", ((LambString*)obj)->chars);
      break;
    }
    case OtArray: {
      LambArray* arr = (LambArray*)obj;
      printf("[");
      for(i32 i = 0; i < arr->items.len; i++) {
        print_value(arr->items.values[i]);
        if (i != arr->items.len - 1) {
          printf(", ");
        }
      }
      printf("]");
      break;
    }
    case OtFunc: {
      LambFunc* func = (LambFunc*)obj;
      if (func->name == NULL) {
        printf("<script>");
      } else {
        printf("<fn %s>", func->name);
      }
      break;
    }
    case OtNative: {
      printf("<native fn>");
      break;
    }
    case OtClosure: {
      printf("<closure fn>");
      break; 
    }
    case OtUpvalue: {
      printf("<upvalue>");
      break;
    }
  }
}
