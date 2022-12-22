#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "vm.h"
#include "debug.h"

void vm_init(Vm* vm) {
  vm->ip = NULL;
  vm->chunk = NULL;
  vm->stack_top = vm->stack;
}

void vm_init_with_chunk(Vm* vm, Chunk* chunk) {
  vm_init(vm);
  vm->chunk = chunk;
  vm->ip = chunk->bytes;
}

void vm_reset_stack(Vm* vm) {
  vm->stack_top = vm->stack;
}

void vm_push_value(Vm* vm, Value val) {
  *vm->stack_top = val;
  vm->stack_top += 1;
}

Value vm_pop_stack(Vm* vm) {
  vm->stack_top -= 1;
  return *vm->stack_top;
}

u8 vm_read_byte(Vm* vm) {
  return *vm->ip++;
}

Value vm_read_constant(Vm* vm) {
  return vm->chunk->constants.values[vm_read_byte(vm)];
}

Value* vm_peek_stack(Vm* vm) {
  return vm->stack_top - 1;
}

#define BINARY_REL_OP(op)                                                       \
  do {                                                                          \
    Value  rhs = vm_pop_stack(vm);                                              \
    Value* lhs = vm_peek_stack(vm);                                             \
                                                                                \
    if (rhs.kind == lhs->kind) {                                                \
      bool rel;                                                                 \
      switch(rhs.kind) {                                                        \
        case VkBool:   rel = rhs.as.boolean op lhs->as.boolean;          break; \
        case VkInt:    rel = rhs.as.intn op lhs->as.intn;                break; \
        case VkDouble: rel = rhs.as.doubn op lhs->as.doubn;              break; \
        case VkString: rel = strcmp(rhs.as.string, lhs->as.string) op 0; break; \
        case VkChar:   rel = rhs.as.ch op lhs->as.ch;                    break; \
      }                                                                         \
                                                                                \
      lhs->kind = VkBool;                                                       \
      lhs->as.boolean = rel;                                                    \
    } else {                                                                    \
      /* RUNTIME ERR: Operands must be of the same type */                      \
    }                                                                           \
  } while(0)

#define BINARY_INT_DOUBLE_OP(op)                                \
  do {                                                          \
    Value  rhs = vm_pop_stack(vm);                              \
    Value* lhs = vm_peek_stack(vm);                             \
                                                                \
    if (rhs.kind == lhs->kind && rhs.kind == VkInt) {           \
      lhs->as.intn = lhs->as.intn op rhs.as.intn;               \
    } else if (rhs.kind == lhs->kind && rhs.kind == VkDouble) { \
      lhs->as.doubn = lhs->as.doubn op rhs.as.doubn;            \
    } else {                                                    \
      /* RUNTIME ERR: Operands must be of type i64 of f64 */    \
    }                                                           \
  } while(0)                                                    \

#define BINARY_INT_OP(op)                             \
  do {                                                \
    Value  rhs = vm_pop_stack(vm);                    \
    Value* lhs = vm_peek_stack(vm);                   \
                                                      \
    if (rhs.kind == lhs->kind && rhs.kind == VkInt) { \
      lhs->as.intn = lhs->as.intn op rhs.as.intn;     \
    } else {                                          \
      /* RUNTIME ERR: Operands must be of type i64 */ \
    }                                                 \
  } while(0)

#define BINARY_BOOL_SS_OP(op)                              \
  do {                                                     \
    Value  rhs = vm_pop_stack(vm);                         \
    Value* lhs = vm_peek_stack(vm);                        \
                                                           \
    if (rhs.kind == lhs->kind && rhs.kind == VkBool) {     \
      lhs->as.boolean = lhs->as.boolean op rhs.as.boolean; \
    } else {                                               \
      /* RUNTIME ERR: Operands must be of type bool */     \
    }                                                      \
  } while(0)


void vm_run(Vm* vm) {
  for(;;) {
    switch (vm_read_byte(vm)) {
      case OpConstant: {
        Value val = vm_read_constant(vm);
        vm_push_value(vm, val);
        break;
      }
      case OpLongConstant: {
        u8 lo = vm_read_byte(vm);
        u8 mi = vm_read_byte(vm);
        u8 hi = vm_read_byte(vm);
        
        i32 idx = ((i32)hi) << 16 | ((i32)mi) << 8 | ((i32)lo) << 0;
        Value val = vm->chunk->constants.values[idx];

        vm_push_value(vm, val);
        break;
      }
      case OpNumNeg: {
        Value* val = vm_peek_stack(vm);
        if (val->kind == VkInt) {
          val->as.intn = -val->as.intn;
        } else if (val->kind == VkDouble) {
          val->as.doubn = -val->as.doubn;
        } else {
          // Runtime Error "Unary operation '-' is only defined for values of type i64 or f64"
        }
        break;
      }
      case OpBinNeg: {
        Value* val = vm_peek_stack(vm);
        if (val->kind == VkInt) {
          val->as.intn = ~val->as.intn;
        } else {
          // Runtime Error "Unary operation '~' is only defined for values of type i64"
        }
        break;
      }
      case OpLogNeg: {
        Value* val = vm_peek_stack(vm);
        if (val->kind == VkBool) {
          val->as.boolean = !val->as.boolean;
        } else {
          // Runtime Error "Unary operation '!' is only defined for values of type bool"
        }
        break;
      }
      case OpAdd:    BINARY_INT_DOUBLE_OP(+); break;
      case OpSub:    BINARY_INT_DOUBLE_OP(-); break;
      case OpMul:    BINARY_INT_DOUBLE_OP(*); break;
      case OpDiv:    BINARY_INT_DOUBLE_OP(/); break;
      case OpMod:    BINARY_INT_OP(%); break;
      case OpBinAnd: BINARY_INT_OP(&); break;
      case OpBinOr:  BINARY_INT_OP(|); break;
      case OpBinXor: BINARY_INT_OP(^); break;
      case OpEq:     BINARY_REL_OP(==); break;
      case OpNe:     BINARY_REL_OP(!=); break;
      case OpGt:     BINARY_REL_OP(>);  break; 
      case OpGe:     BINARY_REL_OP(>=); break;
      case OpLt:     BINARY_REL_OP(<);  break;
      case OpLe:     BINARY_REL_OP(<=); break;
      case OpLogAnd: BINARY_BOOL_SS_OP(&&); break;
      case OpLogOr:  BINARY_BOOL_SS_OP(||); break;
      case OpReturn: {
        Value ret = vm_pop_stack(vm);
        printf("Returning: ");
        print_value(ret);
        printf("\n");
        break;
      }
      case OpPop: {
        Value ret = vm_pop_stack(vm);
        printf("Popping: ");
        print_value(ret);
        printf("\n");
        break;
      }
      case OpHalt: {
        return;
      }
      case OpLApply:
      case OpRApply:
      case OpLCompose:
      case OpRCompose:
      case OpRShift:
      case OpLShift:
        break;
      }
  }
}

void vm_free(Vm* vm) {
  
}

#undef RELATIVE_BIN_OP
