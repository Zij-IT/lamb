#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "vm.h"
#include "debug.h"

void vm_init(Vm* vm) {
  vm->ip = NULL;
  vm->chunk = NULL;
  vm->poor_mans_gc = NULL;
  vm->stack_top = vm->stack;
  table_init(&vm->strings);
  table_init(&vm->globals);
  vm->curr_compiler = malloc(sizeof(Compiler));
  compiler_init(vm->curr_compiler);
}

void vm_set_chunk(Vm* vm, Chunk* chunk) {
  vm->chunk = chunk;
  vm_reset_ip(vm);
}

void vm_reset_ip(Vm* vm) {
  vm->ip = vm->chunk->bytes;
}

void vm_reset_stack(Vm* vm) {
  vm->stack_top = vm->stack;
}

void vm_push_stack(Vm* vm, Value val) {
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

u16 vm_read_short(Vm* vm) {
  u8 hi = vm_read_byte(vm);
  u8 lo = vm_read_byte(vm);
  
  return ((u16)hi << 8) | (u16)lo;
}

Value vm_read_constant(Vm* vm) {
  return vm->chunk->constants.values[vm_read_byte(vm)];
}

Value* vm_peek_stack(Vm* vm) {
  return vm->stack_top - 1;
}

#define BINARY_REL_OP(vm, op)                                          \
  do {                                                                 \
    Value  rhs = vm_pop_stack(vm);                                     \
    Value* lhs = vm_peek_stack(vm);                                    \
                                                                       \
    if (rhs.kind == lhs->kind) {                                       \
      bool rel;                                                        \
      switch(rhs.kind) {                                               \
        case VkBool:   rel = rhs.as.boolean op lhs->as.boolean; break; \
        case VkInt:    rel = rhs.as.intn op lhs->as.intn;       break; \
        case VkDouble: rel = rhs.as.doubn op lhs->as.doubn;     break; \
        case VkChar:   rel = rhs.as.ch op lhs->as.ch;           break; \
        case VkObj:    rel = rhs.as.obj op lhs->as.obj;         break; \
      }                                                                \
                                                                       \
      lhs->kind = VkBool;                                              \
      lhs->as.boolean = rel;                                           \
    } else {                                                           \
      /* RUNTIME ERR: Operands must be of the same type */             \
    }                                                                  \
  } while(0)

#define BINARY_INT_DOUBLE_OP(vm, op)                            \
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
  } while(0)                                                    

#define BINARY_INT_OP(vm, op)                         \
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

#define BINARY_BOOL_SS_OP(vm, op)                          \
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
        vm_push_stack(vm, val);
        break;
      }
      case OpLongConstant: {
        u8 hi = vm_read_byte(vm);
        u8 mi = vm_read_byte(vm);
        u8 lo = vm_read_byte(vm);
        
        i32 idx = ((i32)hi) << 16 | ((i32)mi) << 8 | ((i32)lo) << 0;
        Value val = vm->chunk->constants.values[idx];

        vm_push_stack(vm, val);
        break;
      }
      case OpDefineGlobal: {
        LambString* ident = (LambString*)vm_pop_stack(vm).as.obj;
        Value* val = vm_peek_stack(vm);
       
        if(!table_insert(&vm->globals, ident, *val)) {
          printf("Multiple definitions found for %s\n", ident->chars);
          // RuntimeError: Multiple definitions found for a global variable
        }

        vm_pop_stack(vm);
       
        break;
      }
      case OpGetGlobal: {
        Value val = vm_pop_stack(vm);
        LambString* ident = (LambString*)val.as.obj;

        Value value;
        if (!table_get(&vm->globals, ident, &value)) {
          printf("Globals doesn't contain '%s'\n", ident->chars);
          // Runtime Error "Undefined variabel '%s'"
          return;
        }

        vm_push_stack(vm, value);
        break;
      }
      case OpDefineLocal: {
        i32 slot = vm_pop_stack(vm).as.intn;
        vm->stack[slot] = *vm_peek_stack(vm);
        break; 
      }
      case OpGetLocal: {
        i32 slot = vm_pop_stack(vm).as.intn;
        vm_push_stack(vm, vm->stack[slot]);
        break; 
      }
      case OpJumpIfFalse: {
        u16 offset = vm_read_short(vm);
        if (is_bool(*vm_peek_stack(vm)) && !vm_peek_stack(vm)->as.boolean) {
          vm->ip += offset;
        }
        break;
      }
      case OpJump: {
        u16 offset = vm_read_short(vm);
        vm->ip += offset;
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
      case OpAdd: {
          Value rhs = vm_pop_stack(vm);
          Value *lhs = vm_peek_stack(vm);
          if (rhs.kind == lhs->kind && rhs.kind == VkInt) {
            lhs->as.intn = lhs->as.intn + rhs.as.intn;
          } else if (rhs.kind == lhs->kind && rhs.kind == VkDouble) {
            lhs->as.doubn = lhs->as.doubn + rhs.as.doubn;
          } else if (is_object(*lhs) && is_of_type(lhs->as.obj, OtString) && is_object(rhs) && is_of_type(rhs.as.obj, OtString)) {
            LambString* st = concat(vm, (LambString*)lhs->as.obj, (LambString*)rhs.as.obj);
            vm_pop_stack(vm);
            vm_push_stack(vm, new_object((Object*)st));
          } else {
            /* Runtime Error "Binary '+' is only applicable for f64, i64 and string" */
          }
          break;
        }
      case OpSub:    BINARY_INT_DOUBLE_OP(vm, -); break;
      case OpMul:    BINARY_INT_DOUBLE_OP(vm, *); break;
      case OpDiv:    BINARY_INT_DOUBLE_OP(vm, /); break;
      case OpMod:    BINARY_INT_OP(vm,  %); break;
      case OpBinAnd: BINARY_INT_OP(vm,  &); break;
      case OpBinOr:  BINARY_INT_OP(vm,  |); break;
      case OpBinXor: BINARY_INT_OP(vm,  ^); break;
      case OpLShift: BINARY_INT_OP(vm, <<); break;
      case OpRShift: BINARY_INT_OP(vm, >>); break;
      case OpEq:     BINARY_REL_OP(vm, ==); break;
      case OpNe:     BINARY_REL_OP(vm, !=); break;
      case OpGt:     BINARY_REL_OP(vm, >);  break; 
      case OpGe:     BINARY_REL_OP(vm, >=); break;
      case OpLt:     BINARY_REL_OP(vm, <);  break;
      case OpLe:     BINARY_REL_OP(vm, <=); break;
      case OpLogAnd: BINARY_BOOL_SS_OP(vm, &&); break;
      case OpLogOr:  BINARY_BOOL_SS_OP(vm, ||); break;
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
        break;
      }
  }
}

void vm_free(Vm* vm) {
  Object* obj = vm->poor_mans_gc;
  while(obj != NULL) {
    Object* next = obj->next;    
    object_free(obj);
    obj = next;
  }
  
  table_free(&vm->strings);
  table_free(&vm->globals);
  compiler_free(vm->curr_compiler);
  free(vm->curr_compiler);
}

#undef RELATIVE_BIN_OP
