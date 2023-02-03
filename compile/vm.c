#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "vm.h"
#include "value.h"
#include "debug.h"

void vm_init(Vm* vm) {
  // TODO: This + 1 is due to the first local of the compiler being used for
  //       an empty space. Test later in the implementation if it's needed
  vm->stack_top = vm->stack + 1;
  vm->poor_mans_gc = NULL;
  vm->ip = NULL;

  table_init(&vm->strings);
  table_init(&vm->globals);

  vm->curr_compiler = malloc(sizeof(Compiler));
  compiler_init(vm->curr_compiler, FtScript);
  vm->curr_compiler->function = (LambFunc*)alloc_obj(vm, OtFunc);

  vm_reset_ip(vm);
}

void vm_reset_ip(Vm* vm) {
  vm->ip = vm_chunk(vm)->bytes;
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
  return vm_chunk(vm)->constants.values[vm_read_byte(vm)];
}

Value* vm_peek_stack(Vm* vm) {
  return vm->stack_top - 1;
}

Chunk* vm_chunk(Vm* vm) {
  return &vm->curr_compiler->function->chunk;
}

#define BINARY_REL_OP(vm, op)                                                                \
  do {                                                                                       \
    Value  rhs = vm_pop_stack(vm);                                                           \
    Value* lhs = vm_peek_stack(vm);                                                          \
                                                                                             \
    if (rhs.kind == lhs->kind) {                                                             \
      bool rel;                                                                              \
      switch(rhs.kind) {                                                                     \
        case VkBool:   rel = rhs.as.boolean op lhs->as.boolean; break;                       \
        case VkInt:    rel = rhs.as.intn op lhs->as.intn;       break;                       \
        case VkDouble: rel = rhs.as.doubn op lhs->as.doubn;     break;                       \
        case VkChar:   rel = rhs.as.ch op lhs->as.ch;           break;                       \
        case VkObj:    rel = rhs.as.obj op lhs->as.obj;         break;                       \
        case VkNil:    rel = true;                              break;                       \
      }                                                                                      \
                                                                                             \
      lhs->kind = VkBool;                                                                    \
      lhs->as.boolean = rel;                                                                 \
    } else {                                                                                 \
      printf("RuntimeError: Operands for binary operator "#op" must be of the same type\n"); \
      return InterpretRuntimeError;                                                          \
    }                                                                                        \
  } while(0)

#define BINARY_INT_DOUBLE_OP(vm, op)                                                                    \
  do {                                                                                                  \
    Value  rhs = vm_pop_stack(vm);                                                                      \
    Value* lhs = vm_peek_stack(vm);                                                                     \
                                                                                                        \
    if (rhs.kind == lhs->kind && rhs.kind == VkInt) {                                                   \
      lhs->as.intn = lhs->as.intn op rhs.as.intn;                                                       \
    } else if (rhs.kind == lhs->kind && rhs.kind == VkDouble) {                                         \
      lhs->as.doubn = lhs->as.doubn op rhs.as.doubn;                                                    \
    } else {                                                                                            \
      printf("RuntimeError: Operands for binary operator "#op" must be of the same type i64 or f64\n"); \
      return InterpretRuntimeError;                                                                     \
    }                                                                                                   \
  } while(0)

#define BINARY_INT_OP(vm, op)                                                           \
  do {                                                                                  \
    Value  rhs = vm_pop_stack(vm);                                                      \
    Value* lhs = vm_peek_stack(vm);                                                     \
                                                                                        \
    if (rhs.kind == lhs->kind && rhs.kind == VkInt) {                                   \
      lhs->as.intn = lhs->as.intn op rhs.as.intn;                                       \
    } else {                                                                            \
      printf("RuntimeError: Operands for binary operator "#op" must be of type i64\n"); \
      return InterpretRuntimeError;                                                     \
    }                                                                                   \
  } while(0)

InterpretResult vm_run(Vm* vm) {
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
        Value val = vm_chunk(vm)->constants.values[idx];

        vm_push_stack(vm, val);
        break;
      }
      case OpDefineGlobal: {
        LambString* ident = (LambString*)vm_pop_stack(vm).as.obj;
        Value* val = vm_peek_stack(vm);
       
        if(!table_insert(&vm->globals, ident, *val)) {
          printf("RuntimeError: Multiple definitions found for global %s\n", ident->chars);
          return InterpretRuntimeError;
        }

        vm_pop_stack(vm);
       
        break;
      }
      case OpGetGlobal: {
        Value val = vm_pop_stack(vm);
        LambString* ident = (LambString*)val.as.obj;

        Value value;
        if (!table_get(&vm->globals, ident, &value)) {
          printf("RuntimeError: '%s' does not have an associated binding\n", ident->chars);
          return InterpretRuntimeError;
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
        if (!is_bool(*vm_peek_stack(vm))) {
          printf("RuntimeError: A branching expression '&&', '||', 'if' and 'case' cannot branch based on a value of type ");          
          print_kind(*vm_peek_stack(vm));
          printf("\n");
          return InterpretRuntimeError;
        }

        if (!vm_peek_stack(vm)->as.boolean) {
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
          printf("RuntimeError: Unary '-' operator is not defined for values of type ");
          print_kind(*val);
          printf("\n");
          return InterpretRuntimeError;
        }
        break;
      }
      case OpBinNeg: {
        Value* val = vm_peek_stack(vm);
        if (val->kind == VkInt) {
          val->as.intn = ~val->as.intn;
        } else {
          printf("RuntimeError: Unary '~' operator is not defined for values of type ");
          print_kind(*val);
          printf("\n");
          return InterpretRuntimeError;
        }
        break;
      }
      case OpLogNeg: {
        Value* val = vm_peek_stack(vm);
        if (val->kind == VkBool) {
          val->as.boolean = !val->as.boolean;
        } else {
          printf("RuntimeError: Unary '!' operator is not defined for values of type ");
          print_kind(*val);
          printf("\n");
          return InterpretRuntimeError;
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
            printf("RuntimeError: Binary '+' is not possible with the following type combination: lhs(");
            print_kind(*lhs);
            printf("), rhs(");
            print_kind(rhs);
            printf(")\n");
            return InterpretRuntimeError;
          }
          break;
        }
      case OpSub:    BINARY_INT_DOUBLE_OP(vm, -); break;
      case OpMul:    BINARY_INT_DOUBLE_OP(vm, *); break;
      case OpDiv:    BINARY_INT_DOUBLE_OP(vm, /); break;
      // This operator must be expanded to escape % in the printf
      case OpMod: {
        Value rhs = vm_pop_stack(vm);
        Value *lhs = vm_peek_stack(vm);
        if (rhs.kind == lhs->kind && rhs.kind == VkInt) {
          lhs->as.intn = lhs->as.intn % rhs.as.intn;
        } else {
          printf("RuntimeError: Operands for binary operator %% must be of type i64\n");
          return InterpretRuntimeError;
        }
        break;
      }
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
      case OpMakeArray: {
        i32 len = vm_pop_stack(vm).as.intn;
        ValueArray items;
        value_arr_init(&items);
        
        for(i32 i = 0; i < len; i++) {
          value_arr_write(&items, vm_pop_stack(vm));
        }

        LambArray* arr = (LambArray*)alloc_obj(vm, OtArray);
        arr->items = items;
        vm_push_stack(vm, new_object((Object*)arr));
        break;
      }
      case OpIndexArray: {
        Value idx = vm_pop_stack(vm);
        Value arr_val = vm_pop_stack(vm);
        
        if (!is_object(arr_val) && !is_of_type(arr_val.as.obj, OtArray)) {
          printf("RuntimeError: Attempt to index into item of type ");
          print_kind(arr_val);
          printf("\n");
          return InterpretRuntimeError;
        }
        
        LambArray* arr = (LambArray*)arr_val.as.obj;
        if (is_integer(idx)) {
          if (idx.as.intn < arr->items.len) {
            vm_push_stack(vm, arr->items.values[idx.as.intn]);
          } else {
            printf("RuntimeError: Index out of bounds. Desired index: (%ld), Max index: (%d)\n", idx.as.intn, arr->items.len);
            return InterpretRuntimeError;
          }
        } else {
          printf("RuntimeError: Unable to index into an array with a value of type ");
          print_kind(arr_val);
          printf("\n");
          return InterpretRuntimeError;
        }
        break; 
      }
      case OpDup: {
        Value* ret = vm_peek_stack(vm);
        vm_push_stack(vm, *ret);
        break;
      }
      case OpHalt: {
        return InterpretOk;
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
