#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#include "debug.h"
#include "native.h"
#include "value.h"
#include "vm.h"

#define vm_assert(msg, x) lamb_assert("[LambVm] " msg, (x))

#define runtime_error(...)                                                     \
  do {                                                                         \
    fprintf(stderr, "[Lamb] RuntimeError: "__VA_ARGS__);                       \
    fprintf(stderr, "\n");                                                     \
    return InterpretRuntimeError;                                              \
  } while (0)

#define binary_type_error(lhs, op_str, rhs)                                    \
  runtime_error(                                                               \
      "Binary '" op_str                                                        \
      "' is not defined for the following type combination: lhs(%s) " op_str   \
      " rhs(%s)",                                                              \
      kind_as_cstr(lhs), kind_as_cstr(rhs))

#define unary_type_error(op_str, rhs)                                          \
  runtime_error("Unary '" op_str                                               \
                "' operator is not defined for values of type %s",             \
                kind_as_cstr(rhs));

static Callframe *vm_frame(Vm *vm) { return &vm->frames[vm->frame_count - 1]; }

static LambUpvalue *capture_upvalue(Vm *vm, Value *local) {
  LambUpvalue *prev_upvalue = NULL;
  LambUpvalue *curr_upvalue = vm->open_upvalues;

  while (curr_upvalue != NULL && curr_upvalue->location > local) {
    prev_upvalue = curr_upvalue;
    curr_upvalue = curr_upvalue->next;
  }

  if (curr_upvalue != NULL && curr_upvalue->location == local) {
    return curr_upvalue;
  }

  LambUpvalue *created_upvalue = to_upvalue(vm, local);
  created_upvalue->next = curr_upvalue;

  if (prev_upvalue == NULL) {
    vm->open_upvalues = created_upvalue;
  } else {
    prev_upvalue->next = created_upvalue;
  }

  return created_upvalue;
}

static void close_upvalues(Vm *vm, Value *last) {
  while (vm->open_upvalues != NULL && vm->open_upvalues->location >= last) {
    LambUpvalue *upvalue = vm->open_upvalues;
    upvalue->closed = *upvalue->location;
    upvalue->location = &upvalue->closed;
    vm->open_upvalues = upvalue->next;
  }
}

static Chunk *vm_chunk(Vm *vm) {
  return &vm_frame(vm)->closure->function->chunk;
}

static u8 vm_read_byte(Vm *vm) {
  vm_assert("Reading bytes past end of chunk",
            vm_frame(vm)->ip - vm_chunk(vm)->bytes < vm_chunk(vm)->len);

  return *vm_frame(vm)->ip++;
}

static u16 vm_read_short(Vm *vm) {
  u8 hi = vm_read_byte(vm);
  u8 lo = vm_read_byte(vm);

  return ((u16)hi << 8) | (u16)lo;
}

static Value vm_read_constant(Vm *vm) {
  if (vm_read_byte(vm) == OpConstant) {
    return vm_chunk(vm)->constants.values[vm_read_byte(vm)];
  } else {
    u8 hi = vm_read_byte(vm);
    u8 mi = vm_read_byte(vm);
    u8 lo = vm_read_byte(vm);

    i32 idx = ((i32)hi) << 16 | ((i32)mi) << 8 | ((i32)lo) << 0;
    return vm_chunk(vm)->constants.values[idx];
  }
}

static Value *vm_peek_stack(Vm *vm) {
  vm_assert("Peeking non-stack bytes", vm->stack_top != vm->stack);

  return vm->stack_top - 1;
}

static Value *vm_peekn_stack(Vm *vm, i32 n) {
  vm_assert("Peeking non-stack bytes", vm->stack_top != vm->stack - n);

  return vm->stack_top - n - 1;
}

void vm_init(Vm *vm, VmOptions options) {
  vm->frame_count = 0;
  vm->bytes_allocated = 0;
  vm->next_collection = 2;
  vm->stack_top = vm->stack;
  vm->poor_mans_gc = NULL;
  vm->open_upvalues = NULL;
  vm->curr_compiler = NULL;
  vm->options = options;

  table_init(&vm->strings);
  table_init(&vm->globals);
  objectptr_array_init(&vm->gray_stack);

  srand(time(NULL));
  set_natives(vm);
}

void vm_push_stack(Vm *vm, Value val) {
  vm_assert("Stack overflow", vm->stack_top - vm->stack != MAX_VALUES);

  *vm->stack_top = val;
  vm->stack_top += 1;
}

Value vm_pop_stack(Vm *vm) {
  vm_assert("Stack underflow", vm->stack_top != vm->stack);

  vm->stack_top -= 1;
  return *vm->stack_top;
}

#define BINARY_INT_DOUBLE_OP(vm, op)                                           \
  do {                                                                         \
    Value rhs = vm_pop_stack(vm);                                              \
    Value *lhs = vm_peek_stack(vm);                                            \
                                                                               \
    if (rhs.kind == lhs->kind && rhs.kind == VkInt) {                          \
      lhs->as.intn = lhs->as.intn op rhs.as.intn;                              \
    } else if (rhs.kind == lhs->kind && rhs.kind == VkDouble) {                \
      lhs->as.doubn = lhs->as.doubn op rhs.as.doubn;                           \
    } else {                                                                   \
      binary_type_error(*lhs, #op, rhs);                                       \
    }                                                                          \
  } while (0)

#define BINARY_INT_OP(vm, op)                                                  \
  do {                                                                         \
    Value rhs = vm_pop_stack(vm);                                              \
    Value *lhs = vm_peek_stack(vm);                                            \
                                                                               \
    if (rhs.kind == lhs->kind && rhs.kind == VkInt) {                          \
      lhs->as.intn = lhs->as.intn op rhs.as.intn;                              \
    } else {                                                                   \
      binary_type_error(*lhs, #op, rhs);                                       \
    }                                                                          \
  } while (0)

InterpretResult vm_run(Vm *vm) {
  for (;;) {
    switch (vm_read_byte(vm)) {
    case OpConstant: {
      Value val = vm_chunk(vm)->constants.values[vm_read_byte(vm)];
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
      LambString *ident = (LambString *)vm_read_constant(vm).as.obj;
      Value *val = vm_peek_stack(vm);

      if (!table_insert(vm, &vm->globals, ident, *val)) {
        runtime_error("Multiple definitions found for global %s", ident->chars);
      }

      vm_pop_stack(vm);
      break;
    }
    case OpGetGlobal: {
      Value val = vm_read_constant(vm);
      LambString *ident = (LambString *)val.as.obj;

      Value value;
      if (!table_get(&vm->globals, ident, &value)) {
        runtime_error("'%s' does not have an associated binding", ident->chars);
      }

      vm_push_stack(vm, value);
      break;
    }
    case OpDefineLocal: {
      i32 slot = vm_read_constant(vm).as.intn;
      vm_frame(vm)->slots[slot] = *vm_peek_stack(vm);
      break;
    }
    case OpGetLocal: {
      i32 slot = vm_read_constant(vm).as.intn;
      vm_push_stack(vm, vm_frame(vm)->slots[slot]);
      break;
    }
    case OpGetUpvalue: {
      i32 slot = vm_read_constant(vm).as.intn;
      vm_push_stack(vm, *vm_frame(vm)->closure->upvalues[slot]->location);
      break;
    }
    case OpJumpIfFalse: {
      u16 offset = vm_read_short(vm);
      if (!is_bool(*vm_peek_stack(vm))) {
        runtime_error("A branching expression '&&', '||' and 'if' cannot "
                      "branch with expressions of type %s",
                      "__PLACEHOLDER__");
      }

      if (!vm_peek_stack(vm)->as.boolean) {
        vm_frame(vm)->ip += offset;
      }
      break;
    }
    case OpJump: {
      u16 offset = vm_read_short(vm);
      vm_frame(vm)->ip += offset;
      break;
    }
    case OpNumNeg: {
      Value *val = vm_peek_stack(vm);
      if (val->kind == VkInt) {
        val->as.intn = -val->as.intn;
      } else if (val->kind == VkDouble) {
        val->as.doubn = -val->as.doubn;
      } else {
        unary_type_error("-", *val);
      }
      break;
    }
    case OpBinNeg: {
      Value *val = vm_peek_stack(vm);
      if (val->kind == VkInt) {
        val->as.intn = ~val->as.intn;
      } else {
        unary_type_error("~", *val);
      }
      break;
    }
    case OpLogNeg: {
      Value *val = vm_peek_stack(vm);
      if (val->kind == VkBool) {
        val->as.boolean = !val->as.boolean;
      } else {
        unary_type_error("!", *val);
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
      } else if (is_object(*lhs) && is_of_type(lhs->as.obj, OtString) &&
                 is_object(rhs) && is_of_type(rhs.as.obj, OtString)) {
        LambString *st =
            concat(vm, (LambString *)lhs->as.obj, (LambString *)rhs.as.obj);
        vm_pop_stack(vm);
        vm_push_stack(vm, new_object((Object *)st));
      } else {
        binary_type_error(*lhs, "+", rhs);
      }
      break;
    }
    case OpSub:
      BINARY_INT_DOUBLE_OP(vm, -);
      break;
    case OpMul:
      BINARY_INT_DOUBLE_OP(vm, *);
      break;
    case OpDiv:
      BINARY_INT_DOUBLE_OP(vm, /);
      break;
    // This operator must be expanded to escape % in the printf
    case OpMod: {
      Value rhs = vm_pop_stack(vm);
      Value *lhs = vm_peek_stack(vm);
      if (rhs.kind == lhs->kind && rhs.kind == VkInt) {
        lhs->as.intn = lhs->as.intn % rhs.as.intn;
      } else {
        binary_type_error(*lhs, "%%", rhs);
      }
      break;
    }
    case OpBinAnd:
      BINARY_INT_OP(vm, &);
      break;
    case OpBinOr:
      BINARY_INT_OP(vm, |);
      break;
    case OpBinXor:
      BINARY_INT_OP(vm, ^);
      break;
    case OpLShift:
      BINARY_INT_OP(vm, <<);
      break;
    case OpRShift:
      BINARY_INT_OP(vm, >>);
      break;
    case OpEq: {
      Value rhs = vm_pop_stack(vm);
      Value lhs = vm_pop_stack(vm);
      if (rhs.kind == lhs.kind) {
        vm_push_stack(vm, new_boolean(value_compare(&lhs, &rhs) == OrderEqual));
        break;
      } else {
        binary_type_error(lhs, "=", rhs);
      }
    }
    case OpNe: {
      Value rhs = vm_pop_stack(vm);
      Value lhs = vm_pop_stack(vm);
      if (rhs.kind == lhs.kind) {
        vm_push_stack(vm, new_boolean(value_compare(&lhs, &rhs) != OrderEqual));
        break;
      } else {
        binary_type_error(lhs, "!=", rhs);
      }
    }
    case OpGt: {
      Value rhs = vm_pop_stack(vm);
      Value lhs = vm_pop_stack(vm);
      if (rhs.kind == lhs.kind) {
        vm_push_stack(vm,
                      new_boolean(value_compare(&lhs, &rhs) == OrderGreater));
        break;
      } else {
        binary_type_error(lhs, ">", rhs);
      }
    }
    case OpGe: {
      Value rhs = vm_pop_stack(vm);
      Value lhs = vm_pop_stack(vm);
      if (rhs.kind == lhs.kind) {
        vm_push_stack(vm, new_boolean(value_compare(&lhs, &rhs) != OrderLess));
        break;
      } else {
        binary_type_error(lhs, ">=", rhs);
      }
    }
    case OpLt: {
      Value rhs = vm_pop_stack(vm);
      Value lhs = vm_pop_stack(vm);
      if (rhs.kind == lhs.kind) {
        vm_push_stack(vm, new_boolean(value_compare(&lhs, &rhs) == OrderLess));
        break;
      } else {
        binary_type_error(lhs, "<", rhs);
      }
    }
    case OpLe: {
      Value rhs = vm_pop_stack(vm);
      Value lhs = vm_pop_stack(vm);
      if (rhs.kind == lhs.kind) {
        vm_push_stack(vm,
                      new_boolean(value_compare(&lhs, &rhs) != OrderGreater));
        break;
      } else {
        binary_type_error(lhs, "<=", rhs);
      }
    } break;
    case OpMakeArray: {
      i32 len = vm_pop_stack(vm).as.intn;
      ValueArray items;
      value_arr_init(&items);

      for (i32 i = 0; i < len; i++) {
        value_arr_write(vm, &items, vm_pop_stack(vm));
      }

      LambArray *arr = (LambArray *)alloc_obj(vm, OtArray);
      arr->items = items;
      vm_push_stack(vm, new_object((Object *)arr));
      break;
    }
    case OpIndexArray: {
      Value idx = vm_pop_stack(vm);
      Value arr_val = vm_pop_stack(vm);

      if (!is_integer(idx)) {
        runtime_error("Unable to index into an array with a value of type %s",
                      kind_as_cstr(idx));
      } else if (!is_object(arr_val)) {
        runtime_error("Unable to index into an item of type %s",
                      kind_as_cstr(arr_val));
      }

      switch (arr_val.as.obj->type) {
      case OtString: {
        LambString *st = (LambString *)arr_val.as.obj;
        if (idx.as.intn < st->len) {
          vm_push_stack(vm, new_char(st->chars[idx.as.intn]));
        } else {
          runtime_error(
              "Index out of bounds. Desired index: (%ld) | Length: (%d)",
              idx.as.intn, st->len);
        }
        break;
      }
      case OtArray: {
        LambArray *arr = (LambArray *)arr_val.as.obj;
        if (idx.as.intn < arr->items.len) {
          vm_push_stack(vm, arr->items.values[idx.as.intn]);
        } else {
          runtime_error(
              "Index out of bounds. Desired index: (%ld) | Length: (%d)",
              idx.as.intn, arr->items.len);
        }
        break;
      }
      case OtFunc:
      case OtNative:
      case OtClosure:
      case OtUpvalue:
        runtime_error("Unable to index into value of type %s",
                      kind_as_cstr(arr_val));
      }
      break;
    }
    case OpCall: {
      i32 arg_count = vm_read_constant(vm).as.intn;
      Value *callee = vm_peekn_stack(vm, arg_count);

      if (!is_object(*callee)) {
        runtime_error("Unable to call a value of type %s",
                      kind_as_cstr(*callee));
      }

      switch (callee->as.obj->type) {
      case OtClosure: {
        LambClosure *closure = (LambClosure *)callee->as.obj;
        if (arg_count != closure->function->arity) {
          runtime_error("Expected %d arguments, but received %d instead",
                        closure->function->arity, arg_count);
        }

        if (vm->frame_count == MAX_FRAMES) {
          runtime_error("Callstack overflow");
        }

        Callframe *frame = &vm->frames[vm->frame_count++];
        frame->closure = closure;
        frame->ip = closure->function->chunk.bytes;
        frame->slots = vm->stack_top - arg_count - 1;
        break;
      }
      case OtNative: {
        NativeFunc *native = (NativeFunc *)callee->as.obj;
        Value result = native->func(arg_count, vm->stack_top - arg_count);
        vm->stack_top -= arg_count + 1;
        vm_push_stack(vm, result);
        break;
      }
      default: {
        runtime_error("Unable to call a value of type %s",
                      kind_as_cstr(*callee));
      }
      }

      break;
    }
    case OpClosure: {
      LambFunc *function = (LambFunc *)vm_read_constant(vm).as.obj;
      LambClosure *closure = to_closure(vm, function);
      vm_push_stack(vm, new_object((Object *)closure));

      for (i32 i = 0; i < closure->upvalue_count; i++) {
        bool is_local = vm_read_byte(vm);
        u8 index = vm_read_byte(vm);

        if (is_local) {
          closure->upvalues[i] =
              capture_upvalue(vm, vm_frame(vm)->slots + index);
        } else {
          closure->upvalues[i] = vm_frame(vm)->closure->upvalues[index];
        }
      }

      break;
    }
    case OpReturn: {
      Value ret = vm_pop_stack(vm);
      close_upvalues(vm, vm_frame(vm)->slots);
      vm->stack_top = vm_frame(vm)->slots;
      vm->frame_count--;
      if (vm->frame_count == 0) {
        vm_assert("Stack is empty upon ending script",
                  vm->stack_top == vm->stack);
        return InterpretOk;
      }

      vm_push_stack(vm, ret);
      break;
    }
    case OpPop: {
      vm_pop_stack(vm);
      break;
    }
    case OpCloseValue: {
      close_upvalues(vm, vm->stack_top - 1);
      vm_pop_stack(vm);
      break;
    }
    case OpDup: {
      Value *ret = vm_peek_stack(vm);
      vm_push_stack(vm, *ret);
      break;
    }
    }
  }
}

void vm_free(Vm *vm) {
  Object *obj = vm->poor_mans_gc;
  while (obj != NULL) {
    Object *next = obj->next;
    object_free(vm, obj);
    obj = next;
  }

  table_free(vm, &vm->strings);
  table_free(vm, &vm->globals);
  objectptr_array_free(vm, &vm->gray_stack);
}

#undef BINARY_INT_DOUBLE_OP
#undef BINARY_INT_OP
#undef unary_type_error
#undef binary_type_error
#undef runtime_error
#undef vm_assert
