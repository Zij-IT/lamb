#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#include "../compile/value.hpp"
#include "../debug/debug.hpp"
#include "native.hpp"
#include "vm.hpp"

#define vm_assert(msg, x) lamb_assert("[LambVm] " msg, (x))

#define runtime_error(...)                                                                         \
    do {                                                                                           \
        return InterpretRuntimeError;                                                              \
    } while (0)

#define binary_type_error(lhs, op_str, rhs)                                                        \
    runtime_error("Binary '" op_str                                                                \
                  "' is not defined for the following type combination: lhs(%s) " op_str           \
                  " rhs(%s)",                                                                      \
                  kind_as_cstr(lhs), kind_as_cstr(rhs))

#define unary_type_error(op_str, rhs)                                                              \
    runtime_error("Unary '" op_str "' operator is not defined for values of type %s",              \
                  kind_as_cstr(rhs));

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
    vm->next_collection = 1024 * 1024;
    vm->stack_top = vm->stack;
    vm->objects = NULL;
    vm->open_upvalues = NULL;
    vm->curr_compiler = NULL;
    vm->options = options;
    vm->saved_value = new_nil();

    vm->strings = Table();
    vm->globals = Table();
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

InterpretResult vm_run(Vm *vm) {
    Callframe *frame = &vm->frames[vm->frame_count - 1];

#define READ_BYTE() (*frame->ip++)
#define READ_SHORT() (frame->ip += 2, (uint16_t)((frame->ip[-2] << 8) | frame->ip[-1]))

#define READ_CONSTANT()                                                                            \
    (frame->ip += 1, frame->closure->function->chunk.constants.values[READ_SHORT()])

#define PUSH(val) (*vm->stack_top = val, vm->stack_top += 1)

#define POP() (*--vm->stack_top)

#define DROP() (vm->stack_top -= 1)

#define BINARY_INT_DOUBLE_OP(vm, op)                                                               \
    do {                                                                                           \
        Value rhs = POP();                                                                         \
        Value *lhs = vm_peek_stack(vm);                                                            \
                                                                                                   \
        if (rhs.kind == lhs->kind && rhs.kind == VkInt) {                                          \
            lhs->as.intn = lhs->as.intn op rhs.as.intn;                                            \
        } else if (rhs.kind == lhs->kind && rhs.kind == VkDouble) {                                \
            lhs->as.doubn = lhs->as.doubn op rhs.as.doubn;                                         \
        } else {                                                                                   \
            binary_type_error(*lhs, #op, rhs);                                                     \
        }                                                                                          \
    } while (0)

#define BINARY_INT_OP(vm, op)                                                                      \
    do {                                                                                           \
        Value rhs = POP();                                                                         \
        Value *lhs = vm_peek_stack(vm);                                                            \
                                                                                                   \
        if (rhs.kind == lhs->kind && rhs.kind == VkInt) {                                          \
            lhs->as.intn = lhs->as.intn op rhs.as.intn;                                            \
        } else {                                                                                   \
            binary_type_error(*lhs, #op, rhs);                                                     \
        }                                                                                          \
    } while (0)

    for (;;) {
        switch (READ_BYTE()) {
            case OpConstant: {
                PUSH(frame->closure->function->chunk.constants.values[READ_SHORT()]);
                break;
            }
            case OpDefineGlobal: {
                LambString *ident = (LambString *)READ_CONSTANT().as.obj;
                Value *val = vm_peek_stack(vm);

                if (!vm->globals.insert(vm, ident, *val)) {
                    runtime_error("Multiple definitions found for global %s", ident->chars);
                }

                DROP();
                break;
            }
            case OpGetGlobal: {
                Value val = READ_CONSTANT();
                LambString *ident = (LambString *)val.as.obj;

                Value value;
                if (!table_get(&vm->globals, ident, &value)) {
                    runtime_error("'%s' does not have an associated binding", ident->chars);
                }

                PUSH(value);
                break;
            }
            case OpGetLocal: {
                i32 slot = READ_CONSTANT().as.intn;
                PUSH(frame->slots[slot]);
                break;
            }
            case OpGetUpvalue: {
                i32 slot = READ_CONSTANT().as.intn;
                PUSH(*frame->closure->upvalues[slot]->location);
                break;
            }
            case OpJumpIfFalse: {
                u16 offset = READ_SHORT();
                if (!is_bool(*vm_peek_stack(vm))) {
                    runtime_error("A branching expression '&&', '||' and 'if' cannot "
                                  "branch with expressions of type %s",
                                  kind_as_cstr(*vm_peek_stack(vm)));
                }

                if (!vm_peek_stack(vm)->as.boolean) {
                    frame->ip += offset;
                }
                break;
            }
            case OpJump: {
                u16 offset = READ_SHORT();
                frame->ip += offset;
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
                Value *lhs = vm_peekn_stack(vm, 1);
                Value *rhs = vm_peek_stack(vm);
                if (rhs->kind == lhs->kind && rhs->kind == VkInt) {
                    lhs->as.intn = lhs->as.intn + rhs->as.intn;
                    DROP();
                } else if (rhs->kind == lhs->kind && rhs->kind == VkDouble) {
                    lhs->as.doubn = lhs->as.doubn + rhs->as.doubn;
                    DROP();
                } else if (is_object(*lhs) && is_of_type(lhs->as.obj, OtString) &&
                           is_object(*rhs) && is_of_type(rhs->as.obj, OtString)) {
                    LambString *st =
                        concat(vm, (LambString *)lhs->as.obj, (LambString *)rhs->as.obj);
                    DROP();
                    DROP();
                    PUSH(new_object((Object *)st));
                } else {
                    binary_type_error(*lhs, "+", *rhs);
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
                Value rhs = POP();
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
                Value rhs = POP();
                Value lhs = POP();
                if (rhs.kind == lhs.kind) {
                    PUSH(new_boolean(value_compare(&lhs, &rhs) == OrderEqual));
                    break;
                } else {
                    binary_type_error(lhs, "=", rhs);
                }
            }
            case OpNe: {
                Value rhs = POP();
                Value lhs = POP();
                if (rhs.kind == lhs.kind) {
                    PUSH(new_boolean(value_compare(&lhs, &rhs) != OrderEqual));
                    break;
                } else {
                    binary_type_error(lhs, "!=", rhs);
                }
            }
            case OpGt: {
                Value rhs = POP();
                Value lhs = POP();
                if (rhs.kind == lhs.kind) {
                    PUSH(new_boolean(value_compare(&lhs, &rhs) == OrderGreater));
                    break;
                } else {
                    binary_type_error(lhs, ">", rhs);
                }
            }
            case OpGe: {
                Value rhs = POP();
                Value lhs = POP();
                if (rhs.kind == lhs.kind) {
                    PUSH(new_boolean(value_compare(&lhs, &rhs) != OrderLess));
                    break;
                } else {
                    binary_type_error(lhs, ">=", rhs);
                }
            }
            case OpLt: {
                Value rhs = POP();
                Value lhs = POP();
                if (rhs.kind == lhs.kind) {
                    PUSH(new_boolean(value_compare(&lhs, &rhs) == OrderLess));
                    break;
                } else {
                    binary_type_error(lhs, "<", rhs);
                }
            }
            case OpLe: {
                Value rhs = POP();
                Value lhs = POP();
                if (rhs.kind == lhs.kind) {
                    PUSH(new_boolean(value_compare(&lhs, &rhs) != OrderGreater));
                    break;
                } else {
                    binary_type_error(lhs, "<=", rhs);
                }
            } break;
            case OpMakeArray: {
                i32 len = POP().as.intn;
                ValueArray items;
                value_arr_init(&items);

                for (i32 i = 0; i < len; i++) {
                    value_arr_write(vm, &items, POP());
                }

                LambArray *arr = (LambArray *)alloc_obj(vm, OtArray);
                arr->items = items;
                PUSH(new_object((Object *)arr));
                break;
            }
            case OpIndexArray: {
                Value idx = POP();
                Value arr_val = POP();

                if (!is_integer(idx)) {
                    runtime_error("Unable to index into an array with a value of type %s",
                                  kind_as_cstr(idx));
                } else if (!is_object(arr_val)) {
                    runtime_error("Unable to index into an item of type %s", kind_as_cstr(arr_val));
                }

                switch (arr_val.as.obj->type) {
                    case OtString: {
                        LambString *st = (LambString *)arr_val.as.obj;
                        if (idx.as.intn < st->len) {
                            PUSH(new_char(st->chars[idx.as.intn]));
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
                            PUSH(arr->items.values[idx.as.intn]);
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
                i32 arg_count = READ_CONSTANT().as.intn;
                Value *callee = vm_peekn_stack(vm, arg_count);

                if (!is_object(*callee)) {
                    runtime_error("Unable to call a value of type %s", kind_as_cstr(*callee));
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

                        Callframe *new_frame = &vm->frames[vm->frame_count++];
                        new_frame->closure = closure;
                        new_frame->ip = closure->function->chunk.bytes;
                        new_frame->slots = vm->stack_top - arg_count - 1;
                        frame = new_frame;
                        break;
                    }
                    case OtNative: {
                        NativeFunc *native = (NativeFunc *)callee->as.obj;
                        Value result = native->func(arg_count, vm->stack_top - arg_count);
                        vm->stack_top -= arg_count + 1;
                        PUSH(result);
                        break;
                    }
                    default: {
                        runtime_error("Unable to call a value of type %s", kind_as_cstr(*callee));
                    }
                }

                break;
            }
            case OpClosure: {
                LambFunc *function = (LambFunc *)READ_CONSTANT().as.obj;
                LambClosure *closure = to_closure(vm, function);
                PUSH(new_object((Object *)closure));

                for (i32 i = 0; i < closure->upvalue_count; i++) {
                    bool is_local = READ_BYTE();
                    u8 index = READ_BYTE();

                    if (is_local) {
                        closure->upvalues[i] = capture_upvalue(vm, frame->slots + index);
                    } else {
                        closure->upvalues[i] = frame->closure->upvalues[index];
                    }
                }

                break;
            }
            case OpReturn: {
                Value ret = POP();
                close_upvalues(vm, frame->slots);
                vm->stack_top = frame->slots;
                vm->frame_count--;
                if (vm->frame_count == 0) {
                    vm_assert("Stack is empty upon ending script", vm->stack_top == vm->stack);
                    return InterpretOk;
                }

                frame = &vm->frames[vm->frame_count - 1];
                PUSH(ret);
                break;
            }
            case OpPop: {
                DROP();
                break;
            }
            case OpCloseValue: {
                close_upvalues(vm, vm->stack_top - 1);
                DROP();
                break;
            }
            case OpDup: {
                Value *ret = vm_peek_stack(vm);
                PUSH(*ret);
                break;
            }
            case OpSaveValue: {
                vm->saved_value = POP();
                break;
            }
            case OpUnsaveValue: {
                PUSH(vm->saved_value);
                break;
            }
        }
    }
}

void vm_free(Vm *vm) {
    Object *obj = vm->objects;
    while (obj != NULL) {
        Object *next = obj->next;
        object_free(vm, obj);
        obj = next;
    }

    vm->strings.destroy(vm);
    vm->globals.destroy(vm);
    objectptr_array_free(vm, &vm->gray_stack);
}

#undef BINARY_INT_DOUBLE_OP
#undef BINARY_INT_OP
#undef unary_type_error
#undef binary_type_error
#undef runtime_error
#undef vm_assert
