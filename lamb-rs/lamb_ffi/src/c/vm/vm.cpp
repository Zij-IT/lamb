#include <cassert>
#include <cstdlib>
#include <ctime>

#include "../compile/chunk.hpp"
#include "../compile/gcvec.hpp"
#include "../compile/object.hpp"
#include "../compile/value.hpp"
#include "../types.hpp"
#include "native.hpp"
#include "vm.hpp"

// NOLINTBEGIN(cppcoreguidelines-pro-bounds-pointer-arithmetic)

#define lamb_assert(msg, x) assert((((void)(msg)), (x)))
#define vm_assert(msg, x) lamb_assert("[LambVm] " msg, (x))

// TODO: Make these actually useful methods instead of whatever the hell this
//       is.
#define runtime_error(...)                                                                         \
    /* NOLINTBEGIN(cppcoreguidelines-avoid-do-while) */                                            \
    do {                                                                                           \
        return InterpretRuntimeError;                                                              \
    } while (0) /* NOLINTEND(cppcoreguidelines-avoid-do-while) */

#define binary_type_error(lhs, op_str, rhs)                                                        \
    runtime_error("Binary '" op_str                                                                \
                  "' is not defined for the following type combination: lhs(%s) " op_str           \
                  " rhs(%s)",                                                                      \
                  lhs.kind_as_cstr(), rhs.kind_as_cstr())

#define unary_type_error(op_str, rhs)                                                              \
    runtime_error("Unary '" op_str "' operator is not defined for values of type %s",              \
                  rhs.kind_as_cstr());

// This function is not a member because for whatever reason the compiler can't properly optimize
// it's use in Vm::run as well as it can when the function is used as a static function. Performance
// was ~15% worse for calculating the 35th fibonacci number. Consistently 2.6 to 2.3 seconds, with
// flamegraph showing this function taking up lots of time. I assume that the compiler doesn't
// inline the method call, as it post move is no longer visible in a flamegraph.

namespace {
void close_upvalues(Vm &vm, Value *last) {
    while (vm.open_upvalues != nullptr && vm.open_upvalues->location >= last) {
        LambUpvalue *upvalue = vm.open_upvalues;
        upvalue->closed = *upvalue->location;
        upvalue->location = &upvalue->closed;
        vm.open_upvalues = upvalue->next;
    }
}
} // namespace

LambUpvalue *Vm::capture_upvalue(Value *local) {
    LambUpvalue *prev_upvalue = nullptr;
    LambUpvalue *curr_upvalue = this->open_upvalues;

    while (curr_upvalue != nullptr && curr_upvalue->location > local) {
        prev_upvalue = curr_upvalue;
        curr_upvalue = curr_upvalue->next;
    }

    if (curr_upvalue != nullptr && curr_upvalue->location == local) {
        return curr_upvalue;
    }

    auto *created_upvalue = LambUpvalue::alloc(*this, local, curr_upvalue);
    if (prev_upvalue == nullptr) {
        this->open_upvalues = created_upvalue;
    } else {
        prev_upvalue->next = created_upvalue;
    }

    return created_upvalue;
}

constexpr Value *Vm::peek_stack(u8 n) const {
    vm_assert("Peeking non-stack bytes", this->stack_top != this->stack);

    // NOLINTNEXTLINE(cppcoreguidelines-pro-bounds-pointer-arithmetic)
    return this->stack_top - n - 1;
}

Vm::Vm(VmOptions options)
    : curr_compiler(nullptr), saved_value(Value::nil()), frame_count(0), open_upvalues(nullptr),
      options(options) {
    this->stack_top = this->stack;
    srand(time(nullptr));
    set_natives(*this);
}

void Vm::push_stack(Value val) {
    vm_assert("Stack overflow", this->stack_top - this->stack != MAX_VALUES);

    *this->stack_top = val;
    this->stack_top += 1;
}

Value Vm::pop_stack() {
    vm_assert("Stack underflow", this->stack_top != this->stack);
    this->stack_top -= 1;
    return *this->stack_top;
}

// NOLINTNEXTLINE(readability-function-cognitive-complexity)
InterpretResult Vm::run() {
    Callframe *frame = &this->frames[this->frame_count - 1];

#define READ_BYTE() (*frame->ip++)

#define READ_SHORT() (frame->ip += 2, (u16)((frame->ip[-2] << 8) | frame->ip[-1]))

#define READ_CONSTANT() (frame->ip += 1, frame->closure->function->chunk.constants[READ_SHORT()])

#define PUSH(val) (*this->stack_top = (val), this->stack_top += 1)

#define POP() (*--this->stack_top)

#define DROP() (this->stack_top -= 1)

#define BINARY_INT_DOUBLE_OP(vm, op)                                                               \
    /* NOLINTBEGIN(cppcoreguidelines-avoid-do-while) */                                            \
    do {                                                                                           \
        Value rhs = POP();                                                                         \
        Value *lhs = (vm)->peek_stack();                                                           \
                                                                                                   \
        if (rhs.kind == lhs->kind && rhs.kind == VkInt) {                                          \
            lhs->as.intn = lhs->as.intn op rhs.as.intn;                                            \
        } else if (rhs.kind == lhs->kind && rhs.kind == VkDouble) {                                \
            lhs->as.doubn = lhs->as.doubn op rhs.as.doubn;                                         \
        } else {                                                                                   \
            binary_type_error(*lhs, #op, rhs);                                                     \
        }                                                                                          \
    } while (0) /* NOLINTEND(cppcoreguidelines-avoid-do-while) */

#define BINARY_INT_OP(vm, op)                                                                      \
    /* NOLINTBEGIN(cppcoreguidelines-avoid-do-while) */                                            \
    do {                                                                                           \
        Value rhs = POP();                                                                         \
        Value *lhs = (vm)->peek_stack();                                                           \
                                                                                                   \
        if (rhs.kind == lhs->kind && rhs.kind == VkInt) {                                          \
            lhs->as.intn = lhs->as.intn op rhs.as.intn;                                            \
        } else {                                                                                   \
            binary_type_error(*lhs, #op, rhs);                                                     \
        }                                                                                          \
    } while (0) /* NOLINTEND(cppcoreguidelines-avoid-do-while) */

    for (;;) {
        switch ((OpCode)READ_BYTE()) {
            case OpConstant: {
                PUSH(frame->closure->function->chunk.constants[READ_SHORT()]);
                break;
            }
            case OpDefineGlobal: {
                auto *ident = (LambString *)READ_CONSTANT().as.obj;
                Value *val = this->peek_stack();

                if (!this->globals.insert(*this, ident, *val)) {
                    runtime_error("Multiple definitions found for global %s", ident->chars);
                }

                DROP();
                break;
            }
            case OpGetGlobal: {
                Value val = READ_CONSTANT();
                auto *ident = (LambString *)val.as.obj;

                auto global = this->globals.get(ident);
                if (!global) {
                    runtime_error("'%s' does not have an associated binding", ident->chars);
                }

                PUSH(global.value());
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
            case OpJumpIfTrue: {
                u16 offset = READ_SHORT();
                if (!this->peek_stack()->is_bool()) {
                    runtime_error("A branching expression '&&', '||' and 'if' cannot "
                                  "branch with expressions of type %s",
                                  kind_as_cstr(*vm_peek_stack(vm)));
                }

                if (this->peek_stack()->as.boolean) {
                    frame->ip += offset;
                }
                break;
            }
            case OpJumpIfFalse: {
                u16 offset = READ_SHORT();
                if (!this->peek_stack()->is_bool()) {
                    runtime_error("A branching expression '&&', '||' and 'if' cannot "
                                  "branch with expressions of type %s",
                                  kind_as_cstr(*vm_peek_stack(vm)));
                }

                if (!this->peek_stack()->as.boolean) {
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
                Value *val = this->peek_stack();
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
                Value *val = this->peek_stack();
                if (val->kind == VkInt) {
                    val->as.intn = ~val->as.intn;
                } else {
                    unary_type_error("~", *val);
                }
                break;
            }
            case OpLogNeg: {
                Value *val = this->peek_stack();
                if (val->kind == VkBool) {
                    val->as.boolean = !val->as.boolean;
                } else {
                    unary_type_error("!", *val);
                }
                break;
            }
            case OpAdd: {
                Value *lhs = this->peek_stack(1);
                Value *rhs = this->peek_stack();
                if (rhs->kind == lhs->kind && rhs->kind == VkInt) {
                    lhs->as.intn = lhs->as.intn + rhs->as.intn;
                    DROP();
                } else if (rhs->kind == lhs->kind && rhs->kind == VkDouble) {
                    lhs->as.doubn = lhs->as.doubn + rhs->as.doubn;
                    DROP();
                } else if (lhs->is_object() && lhs->as.obj->is(OtString) && rhs->is_object() &&
                           rhs->as.obj->is(OtString)) {
                    auto *left = (LambString *)lhs->as.obj;
                    auto *right = (LambString *)rhs->as.obj;
                    auto *result = left->concat(*this, right);

                    DROP();
                    DROP();
                    PUSH(Value::from_obj((Object *)result));
                } else {
                    binary_type_error(*lhs, "+", *rhs);
                }
                break;
            }
            case OpSub:
                BINARY_INT_DOUBLE_OP(this, -);
                break;
            case OpMul:
                BINARY_INT_DOUBLE_OP(this, *);
                break;
            case OpDiv:
                BINARY_INT_DOUBLE_OP(this, /);
                break;
            case OpMod: {
                Value rhs = POP();
                Value *lhs = this->peek_stack();
                if (rhs.kind == lhs->kind && rhs.kind == VkInt) {
                    lhs->as.intn = lhs->as.intn % rhs.as.intn;
                } else {
                    binary_type_error(*lhs, "%%", rhs);
                }
                break;
            }
            case OpBinAnd:
                BINARY_INT_OP(this, &);
                break;
            case OpBinOr:
                BINARY_INT_OP(this, |);
                break;
            case OpBinXor:
                BINARY_INT_OP(this, ^);
                break;
            case OpLShift:
                BINARY_INT_OP(this, <<);
                break;
            case OpRShift:
                BINARY_INT_OP(this, >>);
                break;
            case OpEq: {
                Value rhs = POP();
                Value lhs = POP();
                if (rhs.kind == lhs.kind) {
                    PUSH(Value::from_bool(lhs.cmp(rhs) == OrderEqual));
                    break;
                } else {
                    binary_type_error(lhs, "=", rhs);
                }
            }
            case OpNe: {
                Value rhs = POP();
                Value lhs = POP();
                if (rhs.kind == lhs.kind) {
                    PUSH(Value::from_bool(lhs.cmp(rhs) != OrderEqual));
                    break;
                } else {
                    binary_type_error(lhs, "!=", rhs);
                }
            }
            case OpGt: {
                Value rhs = POP();
                Value lhs = POP();
                if (rhs.kind == lhs.kind) {
                    PUSH(Value::from_bool(lhs.cmp(rhs) == OrderGreater));
                    break;
                } else {
                    binary_type_error(lhs, ">", rhs);
                }
            }
            case OpGe: {
                Value rhs = POP();
                Value lhs = POP();
                if (rhs.kind == lhs.kind) {
                    PUSH(Value::from_bool(lhs.cmp(rhs) != OrderLess));
                    break;
                } else {
                    binary_type_error(lhs, ">=", rhs);
                }
            }
            case OpLt: {
                Value rhs = POP();
                Value lhs = POP();
                if (rhs.kind == lhs.kind) {
                    PUSH(Value::from_bool(lhs.cmp(rhs) == OrderLess));
                    break;
                } else {
                    binary_type_error(lhs, "<", rhs);
                }
            }
            case OpLe: {
                Value rhs = POP();
                Value lhs = POP();
                if (rhs.kind == lhs.kind) {
                    PUSH(Value::from_bool(lhs.cmp(rhs) != OrderGreater));
                    break;
                } else {
                    binary_type_error(lhs, "<=", rhs);
                }
            } break;
            case OpMakeArray: {
                i32 len = READ_CONSTANT().as.intn;
                GcVec<Value> items;
                for (i32 i = 0; i < len; i++) {
                    items.push(*this, POP());
                }

                auto *arr = LambArray::alloc(*this, items);
                PUSH(Value::from_obj((Object *)arr));
                break;
            }
            case OpIndex: {
                Value idx = POP();
                Value arr_val = POP();

                if (!idx.is_integer()) {
                    runtime_error("Unable to index into an array with a value of type %s",
                                  kind_as_cstr(idx));
                } else if (!arr_val.is_object()) {
                    runtime_error("Unable to index into an item of type %s", kind_as_cstr(arr_val));
                }

                switch (arr_val.as.obj->type) {
                    case OtString: {
                        auto *st = (LambString *)arr_val.as.obj;
                        if (idx.as.intn < st->len && idx.as.intn >= 0) {
                            PUSH(Value::from_char(st->chars[idx.as.intn]));
                        } else {
                            runtime_error(
                                "Index out of bounds. Desired index: (%ld) | Length: (%d)",
                                idx.as.intn, st->len);
                        }
                        break;
                    }
                    case OtArray: {
                        auto *arr = (LambArray *)arr_val.as.obj;
                        if (idx.as.intn < arr->items.len() && idx.as.intn >= 0) {
                            PUSH(arr->items[idx.as.intn]);
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
                Value *callee = this->peek_stack(arg_count);

                if (!callee->is_object()) {
                    runtime_error("Unable to call a value of type %s", kind_as_cstr(*callee));
                }

                switch (callee->as.obj->type) {
                    case OtClosure: {
                        auto *closure = (LambClosure *)callee->as.obj;
                        if (arg_count != closure->function->arity) {
                            runtime_error("Expected %d arguments, but received %d instead",
                                          closure->function->arity, arg_count);
                        }

                        if (this->frame_count == MAX_FRAMES) {
                            runtime_error("Callstack overflow");
                        }

                        Callframe *new_frame = &this->frames[this->frame_count++];
                        new_frame->closure = closure;
                        new_frame->ip = closure->function->chunk.bytes.as_raw();
                        new_frame->slots = this->stack_top - arg_count - 1;
                        frame = new_frame;
                        break;
                    }
                    case OtNative: {
                        auto *native = (NativeFunc *)callee->as.obj;
                        Value result = native->func(arg_count, this->stack_top - arg_count);
                        this->stack_top -= arg_count + 1;
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
                auto *function = (LambFunc *)READ_CONSTANT().as.obj;
                auto *closure = LambClosure::alloc(*this, function);
                PUSH(Value::from_obj((Object *)closure));

                for (i32 i = 0; i < closure->upvalue_count; i++) {
                    bool is_local = READ_BYTE();
                    u8 index = READ_BYTE();

                    if (is_local) {
                        closure->upvalues[i] = this->capture_upvalue(frame->slots + index);
                    } else {
                        closure->upvalues[i] = frame->closure->upvalues[index];
                    }
                }

                break;
            }
            case OpReturn: {
                Value ret = POP();
                close_upvalues(*this, frame->slots);
                this->stack_top = frame->slots;
                this->frame_count--;
                if (this->frame_count == 0) {
                    vm_assert("Stack is empty upon ending script", this->stack_top == this->stack);
                    return InterpretOk;
                }

                frame = &this->frames[this->frame_count - 1];
                PUSH(ret);
                break;
            }
            case OpPop: {
                DROP();
                break;
            }
            case OpCloseValue: {
                close_upvalues(*this, this->stack_top - 1);
                DROP();
                break;
            }
            case OpDup: {
                Value *ret = this->peek_stack();
                PUSH(*ret);
                break;
            }
            case OpSaveValue: {
                this->saved_value = POP();
                break;
            }
            case OpUnsaveValue: {
                PUSH(this->saved_value);
                break;
            }
        }
    }

#undef BINARY_INT_OP
#undef BINARY_INT_DOUBLE_OP
#undef DROP
#undef POP
#undef PUSH
#undef READ_CONSTANT
#undef READ_SHORT
#undef READ_BYTE
}

void Vm::destroy() {
    this->globals.destroy(*this);
    this->strings.destroy(*this);
    this->gc.destroy(*this);
}

#undef unary_type_error
#undef binary_type_error
#undef runtime_error
#undef vm_assert

// NOLINTEND(cppcoreguidelines-pro-bounds-pointer-arithmetic)
