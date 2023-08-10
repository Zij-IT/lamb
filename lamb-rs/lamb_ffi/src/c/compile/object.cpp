#include <cstdlib>
#include <cstring>
#include <ranges>
#include <sstream>
#include <string>

#include "../types.hpp"
#include "../vm/vm.hpp"
#include "chunk.hpp"
#include "gcvec.hpp"
#include "misc.hpp"
#include "object.hpp"
#include "value.hpp"

std::string Object::to_string() const {
    std::ostringstream out;
    switch (this->type) {
        case OtString:
            out << ((LambString *)(this))->chars;
            break;
        case OtArray: {
            auto *arr = (LambArray *)this;
            out << '[';
            for (i32 i = 0; i < arr->items.len(); i++) {
                out << arr->items[i].to_string();
                if (i != arr->items.len() - 1) {
                    out << ", ";
                }
            }
            out << ']';
            break;
        }
        case OtFunc: {
            auto *func = (LambFunc *)this;
            if (func->name == nullptr) {
                out << "<script>";
            } else {
                out << "<fn " << func->name << ">";
            }
            break;
        }
        case OtNative:
            out << "<native fn>";
            break;
        case OtClosure:
            out << "<native fn>";
            break;
        case OtUpvalue:
            out << "upvalue";
            break;
    }

    return out.str();
}

LambString *LambString::alloc(Vm &vm, char const *chars, i32 len, u32 hash) {
    auto *st = (LambString *)vm.gc.alloc(vm, sizeof(LambString), 1);
    st->obj = {
        .next = nullptr,
        .type = OtString,
        .is_marked = false,
    };
    st->chars = chars;
    st->len = len;
    st->hash = hash;

    vm.gc.add_object(&st->obj);
    return st;
}

LambString *LambString::slice(Vm &vm, char const *chars, u32 start, u32 end) {
    char *chs = strndup(chars + start, end - start);
    return LambString::from_cstr(vm, chs);
}

LambString *LambString::from_cstr(Vm &vm, char const *chars) {
    u64 len = strlen(chars);
    u32 hash = hash_string(chars);
    auto match = vm.strings.find_matching_key(chars, len, hash);
    if (match) {
        return match.value();
    }

    auto *lamb_str = LambString::alloc(vm, strdup(chars), len, hash);
    vm.push_stack(Value::from_obj((Object *)lamb_str));
    vm.strings.insert(vm, lamb_str, Value::from_bool(false));
    vm.pop_stack();

    return lamb_str;
}

LambString *LambString::concat(Vm &vm, LambString *rhs) const {
    i32 len = this->len + rhs->len;

    char *chars = (char *)vm.gc.alloc(vm, sizeof(char), len + 1);
    strncpy(chars, this->chars, this->len);
    strncpy(chars + this->len, rhs->chars, rhs->len);
    chars[len] = '\0';

    u32 hash = hash_string(chars);
    auto interned = vm.strings.find_matching_key(chars, len, hash);
    if (interned) {
        free(chars);
        return interned.value();
    }

    auto *ret = LambString::alloc(vm, chars, len, hash);
    vm.push_stack(Value::from_obj((Object *)(ret)));
    vm.strings.insert(vm, ret, Value::from_bool(false));
    vm.pop_stack();

    return ret;
}

LambArray *LambArray::alloc(Vm &vm, GcVec<Value> items) {
    auto *arr = (LambArray *)vm.gc.alloc(vm, sizeof(LambArray), 1);
    arr->obj = {
        .next = nullptr,
        .type = OtArray,
        .is_marked = false,
    };
    arr->items = items;

    vm.gc.add_object(&arr->obj);
    return arr;
}

LambFunc *LambFunc::alloc(Vm &vm, char const *name, u8 arity) {
    auto *func = (LambFunc *)vm.gc.alloc(vm, sizeof(LambFunc), 1);
    func->obj = {
        .next = nullptr,
        .type = OtFunc,
        .is_marked = false,
    };
    func->name = name;
    func->arity = arity;
    func->chunk = Chunk();
    func->upvalue_count = 0;

    vm.gc.add_object(&func->obj);
    return func;
}

NativeFunc *NativeFunc::alloc(Vm &vm, CFunc func) {
    auto *native = (NativeFunc *)vm.gc.alloc(vm, sizeof(NativeFunc), 1);
    Object obj = {
        .next = nullptr,
        .type = OtNative,
        .is_marked = false,
    };
    native->obj = obj;
    native->func = func;

    vm.gc.add_object(&native->obj);
    return native;
}

LambUpvalue *LambUpvalue::alloc(Vm &vm, Value *slot, LambUpvalue *next) {
    auto *upvalue = (LambUpvalue *)vm.gc.alloc(vm, sizeof(LambUpvalue), 1);
    upvalue->obj = {
        .next = nullptr,
        .type = OtUpvalue,
        .is_marked = false,
    };
    upvalue->location = slot;
    upvalue->next = next;
    upvalue->closed = Value::nil();

    vm.gc.add_object(&upvalue->obj);
    return upvalue;
}

LambClosure *LambClosure::alloc(Vm &vm, LambFunc *func) {
    auto *closure = (LambClosure *)vm.gc.alloc(vm, sizeof(LambClosure), 1);
    closure->obj = {
        .next = nullptr,
        .type = OtClosure,
        .is_marked = false,
    };
    closure->function = func;
    closure->upvalue_count = func->upvalue_count;

    closure->upvalues =
        (LambUpvalue **)vm.gc.alloc(vm, sizeof(LambUpvalue *), closure->upvalue_count);
    for (int i = 0; i < closure->upvalue_count; i++) {
        closure->upvalues[i] = nullptr;
    }

    vm.gc.add_object(&closure->obj);
    return closure;
}

void object_free(Vm &vm, Object *obj) {
    switch (obj->type) {
        case OtString: {
            auto *st = (LambString *)obj;
            vm.gc.free_array((char *)st->chars, sizeof(char), st->len + 1);
            vm.gc.free(st, sizeof(LambString));
            break;
        }
        case OtArray: {
            auto *arr = (LambArray *)obj;
            arr->items.destroy(vm);
            vm.gc.free(arr, sizeof(LambArray));
            break;
        }
        case OtFunc: {
            auto *func = (LambFunc *)obj;
            func->chunk.destroy(vm);
            vm.gc.free(func, sizeof(LambFunc));
            break;
        }
        case OtNative: {
            vm.gc.free((NativeFunc *)obj, sizeof(NativeFunc));
            break;
        }
        case OtClosure: {
            auto *closure = (LambClosure *)obj;
            vm.gc.free_array(closure->upvalues, sizeof(LambUpvalue *), closure->upvalue_count);
            vm.gc.free(closure, sizeof(LambClosure));
            break;
        }
        case OtUpvalue: {
            vm.gc.free((LambUpvalue *)obj, sizeof(LambUpvalue));
            break;
        }
    }
}

LambString *concat(Vm &vm, LambString *lhs, LambString *rhs) {
    i32 len = lhs->len + rhs->len;

    char *chars = (char *)vm.gc.alloc(vm, sizeof(char), len + 1);
    strncpy(chars, lhs->chars, lhs->len);
    strncpy(chars + lhs->len, rhs->chars, rhs->len);
    chars[len] = '\0';

    u32 hash = hash_string(chars);
    auto interned = vm.strings.find_matching_key(chars, len, hash);
    if (interned) {
        free(chars);
        return interned.value();
    }

    auto *ret = LambString::alloc(vm, chars, len, hash);
    vm.push_stack(Value::from_obj((Object *)(ret)));
    vm.strings.insert(vm, ret, Value::from_bool(false));
    vm.pop_stack();

    return ret;
}
