#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "../debug/debug.hpp"
#include "../vm/vm.hpp"
#include "misc.hpp"
#include "object.hpp"

LambString* LambString::alloc(Vm& vm, char const* chars, i32 len, u32 hash) {
    auto st = (LambString*)vm.gc.alloc(vm, sizeof(LambString), 1);
    st->obj = { .next = vm.objects, .type = OtString, .is_marked = false, };
    st->chars = chars;
    st->len = len;
    st->hash = hash;

    vm.objects = (Object *)st;
    return st;
}

LambString* LambString::from_cstr(Vm&vm, char const* chars) {
    u64 len = strlen(chars);
    u32 hash = hash_string(chars);
    auto match = vm.strings.find_matching_key(chars, len, hash);
    if (match) {
        return match.value();
    }
   
    auto lamb_str = LambString::alloc(vm, strdup(chars), len, hash);
    vm_push_stack(vm, Value::from_obj((Object *)lamb_str));
    vm.strings.insert(vm, lamb_str, Value::from_bool(false));
    vm_pop_stack(vm);

    return lamb_str;
}


LambString* LambString::concat(Vm& vm, LambString* rhs) {
    i32 len = this->len + rhs->len;

    char* chars = (char*)vm.gc.alloc(vm, sizeof(char), len + 1);
    strcpy(chars, this->chars);
    strcpy(chars + this->len, rhs->chars);
    chars[len] = '\0';

    u32 hash = hash_string(chars);
    auto interned = vm.strings.find_matching_key(chars, len, hash);
    if (interned) {
        free(chars);
        return interned.value();
    }

    auto ret = LambString::alloc(vm, chars, len, hash);
    vm_push_stack(vm, Value::from_obj((Object *)(ret)));
    vm.strings.insert(vm, ret, Value::from_bool(false));
    vm_pop_stack(vm);

    return ret;
} 

LambArray* LambArray::alloc(Vm& vm, GcVec<Value> items) {
    auto arr = (LambArray*)vm.gc.alloc(vm, sizeof(LambArray), 1);
    arr->obj = { .next = vm.objects, .type = OtArray, .is_marked = false, };
    arr->items = items;

    vm.objects = (Object *)arr;
    return arr;
}

LambFunc* LambFunc::alloc(Vm& vm, char const* name, u8 arity) {
    auto func = (LambFunc*)vm.gc.alloc(vm, sizeof(LambFunc), 1);
    func->obj = { .next = vm.objects, .type = OtFunc, .is_marked = false, };
    func->name = name;
    func->arity = arity;
    func->chunk = Chunk();
    func->upvalue_count = 0;

    vm.objects = (Object *)func;
    return func;
}

NativeFunc* NativeFunc::alloc(Vm& vm, CFunc func) {
    auto native = (NativeFunc *)vm.gc.alloc(vm, sizeof(NativeFunc), 1);
    Object obj = { .next = vm.objects, .type = OtNative, .is_marked = false, };
    native->obj = obj;
    native->func = func;

    vm.objects = (Object *)native;
    return native;
}

LambUpvalue* LambUpvalue::alloc(Vm& vm, Value* slot, LambUpvalue* next) {
    auto upvalue = (LambUpvalue *)vm.gc.alloc(vm, sizeof(LambUpvalue), 1);
    upvalue->obj = { .next = vm.objects, .type = OtUpvalue, .is_marked = false, };
    upvalue->location = slot;
    upvalue->next = next;
    upvalue->closed = Value::nil();

    vm.objects = (Object *)upvalue;
    return upvalue;
}

LambClosure* LambClosure::alloc(Vm& vm, LambFunc* func) {
    auto closure = (LambClosure *)vm.gc.alloc(vm, sizeof(LambClosure), 1);
    closure->obj = { .next = vm.objects, .type = OtClosure, .is_marked = false, };
    closure->function = func;
    closure->upvalue_count = func->upvalue_count;

    closure->upvalues =(LambUpvalue **)vm.gc.alloc(vm, sizeof(LambUpvalue *), closure->upvalue_count);
    for (int i = 0; i < closure->upvalue_count; i++) {
        closure->upvalues[i] = NULL;
    }

    vm.objects = (Object *)closure;
    return closure;
}

void object_free(Vm& vm, Object *obj) {
    switch (obj->type) {
        case OtString: {
#ifdef DEBUG_LOG_GC
            printf("Freeing %p of type OtString [%s]\n", obj, ((LambString *)obj)->chars);
#endif
            LambString *st = (LambString *)obj;
            vm.gc.free_array(vm, (char*)st->chars, sizeof(char), st->len + 1);
            vm.gc.free(vm, st, sizeof(LambString));
            break;
        }
        case OtArray: {
#ifdef DEBUG_LOG_GC
            printf("Freeing %p of type OtArray\n", obj);
#endif
            LambArray *arr = (LambArray *)obj;
            arr->items.destroy(vm);
            vm.gc.free(vm, arr, sizeof(LambArray));
            break;
        }
        case OtFunc: {
#ifdef DEBUG_LOG_GC
            printf("Freeing %p of type OtFunc\n", obj);
#endif
            LambFunc *func = (LambFunc *)obj;
            func->chunk.destroy(vm);
            vm.gc.free(vm, func, sizeof(LambFunc));
            break;
        }
        case OtNative: {
#ifdef DEBUG_LOG_GC
            printf("Freeing %p of type OtNative\n", obj);
#endif
            vm.gc.free(vm, (NativeFunc*)obj, sizeof(NativeFunc));
            break;
        }
        case OtClosure: {
#ifdef DEBUG_LOG_GC
            printf("Freeing %p of type OtClosure\n", obj);
#endif
            LambClosure *closure = (LambClosure *)obj;
            vm.gc.free_array(vm, closure->upvalues, sizeof(LambUpvalue *), closure->upvalue_count);
            vm.gc.free(vm, closure, sizeof(LambClosure));
            break;
        }
        case OtUpvalue: {
#ifdef DEBUG_LOG_GC
            printf("Freeing %p of type OtUpvalue\n", obj);
#endif
            vm.gc.free(vm, (LambUpvalue*)obj, sizeof(LambUpvalue));
            break;
        }
    }
}

bool is_of_type(Object *obj, ObjectType type) { return obj->type == type; }

LambString *concat(Vm& vm, LambString *lhs, LambString *rhs) {
    i32 len = lhs->len + rhs->len;

    char* chars = (char*)vm.gc.alloc(vm, sizeof(char), len + 1);
    strcpy(chars, lhs->chars);
    strcpy(chars + lhs->len, rhs->chars);
    chars[len] = '\0';

    u32 hash = hash_string(chars);
    auto interned = vm.strings.find_matching_key(chars, len, hash);
    if (interned) {
        free(chars);
        return interned.value();
    }

    auto ret = LambString::alloc(vm, chars, len, hash);
    vm_push_stack(vm, Value::from_obj((Object *)(ret)));
    vm.strings.insert(vm, ret, Value::from_bool(false));
    vm_pop_stack(vm);

    return ret;
}
