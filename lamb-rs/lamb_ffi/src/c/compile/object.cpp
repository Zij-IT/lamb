#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "../debug/debug.hpp"
#include "../vm/vm.hpp"
#include "memory.hpp"
#include "misc.hpp"
#include "object.hpp"

LambString* LambString::alloc(Vm& vm, char const* chars, i32 len, u32 hash) {
    LambString *st = ALLOCATE(vm, LambString, 1);
    st->obj = { .next = vm.objects, .type = OtString, .is_marked = false, };
    st->chars = chars;
    st->len = len;
    st->hash = hash;

    vm.objects = (Object *)st;
    return st;
}

Object *alloc_obj(Vm& vm, ObjectType type) {
    switch (type) {
        case OtString: {
            printf("Uncaught alloc_obj!?\n");
            exit(1);
        }
        case OtArray: {
            LambArray *arr = ALLOCATE(vm, LambArray, 1);
#ifdef DEBUG_LOG_GC
            printf("%p allocating OtArray\n", (void *)arr);
#endif
            Object obj = {
                .next = vm.objects,
                .type = type,
                .is_marked = false,
            };
            arr->obj = obj;
            arr->items = GcVec<Value>();
            vm.objects = (Object *)arr;
            return vm.objects;
        }
        case OtFunc: {
            LambFunc *func = ALLOCATE(vm, LambFunc, 1);
#ifdef DEBUG_LOG_GC
            printf("%p allocating OtFunc\n", (void *)func);
#endif
            Object obj = {
                .next = vm.objects,
                .type = type,
                .is_marked = false,
            };
            func->obj = obj;
            func->name = NULL;
            func->arity = 0;
            func->upvalue_count = 0;
            func->chunk = Chunk();
            vm.objects = (Object *)func;
            return vm.objects;
        }
        case OtNative: {
            NativeFunc *func = ALLOCATE(vm, NativeFunc, 1);
#ifdef DEBUG_LOG_GC
            printf("%p allocating OtNative\n", (void *)func);
#endif
            Object obj = {
                .next = vm.objects,
                .type = type,
                .is_marked = false,
            };
            func->obj = obj;
            func->func = NULL;
            vm.objects = (Object *)func;
            return vm.objects;
        }
        case OtClosure: {
            LambClosure *closure = ALLOCATE(vm, LambClosure, 1);
#ifdef DEBUG_LOG_GC
            printf("%p allocating OtClosure\n", (void *)closure);
#endif
            Object obj = {
                .next = vm.objects,
                .type = type,
                .is_marked = false,
            };
            closure->obj = obj;
            closure->function = NULL;
            vm.objects = (Object *)closure;
            return vm.objects;
        }
        case OtUpvalue: {
            LambUpvalue *upvalue = ALLOCATE(vm, LambUpvalue, 1);
#ifdef DEBUG_LOG_GC
            printf("%p allocating OtUpvalue\n", (void *)upvalue);
#endif
            Object obj = {
                .next = vm.objects,
                .type = type,
                .is_marked = false,
            };
            upvalue->obj = obj;
            upvalue->next = NULL;
            upvalue->location = NULL;
            vm.objects = (Object *)upvalue;
            return vm.objects;
        }
    }

    return NULL;
}

void object_free(Vm& vm, Object *obj) {
    switch (obj->type) {
        case OtString: {
#ifdef DEBUG_LOG_GC
            printf("Freeing %p of type OtString [%s]\n", obj, ((LambString *)obj)->chars);
#endif
            LambString *st = (LambString *)obj;
            FREE_ARRAY(vm, char, st->chars, st->len + 1);
            FREE(vm, LambString, st);
            break;
        }
        case OtArray: {
#ifdef DEBUG_LOG_GC
            printf("Freeing %p of type OtArray\n", obj);
#endif
            LambArray *arr = (LambArray *)obj;
            arr->items.destroy(vm);
            FREE(vm, LambArray, arr);
            break;
        }
        case OtFunc: {
#ifdef DEBUG_LOG_GC
            printf("Freeing %p of type OtFunc\n", obj);
#endif
            LambFunc *func = (LambFunc *)obj;
            func->chunk.destroy(vm);
            FREE(vm, LambFunc, func);
            break;
        }
        case OtNative: {
#ifdef DEBUG_LOG_GC
            printf("Freeing %p of type OtNative\n", obj);
#endif
            FREE(vm, NativeFunc, obj);
            break;
        }
        case OtClosure: {
#ifdef DEBUG_LOG_GC
            printf("Freeing %p of type OtClosure\n", obj);
#endif
            LambClosure *closure = (LambClosure *)obj;
            FREE_ARRAY(vm, LambUpvalue *, closure->upvalues, closure->upvalue_count);
            FREE(vm, LambClosure, closure);
            break;
        }
        case OtUpvalue: {
#ifdef DEBUG_LOG_GC
            printf("Freeing %p of type OtUpvalue\n", obj);
#endif
            FREE(vm, LambUpvalue, obj);
            break;
        }
    }
}

bool is_of_type(Object *obj, ObjectType type) { return obj->type == type; }

LambString *cstr_to_lambstring(Vm& vm, char const*  cstr) {
    u64 len = strlen(cstr);
    u32 hash = hash_string(cstr);
    auto match = vm.strings.find_matching_key(cstr, len, hash);
    if (match) {
        return match.value();
    }
   
    auto lamb_str = LambString::alloc(vm, strdup(cstr), len, hash);
    vm_push_stack(vm, Value::from_obj((Object *)lamb_str));
    vm.strings.insert(vm, lamb_str, Value::from_bool(false));
    vm_pop_stack(vm);

    return lamb_str;
}

LambString *concat(Vm& vm, LambString *lhs, LambString *rhs) {
    i32 len = lhs->len + rhs->len;

    string chars = ALLOCATE(vm, char, len + 1);
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

LambClosure *to_closure(Vm& vm, LambFunc *func) {
    LambClosure *closure = (LambClosure *)alloc_obj(vm, OtClosure);
    closure->function = func;
    closure->upvalue_count = func->upvalue_count;
    closure->upvalues = ALLOCATE(vm, LambUpvalue *, closure->upvalue_count);
    for (int i = 0; i < closure->upvalue_count; i++) {
        closure->upvalues[i] = NULL;
    }

    return closure;
}

LambUpvalue *to_upvalue(Vm& vm, Value *slot) {
    LambUpvalue *upvalue = (LambUpvalue *)alloc_obj(vm, OtUpvalue);
    upvalue->location = slot;
    upvalue->closed = Value::nil();
    return upvalue;
}
