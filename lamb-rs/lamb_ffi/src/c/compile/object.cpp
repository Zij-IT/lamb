#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "../debug/debug.hpp"
#include "../vm/vm.hpp"
#include "memory.hpp"
#include "misc.hpp"
#include "object.hpp"

Object *alloc_obj(Vm *vm, ObjectType type) {
    switch (type) {
        case OtString: {
            LambString *st = ALLOCATE(vm, LambString, 1);
#ifdef DEBUG_LOG_GC
            printf("%p allocating OtString\n", (void *)st);
#endif
            Object obj = {
                .type = type,
                .is_marked = false,
                .next = vm->objects,
            };
            st->obj = obj;
            st->chars = NULL;
            st->len = 0;
            st->hash = 0;
            vm->objects = (Object *)st;
            return vm->objects;
        }
        case OtArray: {
            LambArray *arr = ALLOCATE(vm, LambArray, 1);
#ifdef DEBUG_LOG_GC
            printf("%p allocating OtArray\n", (void *)arr);
#endif
            Object obj = {
                .type = type,
                .is_marked = false,
                .next = vm->objects,
            };
            arr->obj = obj;
            ValueArray v_arr;
            value_arr_init(&v_arr);
            arr->items = v_arr;
            vm->objects = (Object *)arr;
            return vm->objects;
        }
        case OtFunc: {
            LambFunc *func = ALLOCATE(vm, LambFunc, 1);
#ifdef DEBUG_LOG_GC
            printf("%p allocating OtFunc\n", (void *)func);
#endif
            Object obj = {
                .type = type,
                .is_marked = false,
                .next = vm->objects,
            };
            func->obj = obj;
            func->name = NULL;
            func->arity = 0;
            func->upvalue_count = 0;
            func->chunk = Chunk();
            vm->objects = (Object *)func;
            return vm->objects;
        }
        case OtNative: {
            NativeFunc *func = ALLOCATE(vm, NativeFunc, 1);
#ifdef DEBUG_LOG_GC
            printf("%p allocating OtNative\n", (void *)func);
#endif
            Object obj = {
                .type = type,
                .is_marked = false,
                .next = vm->objects,
            };
            func->obj = obj;
            func->func = NULL;
            vm->objects = (Object *)func;
            return vm->objects;
        }
        case OtClosure: {
            LambClosure *closure = ALLOCATE(vm, LambClosure, 1);
#ifdef DEBUG_LOG_GC
            printf("%p allocating OtClosure\n", (void *)closure);
#endif
            Object obj = {
                .type = type,
                .is_marked = false,
                .next = vm->objects,
            };
            closure->obj = obj;
            closure->function = NULL;
            vm->objects = (Object *)closure;
            return vm->objects;
        }
        case OtUpvalue: {
            LambUpvalue *upvalue = ALLOCATE(vm, LambUpvalue, 1);
#ifdef DEBUG_LOG_GC
            printf("%p allocating OtUpvalue\n", (void *)upvalue);
#endif
            Object obj = {
                .type = type,
                .is_marked = false,
                .next = vm->objects,
            };
            upvalue->obj = obj;
            upvalue->next = NULL;
            upvalue->location = NULL;
            vm->objects = (Object *)upvalue;
            return vm->objects;
        }
    }

    return NULL;
}

void object_free(Vm *vm, Object *obj) {
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
            value_arr_free(vm, &arr->items);
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

LambString *cstr_to_lambstring(Vm *vm, char const*  cstr) {
    u64 len = strlen(cstr);
    u32 hash = hash_string(cstr);
    auto match = vm->strings.find_matching_key(cstr, len, hash);
    if (match) {
        return match.value();
    }
   
    auto lamb_str = (LambString *)alloc_obj(vm, OtString);
    lamb_str->chars = strdup(cstr);
    lamb_str->hash = hash;
    lamb_str->len = len;

    vm_push_stack(vm, Value::from_obj((Object *)lamb_str));
    vm->strings.insert(vm, lamb_str, Value::from_bool(false));
    vm_pop_stack(vm);

    return lamb_str;
}

LambString *concat(Vm *vm, LambString *lhs, LambString *rhs) {
    i32 len = lhs->len + rhs->len;

    string chars = ALLOCATE(vm, char, len + 1);
    strcpy(chars, lhs->chars);
    strcpy(chars + lhs->len, rhs->chars);
    chars[len] = '\0';

    u32 hash = hash_string(chars);
    auto interned = vm->strings.find_matching_key(chars, len, hash);
    if (interned) {
        free(chars);
        return interned.value();
    }

    LambString *ret = (LambString *)alloc_obj(vm, OtString);
    ret->chars = chars;
    ret->len = len;
    ret->hash = hash_string(chars);

    vm_push_stack(vm, Value::from_obj((Object *)(ret)));
    vm->strings.insert(vm, ret, Value::from_bool(false));
    vm_pop_stack(vm);

    return ret;
}

LambClosure *to_closure(Vm *vm, LambFunc *func) {
    LambClosure *closure = (LambClosure *)alloc_obj(vm, OtClosure);
    closure->function = func;
    closure->upvalue_count = func->upvalue_count;
    closure->upvalues = ALLOCATE(vm, LambUpvalue *, closure->upvalue_count);
    for (int i = 0; i < closure->upvalue_count; i++) {
        closure->upvalues[i] = NULL;
    }

    return closure;
}

LambUpvalue *to_upvalue(Vm *vm, Value *slot) {
    LambUpvalue *upvalue = (LambUpvalue *)alloc_obj(vm, OtUpvalue);
    upvalue->location = slot;
    upvalue->closed = Value::nil();
    return upvalue;
}

void objectptr_array_init(ObjectPtrArray *arr) {
    arr->capacity = 0;
    arr->len = 0;
    arr->values = NULL;
}

void objectptr_array_write(__attribute__((unused)) Vm *vm, ObjectPtrArray *arr, Object *val) {
    if (arr->capacity < arr->len + 1) {
        i32 old_cap = arr->capacity;
        arr->capacity = GROW_CAPACITY(old_cap);
        arr->values = (Object**)realloc(arr->values, sizeof(Object *) * arr->capacity);
    }

    vm->bytes_allocated = 0;

    arr->values[arr->len] = val;
    arr->len += 1;
}

void objectptr_array_free(Vm *vm, ObjectPtrArray *arr) {
    FREE_ARRAY(vm, Object *, arr->values, arr->capacity);
    objectptr_array_init(arr);
}
