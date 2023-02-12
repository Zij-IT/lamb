#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "memory.h"
#include "debug.h"
#include "misc.h"
#include "object.h"
#include "vm.h"

Object *alloc_obj(Vm *vm, ObjectType type) {
  switch (type) {
  case OtString: {
    LambString *st = ALLOCATE(vm, LambString, 1);
    #ifdef DEBUG_LOG_GC
    printf("%p allocating OtString\n");
    #endif
    Object obj = {
        .type = type,
        .is_marked = false,
        .next = vm->poor_mans_gc,
    };
    st->obj = obj;
    st->chars = NULL;
    st->len = 0;
    st->hash = 0;
    vm->poor_mans_gc = (Object *)st;
    return vm->poor_mans_gc;
  }
  case OtArray: {
    LambArray *arr = ALLOCATE(vm, LambArray, 1);
    #ifdef DEBUG_LOG_GC
    printf("%p allocating OtArray\n");
    #endif
    Object obj = {
        .type = type,
        .is_marked = false,
        .next = vm->poor_mans_gc,
    };
    arr->obj = obj;
    ValueArray v_arr;
    value_arr_init(&v_arr);
    arr->items = v_arr;
    vm->poor_mans_gc = (Object *)arr;
    return vm->poor_mans_gc;
  }
  case OtFunc: {
    LambFunc *func = ALLOCATE(vm, LambFunc, 1);
    #ifdef DEBUG_LOG_GC
    printf("%p allocating OtFunc\n");
    #endif
    Object obj = {
        .type = type,
        .is_marked = false,
        .next = vm->poor_mans_gc,
    };
    func->obj = obj;
    func->name = NULL;
    func->arity = 0;
    func->upvalue_count = 0;
    chunk_init(&func->chunk);
    vm->poor_mans_gc = (Object *)func;
    return vm->poor_mans_gc;
  }
  case OtNative: {
    NativeFunc *func = ALLOCATE(vm, NativeFunc, 1);
    #ifdef DEBUG_LOG_GC
    printf("%p allocating OtNative\n");
    #endif
    Object obj = {
        .type = type,
        .is_marked = false,
        .next = vm->poor_mans_gc,
    };
    func->obj = obj;
    func->func = NULL;
    vm->poor_mans_gc = (Object *)func;
    return vm->poor_mans_gc;
  }
  case OtClosure: {
    LambClosure *closure = ALLOCATE(vm, LambClosure, 1);
    #ifdef DEBUG_LOG_GC
    printf("%p allocating OtClosure\n");
    #endif
    Object obj = {
        .type = type,
        .is_marked = false,
        .next = vm->poor_mans_gc,
    };
    closure->obj = obj;
    closure->function = NULL;
    vm->poor_mans_gc = (Object *)closure;
    return vm->poor_mans_gc;
  }
  case OtUpvalue: {
    LambUpvalue *upvalue = ALLOCATE(vm, LambUpvalue, 1);
    #ifdef DEBUG_LOG_GC
    printf("%p allocating OtUpvalue\n");
    #endif
    Object obj = {
        .type = type,
        .is_marked = false,
        .next = vm->poor_mans_gc,
    };
    upvalue->obj = obj;
    upvalue->next = NULL;
    upvalue->location = NULL;
    vm->poor_mans_gc = (Object *)upvalue;
    return vm->poor_mans_gc;
  }
  }

  return NULL;
}

void object_free(Vm* vm, Object *obj) {
  switch (obj->type) {
  case OtString: {
    #ifdef DEBUG_LOG_GC
    printf("Freeing %p of type OtString\n", obj);
    #endif
    LambString *st = (LambString *)obj;
    FREE_ARRAY(char, st->chars, st->len + 1);
    FREE(vm, LambString, st);
    break;
  }
  case OtArray: {
    #ifdef DEBUG_LOG_GC
    printf("Freeing %p of type OtArray\n", obj);
    #endif
    LambArray *arr = (LambArray *)obj;
    value_arr_free(&arr->items);
    FREE(vm, LambArray, arr);
    break;
  }
  case OtFunc: {
    #ifdef DEBUG_LOG_GC
    printf("Freeing %p of type OtFunc\n", obj);
    #endif
    LambFunc *func = (LambFunc *)obj;
    chunk_free(vm, &func->chunk);
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
    FREE_ARRAY(LambUpvalue *, closure->upvalues, closure->upvalue_count);
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

LambString *cstr_to_lambstring(Vm *vm, str cstr) {
  u64 len = strlen(cstr);
  u32 hash = hash_string(cstr);
  LambString *interned = table_find_string(&vm->strings, cstr, len, hash);
  if (interned == NULL) {
    interned = (LambString *)alloc_obj(vm, OtString);
    interned->chars = strdup(cstr);
    interned->hash = hash;
    interned->len = len;

    table_insert(vm, &vm->strings, interned, new_boolean(false));
  }

  return interned;
}

LambString *concat(Vm *vm, LambString *lhs, LambString *rhs) {
  i32 len = lhs->len + rhs->len;

  string chars = ALLOCATE(vm, char, len + 1);
  strcpy(chars, lhs->chars);
  strcpy(chars + lhs->len, rhs->chars);
  chars[len] = '\0';

  u32 hash = hash_string(chars);
  LambString *interned = table_find_string(&vm->strings, chars, len, hash);
  if (interned != NULL) {
    free(chars);
    return interned;
  }

  LambString *ret = (LambString *)alloc_obj(vm, OtString);
  ret->chars = chars;
  ret->len = len;
  ret->hash = hash_string(chars);

  table_insert(vm, &vm->strings, ret, new_boolean(false));

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
  upvalue->closed = new_nil();
  return upvalue;
}

void objectptr_array_init(ObjectPtrArray *arr) {
  arr->capacity = 0;
  arr->len = 0;
  arr->values = NULL;
}

void objectptr_array_write(ObjectPtrArray *arr, Object *val) {
  if (arr->capacity < arr->len + 1) {
    i32 old_cap = arr->capacity;
    arr->capacity = GROW_CAPACITY(old_cap);
    arr->values = GROW_ARRAY(NULL, Object*, arr->values, old_cap, arr->capacity);
  }

  arr->values[arr->len] = val;
  arr->len += 1;
}

void objectptr_array_free(ObjectPtrArray *arr) {
  FREE_ARRAY(Object*, arr->values, arr->capacity);
  objectptr_array_init(arr);
}
