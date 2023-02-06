#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "../memory.h"
#include "debug.h"
#include "misc.h"
#include "object.h"
#include "vm.h"

Object *alloc_obj(Vm *vm, ObjectType type) {
  switch (type) {
  case OtString: {
    LambString *st = ALLOCATE(LambString, 1);
    Object obj = {
        .type = type,
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
    LambArray *arr = ALLOCATE(LambArray, 1);
    Object obj = {
        .type = type,
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
    LambFunc *func = ALLOCATE(LambFunc, 1);
    Object obj = {
        .type = type,
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
    NativeFunc *func = ALLOCATE(NativeFunc, 1);
    Object obj = {
        .type = type,
        .next = vm->poor_mans_gc,
    };
    func->obj = obj;
    func->func = NULL;
    vm->poor_mans_gc = (Object *)func;
    return vm->poor_mans_gc;
  }
  case OtClosure: {
    LambClosure *closure = ALLOCATE(LambClosure, 1);
    Object obj = {
        .type = type,
        .next = vm->poor_mans_gc,
    };
    closure->obj = obj;
    closure->function = NULL;
    vm->poor_mans_gc = (Object *)closure;
    return vm->poor_mans_gc;
  }
  case OtUpvalue: {
    LambUpvalue *upvalue = ALLOCATE(LambUpvalue, 1);
    Object obj = {
        .type = type,
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

void object_free(Object *obj) {
  switch (obj->type) {
  case OtString: {
    LambString *st = (LambString *)obj;
    FREE_ARRAY(char, st->chars, st->len + 1);
    FREE(LambString, st);
    break;
  }
  case OtArray: {
    LambArray *arr = (LambArray *)obj;
    value_arr_free(&arr->items);
    FREE(LambArray, arr);
    break;
  }
  case OtFunc: {
    LambFunc *func = (LambFunc *)obj;
    chunk_free(&func->chunk);
    FREE(LambFunc, func);
    break;
  }
  case OtNative: {
    FREE(NativeFunc, obj);
    break;
  }
  case OtClosure: {
    LambClosure *closure = (LambClosure *)obj;
    FREE_ARRAY(LambUpvalue *, closure->upvalues, closure->upvalue_count);
    FREE(LambClosure, closure);
    break;
  }
  case OtUpvalue: {
    FREE(LambUpvalue, obj);
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

    table_insert(&vm->strings, interned, new_boolean(false));
  }

  return interned;
}

LambString *concat(Vm *vm, LambString *lhs, LambString *rhs) {
  i32 len = lhs->len + rhs->len;

  string chars = ALLOCATE(char, len + 1);
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

  table_insert(&vm->strings, ret, new_boolean(false));

  return ret;
}

LambClosure *to_closure(Vm *vm, LambFunc *func) {
  LambClosure *closure = (LambClosure *)alloc_obj(vm, OtClosure);
  closure->function = func;
  closure->upvalue_count = func->upvalue_count;
  closure->upvalues = ALLOCATE(LambUpvalue *, closure->upvalue_count);
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
