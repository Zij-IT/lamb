#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "misc.h"
#include "object.h"
#include "vm.h"
#include "../memory.h"

Object* alloc_obj(Vm* vm, ObjectType type) {
  switch(type) {
    case OtString: {
      LambString* st = malloc(sizeof(LambString));
      Object obj = { .type = type, .next = vm->poor_mans_gc, };
      st->obj = obj;
      st->chars = NULL;
      st->len = 0;
      st->hash = 0;
      vm->poor_mans_gc = (Object*)st;
      return vm->poor_mans_gc;
    }
  }
  
  return NULL;
}

void object_free(Object* obj) {
  switch(obj->type) {
    case OtString: {
      LambString* st = (LambString*)obj;
      FREE_ARRAY(char, st->chars, st->len + 1);
      FREE(LambString, st);
      break;
    }
  }
}

bool is_of_type(Object* obj, ObjectType type) {
  return obj->type == type;
}

LambString* concat(Vm* vm, LambString* lhs, LambString* rhs) {
  i32 len = lhs->len + rhs->len;

  string chars = malloc(sizeof(char) * len + 1);
  strcpy(chars, lhs->chars);
  strcpy(chars + lhs->len, rhs->chars);
  chars[len] = '\0';

  u32 hash = hash_string(chars);
  LambString* interned = table_find_string(&vm->strings, chars, len, hash);
  if (interned != NULL) {
    free(chars);
    return interned;
  }

  LambString* ret = (LambString*)alloc_obj(vm, OtString);
  ret->chars = chars;
  ret->len = len;
  ret->hash = hash_string(chars);
  
  table_insert(&vm->strings, ret, new_boolean(false));
  
  return ret;
}
