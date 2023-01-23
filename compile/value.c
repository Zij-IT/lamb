#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "./value.h"
#include "./object.h"
#include "../memory.h"

Value new_nil() {
  Value val = { .kind = VkNil, .as.boolean = false, };
  return val;
}

Value new_boolean(bool b) {
  Value val = { .kind = VkBool, .as.boolean = b };
  return val;
}

Value new_int(i64 num) {
  Value val = { .kind = VkInt, .as.intn = num };
  return val;
}

Value new_double(double num) {
  Value val = { .kind = VkDouble, .as.doubn = num };
  return val;
}

Value new_char(char c) {
  Value val = { .kind = VkChar, .as.ch = c };
  return val;
}

Value new_object(Object* obj) {
  Value val = { .kind = VkObj, .as.obj = obj };
  return val;
}

bool is_nil(Value val) {
  return val.kind == VkNil;
}

bool is_bool(Value val) {
  return val.kind == VkBool;
}

bool is_double(Value val) {
  return val.kind == VkDouble;
}

bool is_integer(Value val) {
  return val.kind == VkInt;
}

bool is_char(Value val) {
  return val.kind == VkChar;
}

bool is_object(Value val) {
  return val.kind == VkObj;
}

void print_kind(Value val) {
  switch(val.kind) {
    case VkNil:    printf("nil");    break;
    case VkBool:   printf("bool");   break;
    case VkInt:    printf("int");    break;
    case VkDouble: printf("double"); break;
    case VkChar:   printf("char");   break;
    case VkObj: {
      switch(val.as.obj->type) {
        case OtString: printf("string"); break;
        case OtArray:  printf("array"); break;
      }
      break;
    }
  }
}

void arr_init(ValueArray* arr) {
  arr->len = 0;
  arr->capacity = 0;
  arr->values = NULL;
}

void arr_write(ValueArray* arr, Value val) {
  if(arr->capacity < arr->len + 1) {
    i32 old_cap = arr->capacity;
    arr->capacity = GROW_CAPACITY(old_cap);
    arr->values = GROW_ARRAY(Value, arr->values, old_cap, arr->capacity);
  }  

  arr->values[arr->len] = val;
  arr->len += 1;
}

void arr_free(ValueArray* arr) {
  FREE_ARRAY(Value, arr->values, arr->capacity);
  arr_init(arr);
}
