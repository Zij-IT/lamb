#include <stdlib.h>
#include <stdio.h>

#include "./value.h"
#include "../memory.h"

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

Value new_string(string st) {
  Value val = { .kind = VkObject, .as.obj = malloc(sizeof(ObjString*))};
  val.as.obj->type = OtString;
  as_string(val)->chars = st;
  return val;
}

Value new_char(char c) {
  Value val = { .kind = VkChar, .as.ch = c };
  return val;
}

Value new_obj(Obj* obj) {
  Value val = { .kind = VkObject, .as.obj = obj };
  return val;
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

bool is_obj(Value val) {
  return val.kind == VkObject;
}

bool is_obj_of_type(Value val, ObjectType type) {
  return is_obj(val) && val.as.obj->type == type;
}

ObjString* as_string(Value val) {
  return (ObjString*)val.as.obj;
}

str as_cstring(Value val) {
  return as_string(val)->chars;
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
