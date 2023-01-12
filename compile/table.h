#ifndef TABLE_HEADER
#define TABLE_HEADER

#include "../types.h"
#include "object.h"
#include "value.h"

#define TABLE_MAX_LOAD 0.75

typedef struct {
  LambString* key;
  Value val;
} Entry;

typedef struct {
  i32 len;
  i32 capacity;
  Entry* entries;
} Table;

void table_init(Table* table);

void table_free(Table* table);

void table_copy_entries(Table* from, Table* to);

Entry* table_find(Table* table, LambString* key);

LambString* table_find_string(Table* table, str chars, i32 len, u32 hash);

bool table_insert(Table* table, LambString* key, Value val);

bool table_remove(Table* table, LambString* key);

bool table_get(Table* table, LambString* key, Value* val);

#endif//TABLE_HEADER
