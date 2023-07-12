#ifndef TABLE_HEADER
#define TABLE_HEADER

#include "../types.hpp"
#include "object.hpp"

#define TABLE_MAX_LOAD 0.75

typedef struct {
    LambString *key;
    Value val;
} Entry;

typedef struct Table {
    i32 len;
    i32 capacity;
    Entry *entries;
} Table;

void table_init(Table *table);

void table_free(Vm *vm, Table *table);

Entry *table_find(Table *table, LambString *key);

LambString *table_find_string(Table *table, char const* chars, i32 len, u32 hash);

bool table_insert(Vm *vm, Table *table, LambString *key, Value val);

bool table_remove(Table *table, LambString *key);

void table_remove_white(Table *table);

bool table_get(Table *table, LambString *key, Value *val);

#endif // TABLE_HEADER
