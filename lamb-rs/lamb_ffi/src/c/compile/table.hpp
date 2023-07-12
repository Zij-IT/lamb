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

    Table();

    Table(i32 len, i32 capacity, Entry* entries);

    void destroy(Vm *vm);

    Entry *find(LambString* key);

    LambString *find_string(char const* chars, i32 len, u32 hash);

    bool get(LambString *key, Value *val);

    bool insert(Vm *vm, LambString *key, Value val);

    bool remove(LambString* key);

    void remove_marked();
} Table;

#endif // TABLE_HEADER
