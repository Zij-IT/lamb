#ifndef TABLE_HEADER
#define TABLE_HEADER

#include "../types.hpp"
#include "object.hpp"
#include <optional>

#define TABLE_MAX_LOAD 0.75

struct Entry {
    LambString *key;
    Value val;
};

struct Table {
    Entry *entries;
    i32 capacity;
    i32 len;

    Table();

    Table(i32 len, i32 capacity, Entry* entries);

    void destroy(Vm& vm);

    Entry *entry(LambString* key) const;

    std::optional<LambString*> find_matching_key(char const* chars, i32 len, u32 hash) const;

    std::optional<Value> get(LambString *key) const;

    bool insert(Vm& vm, LambString *key, Value val);

    bool remove(LambString* key);

    void remove_marked();
};

#endif // TABLE_HEADER
