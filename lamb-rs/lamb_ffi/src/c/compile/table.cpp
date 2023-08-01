#include <cstring>
#include <optional>
#include <sstream>
#include <string>

#include "../types.hpp"
#include "../vm/vm.hpp"
#include "gc.hpp"
#include "object.hpp"
#include "table.hpp"
#include "value.hpp"

#define TOMBSTONE Value::from_bool(true)

namespace {
bool is_tombstone(Entry *entry) { return entry->val.is_bool() && entry->val.as.boolean; }

void table_adjust_capacity(Vm &vm, Table *table, i32 capacity) {
    auto *entries = (Entry *)vm.gc.alloc(vm, sizeof(Entry), capacity);

    // This table is required due to table_find call below, and has no other
    // function hence the length being set to 0 despite entries being filled
    Table temp_table(0, capacity, entries);

    for (i32 i = 0; i < capacity; i++) {
        entries[i].key = nullptr;
        // This is a dummy value and should probably be replaced with something
        // in the future. This is a poor man's Option::None
        entries[i].val = Value::from_bool(false);
    }

    table->len = 0;
    for (i32 i = 0; i < table->capacity; i++) {
        Entry *entry = &table->entries[i];
        if (entry->key == nullptr) {
            continue;
        }

        Entry *dest = temp_table.entry(entry->key);
        dest->key = entry->key;
        dest->val = entry->val;
        table->len++;
    }

    vm.gc.free_array(table->entries, sizeof(Entry), table->capacity);
    table->entries = entries;
    table->capacity = capacity;
}
} // namespace

Table::Table() : entries(nullptr), capacity(0), len(0) {}

Table::Table(i32 len, i32 capacity, Entry *entries)
    : entries(entries), capacity(capacity), len(len) {}

void Table::destroy(Vm &vm) {
    vm.gc.free_array(this->entries, sizeof(Entry), this->capacity);
    this->entries = nullptr;
    this->capacity = 0;
    this->len = 0;
}

Entry *Table::entry(LambString *key) const {
    // Correctness: Table capacity is guarunteed to be a multiple of 2
    //              in which case x % n is the same as X & (n - 1)
    u32 index = key->hash & (this->capacity - 1);
    Entry *tombstone = nullptr;

    while (true) {
        Entry *entry = &this->entries[index];
        if (entry->key == nullptr) {
            if (is_tombstone(entry)) {
                if (tombstone == nullptr) {
                    tombstone = entry;
                }
            } else {
                return tombstone != nullptr ? tombstone : entry;
            }
        } else if (entry->key == key) {
            return entry;
        }

        index = (index + 1) & (this->capacity - 1);
    }
}

std::optional<LambString *> Table::find_matching_key(char const *chars, i32 len, u32 hash) const {
    if (this->len == 0) {
        return std::nullopt;
    }

    // Correctness: the capacity is guarunteed to be a multiple of 2
    //              in which case x % n is the same as X & (n - 1)
    u32 index = hash & (this->capacity - 1);
    while (true) {
        Entry *entry = &this->entries[index];
        if (entry->key == nullptr) {
            if (!is_tombstone(entry)) {
                return std::nullopt;
            }
        } else if (entry->key->len == len && entry->key->hash == hash &&
                   memcmp(entry->key->chars, chars, len) == 0) {
            return entry->key;
        }

        index = (index + 1) & (this->capacity - 1);
    }
}

std::optional<Value> Table::get(LambString *key) const {
    if (this->len == 0) {
        return std::nullopt;
    }

    Entry *entry = this->entry(key);
    if (entry->key == nullptr) {
        return std::nullopt;
    }

    return entry->val;
}

bool Table::insert(Vm &vm, LambString *key, Value value) {
    if (this->len + 1 > this->capacity * TABLE_MAX_LOAD) {
        i32 capacity = GROW_CAPACITY(this->capacity);
        table_adjust_capacity(vm, this, capacity);
    }

    Entry *entry = this->entry(key);
    bool is_new = entry->key == nullptr;
    if (is_new && !is_tombstone(entry)) {
        this->len++;
    }

    entry->key = key;
    entry->val = value;

    return is_new;
}

bool Table::remove(LambString *key) const {
    if (this->len == 0) {
        return false;
    }

    Entry *entry = this->entry(key);
    if (entry->key == nullptr) {
        return false;
    }

    entry->key = nullptr;
    entry->val = TOMBSTONE;

    return true;
}

void Table::remove_marked() const {
    for (i32 i = 0; i < this->capacity; i++) {
        Entry *entry = &this->entries[i];
        if (entry->key != nullptr && !entry->key->obj.is_marked) {
            this->remove(entry->key);
        }
    }
}

std::string Table::to_string() const {
    std::ostringstream out;

    out << "Table: {\n";
    for (i32 i = 0; i < this->capacity; i++) {
        Entry *entry = &this->entries[i];
        if (entry->key != nullptr) {
            auto key = entry->key->obj.to_string();
            auto val = entry->val.to_string();

            out << "    " << key << ": " << val << ",\n";
        }
    }
    out << "}";

    return out.str();
}

#undef TOMBSTONE
