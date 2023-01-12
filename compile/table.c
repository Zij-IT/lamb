#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "debug.h"
#include "table.h"
#include "../memory.h"

#define TOMBSTONE new_boolean(true)

static void table_adjust_capacity(Table* table, i32 capacity) {
  Entry* entries = ALLOCATE(Entry, capacity);
  
  // This table is required due to table_find call below, and has no other function
  // hence the length being set to 0 despite entries being filled
  Table temp_table = { .capacity = capacity, .entries = entries, .len = 0 };

  for (i32 i = 0; i < capacity; i++) {
    entries[i].key = NULL;
    // This is a dummy value and should probably be replaced with something
    // in the future. This is a poor man's Option::None
    entries[i].val = new_boolean(false);
  }
  
  table->len = 0;
  for (i32 i = 0; i < table->capacity; i++) {
    Entry* entry = &table->entries[i];
    if (entry->key == NULL) {
      continue;
    }
    
    Entry* dest = table_find(&temp_table, entry->key);
    dest->key = entry->key;
    dest->val = entry->val;
    table->len++;
  }
  
  FREE_ARRAY(Entry, table->entries, table->capacity);
  table->entries = entries;
  table->capacity = capacity;
}

static bool is_tombstone(Entry* entry) {
  return is_bool(entry->val) && entry->val.as.boolean;
}

void table_init(Table* table) {
  table->capacity = 0;
  table->len = 0;
  table->entries = NULL;
}

void table_copy_entries(Table* from, Table* to) {
  for (i32 i = 0; i < from->capacity; i++) {
    Entry* entry = &from->entries[i];
    if (entry->key != NULL) {
      table_insert(to, entry->key, entry->val);
    }
  }
}

bool table_insert(Table* table, LambString* key, Value val) {
  // printf("Inserting into table key: '%s'\n", key->chars);
  if (table->len + 1 > table->capacity * TABLE_MAX_LOAD) {
    i32 capacity = GROW_CAPACITY(table->capacity);
    table_adjust_capacity(table, capacity);
  }

  Entry* entry = table_find(table, key);
  bool is_new = entry->key == NULL;
  if (is_new && !is_tombstone(entry)) {
    table->len++;
  }
  
  entry->key = key;
  entry->val = val;
  
  return is_new;
}

bool table_remove(Table* table, LambString* key) {
  if (table->len == 0) {
    return false;
  }
  
  Entry* entry = table_find(table, key);
  if (entry->key == NULL) {
    return false;
  }
  
  entry->key = NULL;
  entry->val = TOMBSTONE;
  
  return true;
}

bool table_get(Table* table, LambString* key, Value* out) {
  if (table->len == 0) {
    return false;
  }
  
  Entry* entry = table_find(table, key);
  if (entry->key == NULL) {
    return false;
  }
  
  *out = entry->val;
  return true;
}

Entry* table_find(Table* table, LambString* key) {
  // This works because capacity is guarunteed to be a multiple of 2
  u32 index = key->hash & (table->capacity - 1);
  Entry* tombstone = NULL; 

  while(true) {
    Entry* entry = &table->entries[index];
    if (entry->key == NULL) {
      if (is_tombstone(entry)) {
        if (tombstone == NULL) {
          tombstone = entry;
        }
      } else {
        return tombstone != NULL ? tombstone : entry;
      }
    } else if (entry->key == key) {
      return entry;
    }
    
    index = (index + 1) & (table->capacity - 1);
  }
}

LambString* table_find_string(Table* table, str chars, i32 len, u32 hash) {
  if (table->len == 0) {
    // printf("No match found. Not interned\n");
    return NULL;
  }
  
  // Test: Print all entries in table
  // printf("Table: {\n");
  // for(int i = 0; i < table->capacity; i++) {
  //   if(table->entries[i].key != NULL) {
  //     LambString* st = table->entries[i].key;
  //     printf("  key: LambString { chars: %s, len: %u, hash: %u }\n", st->chars, st->len, st->hash);
  //   }
  // }
  // printf("}\n");
  
  // Correctness: Table capacity is guarunteed to be a multiple of 2
  //              in which case x % n is the same as X & (n - 1)
  u32 index = hash & (table->capacity - 1);
  // printf("Searching: LambString { chars: %s, len: %u, hash: %u }\n", chars, len, hash);
  while(true) {
    Entry* entry = &table->entries[index];
    if (entry->key != NULL) {
      // printf("Entry: { key: LambString { chars: %s, len: %u, hash: %u }, val: __ }\n", entry->key->chars, entry->key->len, entry->key->hash);    
    }    

    if (entry->key == NULL) {
      if (!is_tombstone(entry)) { 
        // printf("No match found. Not interned.\n");
        return NULL;
      }
    } else if (entry->key->len == len && entry->key->hash == hash && memcmp(entry->key->chars, chars, len) == 0) {
        // printf("Match found. Interned.\n");
      return entry->key;
    }
    
    index = (index + 1) & (table->capacity - 1);
  }
}

void table_free(Table* table) {
  FREE_ARRAY(Entry, table->entries, table->capacity);
  table_init(table);
}
