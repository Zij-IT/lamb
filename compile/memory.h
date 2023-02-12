#ifndef MEMORY_HEADER
#define MEMORY_HEADER

#include <stdlib.h>

#include "value.h"

typedef struct Vm Vm;
typedef struct Table Table;

#define ALLOCATE(vm, type, count) \
    (type*)reallocate(vm, NULL, 0, sizeof(type) * (count))

#define FREE(vm, type, pointer) reallocate(vm, pointer, sizeof(type), 0)

// Must always be in multiples of two due to table :D
#define GROW_CAPACITY(capacity) \
    ((capacity) < 8 ? 8 : (capacity) * 2)

#define GROW_ARRAY(vm, type, pointer, oldCount, newCount) \
    (type*)reallocate(vm, pointer, sizeof(type) * (oldCount), \
        sizeof(type) * (newCount))

#define FREE_ARRAY(vm, type, pointer, oldCount) \
    reallocate(vm, pointer, sizeof(type) * (oldCount), 0)

void* reallocate(Vm* vm, void* pointer, size_t oldSize, size_t newSize);

void mark_object(Vm* vm, Object* obj);

void mark_value(Vm* vm, Value* value);

void mark_table(Vm* vm, Table* table);

void collect_garbage(Vm* vm);

#endif//MEMORY_HEADER

