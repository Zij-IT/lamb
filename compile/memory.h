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

#define FREE_ARRAY(type, pointer, oldCount) \
    reallocate(NULL, pointer, sizeof(type) * (oldCount), 0)

// NOTE: Function requires access to Vm
// TODO: Add Vm parameter to function: reallocate(Vm *vm, void* ptr, size_t old_size, size_t new_size)
void* reallocate(Vm* vm, void* pointer, size_t oldSize, size_t newSize);

// NOTE: Function requires access to Vm
// TODO: Add Vm parameter to function: mark_object(Vm *vm, Object *object)
void mark_object(Object* obj);

// NOTE: Function requires access to Vm
// TODO: Add Vm parameter to function: mark_value(Vm *vm, Value *value)
void mark_value(Value* value);

// NOTE: Function requires access to Vm
// TODO: Add Vm parameter to function: mark_table(Vm *vm, Table *table)
void mark_table(Table* table);

void collect_garbage(Vm* vm);

#endif//MEMORY_HEADER

