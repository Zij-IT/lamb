#ifndef MEMORY_HEADER
#define MEMORY_HEADER

#include <stdlib.h>

#include "value.hpp"

struct Vm;
struct Table;

#define ALLOCATE(vm, type, count) ((type *)vm.gc.reallocate(vm, NULL, 0, sizeof(type) * (count)))

#define FREE(vm, type, pointer) (vm.gc.reallocate(vm, pointer, sizeof(type), 0))

// Must always be in multiples of two due to table :D
#define GROW_CAPACITY(capacity) ((capacity) < 8 ? 8 : (capacity)*2)

#define GROW_ARRAY(vm, type, pointer, oldCount, newCount)                                          \
    ((type *)vm.gc.reallocate(vm, pointer, sizeof(type) * (oldCount), sizeof(type) * (newCount)))

#define FREE_ARRAY(vm, type, pointer, oldCount)                                                    \
    ((type*)vm.gc.reallocate(vm, (void*)pointer, sizeof(type) * (oldCount), 0))

#endif // MEMORY_HEADER
