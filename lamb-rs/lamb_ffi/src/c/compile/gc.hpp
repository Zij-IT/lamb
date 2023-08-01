#ifndef GC_HEADER
#define GC_HEADER

#include <cstdlib>
#include <vector>

#include "../types.hpp"
#include "compiler.hpp"
#include "object.hpp"
#include "table.hpp"
#include "value.hpp"

// Must always be in multiples of two due to table :D
#define GROW_CAPACITY(capacity) ((capacity) < 8 ? 8 : (capacity)*2)

class MarkAndSweep {
    std::vector<Object *> gray_stack;
    Object *objects = nullptr;
    u64 bytes_allocated = 0;
    u64 next_collection = 1024 * 1024;

    void collect(Vm &vm);

    void mark_roots(Vm &vm);

    void mark_compiler(Compiler *compiler);

    void mark_table(Table &table);

    void mark_object(Object *object);

    void mark_value(Value *value);

    void trace_refs();

    void sweep_unused(Vm &);

  public:
    void *alloc(Vm &vm, size_t t_size, size_t count);

    void *grow_array(Vm &vm, void *, size_t t_size, size_t old_count, size_t new_count);

    void add_object(Object *obj);

    void free(void *ptr, size_t t_size);

    void free_array(void *ptr, size_t t_size, size_t len);

    void destroy(Vm &);
};

#endif
