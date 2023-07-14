#ifndef GC_HEADER
#define GC_HEADER

#include "../types.hpp"
#include "compiler.hpp"
#include "object.hpp"
#include "table.hpp"
#include <vector>

// Must always be in multiples of two due to table :D
#define GROW_CAPACITY(capacity) ((capacity) < 8 ? 8 : (capacity)*2)

class MarkAndSweep {
    void collect(Vm& vm); 

    void mark_roots(Vm& vm);

    void mark_compiler(Vm& vm, Compiler* compiler);

    void mark_table(Vm& vm, Table& table);

    void mark_object(Vm& vm, Object* object);

    void mark_value(Vm& vm, Value* value);
  
    void trace_refs(Vm&);

    void sweep_unused(Vm&);

  public:
    void* alloc(Vm& vm, size_t t_size, size_t count);

    void* grow_array(Vm& vm, void*, size_t t_size, size_t old_count, size_t new_count);

    void free(Vm& vm, void* ptr, size_t t_size);

    void free_array(Vm& vm, void* ptr, size_t t_size, size_t len);
};

#endif
