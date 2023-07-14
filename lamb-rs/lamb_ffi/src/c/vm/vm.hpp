#ifndef VM_HEADER
#define VM_HEADER

#include <vector>

#include "../compile/chunk.hpp"
#include "../compile/compiler.hpp"
#include "../compile/table.hpp"
#include "../compile/gc.hpp"

#define MAX_FRAMES 1024
#define MAX_VALUES (MAX_FRAMES * UINT8_MAX)

enum InterpretResult {
    InterpretOk,
    InterpretRuntimeError,
};

struct VmOptions {
    bool print_main_chunk;
    bool print_fn_chunks;
    bool print_ast;
    bool optimized;
};

struct Callframe {
    LambClosure *closure;
    Value *slots;
    u8 *ip;
};

struct Vm {
    Table strings;
    Table globals;

    MarkAndSweep gc;
    Compiler *curr_compiler;

    Value *stack_top;
    Value stack[MAX_VALUES];
    Value saved_value;

    Callframe frames[MAX_FRAMES];
    u16 frame_count;

    LambUpvalue *open_upvalues;

    VmOptions options;
};

void vm_init(Vm& vm, VmOptions options);

void vm_free(Vm& vm);

void vm_push_stack(Vm& vm, Value val);

Value vm_pop_stack(Vm& vm);

InterpretResult vm_run(Vm& vm);

#endif // VM_HEADER
