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

    Vm(VmOptions options);

    void destroy();

    void push_stack(Value val);

    Value pop_stack();

    InterpretResult run();

  private:
    Value* peek_stack(u8 n = 0);
    
    void drop_top_n(u8 n);

    LambUpvalue *capture_upvalue(Value *local);

    void close_upvalues(Value *last);
};

#endif // VM_HEADER
