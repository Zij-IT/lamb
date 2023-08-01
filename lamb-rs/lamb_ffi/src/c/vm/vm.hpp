#ifndef VM_HEADER
#define VM_HEADER

#include <limits>

#include "../compile/compiler.hpp"
#include "../compile/gc.hpp"
#include "../compile/object.hpp"
#include "../compile/table.hpp"
#include "../compile/value.hpp"
#include "../types.hpp"

#define MAX_FRAMES 1024
#define MAX_VALUES (MAX_FRAMES * std::numeric_limits<u8>::max())

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

    explicit Vm(VmOptions options);

    void destroy();

    void push_stack(Value val);

    Value pop_stack();

    InterpretResult run();

  private:
    [[nodiscard]] constexpr Value* peek_stack(u8 n = 0) const;
    
    LambUpvalue *capture_upvalue(Value *local);
};

#endif // VM_HEADER
