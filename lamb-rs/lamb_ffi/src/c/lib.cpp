#include "./ast/ast.hpp"
#include "./compile/ast.hpp"
#include "./compile/chunk.hpp"
#include "./debug/debug.hpp"
#include "./vm/vm.hpp"
#include <stdio.h>
#include <stdlib.h>

extern "C" void run_ast(AstNode *root, bool print_fns, bool print_main) {
    // Occurs when with an empty script
    if (root == NULL) {
        return;
    }

    VmOptions options = {
        .print_main_chunk = print_main,
        .print_fn_chunks = print_fns,
        .print_ast = false,
        .optimized = false,
    };

    Vm vm(options);

    Block block = {.prev = NULL, .base = 0, .offset = 1, .depth = 0};
    Compiler compiler(vm, nullptr, &block, FtScript, "", 0);

    vm.curr_compiler = &compiler;
    vm.push_stack(Value::from_obj((Object *)compiler.function));

    CompileAstResult car = compile(vm, &compiler, root);

    if (car == CarOk) {
        compiler.chunk().write(vm, OpReturn);
        auto closure = LambClosure::alloc(vm, compiler.function);
        vm.pop_stack();
        vm.push_stack(Value::from_obj((Object *)closure));

        Callframe *frame = &vm.frames[vm.frame_count++];
        frame->closure = closure;
        frame->ip = compiler.function->chunk.bytes.as_raw();
        frame->slots = vm.stack;

        if (options.print_main_chunk) {
            chunk_debug(compiler.chunk(), "Script Chunk");
        }

        vm.run();
        printf("\n");
    } else {
        printf("Lamb: Your source code contains code that is not able to be "
               "compiled.\n");
    }

    compiler.destroy(vm);
    vm.destroy();
}
