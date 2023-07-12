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

    Vm vm;
    vm_init(&vm, options);

    Block block = {.base = 0, .offset = 1, .depth = 0, .prev = NULL};
    Compiler compiler(&vm, nullptr, &block, FtScript, "", 0);

    vm.curr_compiler = &compiler;
    vm_push_stack(&vm, new_object((Object *)compiler.function));

    CompileAstResult car = compile(&vm, &compiler, root);

    if (car == CarOk) {
        chunk_write(&vm, &compiler.function->chunk, OpReturn);
        LambClosure *closure = to_closure(&vm, compiler.function);
        vm_pop_stack(&vm);
        vm_push_stack(&vm, new_object((Object *)closure));

        Callframe *frame = &vm.frames[vm.frame_count++];
        frame->closure = closure;
        frame->ip = compiler.function->chunk.bytes;
        frame->slots = vm.stack;

        if (options.print_main_chunk) {
            chunk_debug(&compiler.function->chunk, "Script Chunk");
        }

        vm_run(&vm);
        printf("\n");
    } else {
        printf("Lamb: Your source code contains code that is not able to be "
               "compiled.\n");
    }

    compiler.destroy(&vm);
    vm_free(&vm);
}