#include <iostream>

#include "ast/ast.hpp"
#include "compile/ast.hpp"
#include "compile/chunk.hpp"
#include "compile/compiler.hpp"
#include "compile/object.hpp"
#include "compile/value.hpp"
#include "vm/vm.hpp"

extern "C" void run_ast(AstNode *root, bool print_fns, bool print_main) {
    // Occurs when with an empty script
    if (root == nullptr) {
        return;
    }

    VmOptions options = {
        .print_main_chunk = print_main,
        .print_fn_chunks = print_fns,
        .print_ast = false,
        .optimized = false,
    };

    Vm vm(options);

    Block block = {.prev = nullptr, .base = 0, .offset = 1, .depth = 0};
    Compiler compiler(vm, nullptr, &block, FtScript, "", 0);
    vm.curr_compiler = &compiler;

    CompileAstResult car = compile(vm, &compiler, root);

    if (car == CarOk) {
        compiler.chunk().write(vm, OpReturn);
        auto *closure = LambClosure::alloc(vm, compiler.function);
        vm.push_stack(Value::from_obj((Object *)closure));

        Callframe *frame = &vm.frames[0];
        vm.frame_count++;
        frame->closure = closure;
        frame->ip = compiler.function->chunk.bytes.as_raw();
        frame->slots = (Value*)vm.stack;

        if (options.print_main_chunk) {
            std::cerr << "\n\n" << compiler.chunk().to_string() << '\n';
        }

        vm.run();
        std::cout << '\n';
    } else {
        std::cerr << "Lamb: Compilation error... no more details :/" << '\n';
    }

    compiler.destroy(vm);
    vm.destroy();
}
