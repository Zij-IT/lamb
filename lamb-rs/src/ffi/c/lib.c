#include "./ast/ast.h"
#include "./compile/ast.h"
#include "./compile/chunk.h"
#include "./debug/debug.h"
#include "./vm/vm.h"
#include "parsing/lexer.h"
#include <stdio.h>
#include <stdlib.h>

void run_ast(AstNode *root, bool print_fns, bool print_main) {
    VmOptions options = { 
        .print_main_chunk = print_main,
        .print_fn_chunks = print_fns,
        .print_ast = false,
        .optimized = false,
    };
    
    Vm vm;
    vm_init(&vm, options);

    Compiler compiler;
    vm.curr_compiler = &compiler;
    compiler_init(&vm, &compiler, FtScript);
    compiler.function = (LambFunc *)alloc_obj(&vm, OtFunc);
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

    compiler_free(&vm, &compiler);
    vm_free(&vm);
}

AstNode* parse_or_die(void) {
    AstNode* root = NULL;
    ParseResult res = yyparse(&root);
    if (res == ParseResultReject) {
        fprintf(stderr, "\nSyntax Error. Sorry.");
        exit(EXIT_FAILURE);
    }

    return root;
}

AstNode* parse_stdin(void) {
    set_lexer_file(stdin);
    printf("~Lamb> Enter your code. Press Ctrl-D when finished.\n");
    return parse_or_die();
}

AstNode* parse_path(str path) {
    if (path == NULL) {
        return NULL;
    }

    FILE *file = fopen(path, "r");
    if (file == NULL) {
        fprintf(stderr, "Unable to open '%s'. Exiting...\n", path);
        exit(EXIT_FAILURE);
    }
    set_lexer_file(file);

    AstNode* root = parse_or_die();
    fclose(file);
    return root;
}
