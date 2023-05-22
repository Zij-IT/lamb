#include "./ast/ast.h"
#include "./compile/ast.h"
#include "./compile/chunk.h"
#include "./debug/debug.h"
#include "./vm/vm.h"
#include "parsing/lexer.h"
#include <getopt.h>
#include <stdio.h>
#include <stdlib.h>

extern void pretty_print(AstNode* root);

void compile_with_options(AstNode *root, VmOptions options) {
    if (options.optimized) {
        optimize_ast(root);
    }

    if (options.print_ast) {
        pretty_print(root);
    }

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

int main(int argc, char **argv) {
    bool print_fn_chunks = false;
    bool print_main_chunk = false;
    bool optimized = false;
    bool print_ast = false;
    int c = 0;

    while ((c = getopt(argc, argv, "d:oh")) != -1) {
        switch (c) {
            case 'd': {
                i32 debug_level = atoi(optarg);
                switch (debug_level) {
                    case 3:
                        print_main_chunk = print_fn_chunks = print_ast = true;
                        break;
                    case 2:
                        print_main_chunk = print_fn_chunks = true;
                        break;
                    case 1:
                        print_main_chunk = true;
                        break;
                    default:
                        break;
                }
                break;
            }
            case 'o': {
                optimized = true;
                break;
            }
            default: {
                fprintf(stderr, "Usage: %s [-d debug_level] [-o] [file]\n", argv[0]);
                exit(EXIT_FAILURE);
            }
        }
    }

    FILE *file = stdin;
    if (optind < argc) {
        file = fopen(argv[optind], "r");
        if (!file) {
            fprintf(stderr, "Unable to open '%s'. Exiting...\n", optarg);
            exit(EXIT_FAILURE);
        }
    }

    if (file == stdin) {
        printf("~Lamb> Enter your code. Press Ctrl-D when finished.\n");
    }

    set_lexer_file(file);

    AstNode **root = malloc(sizeof(AstNode *));
    ParseResult res = yyparse(root);
    if (res == ParseResultReject) {
        printf("\nSyntax Error. Unfortunately I can't help you.\n");
        return 1;
    } else if (root == NULL || *root == NULL) {
        // If the user immediately Ctrl-D without providing input that can be
        // parsed.
        free(root);
        printf("Exiting...\n");
        return EXIT_SUCCESS;
    } else {
        pretty_print(*root);
        free_ast(*root);
        free(root);
    }

    // VmOptions options = {
    //     .print_fn_chunks = print_fn_chunks,
    //     .print_main_chunk = print_main_chunk,
    //     .print_ast = print_ast,
    //     .optimized = optimized,
    // };

    // compile_with_options(*root, options);

    if (file != stdin && file != NULL) {
        fclose(file);
    }

    return 0;
}
