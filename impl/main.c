#include "./ast/ast.h"
#include "./compile/ast.h"
#include "./compile/chunk.h"
#include "./debug/debug.h"
#include "./vm/vm.h"
#include "parsing/lexer.h"
#include <stdio.h>
#include <stdlib.h>

typedef struct CliOptions {
    i32 debug_level;
    i32 gc_debug_level;
    i32 optimization_level;
    str path;
} CliOptions;

extern void pretty_print(AstNode* root);
extern CliOptions parse_options(void);
extern void drop_options(CliOptions);

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
    CliOptions cli_options = parse_options();

    FILE *file = stdin;
    if (cli_options.path != NULL) {
        file = fopen(cli_options.path, "r");
        if (!file) {
            fprintf(stderr, "Unable to open '%s'. Exiting...\n", cli_options.path);
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
        return EXIT_FAILURE;
    } else if (root == NULL || *root == NULL) {
        // If the user immediately Ctrl-D without providing input that can be
        // parsed.
        free(root);
        printf("Exiting...\n");
        return EXIT_SUCCESS;
    } else {
        VmOptions options = {
            .print_fn_chunks = cli_options.debug_level >= 1,
            .print_main_chunk = cli_options.debug_level >= 1,
            .print_ast = cli_options.debug_level == 2,
            .optimized = cli_options.optimization_level > 0,
        };

        compile_with_options(*root, options);

        free_ast(*root);
        free(root);
    }

    if (file != stdin && file != NULL) {
        fclose(file);
        drop_options(cli_options);
    }

    return 0;
}
