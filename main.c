#include <stdio.h>
#include <stdlib.h>
#include "./ast/ast.h"
#include "parsing/lexer.h"
#include "./compile/chunk.h"
#include "./compile/vm.h"
#include "./compile/ast.h"
#include "./compile/debug.h"

AstNode* get_node() {
	return new_astnode(AstntStmts);
}

void debug_compile_ast(AstNode* root, str name) {
		printf("\n");
	  printf("%s\n", name);
		print_ast(root, 0);
		printf("\n");
	
		Vm vm;
		vm_init(&vm);
	
		Compiler compiler;
		compiler_init(&compiler, FtScript);
		compiler.function = (LambFunc*)alloc_obj(&vm, OtFunc);
	
		CompileAstResult car = compile_to_chunk(&vm, &compiler, root);
		printf("\n");
	
		if (car == CarOk) {
			// Pull the function out from the compiler
			Callframe* frame = &vm.frames[vm.frame_count++];
			frame->function = compiler.function;
			frame->ip = compiler.function->chunk.bytes;
			frame->slots = vm.stack;
		
			chunk_write(&compiler.function->chunk, OpHalt);
			chunk_debug(&compiler.function->chunk, "Compiled Ast");

			// Must be done after chunk_write incase it forces a reallocation
			vm_reset_ip(&vm);
		
			return;
			if (InterpretOk == vm_run(&vm)) {
				printf("Lamb: Your flock has done their job successfully!\n");
			} else {
				printf("Lamb: Your flock didn't do well... sorry.\n");
			}
		} else {
			printf("Lamb: Your source code contains code that is not able to be compiled.\n");
		}

	  // This does not result in a double free of the function. The internal function of a 'Compiler'
	  // object is not freed in compiler_free because it is allocated via the vm
		compiler_free(&compiler);
		vm_free(&vm);
}

int main(int argc, char** argv) {
	FILE* file = stdin;
	if (argc == 2) {
		file = fopen(argv[1], "r");
		if (!file) {
			fprintf(stderr, "Unable to open '%s'. Exiting...\n", argv[1]);
			exit(1);
		}
	}
  
	if (file == stdin) {
		printf("~Lamb> Enter your code. Press Ctrl-D when finished.\n");
	}
  set_lexer_file(file);

	AstNode** root = malloc(sizeof(AstNode*));
	ParseResult res = yyparse(root);
	switch(res) {
      case ParseResultAccept: printf("\nWord accepted\n"); break;
      case ParseResultReject: printf("\nWord rejected\n"); return 1;
  }
	
	debug_compile_ast(*root, "Unoptimized AST");;
	
	optimize_ast(*root);
	debug_compile_ast(*root, "Optimized AST");
	
	if (file != stdin && file != NULL) {
		fclose(file);
	}
	
	free_ast(*root);
	free(root);
	
	return 0;
}
