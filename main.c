#include <stdio.h>
#include <stdlib.h>
#include "./ast/ast.h"
#include "parsing/lexer.h"
#include "./compiling/chunk.h"
#include "./compiling/vm.h"
#include "./compiling/ast.h"
#include "./compiling/debug.h"

AstNode* get_node() {
	return new_astnode(AstntStmts);
}

void debug_compile_ast(AstNode* root, str name) {
		printf("\n");
	  printf("%s\n", name);
		print_ast(root, 0);
		printf("\n");
	
		Chunk chunk;
		chunk_init(&chunk);

		compile_ast(&chunk, root);
		chunk_write(&chunk, OpHalt);
	
		printf("\n");
		chunk_debug(&chunk, "Compiled Ast");

		Vm vm;
		vm_init_with_chunk(&vm, &chunk);
		vm_run(&vm);
	
		chunk_free(&chunk);
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
