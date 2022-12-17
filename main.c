#include <stdio.h>
#include <stdlib.h>
#include "parsing/lexer.h"

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

	ParseResult res = yyparse();

	switch(res) {
      case ParseResultAccept: printf("Word accepted"); break;
      case ParseResultReject: printf("Word rejected");  break;
  }
	
	if (file != stdin && file != NULL) {
		fclose(file);
	}
}
